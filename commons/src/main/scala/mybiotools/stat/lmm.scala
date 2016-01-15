/* 
* The MIT License
*
* Copyright (c) 2015 ECOLE POLYTECHNIQUE FEDERALE DE LAUSANNE, Switzerland, 
* Group Fellay
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the "Software"),
* to deal in the Software without restriction, including without limitation 
* the rights to use, copy, modify, merge, publish, distribute, sublicense, 
* and/or sell copies of the Software, and to permit persons to whom the Software
* is furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
* SOFTWARE.
*/

package mybiotools.stat

import org.saddle._
import mybiotools._
import mybiotools.stat.LinearRegression._
import mybiotools.eq._
import org.apache.commons.math3.random.{ RandomGenerator, RandomDataGenerator, Well44497b }
import org.ejml.simple.SimpleMatrix
import org.ejml.data.DenseMatrix64F
import scala.util._

case class VarianceComponentResult(fixed: Vec[Double], varianceComponents: Vec[Double], varianceOfFixed: Mat[Double], varianceOfVC: Mat[Double], logL: LogLikelihood, predicted: Vec[Double]) {
  def varianceComponentsRelativeToTotal = {
    //  Using Variance of ratios http://stats.stackexchange.com/a/19580
    // Using the property that covariance is distributive over summation
    val meanTotalExceptFirst = varianceComponents.toSeq.drop(1).sum
    val meanTotal = varianceComponents.sum

    val varianceOfTotal = varianceOfVC.toVec.sum
    val varianceOfTotalExceptFirst = varianceOfVC.withoutRows(0).withoutCols(0).toVec.sum
    val covarianceOfTotalAndTotalExceptFirst = varianceOfVC.withoutCols(0).toVec.sum

    val meanTotalExceptFirstOverTotal = meanTotalExceptFirst / meanTotal

    val varianceOfTotalExceptFirstOverTotal = {
      val ex = meanTotalExceptFirst
      val ey = meanTotal
      val vx = varianceOfTotalExceptFirst
      val vy = varianceOfTotal
      val cvxy = covarianceOfTotalAndTotalExceptFirst
      (ex / ey) * (ex / ey) * (vx / (ex * ex) + vy / (ey * ey) - 2 * cvxy / (ex * ey))
    }

    Effect(meanTotalExceptFirstOverTotal, math.sqrt(varianceOfTotalExceptFirstOverTotal))

  }
}

object VarianceComponentModel {

  def trace(m: Mat[Double]) = (0 until (math.min(m.numRows, m.numCols)) map { i => m.raw(i, i) }) sum

  private def toEjml(m: Mat[Double]) = SimpleMatrix.wrap(DenseMatrix64F.wrap(m.numRows, m.numCols, m.contents))

  private def invertPD(m: Mat[Double]) = invert(m) // Mat(m.numRows, m.numCols, invertPositiveDefinite(toEjml(m)).get.getMatrix.getData)

  private def invert(m: Mat[Double]) = {
    Mat(m.numRows, m.numCols, (toEjml(m).pseudoInverse).getMatrix.getData)
  }

  private def det(m: Mat[Double]) = toEjml(m).determinant

  def reml_bic_direct(
    y: Vec[Double],
    fixed: Mat[Double],
    kernels: IndexedSeq[Mat[Double]],
    rel: Double,
    abs: Double,
    maxit: Int
  ) = {

    val xt = fixed.T

    def v(sigmas: Vec[Double]): Mat[Double] = sigmas.toSeq.zipWithIndex.map {
      case (s, idx) =>
        kernels(idx) * s
    }.reduce(_ + _)

    def p(vinv: Mat[Double]) = {
      vinv - (vinv mult fixed mult invert(xt mult vinv mult fixed) mult xt mult vinv)
    }

    // This is from the GLS solution or eq 6.24 of the book
    def fixedEffectHatMatrix(sigmas: Vec[Double]): Mat[Double] = {
      val vinv = invertPD(v(sigmas))
      fixed mult invert(xt mult vinv mult fixed) mult xt mult vinv
    }

    // Section 6.6.a in book, premultiplied by Z
    def randomEffectHatMatrix(sigmas: Vec[Double]): Mat[Double] =
      if (sigmas.length == 1) mat.diag(vec.zeros(fixed.numRows))
      else {
        val vinv = invertPD(v(sigmas))
        val pp = p(vinv)
        val m1 = (kernels zip sigmas.toSeq).drop(1).map {
          case (k, s) =>
            k * s
        } reduce (_ + _)
        m1 mult pp
      }

    def hatMatrix(sigmas: Vec[Double]) = fixedEffectHatMatrix(sigmas) + randomEffectHatMatrix(sigmas)

    def bic(sigmas: Vec[Double]) = {
      val vv = v(sigmas)
      val pp = p(invertPD(vv))
      val l = (
        ((Mat(y).T mult pp).row(0) dot y) +
        math.log(det(vv)) +
        math.log(det(xt mult invertPD(vv) mult fixed)) //+ 
      // fixed.numRows * math.log(2 * math.Pi)
      ) * (-0.5)

      val df = trace(hatMatrix(sigmas))

      -2 * l + df * math.log(fixed.numRows)

    }

    import org.apache.commons.math3.analysis.{ UnivariateFunction, MultivariateFunction }
    import org.apache.commons.math3.optim.univariate.{ BrentOptimizer, SearchInterval, UnivariateObjectiveFunction, MultiStartUnivariateOptimizer, UnivariateOptimizer }
    import org.apache.commons.math3.optim.nonlinear.scalar._
    import org.apache.commons.math3.optim.nonlinear.scalar.GoalType
    import org.apache.commons.math3.optim.{ MaxEval, MaxIter }
    import org.apache.commons.math3.random.JDKRandomGenerator
    import org.apache.commons.math3.random.SobolSequenceGenerator
    import org.apache.commons.math3.optim.nonlinear.scalar.noderiv._
    import org.apache.commons.math3.optim.InitialGuess
    import org.apache.commons.math3.optim.SimpleBounds
    import org.apache.commons.math3.optim.nonlinear.scalar.MultivariateFunctionMappingAdapter
    import org.apache.commons.math3.random.MersenneTwister

    import org.apache.commons.math3.optim.nonlinear.scalar.noderiv.CMAESOptimizer
    import org.apache.commons.math3.optim.SimpleValueChecker
    import org.apache.commons.math3.random.{ RandomGenerator, RandomDataGenerator, Well44497b }

    class TargetF extends MultivariateFunction {
      def value(x: Array[Double]) = {
        val r = bic(Vec(x))
        r
      }
    }

    val populationSize = 4 + 10 * (math.log(kernels.size).toInt + 1)

    // val init = reml_nr(
    //   y,
    //   fixed,
    //   kernels,
    //   rel,
    //   abs,
    //   maxit).toOption.map(_.varianceComponents).getOrElse(vec.ones(kernels.size))

    val init = vec.ones(kernels.size)

    val rng = new Well44497b()

    // val optimizer = new CMAESOptimizer(maxit, 0.0, true, (maxit * 0.1).toInt, 100, rng, false, new SimpleValueChecker(rel, abs, maxit))
    val optimizer = new SimplexOptimizer(rel, abs)
    val optim = optimizer.optimize(
      new ObjectiveFunction(
        new MultivariateFunctionPenaltyAdapter(
          new TargetF,
          vec.zeros(kernels.size),
          vec.ones(kernels.size) * 1000,
          1E15,
          Array.fill(kernels.size)(1E7)
        )
      ),
      GoalType.MINIMIZE,
      new NelderMeadSimplex(kernels.size),
      new CMAESOptimizer.Sigma((vec.ones(kernels.size) * 500).contents),
      new CMAESOptimizer.PopulationSize(populationSize),
      new InitialGuess(init),
      // new SimpleBounds(vec.zeros(kernels.size).contents, (vec.ones(kernels.size) * 5000).contents),
      new MaxIter(maxit),
      new MaxEval(maxit * 10)
    )

    computeVarianceOfEstimates(y, fixed, kernels, Vec(optim.getPoint))

  }

  def reml_direct(
    y: Vec[Double],
    fixed: Mat[Double],
    kernels: IndexedSeq[Mat[Double]],
    rel: Double,
    abs: Double,
    maxit: Int
  ) = {

    val xt = fixed.T

    def v(sigmas: Vec[Double]): Mat[Double] = sigmas.toSeq.zipWithIndex.map {
      case (s, idx) =>
        kernels(idx) * s
    }.reduce(_ + _)

    def p(vinv: Mat[Double]) = {
      vinv - (vinv mult fixed mult invert(xt mult vinv mult fixed) mult xt mult vinv)
    }

    def restrictedLikeL(sigmas: Vec[Double]) = {
      val vv = v(sigmas)
      val pp = p(invertPD(vv))
      (
        ((Mat(y).T mult pp).row(0) dot y) +
        math.log(det(vv)) +
        math.log(det(xt mult invertPD(vv) mult fixed)) //+ 
      // fixed.numRows * math.log(2 * math.Pi)
      ) * (-0.5)
    }

    import org.apache.commons.math3.analysis.{ UnivariateFunction, MultivariateFunction }
    import org.apache.commons.math3.optim.univariate.{ BrentOptimizer, SearchInterval, UnivariateObjectiveFunction, MultiStartUnivariateOptimizer, UnivariateOptimizer }
    import org.apache.commons.math3.optim.nonlinear.scalar._
    import org.apache.commons.math3.optim.nonlinear.scalar.GoalType
    import org.apache.commons.math3.optim.{ MaxEval, MaxIter }
    import org.apache.commons.math3.random.JDKRandomGenerator
    import org.apache.commons.math3.random.SobolSequenceGenerator
    import org.apache.commons.math3.optim.nonlinear.scalar.noderiv._
    import org.apache.commons.math3.optim.InitialGuess
    import org.apache.commons.math3.optim.SimpleBounds
    import org.apache.commons.math3.optim.nonlinear.scalar.MultivariateFunctionMappingAdapter
    import org.apache.commons.math3.random.MersenneTwister

    import org.apache.commons.math3.optim.nonlinear.scalar.noderiv.CMAESOptimizer
    import org.apache.commons.math3.optim.SimpleValueChecker
    import org.apache.commons.math3.random.{ RandomGenerator, RandomDataGenerator, Well44497b }

    class TargetF extends MultivariateFunction {
      def value(x: Array[Double]) = {
        val r = restrictedLikeL(Vec(x))
        r * (-1)
      }
    }

    val populationSize = 4 + 10 * (math.log(kernels.size).toInt + 1)

    val init = reml_nr(
      y,
      fixed,
      kernels,
      rel,
      abs,
      maxit
    ).toOption.map(_.varianceComponents).getOrElse(vec.ones(kernels.size))

    // val init = vec.ones(kernels.size)

    val rng = new Well44497b()

    // val optimizer = new CMAESOptimizer(maxit, 0.0, true, (maxit * 0.1).toInt, 100, rng, false, new SimpleValueChecker(rel, abs, maxit))
    val optimizer = new SimplexOptimizer(rel, abs)
    val optim = optimizer.optimize(
      new ObjectiveFunction(
        new MultivariateFunctionPenaltyAdapter(
          new TargetF,
          vec.zeros(kernels.size),
          vec.ones(kernels.size) * 5000,
          1E15,
          Array.fill(kernels.size)(1E7)
        )
      ),
      GoalType.MINIMIZE,
      new NelderMeadSimplex(kernels.size),
      new CMAESOptimizer.Sigma((vec.ones(kernels.size) * 500).contents),
      new CMAESOptimizer.PopulationSize(populationSize),
      new InitialGuess(init),
      // new SimpleBounds(vec.zeros(kernels.size).contents, (vec.ones(kernels.size) * 5000).contents),
      new MaxIter(maxit),
      new MaxEval(maxit * 10)
    )

    computeVarianceOfEstimates(y, fixed, kernels, Vec(optim.getPoint))

  }

  /**
   * Newton-Raphson iterations on the restricted likelihood
   *
   * reference: Generalized, Linear, and Mixed Models - McCulloch, Searle, Neuhaus, 2nd ed.
   * page 324, section 14.2.c
   * DOI: 10.1111/j.1467-842X.1990.tb01015.x
   *
   * @param y outcome vetor
   * @param fixed design matrix of fixed effects
   * @param kernels vector of PD kernel matrices
   * @return vector or variance components
   */
  def reml_nr(
    y: Vec[Double],
    fixed: Mat[Double],
    kernels: IndexedSeq[Mat[Double]],
    rel: Double,
    abs: Double,
    maxit: Int
  ): Try[VarianceComponentResult] = {

    val xt = fixed.T

    // book Appendix M.4.f
    val k = mat.diag(vec.ones(fixed.numRows)) - (fixed mult invertPD(xt mult fixed) mult xt)

    def v(sigmas: Vec[Double]): Mat[Double] = sigmas.toSeq.zipWithIndex.map {
      case (s, idx) =>
        kernels(idx) * s
    }.reduce(_ + _)

    def p(vinv: Mat[Double]) = {
      vinv - (vinv mult fixed mult invert(xt mult vinv mult fixed) mult xt mult vinv)
    }

    // Differentiating eq 6.68 with replacement of 6.66 using results from Chapter 14, Appendix, and section 6.11.c
    def firstDerivative(sigmas: Vec[Double], vv: Mat[Double], ivv: Mat[Double], pp: Mat[Double]): Vec[Double] = {

      Vec(sigmas.toSeq.zipWithIndex.map {
        case (_, i) =>
          (
            ((Mat(y).T mult (pp mult kernels(i) mult pp)).row(0) dot y) -
            trace(invertPD(k.T mult vv mult k) mult k.T mult kernels(i) mult k)
          ) * 0.5
      }: _*)

    }

    // Differentiating eq 6.68 with substitutions of 6.66 using results from Chapter 14, Appendix, and section 6.11.c
    // This seems to be unstable so I replaced with the one found in http://www.ltcc.ac.uk/courses/REML/REML_and_LMM_ltcc1.pdf
    // def secondDerivativeElement(sigmas: Vec[Double], i1: Int, i2: Int): Double = {
    //   val vv = v(sigmas)

    //   val vinv = invertPD(vv)
    //   val pp = p(vinv)

    //   def pzpzp(i: Int, j: Int) = (Mat(y).T mult pp mult kernels(i) mult pp mult kernels(j) mult pp).row(0) dot y

    //   pzpzp(i1, i2) * 0.5 +
    //     pzpzp(i2, i1) * 0.5 +        
    //   (trace(invertPD(k.T mult vv mult k) mult k.T mult kernels(i2) mult pp mult kernels(i1) mult k) * 0.5 * -1)
    // }

    // http://www.ltcc.ac.uk/courses/REML/REML_and_LMM_ltcc1.pdf, page 53, "Expected Information Matrix"
    // This does not involve (K' V K)
    def secondDerivativeElement(sigmas: Vec[Double], i1: Int, i2: Int, vv: Mat[Double], ivv: Mat[Double], pp: Mat[Double]): Double = {

      def pzpzp(i: Int, j: Int) = (Mat(y).T mult pp mult kernels(i) mult pp mult kernels(j) mult pp).row(0) dot y

      // pzpzp(i1, i2) +
      trace(pp mult kernels(i1) mult pp mult kernels(i2))
    }

    def hessian(sigmas: Vec[Double], vv: Mat[Double], ivv: Mat[Double], pp: Mat[Double]) = {
      val data = 0 until sigmas.length flatMap { i =>
        (0 until sigmas.length).map { j =>
          secondDerivativeElement(sigmas, i, j, vv, ivv, pp)
        }
      }

      Mat(sigmas.length, sigmas.length, data.toArray)
    }

    // profile ML (eq 14.22)
    // def profL(sigmas: Vec[Double]) = {
    //   val vv = v(sigmas)
    //   val vinv = invertPD(vv)
    //   val pp = p(vinv)
    //   0.5 * ((Mat(y).T mult pp).row(0) dot y * (-1) -
    //     math.log(det(vv)))
    // }

    // This is from Verbyla-90 because det(K' V K) is often negative if I follow the book
    def restrictedLikeL(sigmas: Vec[Double]) = {
      val vv = v(sigmas)
      val ivv = invertPD(vv)
      val pp = p(ivv)
      (
        ((Mat(y).T mult pp).row(0) dot y) +
        math.log(det(vv)) +
        math.log(det(xt mult ivv mult fixed)) //+ 
      // fixed.numRows * math.log(2 * math.Pi)
      ) * (-0.5)
    }

    def step(sigmas: Vec[Double]) = {
      val vv = v(sigmas)
      val ivv = invertPD(vv)
      val pp = p(ivv)
      sigmas + (invertPD(hessian(sigmas, vv, ivv, pp)) mult firstDerivative(sigmas, vv, ivv, pp)).col(0)
    }

    def stop(o1: Double, o2: Double, i: Int) =
      i >= maxit // ||
    // math.abs(o2 - o1) / math.abs(o1) <= rel ||
    // math.abs(o2 - o1) <= abs

    def loop(s: Vec[Double], i: Int, lastObj1: Double): Vec[Double] = {
      println(s.toSeq.toString + " " + i + " " + lastObj1)
      val next = step(s)
      val nextObj = -1d //restrictedLikeL(next)

      if (next.toSeq.exists(_.isNaN)) s
      else {
        if (stop(lastObj1, nextObj, i + 1)) next else {

          loop(next, i + 1, nextObj)
        }
      }
    }

    // val estimatedVC = loop(vec.ones(kernels.size), 0, restrictedLikeL(vec.ones(kernels.size)))
    val estimatedVC = loop(vec.ones(kernels.size), 0, -1d)

    if (estimatedVC.toSeq.exists(_ < 0.0)) Failure(new RuntimeException("Out of bounds: " + estimatedVC.toSeq.toString)) else {

      Success(computeVarianceOfEstimates(y, fixed, kernels, estimatedVC))
    }
  }

  def computeVarianceOfEstimates(
    y: Vec[Double],
    fixed: Mat[Double],
    kernels: IndexedSeq[Mat[Double]],
    estimatedVC: Vec[Double]
  ) = {

    val xt = fixed.T

    def v(sigmas: Vec[Double]): Mat[Double] = sigmas.toSeq.zipWithIndex.map {
      case (s, idx) =>
        kernels(idx) * s
    }.reduce(_ + _)

    def p(vinv: Mat[Double]) = {
      vinv - (vinv mult fixed mult invert(xt mult vinv mult fixed) mult xt mult vinv)
    }

    def restrictedLikeL(sigmas: Vec[Double]) = {
      val vv = v(sigmas)
      val pp = p(invertPD(vv))
      (
        ((Mat(y).T mult pp).row(0) dot y) +
        math.log(det(vv)) +
        math.log(det(xt mult invertPD(vv) mult fixed)) //+ 
      // fixed.numRows * math.log(2 * math.Pi)
      ) * (-0.5)
    }

    // This is from the GLS solution or eq 6.24 of the book
    def fixedCoef(sigmas: Vec[Double]) = {
      val vinv = invertPD(v(sigmas))
      invert(xt mult vinv mult fixed) mult xt mult vinv mult y
    }

    // This is from the GLS solution or eq 6.24 of the book
    def fixedEffectHatMatrix(sigmas: Vec[Double]): Mat[Double] = {
      val vinv = invertPD(v(sigmas))
      fixed mult invert(xt mult vinv mult fixed) mult xt mult vinv
    }

    // Section 6.6.a in book, premultiplied by Z
    def randomEffectHatMatrix(sigmas: Vec[Double]): Mat[Double] =
      if (sigmas.length == 1) mat.diag(vec.zeros(fixed.numRows))
      else {
        val vinv = invertPD(v(sigmas))
        val pp = p(vinv)
        val m1 = (kernels zip sigmas.toSeq).drop(1).map {
          case (k, s) =>
            k * s
        } reduce (_ + _)
        m1 mult pp
      }

    def hatMatrix(sigmas: Vec[Double]) = fixedEffectHatMatrix(sigmas) + randomEffectHatMatrix(sigmas)

    val vv = v(estimatedVC)

    val vinv = invertPD(v(estimatedVC))

    val pp = p(vinv)

    val fixedCoefficients = fixedCoef(estimatedVC)

    // Book Eq 6.63 and https://en.wikipedia.org/wiki/Generalized_least_squares#Properties
    val varianceOfFixedEstimates: Mat[Double] = invert(xt mult vinv mult fixed)

    val varianceOfVCEstimates: Mat[Double] = {
      /**
       * This is not written explictly in the book but derived from:
       * o Section 6.8.b: "Asymptotic sampling variances of ML estimators
       *     are obtained from the inverse of the information matrix"
       * o Information Matrix: Eq 6.74
       * o Derivative in the information matrix on page 183 dV/dsigma = ZZ'
       * o Substitutions in section 6.9.a eq 6.66
       * o Eq 6.77, also on page 177
       * o M4.f page 350
       * o In ML from the Information matrix and the derivation result this would be the variance of the estimates:
       *  trace(vinv mult kernels(i) mult vinv mult kernels(j))
       *  after applying the subsitutions for REML:
       *  `invert(k.T mult vv mult k) mult k.T mult kernels(i) mult pp mult kernels(j) mult k`
       * o However this seems to be numerically unstable, so I replaced it with the expected information matrix from http://www.ltcc.ac.uk/courses/REML/REML_and_LMM_ltcc1.pdf
       */

      def vari(i: Int, j: Int) = {

        trace(
          pp mult kernels(i) mult pp mult kernels(j)

        )
      }

      val m2 = {
        val data = 0 until kernels.length flatMap { i =>
          (0 until kernels.length).map { j =>
            vari(i, j)
          }
        }

        Mat(kernels.length, kernels.length, data.toArray)
      }
      invert(m2) * 2

    }

    val hat = hatMatrix(estimatedVC)

    val df = trace(hat)

    val predicted = (hat mult y).col(0)

    VarianceComponentResult(fixedCoefficients.col(0), estimatedVC, varianceOfFixedEstimates, varianceOfVCEstimates, LogLikelihood(restrictedLikeL(estimatedVC), df, fixed.numRows), predicted)
  }

}