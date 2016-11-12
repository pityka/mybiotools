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

package mybiotools

import org.ejml.data.DenseMatrix64F
import org.ejml.alg.dense.mult.VectorVectorMult
import org.ejml.ops.NormOps

import org.apache.commons.math3.random.MersenneTwister
import org.apache.commons.math3.distribution.{ PoissonDistribution, BinomialDistribution }
import org.apache.commons.math3.analysis.{ UnivariateFunction, MultivariateFunction }
import org.apache.commons.math3.optim.univariate.{ BrentOptimizer, SearchInterval, UnivariateObjectiveFunction, MultiStartUnivariateOptimizer }
import org.apache.commons.math3.optim.nonlinear.scalar._
import org.apache.commons.math3.optim.nonlinear.scalar.GoalType
import org.apache.commons.math3.optim.{ MaxEval, MaxIter }
import org.apache.commons.math3.random.JDKRandomGenerator
import org.apache.commons.math3.random.SobolSequenceGenerator
import org.apache.commons.math3.optim.nonlinear.scalar.noderiv._
import org.apache.commons.math3.optim.InitialGuess
import org.apache.commons.math3.optim.SimpleBounds
import org.apache.commons.math3.optim.nonlinear.scalar.MultivariateFunctionMappingAdapter
import org.apache.commons.math3.optim.nonlinear.scalar.MultivariateFunctionPenaltyAdapter
import org.apache.commons.math3.analysis.{ UnivariateFunction, MultivariateFunction }
import org.apache.commons.math3.optim.univariate.{ BrentOptimizer, SearchInterval, UnivariateObjectiveFunction, MultiStartUnivariateOptimizer }
import org.apache.commons.math3.optim.nonlinear.scalar._
import org.apache.commons.math3.optim.nonlinear.scalar.GoalType
import org.apache.commons.math3.optim.{ MaxEval, MaxIter }
import org.apache.commons.math3.random.JDKRandomGenerator
import org.apache.commons.math3.random.SobolSequenceGenerator
import org.apache.commons.math3.optim.nonlinear.scalar.noderiv._
import org.apache.commons.math3.optim.InitialGuess
import org.apache.commons.math3.optim.SimpleBounds
import org.apache.commons.math3.optim.nonlinear.scalar.MultivariateFunctionMappingAdapter
import org.apache.commons.math3.optim.nonlinear.scalar.MultivariateFunctionPenaltyAdapter
import org.apache.commons.math3.analysis.{ UnivariateFunction, MultivariateFunction }
import org.apache.commons.math3.optim.univariate.{ BrentOptimizer, SearchInterval, UnivariateObjectiveFunction, MultiStartUnivariateOptimizer }
import org.apache.commons.math3.optim.nonlinear.scalar._
import org.apache.commons.math3.optim.nonlinear.scalar.GoalType
import org.apache.commons.math3.optim.{ MaxEval, MaxIter }
import org.apache.commons.math3.random.JDKRandomGenerator
import org.apache.commons.math3.random.SobolSequenceGenerator
import org.apache.commons.math3.optim.nonlinear.scalar.noderiv._
import org.apache.commons.math3.optim.InitialGuess
import org.apache.commons.math3.optim.SimpleBounds
import org.apache.commons.math3.optim.nonlinear.scalar.MultivariateFunctionMappingAdapter
import org.apache.commons.math3.optim.univariate.BrentOptimizer
import org.apache.commons.math3.optim.univariate.MultiStartUnivariateOptimizer
import org.apache.commons.math3.random.Well19937a
import org.apache.commons.math3.optim.univariate.UnivariateObjectiveFunction
import org.apache.commons.math3.analysis._
import org.apache.commons.math3.optim.univariate.SearchInterval
import org.saddle._

package object stat {

  def estimateGammaParameters(sample: Vec[Double]) = {
    // https://en.wikipedia.org/wiki/Gamma_distribution#Maximum_likelihood_estimation
    // psi is the digamma function, psi' is the trigamma function

    val mean = sample.mean
    val s = math.log(mean) - sample.map(math.log).mean
    var k = (3d - s + math.sqrt((s - 3d) * (s - 3d) + 24 * s)) / (12 * s)
    var i = 0
    // while (i < 50) {
    //   k = k - ((math.log(k) - jdistlib.math.PolyGamma.digamma(k) - s) / (1.0 / k - jdistlib.math.PolyGamma.trigamma(k)))
    //   i += 1
    // }

    val theta = mean / k

    (k, theta)
  }

  def logit(x: Double) = math.log(x) - math.log(1.0 - x)

  def logistic(x: Double) = 1.0 / (1.0 + math.exp(-1.0 * x))

  def logsumexp(x: Double*) = {
    val max = x.max
    math.log(x.map(y => math.exp(y - max)).sum) + max
  }

  def cosineDistance(a: Array[Double], b: Array[Double]) = {
    if (a.size != b.size) throw new RuntimeException("a b unequal length")

    var numerator = 0.0
    var denoma = 0.0
    var denomb = 0.0

    var i = 0
    while (i < a.size) {
      val ai = a(i)
      val bi = b(i)
      numerator += ai * bi
      denoma += ai * ai
      denomb += bi * bi
      i += 1
    }
    1.0 - numerator / (math.sqrt(denoma) * math.sqrt(denomb))

  }

  def innerProduct(a: Array[Double], b: Array[Double]) = {
    val da = new DenseMatrix64F(a.size, 1, true, a: _*)
    val db = new DenseMatrix64F(b.size, 1, true, b: _*)
    VectorVectorMult.innerProd(da, db)
  }

  def minimizeN(
    target: Seq[Double] => Double,
    initial: Seq[Double],
    lowerBounds: Seq[Double],
    upperBounds: Seq[Double],
    abs: Double,
    rel: Double
  ) = {

    val n = initial.size

    class TargetF extends MultivariateFunction {
      def value(x: Array[Double]) = {
        target(x)
      }
    }

    val optimizer = new MultiStartMultivariateOptimizer(new SimplexOptimizer(rel, abs), 3, new SobolSequenceGenerator(n))
    val optim = optimizer.optimize(
      new ObjectiveFunction(new MultivariateFunctionPenaltyAdapter(
        new TargetF,
        lowerBounds.toArray,
        upperBounds.toArray,
        1E15,
        Array.fill(n)(1E7)
      )),
      GoalType.MINIMIZE,
      new NelderMeadSimplex(n),
      new InitialGuess(initial.toArray),
      new MaxIter(50000),
      new MaxEval(500000)
    )

    optim.getPoint.toSeq
  }
}
