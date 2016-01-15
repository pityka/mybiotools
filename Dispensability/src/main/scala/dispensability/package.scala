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

import mybiotools.stringstore._

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

package object dispensability {

  implicit def ordering[T <: Annotation[T]](implicit score: AnnotationScore[T]): Ordering[Variant[T]] = score.ordering

  implicit val genesetkeyordering = scala.math.Ordering.by[GeneSetKey, String8](_.name)
  implicit val geneordering = scala.math.Ordering.by[Gene, String8](_.hgnc)

  def optimize2D(target: (Double, Double) => Double, initial: (Double, Double)): (Double, Double) = {

    class TargetF extends MultivariateFunction {
      def value(x: Array[Double]) = target(x(0), x(1))
    }

    val optimizer = new MultiStartMultivariateOptimizer(new PowellOptimizer(1E-15, 1E-15), 3, new SobolSequenceGenerator(2))
    val optim = optimizer.optimize(
      new ObjectiveFunction(new TargetF),
      GoalType.MINIMIZE,
      new InitialGuess(Array(initial._1, initial._2)),
      new MaxIter(50000),
      new MaxEval(500000)
    )

    (optim.getPoint.apply(0), optim.getPoint.apply(1))

  }

  def minimizeN(target: Seq[Double] => Double, initial: Seq[Double], lowerBounds: Seq[Double], upperBounds: Seq[Double]) = {

    val n = initial.size
    assert(initial.size == lowerBounds.size)
    assert(upperBounds.size == lowerBounds.size)

    class TargetF extends MultivariateFunction {
      def value(x: Array[Double]) = {
        target(x)
      }
    }

    val optimizer = new MultiStartMultivariateOptimizer(new SimplexOptimizer(1E-15, 1E-15), 3, new SobolSequenceGenerator(n))
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

  def minimize2DWithBounds(target: (Double, Double) => Double, initial: (Double, Double), bounds: (Double, Double, Double, Double)): (Double, Double) = {

    class TargetF extends MultivariateFunction {
      def value(x: Array[Double]) = {
        target(x(0), x(1))
      }
    }

    val optimizer = new MultiStartMultivariateOptimizer(new SimplexOptimizer(1E-30, 1E-30), 10, new SobolSequenceGenerator(2))
    val optim = optimizer.optimize(
      new ObjectiveFunction(new MultivariateFunctionPenaltyAdapter(
        new TargetF,
        Array(bounds._1, bounds._3),
        Array(bounds._2, bounds._4),
        1E15,
        Array(1E7, 1E7)
      )),
      GoalType.MINIMIZE,
      new NelderMeadSimplex(2),
      new InitialGuess(Array(initial._1, initial._2)),
      new MaxIter(50000),
      new MaxEval(500000)
    )

    (optim.getPoint.apply(0), optim.getPoint.apply(1))

  }

  def minimize1DWithBounds(target: (Double) => Double, bounds: (Double, Double)): Double = {

    class TargetF extends UnivariateFunction {
      def value(x: Double) = target(x)
    }

    val optimizer = new BrentOptimizer(1E-15, 1E-15)
    val optim = optimizer.optimize(
      new UnivariateObjectiveFunction(new TargetF),
      GoalType.MINIMIZE,
      new SearchInterval(bounds._1, bounds._2),
      new MaxIter(50000),
      new MaxEval(500000)
    )

    (optim.getPoint)

  }

}