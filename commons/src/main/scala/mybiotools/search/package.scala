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

import org.apache.commons.math3.random.RandomGenerator

package object search {

  def newton(
    f: Double => Double,
    d: Double => Double,
    q: Double,
    rel: Double,
    abs: Double,
    maxit: Int,
    x0: Double,
    min: Double,
    max: Double
  ) = {
    var fxn = f(x0)
    var dxn = d(x0)
    var xn = x0
    // println(xn + " " + fxn + " " + dxn)
    var rate = 1.0
    var i = 0
    while (i < maxit && ((math.abs(fxn - q) > abs) && math.abs((fxn - q) / q) > rel)) {
      // println(xn + " " + fxn + " " + dxn + " " + rate + " " + max)
      var t = xn - rate * (fxn - q) / dxn
      while ((t <= min || t >= max) && rate > 0.0) {
        rate *= 0.1
        t = xn - rate * (fxn - q) / dxn
      }
      xn = t
      fxn = f(xn)
      dxn = d(xn)
      i += 1
    }
    if (i == maxit) throw new RuntimeException("Failed to converge. " + q + " " + xn)
    // println(i)
    xn
  }

  val Phi = (1.0 + math.sqrt(5)) / 2.0

  val resPhi = 2.0 - Phi

  def cache(f: Double => Double) = {
    val c = collection.mutable.Map[Double, Double]()
    (x: Double) => {
      c.get(x) match {
        case Some(x) => x
        case None => {
          val r = f(x)
          c.update(x, r)
          r
        }
      }
    }
  }

  def findMinimum(x0: Double, x1: Double, eps: Double, maxIt: Int)(g: Double => Double): (Double, Int) = {

    assert(x0 < x1)

    def goldenSectionSearch(a: Double, b: Double, c: Double, tau: Double, it: Int)(f: Double => Double): (Double, Int) = {
      val x = if (c - b > b - a) b + resPhi * (c - b)
      else b - resPhi * (b - a)
      if (it > maxIt) ((c + a) / 2.0, it)
      else {
        if (math.abs(c - a) < (tau * (math.abs(b) + math.abs(x))))
          ((c + a) / 2.0, it);
        else {
          // assert(f(x) != f(b), f(x) + " " + f(b));
          if (f(x) < f(b)) {
            if (c - b > b - a) goldenSectionSearch(b, x, c, tau, it + 1)(f);
            else goldenSectionSearch(a, x, b, tau, it + 1)(f);
          } else {
            if (c - b > b - a) goldenSectionSearch(a, b, x, tau, it + 1)(f);
            else goldenSectionSearch(x, b, c, tau, it + 1)(f);
          }
        }
      }
    }
    val f = cache(g)
    goldenSectionSearch(x0, x1 - resPhi * (x1 - x0), x1, math.sqrt(eps), 0)(f)

  }

  def brent(x0: Double, x1: Double, rel: Double, abs: Double, maxit: Int, starts: Int, seed: Int)(g: Double => Double): (Double, Double) = {
    {
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

      class TargetF extends UnivariateFunction {
        def value(x: Double) = g(x)
      }

      val optimizer = if (starts > 1) new MultiStartUnivariateOptimizer(new BrentOptimizer(rel, abs), starts, new MersenneTwister(seed)) else new BrentOptimizer(rel, abs)
      val optim = optimizer.optimize(
        new UnivariateObjectiveFunction(new TargetF),
        GoalType.MINIMIZE,
        new MaxIter(maxit),
        new MaxEval(maxit * 10),
        new InitialGuess(Array(x0 + (x1 - x0) / 2.0)),
        new SearchInterval(x0, x1, x0 + (x1 - x0) / 2.0)
      )

      (optim.getPoint, optim.getValue)
    }
  }

  def bisect(x0: Double, x1: Double, start: Double, maxit: Int, rel: Double, abs: Double)(f: Double => Double): Double = {
    var i = 0
    var y0 = x0
    var y1 = x1
    var fc = f(start)
    var f0 = f(y0)
    var c = start
    def absmet = math.abs(y1 - y0) < abs
    def relmet = math.abs((y1 - y0) / y0) < rel
    while (i < maxit && !absmet && !relmet && fc != 0.0) {
      c = (y1 + y0) / 2
      fc = f(c)
      if (math.signum(fc) == math.signum(f0)) {
        y0 = c
      } else {
        y1 = c
      }
      i += 1
    }
    c
  }

  def cmaes(x0: Double, x1: Double, init: Double, maxit: Int, stopObj: Double, rel: Double, abs: Double, rng: RandomGenerator, pop: Int)(f: Double => Double): Double = {
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

    class TargetF extends MultivariateFunction {
      def value(x: Array[Double]) = {
        f(x(0))
      }
    }

    val optimizer = new CMAESOptimizer(maxit, stopObj, true, 100, 100, rng, false, new SimpleValueChecker(rel, abs, maxit))
    val optim = optimizer.optimize(
      new ObjectiveFunction(new TargetF),
      GoalType.MINIMIZE,
      new CMAESOptimizer.Sigma(Array(math.abs(x1 - x0))),
      new CMAESOptimizer.PopulationSize(pop),
      new InitialGuess(Array(init)),
      new SimpleBounds(Array(x0), Array(x1)),
      new MaxIter(maxit),
      new MaxEval(maxit * 10)
    )

    optim.getPoint.head
  }

  def cmaes2D(x0: Double, x1: Double, y0: Double, y1: Double, init: (Double, Double), maxit: Int, stopObj: Double, rel: Double, abs: Double, rng: RandomGenerator, pop: Int)(f: ((Double, Double)) => Double): (Double, Double) = {
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

    class TargetF extends MultivariateFunction {
      def value(x: Array[Double]) = {
        f(x(0) -> x(1))
      }
    }

    val optimizer = new CMAESOptimizer(maxit, stopObj, true, (maxit * 0.1).toInt, 100, rng, false, new SimpleValueChecker(rel, abs, maxit))
    val optim = optimizer.optimize(
      new ObjectiveFunction(new TargetF),
      GoalType.MINIMIZE,
      new CMAESOptimizer.Sigma(Array(math.abs(x1 - x0), math.abs(y1 - y0))),
      new CMAESOptimizer.PopulationSize(pop),
      new InitialGuess(Array(init._1, init._2)),
      new SimpleBounds(Array(x0, y0), Array(x1, y1)),
      new MaxIter(maxit),
      new MaxEval(maxit * 10)
    )

    optim.getPoint.head -> optim.getPoint.apply(1)
  }

}