// /*
// * The MIT License
// *
// * Copyright (c) 2015 ECOLE POLYTECHNIQUE FEDERALE DE LAUSANNE, Switzerland,
// * Group Fellay
// *
// * Permission is hereby granted, free of charge, to any person obtaining
// * a copy of this software and associated documentation files (the "Software"),
// * to deal in the Software without restriction, including without limitation
// * the rights to use, copy, modify, merge, publish, distribute, sublicense,
// * and/or sell copies of the Software, and to permit persons to whom the Software
// * is furnished to do so, subject to the following conditions:
// *
// * The above copyright notice and this permission notice shall be included in all
// * copies or substantial portions of the Software.
// *
// * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// * SOFTWARE.
// */
//
// package mybiotools.stat
//
// case class ChiSqTestResult(statistic: Double, pValue: Double, df: Double) extends TestResult
//
// object LikelihoodRatioTest {
//
//   def apply(nullModel: Double, alternative: Double, df: Double): ChiSqTestResult = apply(nullModel, alternative, df, 1.0)
//
//   def apply(nullModel: Double, alternative: Double, df: Double, scale: Double): ChiSqTestResult = {
//
//     val stat = 2 * (alternative - nullModel) / scale
//
//     val p = synchronized { jdistlib.ChiSquare.cumulative(stat, df, false, false) }
//     ChiSqTestResult(stat, p, df)
//   }
//
//   def apply(nullModel: LogLikelihood, alternative: LogLikelihood): ChiSqTestResult = {
//     assert(nullModel.numberOfSamples == alternative.numberOfSamples)
//     val dfeff = {
//       val df = alternative.df - nullModel.df
//       if (df < 1 && math.abs(df - 1.0) < 1E-4) 1
//       else if (df < 1) throw new RuntimeException("df < 1 (" + df.toString + nullModel + alternative + ")")
//       else df
//     }
//
//     apply(nullModel.L, alternative.L, dfeff)
//   }
//
//   def findOptimalDFUnderNull(lrtStatistics: Seq[Double], topFraction: Double) = {
//     assert(lrtStatistics.size > 0, "Empty lrtStatistics list.")
//     val N = lrtStatistics.length
//     val expectedPValues: Seq[Double] = for (i <- 0 until N) yield {
//       -1 * math.log10((i + 1).toDouble / (N + 1))
//     }
//
//     val topN = math.min(math.max((lrtStatistics.size * topFraction).toInt, 200), lrtStatistics.size)
//
//     def target(df: Double, scale: Double): Double = {
//       val dfeff = if (df < 0.0) 0 else df
//       val chisq = new jdistlib.ChiSquare(dfeff)
//       // val dfeff = df
//       val observedPValues = synchronized { lrtStatistics.map(stat => -1.0 * math.log10(chisq.cumulative(stat / scale, false, false))).sortBy(x => -1 * x) }
//
//       ((observedPValues zip expectedPValues).map { case (x: Double, y: Double) => (x - y) * (x - y) }).take(topN).sum / topN
//
//     }
//
//     {
//       import org.apache.commons.math3.analysis.{ UnivariateFunction, MultivariateFunction }
//       import org.apache.commons.math3.optim.univariate.{ BrentOptimizer, SearchInterval, UnivariateObjectiveFunction, MultiStartUnivariateOptimizer }
//       import org.apache.commons.math3.optim.nonlinear.scalar._
//       import org.apache.commons.math3.optim.nonlinear.scalar.GoalType
//       import org.apache.commons.math3.optim.{ MaxEval, MaxIter }
//       import org.apache.commons.math3.random.JDKRandomGenerator
//       import org.apache.commons.math3.random.SobolSequenceGenerator
//       import org.apache.commons.math3.optim.nonlinear.scalar.noderiv._
//       import org.apache.commons.math3.optim.InitialGuess
//       import org.apache.commons.math3.optim.SimpleBounds
//       import org.apache.commons.math3.optim.nonlinear.scalar.MultivariateFunctionMappingAdapter
//
//       class TargetF extends MultivariateFunction {
//         def value(x: Array[Double]) = target(x(0), x(1))
//       }
//
//       val optimizer = new MultiStartMultivariateOptimizer(new SimplexOptimizer(1E-10, 1E-10), 10, new SobolSequenceGenerator(2))
//       val optim = optimizer.optimize(
//         new ObjectiveFunction(new TargetF),
//         GoalType.MINIMIZE,
//         new NelderMeadSimplex(2),
//         new InitialGuess(Array(1.0, 1.0)),
//         // new SimpleBounds(Array(0.0, 0.0), Array(100, 100)),
//         new MaxIter(50000),
//         new MaxEval(500000)
//       )
//
//       (optim.getPoint.apply(0), optim.getPoint.apply(1), optim.getValue + " " + optim.getPoint.deep + " " + lrtStatistics.mkString("c(", ",", ")"))
//     }
//
//   }
//
// }
