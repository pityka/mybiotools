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

import mybiotools.SummaryStat
import scala.util.Random
import org.apache.commons.math3.random.{ RandomGenerator, RandomDataGenerator }
import org.apache.commons.math3.stat.inference.MannWhitneyUTest
import mybiotools.plots.ScatterPlot
import org.apache.commons.math3.random.MersenneTwister
import org.apache.commons.math3.distribution.BinomialDistribution

object PoissonMixture {

  // object Poisson {
  //   def logProbability(k: Int, mean: Double) = jdistlib.Poisson.density(k.toDouble, mean, true)
  // }

  case class Parameters(mean: Double, mixture: Double)

  def generateK(
    params: Seq[(Double, Double)],
    n: Int,
    rnd: RandomGenerator
  ): Seq[Long] = {
    val multinom = new MultinomialRandomGenerator[Double](rnd, false, params: _*)
    val rndd = new RandomDataGenerator(rnd)
    1 to n map { i =>
      val mean = multinom.next
      rndd.nextPoisson(mean)
    }
  }

  def generateKWeighted(
    params: Seq[(Double, Double)],
    weights: Seq[Double],
    rnd: RandomGenerator
  ): Seq[Long] = {
    val multinom = new MultinomialRandomGenerator[Double](rnd, false, params: _*)
    val rndd = new RandomDataGenerator(rnd)
    weights map { i =>
      val mean = multinom.next
      rndd.nextPoisson(mean * i)
    }
  }

  def expectedHistogram(
    params: Seq[Parameters],
    range: Seq[Int],
    dataSize: Int
  ): Seq[(Int, Double)] =
    range map { count =>
      count -> dataSize * math.exp(logLikelihood(params, count :: Nil))
    }

  def expectedHistogramWeighted(
    params: Seq[Parameters],
    range: Seq[Int],
    weights: Seq[Double]
  ): Vector[(Int, Double)] = {
    import org.saddle._
    weights.map {
      case (prob) =>
        Vec(range map { count =>
          math.exp(logLikelihoodLogWeighted(params.map(x => (x.mean, math.log(x.mixture))), List(count -> prob)))
        }: _*)

    }.reduce(_ + _).toSeq.toVector.zip(range).map(_.swap)
  }

  private def logLikelihoodLog(params: Seq[(Double, Double)], data: Seq[(Int)]): Double =
    logLikelihoodLogWeighted(params, data.map(x => x -> 1.0))

  private def logLikelihoodLogWeighted(params: Seq[(Double, Double)], data: Seq[(Int, Double)]): Double = {
    data.foldLeft(0.0) {
      case (s, (datum, weight)) =>
        s + logsumexp(params.map {
          case (mean, logmixture) =>
            logmixture + Poisson.logProbability(datum, mean = mean * weight)
        }: _*)
    }
  }

  def logLikelihood(params: Seq[Parameters], data: Seq[Int]): Double =
    logLikelihoodLog(params.map(x => x.mean -> math.log(x.mixture)), data)

  def logPosteriorOfComponents(params: Seq[Parameters], datum: Int): Seq[Double] =
    logPosteriorOfComponentsLog(params.map(x => x.mean -> math.log(x.mixture)), datum)

  private def logPosteriorOfComponentsLog(params: Seq[(Double, Double)], datum: Int): Seq[Double] = logPosteriorOfComponentsLog(params, datum, 1.0)

  /**
   * Computes the probabilities of component memberships.
   * @param params (mean,log(mixture)) pairs
   * @param datum one data point
   */
  private def logPosteriorOfComponentsLog(params: Seq[(Double, Double)], datum: Int, weight: Double): Seq[Double] = {

    val nominators = params.map {
      case (mean, logmixture) =>
        logmixture + Poisson.logProbability(datum, mean = mean * weight)
    }
    val denominator = logsumexp(nominators: _*)

    nominators.map(_ - denominator)
  }

  def emKComponentPoisson(data: Vector[Int], init: Seq[Parameters]): Seq[Parameters] =
    emKComponentPoissonWeighted(data.map(x => (x, 1.0)), init)

  def emKComponentPoissonWeighted(data: Vector[(Int, Double)], init: Seq[Parameters]): Seq[Parameters] = {

    val k = data.size.toDouble
    val logk = math.log(k)

    def stop(current: Double, next: Double) = math.abs((next - current) / current) <= 1E-8

    def loop2(params: Seq[(Double, Double)], currentLogL: Double): Seq[(Double, Double)] = {
      // println(params + " " + currentLogL)
      assert(math.abs(params.map(x => math.exp(x._2)).sum - 1.0) < 1E-10, params.toString)

      val logPosteriors = data.map {
        case (count, weight) =>
          (logPosteriorOfComponentsLog(params, count, weight).toIndexedSeq, math.log(count.toDouble), math.log(weight))
      }

      val nextMixtures = params.zipWithIndex.map {
        case (_, i) =>
          logsumexp(logPosteriors.map(_._1(i)): _*) - logk
      }

      val logSumPosteriorsTimesW = params.zipWithIndex.map {
        case (_, i) =>
          logsumexp(logPosteriors.map(x => x._1(i) + x._3): _*)
      }

      val logSumPosteriorsTimesX = params.zipWithIndex.map {
        case (_, i) =>
          logsumexp(logPosteriors.map(x => x._1(i) + x._2): _*)
      }

      val nextMeans = {
        logSumPosteriorsTimesW zip logSumPosteriorsTimesX map (x => math.exp(x._2 - x._1))
      }

      val nextParams = nextMeans zip nextMixtures
      val nextLogL = logLikelihoodLogWeighted(nextParams, data)
      if (stop(currentLogL, nextLogL)) nextParams
      else loop2(nextParams, nextLogL)

    }

    val initP = init.map(x => x.mean -> math.log(x.mixture))

    loop2(initP, logLikelihoodLogWeighted(initP, data))
      .map(x => Parameters(x._1, math.exp(x._2)))
      .sortBy(_.mixture)

  }

  def emKComponentPoissonMultistart(data: Vector[(Int)], k: Int, rnd: RandomGenerator, starts: Int): Seq[Parameters] = emKComponentPoissonWeightedMultistart(data.map(x => (x, 1.0)), k, rnd, starts)

  def randomInitialParameters(data: IndexedSeq[(Int, Double)], k: Int, rndd: RandomDataGenerator) = {
    val startingmixtures = {
      val firstKm1 = ((1 until k - 1).foldLeft(rndd.nextUniform(0.0, 0.95) :: Nil) { (last, _) =>
        rndd.nextUniform(0.0, 0.95 - last.sum) :: last
      })
      (1.0 - (firstKm1.sum)) +: firstKm1
    }
    val startingMeans = 1 to k map (i => data(rndd.nextInt(0, data.size - 1))) map (x => x._1 * x._2)

    startingMeans zip startingmixtures map (x => Parameters(x._1 + 1.0, x._2))
  }

  def emKComponentPoissonWeightedMultistart(data: Vector[(Int, Double)], k: Int, rnd: RandomGenerator, starts: Int): Seq[Parameters] = {
    val rndd = new RandomDataGenerator(rnd)

    def doOnce = {
      val init = randomInitialParameters(data, k, rndd)
      emKComponentPoissonWeighted(data, init)
    }

    def doOnceWithNonRandomInit = {

      val min = data.map(x => x._1 * x._2).min
      val max = data.map(x => x._1 * x._2).max
      val initMeans = 0 until k map (i => min + i * (max - min) / k)

      val randomInit = randomInitialParameters(data, k, rndd)

      val init = initMeans zip randomInit.map(_.mixture) map (x => Parameters(x._1 + 1.0, x._2))
      val params = emKComponentPoissonWeighted(data, init)
      val logL = logLikelihoodLogWeighted(params.map(x => (x.mean, math.log(x.mixture))), data)
      (logL, params)
    }

    val d = (1 to starts map { i =>
      val params = doOnce
      val logL = logLikelihoodLogWeighted(params.map(x => (x.mean, math.log(x.mixture))), data)
      (logL, params)
    }) :+ doOnceWithNonRandomInit

    d.sortBy(_._1).last._2

  }

}