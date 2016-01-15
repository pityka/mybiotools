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

package dispensability

import mybiotools.stat.{ MultinomialRandomGenerator, RandomShuffler }
import mybiotools.SummaryStat
import scala.util.Random
import org.apache.commons.math3.random.{ RandomGenerator, RandomDataGenerator }
import org.apache.commons.math3.stat.inference.MannWhitneyUTest
import mybiotools.plots.ScatterPlot
import org.apache.commons.math3.random.MersenneTwister
import org.apache.commons.math3.distribution.BinomialDistribution
import mybiotools.stat.Poisson
import org.saddle._
import org.apache.commons.math3.util.FastMath

case class RelativeParameters(fractionOfEssentials: Double, noise: Double) extends HasFractionOfEssentials with ParamForPlot {
  override def toString = s"Parameters(phi=$fractionOfEssentials, noise=$noise)"
  def extractForPlot = Map("phi" -> fractionOfEssentials, "noise" -> noise)
}
case object RelativeMutationRateModelProxy extends ModelProxy[ScaledProbabilitiesWithCounts, RelativeParameters, TotalNumberOfTruncations] {
  def model = RelativeMutationRateModel
}

object RelativeMutationRateModel
    extends Model[ScaledProbabilitiesWithCounts, RelativeParameters, TotalNumberOfTruncations] with Serializable {
  type Parameters = RelativeParameters
  val Parameters = RelativeParameters

  def indepFactory(t: Int) = TotalNumberOfTruncations(t)

  def maxObservedX(data: ScaledProbabilitiesWithCounts) = data.totalTruncations

  def observedTruncatedGenesInDownsampledData(d: ScaledProbabilitiesWithCounts, x: TotalNumberOfTruncations, rnd: RandomGenerator) =
    TotalNumberOfTruncatedGenes(
      DownsamplingHelpers.downSampleTruncationCountsToTruncationCount(
        observedCounts = d.variantCounts,
        targetTruncationCount = x,
        rnd
      ).count(_.value > 0)
    )

  def copyParameterNeutral(p: Parameters) = p.copy(fractionOfEssentials = 0.0, noise = 0.0)

  def parameterMean(ps: Seq[Parameters]) = RelativeParameters(
    SummaryStat(ps.map(_.fractionOfEssentials)).mean,
    SummaryStat(ps.map(_.noise)).mean
  )

  def predictedCountsPlugBackPosterior(p: Parameters, d: ScaledProbabilitiesWithCounts): Vector[Double] = {
    val v = d.totalTruncations
    d.vector.map { e =>
      val posteriors = probabilityOfHaploinsufficiencyGivenTruncations(
        v,
        p,
        e._2,
        e._1
      )
      val l = lambdas(v, p, e._2)
      l.lambdaHI * posteriors.pHI + l.lambdaHS * posteriors.pHS
    }
  }

  def predictedCountsPlugBackPosteriorHardThreshold(p: Parameters, d: ScaledProbabilitiesWithCounts): Vector[Double] = {
    val v = d.totalTruncations
    d.vector.map { e =>
      val posteriors = probabilityOfHaploinsufficiencyGivenTruncations(
        v,
        p,
        e._2,
        e._1
      )
      val l = lambdas(v, p, e._2)
      if (posteriors.pHI > posteriors.pHS) l.lambdaHI else l.lambdaHS
    }
  }

  def predictedHICounts(p: Parameters, d: ScaledProbabilitiesWithCounts): Vector[Double] = {
    val v = d.totalTruncations
    d.probabilities.vector.map(p0 => lambdas(v, p, p0).lambdaHI * p.fractionOfEssentials)
  }
  def predictedHSCounts(p: Parameters, d: ScaledProbabilitiesWithCounts): Vector[Double] = {
    val v = d.totalTruncations
    d.probabilities.vector.map(p0 => lambdas(v, p, p0).lambdaHS * (1.0 - p.fractionOfEssentials))
  }

  def predictFractionOfNoisyObservedTruncations(p: Parameters, data: ScaledProbabilitiesWithCounts): Double = p.noise

  def predictFractionOfObservedTruncationsWhichAreInHIGenes(p: Parameters, data: ScaledProbabilitiesWithCounts): Double = {

    val v = data.totalTruncations

    val predictedSumOfHIMutations = data.probabilities.vector.map { p0 => RelativeMutationRateModel.lambdas(v, p, p0).lambdaHI * p.fractionOfEssentials }.sum

    val predictedSumOfHSMutations = data.probabilities.vector.map { p0 => RelativeMutationRateModel.lambdas(v, p, p0).lambdaHS * (1.0 - p.fractionOfEssentials) }.sum

    predictedSumOfHIMutations / (predictedSumOfHIMutations + predictedSumOfHSMutations)
  }

  def predictFractionOfObservedTruncationsWhichDoesNotOriginateFromGivenErrorLevelAndAreInHI(p: Parameters, data: ScaledProbabilitiesWithCounts, error: Double): Double =
    (1.0 - error / p.noise) * predictFractionOfObservedTruncationsWhichAreInHIGenes(p, data)

  def predictNumberOfTruncatedGenes(p: Parameters, data: ScaledProbabilitiesWithCounts, x: TotalNumberOfTruncations) = {
    poissonApprox(x, p, data.probabilities)
  }

  def predictNumberOfHaploInsufficientTruncatedGenes(p: Parameters, data: ScaledProbabilitiesWithCounts, x: TotalNumberOfTruncations) = {
    ExpectedNumberOfTruncatedGenes(poissonApproxSeparate(x, p, data.probabilities)._2)
  }

  def predictNumberOfHaploSufficientTruncatedGenes(p: Parameters, data: ScaledProbabilitiesWithCounts, x: TotalNumberOfTruncations) = {
    ExpectedNumberOfTruncatedGenes(poissonApproxSeparate(x, p, data.probabilities)._1)
  }

  def predictFractionOfGenesWithAtLeastOneTruncationNotFromThisErrorRate(p: Parameters, data: ScaledProbabilitiesWithCounts, error: Double, totalTruncations: TotalNumberOfTruncations): ExpectedNumberOfTruncatedGenes = {
    ExpectedNumberOfTruncatedGenes(poissonApproxSeparate(totalTruncations, p.copy(noise = p.noise - error), data.probabilities)._2)
  }

  def predictFractionOfGenesWithAtLeastOneTruncationFromThisErrorRate(p: Parameters, data: ScaledProbabilitiesWithCounts, error: Double, totalTruncations: TotalNumberOfTruncations): ExpectedNumberOfTruncatedGenes = {
    ExpectedNumberOfTruncatedGenes(poissonApproxSeparate(totalTruncations, p.copy(noise = error), data.probabilities)._2)
  }

  case class Lambdas(lambdaHI: Double, lambdaHS: Double)

  def lambdas(
    x: TotalNumberOfTruncations,
    parameters: Parameters,
    p0: NormalizedProbTruncation
  ): Lambdas = {
    val fractionOfNonEssentials = 1.0 - parameters.fractionOfEssentials
    val oneOverFractionOfNonEssentials = 1.0 / parameters.fractionOfEssentials
    val NF: Double = parameters.noise * x.value
    val NT: Double = x.value - NF

    val lambdaHS = NT * p0.value / fractionOfNonEssentials + NF * p0.value
    val lambdaHI = NF * p0.value
    Lambdas(lambdaHI, lambdaHS)
  }

  def simulateData(
    parameters: Parameters,
    neutralDensity: ScaledProbabilitiesWithCounts,
    rnd: RandomGenerator
  ) = {
    val randomDataGenerator = new RandomDataGenerator(rnd)
    val total = neutralDensity.totalTruncations
    ScaledProbabilitiesWithCounts(neutralDensity.probabilities.vector map { p0 =>
      val l = lambdas(
        total,
        parameters, p0
      )
      import l._
      val pHS = randomDataGenerator.nextPoisson(lambdaHS)
      val pHI = randomDataGenerator.nextPoisson(lambdaHI)
      (ObservedTruncationCount(((if (randomDataGenerator.nextUniform(0.0, 1.0) < parameters.fractionOfEssentials) pHI else pHS)).toInt), p0)
    } toVector, neutralDensity.genes)
  }

  def poissonApprox(
    x: TotalNumberOfTruncations,
    parameters: Parameters,
    scaledNeutralDensity: ScaledProbabilities
  ): ExpectedNumberOfTruncatedGenes = {
    val y = poissonApproxSeparate(x, parameters, scaledNeutralDensity)
    ExpectedNumberOfTruncatedGenes(y._1 + y._2)
  }

  def poissonApproxSeparate(
    x: TotalNumberOfTruncations,
    parameters: Parameters,
    scaledNeutralDensity: ScaledProbabilities
  ): (Double, Double) =
    expectedNumbersOfAtLeastOne(scaledNeutralDensity.vector.map(p0 => lambdas(x, parameters, p0)), parameters.fractionOfEssentials)

  def expectedNumbersOfAtLeastOne(lambdavector: Seq[Lambdas], mixture: Double): (Double, Double) = {
    import org.apache.commons.math3.util.FastMath

    val trueSum = FastMath.exp(mybiotools.stat.logsumexp(lambdavector.map(l => -1.0 * l.lambdaHS): _*))
    val falseSum = FastMath.exp(mybiotools.stat.logsumexp(lambdavector.map(l => -1.0 * l.lambdaHI): _*))

    val numGenes = lambdavector.size

    ((1.0 - mixture) * (numGenes - trueSum), mixture * (numGenes - falseSum))

  }

  def deltaPoissonApprox(
    x: TotalNumberOfTruncations,
    parameters: Parameters,
    scaledNeutralDensity: ScaledProbabilities
  ): Double = {

    poissonApprox(x, parameters, scaledNeutralDensity).value - poissonApprox(TotalNumberOfTruncations(x.value - 1), parameters, scaledNeutralDensity).value
  }

  def deltaPoissonApproxSeparateScaled(
    x: TotalNumberOfTruncations,
    parameters: Parameters,
    scaledNeutralDensity: ScaledProbabilities
  ): (Double, Double) = {
    val d1 = poissonApproxSeparate(x, parameters, scaledNeutralDensity)
    val d2 = poissonApproxSeparate(TotalNumberOfTruncations(x.value - 1), parameters, scaledNeutralDensity)
    val dt = (d1._1 + d1._2) - (d2._1 + d2._2)
    val t = ((d1._1 - d2._1) / dt, (d1._2 - d2._2) / dt)
    t
  }

  /**
   * Performs two one-sided tests whether the observed count is higher than the expected HI and whether it is lower than the expected HS.
   *
   * @return  A vector of (p_hi,p_hs) pairs where p_hi is the p-value for the test that the observed count is larger than the expected under HI
   * and p_hs is the p-value for the test that the observed count is smaller than expected under HS
   */
  def poissonTestOfHaploInsufficiency(
    scaledProbs: ScaledProbabilitiesWithCounts,
    parameters: Parameters
  ): Vector[(Double, Double)] = {
    val totalNumberOfTruncations = scaledProbs.totalTruncations
    scaledProbs.vector map {
      case (count, p0) =>
        val l = lambdas(totalNumberOfTruncations, parameters, p0)
        import l._

        val p_hi = jdistlib.Poisson.cumulative(count.value, lambdaHI, false, false)
        val p_hs = jdistlib.Poisson.cumulative(count.value, lambdaHS, true, false)
        (p_hi, p_hs)

    }
  }

  case class Posteriors(pHI: Double, pHS: Double) {
    assert(math.abs(1.0 - (pHI + pHS)) < 1E-7)
  }

  def posteriorOfFirstComponent(
    expectations: Lambdas,
    fractionOfEssentials: Double,
    numberOfTruncations: ObservedTruncationCount
  ): Posteriors = {

    val logPriorHI = FastMath.log(fractionOfEssentials)
    val logPriorHS = FastMath.log(1.0 - fractionOfEssentials)

    import expectations._

    val logLikelihoodTruncationsGivenHI = Poisson.logProbability(numberOfTruncations.value, mean = lambdaHI)
    val logLikelihoodTruncationsGivenHS = Poisson.logProbability(numberOfTruncations.value, mean = lambdaHS)

    val denominator = mybiotools.stat.logsumexp(logPriorHI + logLikelihoodTruncationsGivenHI, logPriorHS + logLikelihoodTruncationsGivenHS)

    val logPosteriorHI = logPriorHI + logLikelihoodTruncationsGivenHI - denominator

    val logPosteriorHS = logPriorHS + logLikelihoodTruncationsGivenHS - denominator

    Posteriors(pHI = FastMath.exp(logPosteriorHI), pHS = FastMath.exp(logPosteriorHS))
  }

  /**
   * P_i(HI|k) :=& \text{P(gene i is haploinsufficient given k truncations in it)} \\
   * P_i(HI|k) =& \frac{P(HI) P(k|HI)}{P(k)} ..\text{(Bayes-rule)} \\
   * P_i(k|HI) =& Poisson(k,\lambda=\text{as in the supplementary for HI})\\
   * P_i(k) =& P(HI) P(k|HI) + P(HS) P(k|HS)\\
   * P(k|HS) =& Poisson(k,\lambda=\text{as in the supplementary for HS})\\
   */
  def probabilityOfHaploinsufficiencyGivenTruncations(
    x: TotalNumberOfTruncations,
    parameters: Parameters,
    scaledNeutralDensity: NormalizedProbTruncation,
    numberOfTruncations: ObservedTruncationCount
  ): Posteriors =
    posteriorOfFirstComponent(
      lambdas(
        x,
        parameters,
        scaledNeutralDensity
      ),
      parameters.fractionOfEssentials,
      numberOfTruncations
    )

  def posteriorOfHaploinsufficient(
    scaledNeutralDensity: ScaledProbabilitiesWithCounts,
    parameters: Parameters
  ): Vector[Posteriors] = {
    val x = scaledNeutralDensity.totalTruncations
    scaledNeutralDensity.vector.map {
      case (count, scaledProb) =>
        probabilityOfHaploinsufficiencyGivenTruncations(x, parameters, scaledProb, count)
    }
  }

  def powerToHaveHighPostieriorOfHaploinsufficiency(
    decisionThresholdOfPosterior: Double,
    data: ScaledProbabilitiesWithCounts,
    parameters: Parameters,
    simulationSampleSize: Int,
    randomGenerator: RandomGenerator
  ): Vector[Double] = powerToHaveHighPostieriorOfHaploinsufficiency(
    decisionThresholdOfPosterior,
    data.totalTruncations,
    parameters,
    data.probabilities,
    simulationSampleSize,
    new RandomDataGenerator(randomGenerator)
  )

  def powerToHaveHighPostieriorOfHaploinsufficiency(
    decisionThresholdOfPosterior: Double,
    totalNumberOfTruncations: TotalNumberOfTruncations,
    parameters: Parameters,
    scaledNeutralDensity: ScaledProbabilities,
    simulationSampleSize: Int,
    randomDataGenerator: RandomDataGenerator
  ): Vector[Double] = {
    scaledNeutralDensity.vector.map {
      case (scaledProb) =>
        val successes = 1 to simulationSampleSize count { i =>
          val count = randomDataGenerator.nextPoisson(lambdas(totalNumberOfTruncations, parameters, scaledProb).lambdaHI)
          probabilityOfHaploinsufficiencyGivenTruncations(totalNumberOfTruncations, parameters, scaledProb, ObservedTruncationCount(count.toInt)).pHI >= decisionThresholdOfPosterior
        }
        successes.toDouble / simulationSampleSize
    }
  }

  def expectedCounts(
    totalNumberOfTruncations: TotalNumberOfTruncations,
    parameters: Parameters,
    scaledNeutralDensity: ScaledProbabilities
  ): Vector[Lambdas] = {
    scaledNeutralDensity.vector.map {
      case (scaledProb) =>
        lambdas(totalNumberOfTruncations, parameters, scaledProb)
    }
  }

  def logLikelihoodOfTruncation(
    x: TotalNumberOfTruncations,
    parameters: Parameters,
    scaledNeutralDensity: NormalizedProbTruncation,
    numberOfTruncations: ObservedTruncationCount
  ): Double = {

    val l = lambdas(x, parameters, scaledNeutralDensity)
    import l._

    val logLikelihoodTruncationsGivenHI = Poisson.logProbability(numberOfTruncations.value, mean = lambdaHI)
    val logLikelihoodTruncationsGivenHS = Poisson.logProbability(numberOfTruncations.value, mean = lambdaHS)

    // (fractionOfEssentials * likelihoodTruncationsGivenHI) + ((1.0 - fractionOfEssentials) * likelihoodTruncationsGivenHS)
    mybiotools.stat.logsumexp(FastMath.log(parameters.fractionOfEssentials) + logLikelihoodTruncationsGivenHI, FastMath.log(1.0 - parameters.fractionOfEssentials) + logLikelihoodTruncationsGivenHS)
  }

  def logLikelihoodOfTruncations(
    x: TotalNumberOfTruncations,
    parameters: Parameters,
    scaledNeutralDensity: ScaledProbabilitiesWithCounts
  ): Double = {
    scaledNeutralDensity.vector.foldLeft(0.0) {
      case (s, (count, scaledProb)) =>
        s + logLikelihoodOfTruncation(x, parameters, scaledProb, count)
    }
  }

  def maximumLikelihood(data: ScaledProbabilitiesWithCounts): tasks.OptimalParameter[Parameters] = {
    val variantCount = data.variantCount
    val function = (fractionOfEssentials: Double, f: Double) => {
      val r = logLikelihoodOfTruncations(variantCount, Parameters(fractionOfEssentials, f), data) * -1

      r
    }
    val initial2 = 0.24
    val nonfixedLower = 0.000001
    val nonfixedUpper = 0.99999
    val (bestn, bestf) = minimize2DWithBounds(function, (0.24, initial2), (0.00001, 0.9999, nonfixedLower, nonfixedUpper))

    tasks.OptimalParameter(Parameters(bestn, bestf), function(bestn, bestf))
  }

  def predictDensity(p: Parameters, d: ScaledProbabilitiesWithCounts) = predictDensity(p, d.probabilities, d.variantCount)

  def predictDensity(
    parameters: Parameters,
    scaledProb: ScaledProbabilities,
    variantCount: TotalNumberOfTruncations
  ): Vector[(Int, Double)] = {
    scaledProb.vector.map {
      case (prob) =>
        Vec(0 to 80 map { count =>
          FastMath.exp(logLikelihoodOfTruncation(
            variantCount,
            parameters,
            prob,
            ObservedTruncationCount(count)
          ))
        }: _*)

    }.reduce(_ + _).toSeq.toVector.zipWithIndex.map(_.swap)
  }

  def cumulativeDistribution(
    totalNumberOfTruncations: TotalNumberOfTruncations,
    parameters: Parameters,
    scaledNeutralDensity: ScaledProbabilities,
    location: ObservedTruncationCount
  ): Double = {

    def logCumulativeProbability(
      totalNumberOfTruncations: TotalNumberOfTruncations,
      parameters: Parameters,
      scaledNeutralDensity: NormalizedProbTruncation,
      location: ObservedTruncationCount
    ): Double = {

      val l = lambdas(totalNumberOfTruncations, parameters, scaledNeutralDensity)
      import l._

      val logCdfHI = jdistlib.Poisson.cumulative(location.value, lambdaHI, true, true)
      val logCdfHS = jdistlib.Poisson.cumulative(location.value, lambdaHS, true, true)

      mybiotools.stat.logsumexp(
        FastMath.log(parameters.fractionOfEssentials) + logCdfHI,
        FastMath.log(1.0 - parameters.fractionOfEssentials) + logCdfHS
      )

    }

    math.exp(mybiotools.stat.logsumexp(scaledNeutralDensity.vector.map { d =>
      logCumulativeProbability(totalNumberOfTruncations, parameters, d, location)
    }: _*)) / scaledNeutralDensity.vector.size

  }

  def cdfFit(data: ScaledProbabilitiesWithCounts): tasks.OptimalParameter[Parameters] = {

    val variantCount = data.variantCount
    val probs = data.probabilities

    val cdf = (param: Seq[Double], loc: Double) => cumulativeDistribution(variantCount, Parameters(param(0), param(1)), probs, ObservedTruncationCount(loc.toInt))

    val initial2 = 0.18
    val nonfixedLower = 0.000001
    val nonfixedUpper = 0.5

    val (optim, minimum) = mybiotools.stat.FitCDF.fitCDF(data.variantCounts.map(_.value.toDouble), cdf, List(0.2, initial2), List(0.0, nonfixedLower), List(1.0, nonfixedUpper))

    tasks.OptimalParameter(Parameters(optim(0), optim(1)), minimum)
  }

  def cdfFitNoPenetrance(data: ScaledProbabilitiesWithCounts): tasks.OptimalParameter[Parameters] = {

    val variantCount = data.variantCount
    val probs = data.probabilities

    val cdf = (param: Seq[Double], loc: Double) => cumulativeDistribution(variantCount, Parameters(param(0), 1E-6), probs, ObservedTruncationCount(loc.toInt))

    val (optim, minimum) = mybiotools.stat.FitCDF.fitCDF(
      data.variantCounts.map(_.value.toDouble),
      cdf,
      List(0.2),
      List(0.0),
      List(1.0)
    )

    tasks.OptimalParameter(Parameters(optim(0), 1E-6), minimum)
  }

  def downsamplingFit(data: ScaledProbabilitiesWithCounts, rnd: RandomGenerator): tasks.OptimalParameter[Parameters] = {

    def predictedNumberOfTruncatedGenes(total: TotalNumberOfTruncations, p: Parameters) = {
      val (hi, hs) = RelativeMutationRateModel.expectedNumbersOfAtLeastOne(data.vector.map(d => lambdas(total, p, d._2)), p.fractionOfEssentials)
      hi + hs
    }

    val referencePoints = 1 to 10 flatMap (x => List(20000, 25000, 30000, 35000, 40000, data.totalTruncations.value)) map (x => TotalNumberOfTruncations(x))

    val observedTruncations = referencePoints.map(x => observedTruncatedGenesInDownsampledData(data, x, rnd))

    val function =
      (p: Seq[Double]) => {
        val r = referencePoints.zipWithIndex.map { t =>
          val pred = predictedNumberOfTruncatedGenes(referencePoints(t._2), Parameters(p(0), p(1)))
          val o: TotalNumberOfTruncatedGenes = observedTruncations(t._2)
          (pred - o.value) * (pred - o.value)
        }.sum
        r
      }

    val optimum = minimizeN(function, List(0.15, 0.15), List(0.0001, 0.00001), List(0.9999, 0.9999999))
    tasks.OptimalParameter(Parameters(optimum(0), optimum(1)), function(optimum))

  }

}