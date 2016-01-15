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
import dispensability.RelativeMutationRateModel.{ Lambdas, Posteriors }
import org.apache.commons.math3.util.FastMath
import org.apache.commons.math3.distribution.PoissonDistribution
import org.saddle._
import dispensability.tasks.OptimalParameter
import dispensability.DownsamplingHelpers.GeneId
import mybiotools.stringstore._

trait DoubleVal extends Any {
  def value: Double
}

trait IntVal extends Any {
  def value: Int
}

case class ExpectedTruncationCount(value: Double) extends AnyVal with DoubleVal
case class ObservedTruncationCount(value: Int) extends AnyVal with IntVal
case class ObservedSynonymCount(value: Int) extends AnyVal with IntVal
case class ProbSynonym(value: Double) extends AnyVal with DoubleVal
case class ProbTruncation(value: Double) extends AnyVal with DoubleVal
case class NormalizedProbTruncation(value: Double) extends AnyVal with DoubleVal
case class TotalNumberOfTruncations(value: Int) extends AnyVal with IntVal
case class TotalNumberOfSynonymVariants(value: Int) extends AnyVal with IntVal
case class TotalNumberOfTruncatedGenes(value: Int) extends AnyVal with IntVal
case class ExpectedNumberOfTruncatedGenes(value: Double) extends AnyVal with DoubleVal

trait ModelProxy[D, P, I] {
  def model: Model[D, P, I]
}

trait Model[Data, Param, IndepAxis] {

  def indepFactory(k: Int): IndepAxis

  def cdfFit(data: Data): OptimalParameter[Param]
  def downsamplingFit(data: Data, rnd: RandomGenerator): OptimalParameter[Param]
  def maximumLikelihood(data: Data): OptimalParameter[Param]
  def cdfFitNoPenetrance(data: Data): OptimalParameter[Param]

  def predictedHICounts(p: Param, d: Data): Vector[Double]
  def predictedHSCounts(p: Param, d: Data): Vector[Double]
  def predictedCountsPlugBackPosterior(p: Param, data: Data): Vector[Double]
  def predictedCountsPlugBackPosteriorHardThreshold(p: Param, data: Data): Vector[Double]

  def predictNumberOfTruncatedGenes(p: Param, data: Data, x: IndepAxis): ExpectedNumberOfTruncatedGenes

  def predictNumberOfHaploInsufficientTruncatedGenes(p: Param, data: Data, x: IndepAxis): ExpectedNumberOfTruncatedGenes

  def predictNumberOfHaploSufficientTruncatedGenes(p: Param, data: Data, x: IndepAxis): ExpectedNumberOfTruncatedGenes

  def predictFractionOfNoisyObservedTruncations(p: Param, data: Data): Double

  def predictFractionOfObservedTruncationsWhichAreInHIGenes(p: Param, data: Data): Double

  def predictFractionOfObservedTruncationsWhichDoesNotOriginateFromGivenErrorLevelAndAreInHI(p: Param, data: Data, error: Double): Double

  def predictFractionOfGenesWithAtLeastOneTruncationNotFromThisErrorRate(p: Param, data: Data, error: Double, t: TotalNumberOfTruncations): ExpectedNumberOfTruncatedGenes

  def predictDensity(p: Param, data: Data): Vector[(Int, Double)]

  def parameterMean(ps: Seq[Param]): Param

  def copyParameterNeutral(p: Param): Param

  def observedTruncatedGenesInDownsampledData(data: Data, x: IndepAxis, rnd: RandomGenerator): TotalNumberOfTruncatedGenes

  def maxObservedX(data: Data): IndepAxis

  def powerToHaveHighPostieriorOfHaploinsufficiency(
    decisionThresholdOfPosterior: Double,
    data: Data,
    parameters: Param,
    simulationSampleSize: Int,
    randomGenerator: RandomGenerator
  ): Vector[Double]

  // first: H0: haploInsuffiient, H1: not haploInsuffiient
  // second: H0: haploSufficient, H1: not haploSufficient
  def poissonTestOfHaploInsufficiency(data: Data, parameters: Param): Vector[(Double, Double)]

  def posteriorOfHaploinsufficient(data: Data, parameters: Param): Vector[Posteriors]
}

trait HasFractionOfEssentials {
  def fractionOfEssentials: Double
}

trait ParamForPlot {
  def extractForPlot: Map[String, Double]
}

trait PerGeneCountData {
  def numberOfGenes: Int
  def totalTruncations: TotalNumberOfTruncations
  def observedCounts: Vector[ObservedTruncationCount]
  def expectedCounts: Vector[ExpectedTruncationCount]
  def genes: Vector[Gene]
}

case class DatumForAbsoluteModel(
    observedTruncations: ObservedTruncationCount,
    expectedTruncations: ExpectedTruncationCount,
    observedSynonyms: ObservedSynonymCount,
    probTruncations: ProbTruncation,
    probSynonym: ProbSynonym
) {
  def expectedOverObservedCount = expectedTruncations.value / observedTruncations.value

  def toLine = s"${observedTruncations.value}\t${expectedTruncations.value}\t${observedSynonyms.value}\t${probTruncations.value}\t${probSynonym.value}"
}

case class DataForAbsoluteModel(vector: Vector[DatumForAbsoluteModel], genes: Vector[Gene]) extends PerGeneCountData {

  def filter(p: DatumForAbsoluteModel => Boolean): DataForAbsoluteModel = {
    val (f1, f2) = vector zip genes filter (x => p(x._1)) unzip

    DataForAbsoluteModel(f1, f2)
  }

  val numberOfGenes = vector.size

  private val totalObservedSynonyms = vector.map(_.observedSynonyms.value).sum
  private val totalObservedTruncationsOverTotalObservedSynonym = vector.map(_.observedTruncations.value).sum.toDouble / totalObservedSynonyms
  private val sumProbTruncOverSumProbSyn = vector.map(x => x.probTruncations.value).sum.toDouble / vector.map(_.probSynonym.value).sum

  def dataForRelativeModel = UnscaledProbabilitiesWithCounts(vector.map(x => x.observedTruncations -> x.probTruncations), genes)
  def observedCounts: Vector[ObservedTruncationCount] = vector.map(_.observedTruncations)
  def expectedCounts: Vector[ExpectedTruncationCount] = vector.map(_.expectedTruncations)
  def expectedRelativeFrequencies: ScaledProbabilities = {
    val sum = expectedCounts.map(_.value).sum
    ScaledProbabilities(expectedCounts.map(x => NormalizedProbTruncation(x.value / sum.toDouble)))
  }

  private def rescaleExpectationToTotalTruncationCount(totalCount: TotalNumberOfTruncations): Vector[ExpectedTruncationCount] =
    expectedRelativeFrequencies.vector.map(f => ExpectedTruncationCount(totalCount.value * f.value))

  /* This assumes that the expected truncations are calculated with regression (fixed alpha). */
  def rescaleExpectationToTotalSynonymCount(totalSynCount: TotalNumberOfSynonymVariants): Vector[ExpectedTruncationCount] = rescaleExpectationToTotalTruncationCount(TotalNumberOfTruncations((sumProbTruncOverSumProbSyn * totalSynCount.value).toInt))

  def projectTotalTruncationsToTotalSynonymCount(totalSynCount: TotalNumberOfSynonymVariants): TotalNumberOfTruncations =
    TotalNumberOfTruncations((totalObservedTruncationsOverTotalObservedSynonym * totalSynCount.value).toInt)

  def totalTruncations = TotalNumberOfTruncations(vector.map(_.observedTruncations.value).sum)

  def totalSynonyms = vector.map(_.observedSynonyms.value).sum

  def makeBootstrapReplicas(
    replica: Int,
    rnd: RandomGenerator
  ): List[DataForAbsoluteModel] = {
    mybiotools.stat.Resampling.bootstrap(replica, vector, rnd) {
      case (sample, idx) =>
        (DataForAbsoluteModel(sample.toVector, genes))
    }.toList
  }

  def downSampleTruncationCountsToSynonymCount(
    targetSynCount: TotalNumberOfSynonymVariants,
    rnd: RandomGenerator
  ): Vector[ObservedTruncationCount] =
    downSampleTruncationCountsToTruncationCount(projectTotalTruncationsToTotalSynonymCount(targetSynCount), rnd)

  def downSampleTruncationCountsToTruncationCount(
    targetTruncationCount: TotalNumberOfTruncations,
    rnd: RandomGenerator
  ): Vector[ObservedTruncationCount] = {
    DownsamplingHelpers.downSampleTruncationCountsToTruncationCount(observedCounts, targetTruncationCount, rnd)
  }

  def numberOfTruncatedGenesInDownsampledData(
    targetTruncationCounts: List[TotalNumberOfTruncations],
    rnd: RandomGenerator
  ): Seq[(TotalNumberOfTruncations, TotalNumberOfTruncatedGenes)] = {

    val fakeVariantList: Stream[GeneId] = DownsamplingHelpers.makeFakeVariantsFromCounts(observedCounts)
    targetTruncationCounts.map { targetVariants =>
      targetVariants -> TotalNumberOfTruncatedGenes(RandomShuffler.reservoirSampling(fakeVariantList, targetVariants.value, rnd).distinct.size)
    }
  }

}

case class UnscaledProbabilities(vector: Vector[ProbTruncation]) {
  def rescale = {
    val sum = vector.map(_.value).sum
    ScaledProbabilities(vector.map(x => NormalizedProbTruncation(x.value / sum)))
  }
}
case class ScaledProbabilities(vector: Vector[NormalizedProbTruncation]) {
  assert(math.abs(vector.map(_.value).sum - 1.0) < 1E-8)
}

case class UnscaledProbabilitiesWithCounts(vector: Vector[(ObservedTruncationCount, ProbTruncation)], genes: Vector[Gene]) {
  def rescale = {
    val sum = vector.map(_._2.value).sum
    ScaledProbabilitiesWithCounts(vector.map(x => x._1 -> NormalizedProbTruncation(x._2.value / sum)), genes)
  }
  lazy val variantCount = TotalNumberOfTruncations(vector.map(_._1.value).sum)
  def variantCounts = vector.map(_._1)
  def probabilities = UnscaledProbabilities(vector.map(_._2))
  def filter(p: ((ObservedTruncationCount, ProbTruncation)) => Boolean) = UnscaledProbabilitiesWithCounts(vector.filter(p), genes)
}

case class ScaledProbabilitiesWithCounts(vector: Vector[(ObservedTruncationCount, NormalizedProbTruncation)], genes: Vector[Gene]) extends PerGeneCountData {
  val numberOfGenes = vector.size
  assert(math.abs(vector.map(_._2.value).sum - 1.0) < 1E-8)
  def variantCount = TotalNumberOfTruncations(vector.map(_._1.value).sum)
  def totalTruncations = variantCount
  def probabilities = ScaledProbabilities(vector.map(_._2))
  def variantCounts = vector.map(_._1)
  def observedCounts = variantCounts
  def expectedCounts = {
    val tot = totalTruncations.value
    vector.map(x => ExpectedTruncationCount(x._2.value * tot))
  }
}

object DataForAbsoluteModel {
  def apply(
    truncationsInGenes: Seq[String],
    expectedCounts: Seq[(Gene, ExpectedTruncationCount)],
    syncounts: Map[String8, ObservedSynonymCount]
  ): DataForAbsoluteModel = {
    val genevar: Map[String, Int] =
      truncationsInGenes
        .groupBy(x => x).toSeq.map(x => (x._1, x._2.size)).toMap

    val list = expectedCounts.map(g => DatumForAbsoluteModel(ObservedTruncationCount(genevar.get(g._1.hgnc.value).getOrElse(0)), (g._2), syncounts(g._1.hgnc), ProbTruncation(g._1.probFS + g._1.probStop), ProbSynonym(g._1.probSynonym))).toVector
    DataForAbsoluteModel(list, expectedCounts.map(_._1).toVector)
  }
}

case class AbsoluteParameters(fractionOfEssentials: Double, falseRate: Double, penetrance: Double) extends HasFractionOfEssentials with ParamForPlot {
  override def toString = s"Parameters(phi=$fractionOfEssentials, pf=$falseRate, pen=$penetrance)"
  def extractForPlot = Map("phi" -> fractionOfEssentials, "pf" -> falseRate, "pen" -> penetrance)
}

case object AbsoluteMutationRateModelProxy extends ModelProxy[DataForAbsoluteModel, AbsoluteParameters, TotalNumberOfSynonymVariants] {
  def model = AbsoluteMutationRateModel
}

object AbsoluteMutationRateModel extends Model[DataForAbsoluteModel, AbsoluteParameters, TotalNumberOfSynonymVariants] with Serializable {
  type Parameters = AbsoluteParameters
  val Parameters = AbsoluteParameters

  def indepFactory(k: Int) = TotalNumberOfSynonymVariants(k)

  def maxObservedX(data: DataForAbsoluteModel) = TotalNumberOfSynonymVariants(data.totalSynonyms)

  def observedTruncatedGenesInDownsampledData(d: DataForAbsoluteModel, x: TotalNumberOfSynonymVariants, rnd: RandomGenerator) = TotalNumberOfTruncatedGenes(d.downSampleTruncationCountsToSynonymCount(x, rnd).count(_.value > 0))

  def copyParameterNeutral(p: Parameters) = p.copy(fractionOfEssentials = 0.0)

  def parameterMean(ps: Seq[Parameters]) = AbsoluteParameters(
    SummaryStat(ps.map(_.fractionOfEssentials)).mean,
    SummaryStat(ps.map(_.falseRate)).mean,
    SummaryStat(ps.map(_.penetrance)).mean
  )

  def predictedHICounts(p: Parameters, d: DataForAbsoluteModel) = d.expectedCounts.map(e => lambdas(e, p).lambdaHI * p.fractionOfEssentials)
  def predictedHSCounts(p: Parameters, d: DataForAbsoluteModel) = d.expectedCounts.map(e => lambdas(e, p).lambdaHS * (1.0 - p.fractionOfEssentials))

  def predictedCountsPlugBackPosterior(p: Parameters, d: DataForAbsoluteModel) = d.vector.map(e => lambdas(e.expectedTruncations, p).lambdaHI * posteriorOfHaploinsufficient(p, e.expectedTruncations, e.observedTruncations).pHI + lambdas(e.expectedTruncations, p).lambdaHS * posteriorOfHaploinsufficient(p, e.expectedTruncations, e.observedTruncations).pHS)

  def predictedCountsPlugBackPosteriorHardThreshold(p: Parameters, d: DataForAbsoluteModel) = d.vector.map { e =>
    val posteriors = posteriorOfHaploinsufficient(p, e.expectedTruncations, e.observedTruncations)
    val l = lambdas(e.expectedTruncations, p)
    if (posteriors.pHI > posteriors.pHS) l.lambdaHI else l.lambdaHS
  }

  def predictNumberOfTruncatedGenes(
    p: Parameters,
    d: DataForAbsoluteModel,
    x: TotalNumberOfSynonymVariants
  ) =
    predictNumberOfGenesWithAtLeastOneSum(
      p,
      d.rescaleExpectationToTotalSynonymCount(x)
    )

  def predictNumberOfHaploInsufficientTruncatedGenes(
    p: Parameters,
    d: DataForAbsoluteModel,
    x: TotalNumberOfSynonymVariants
  ) =
    ExpectedNumberOfTruncatedGenes(predictNumberOfGenesWithAtLeastOne(
      p,
      d.rescaleExpectationToTotalSynonymCount(x)
    )._2)

  def predictNumberOfHaploSufficientTruncatedGenes(
    p: Parameters,
    d: DataForAbsoluteModel,
    x: TotalNumberOfSynonymVariants
  ) =
    ExpectedNumberOfTruncatedGenes(predictNumberOfGenesWithAtLeastOne(
      p,
      d.rescaleExpectationToTotalSynonymCount(x)
    )._1)

  def predictFractionOfNoisyObservedTruncations(p: Parameters, data: DataForAbsoluteModel): Double = {

    val predictedSumOfHIMutations = data.expectedCounts.map { e => AbsoluteMutationRateModel.lambdas(e, p).lambdaHI * p.fractionOfEssentials }.sum

    val predictedSumOfHSMutations = data.expectedCounts.map { e => AbsoluteMutationRateModel.lambdas(e, p).lambdaHS * (1.0 - p.fractionOfEssentials) }.sum

    data.expectedCounts.map(e => AbsoluteMutationRateModel.lambdas(e, p).lambdaHI).sum / (predictedSumOfHIMutations + predictedSumOfHSMutations)
  }

  def predictFractionOfObservedTruncationsWhichAreInHIGenes(p: Parameters, data: DataForAbsoluteModel): Double = {

    val predictedSumOfHIMutations = data.expectedCounts.map { e => AbsoluteMutationRateModel.lambdas(e, p).lambdaHI * p.fractionOfEssentials }.sum

    val predictedSumOfHSMutations = data.expectedCounts.map { e => AbsoluteMutationRateModel.lambdas(e, p).lambdaHS * (1.0 - p.fractionOfEssentials) }.sum

    predictedSumOfHIMutations / (predictedSumOfHIMutations + predictedSumOfHSMutations)
  }

  def predictFractionOfObservedTruncationsWhichDoesNotOriginateFromGivenErrorLevelAndAreInHI(p: Parameters, data: DataForAbsoluteModel, error: Double) = {
    (1.0 - error / predictFractionOfNoisyObservedTruncations(p, data)) * predictFractionOfObservedTruncationsWhichAreInHIGenes(p, data)

  }

  def predictFractionOfGenesWithAtLeastOneTruncationNotFromThisErrorRate(p: Parameters, data: DataForAbsoluteModel, error: Double, t: TotalNumberOfTruncations): ExpectedNumberOfTruncatedGenes = ???

  /**
   * Performs two one-sided tests whether the observed count is higher than the expected HI and whether it is lower than the expected HS.
   *
   * @return  A vector of (p_hi,p_hs) pairs where p_hi is the p-value for the test that the observed count is larger than the expected under HI
   * and p_hs is the p-value for the test that the observed count is smaller than expected under HS
   */
  def poissonTestOfHaploInsufficiency(
    data: DataForAbsoluteModel,
    parameters: Parameters
  ): Vector[(Double, Double)] = {
    data.vector map { d =>
      val l = lambdas(d.expectedTruncations, parameters)
      import l._

      val p_hi = jdistlib.Poisson.cumulative(d.observedTruncations.value, lambdaHI, false, false)
      val p_hs = jdistlib.Poisson.cumulative(d.observedTruncations.value, lambdaHS, true, false)
      (p_hi, p_hs)

    }
  }

  def predictDensity(p: Parameters, d: DataForAbsoluteModel) = predictDensity(p, d.expectedCounts)

  // def calculateNeutralExpectedTruncationsWithoutRegression(genes: Seq[Gene]): Seq[(Gene, ExpectedTruncationCount)] = {
  //   genes.map { g =>
  //     val psyn = g.probSynonym
  //     val ptrunc = g.probSplice + g.probFS + g.probStop
  //     val syncount = g.synonymCount
  //     g -> ExpectedTruncationCount(syncount * ptrunc / psyn)
  //   }
  // }

  def calculateNeutralExpectedTruncationsWithRegression(genes: Seq[Gene], syncounts: Map[String8, ObservedSynonymCount]) = {
    import org.saddle._
    def fit(genes: Seq[Gene]) = {
      val (p, s) = genes.map { g =>
        g.probSynonym -> syncounts(g.hgnc).value
      }.toArray.unzip

      // this is a simple linear regression without intercept
      val xy = p zip s map (x => x._1 * x._2)
      val xx = p map (x => x * x)
      xy.sum / xx.sum

    }

    val slope1 = fit(genes)
    val residuals = genes.map(g => syncounts(g.hgnc).value - g.probSynonym * slope1)
    val residualsSummary = SummaryStat(residuals)
    val filteredgenes = (genes zip residuals).filter(x => math.abs(x._2) <= residualsSummary.stddev * 3) map (_._1)

    val slope2 = fit(filteredgenes)

    val r = filteredgenes.map { gene =>
      gene -> ExpectedTruncationCount(gene.probStop * slope2 + gene.probFS * slope2)
    }

    // println("Filtered total syn: " + r.map(_._1.synonymCount).sum)
    // println("Filtered total expected syn: " + r.map(_._1.probSynonym * slope2).sum)
    r -> slope2
  }

  def lambdas(
    expectedCount: ExpectedTruncationCount,
    parameter: Parameters
  ): Lambdas = {
    val lambdaHI = expectedCount.value * (parameter.falseRate + 1.0) * (1.0 - parameter.penetrance)
    val lambdaHS = expectedCount.value * (parameter.falseRate + 1.0)
    Lambdas(lambdaHI, lambdaHS)
  }

  def predictDensity(
    parameters: Parameters,
    expectedCounts: Vector[ExpectedTruncationCount]
  ): Vector[(Int, Double)] =
    (0 to 80 map { (count: Int) =>
      FastMath.exp(mybiotools.
        stat.logsumexp(expectedCounts.map { expected =>
          logLikelihoodOfTruncationCount(parameters, expected, ObservedTruncationCount(count))
        }: _*))
    }).toSeq.toVector.zipWithIndex.map(_.swap)

  def predictNumberOfGenesWithAtLeastOne(
    parameters: Parameters,
    expectedCounts: Vector[ExpectedTruncationCount]
  ): (Double, Double) =
    RelativeMutationRateModel.expectedNumbersOfAtLeastOne(expectedCounts.map(expected => lambdas(expected, parameters)), parameters.fractionOfEssentials)

  def predictNumberOfGenesWithAtLeastOneSum(
    parameters: Parameters,
    expectedCounts: Vector[ExpectedTruncationCount]
  ): ExpectedNumberOfTruncatedGenes = {
    val r = predictNumberOfGenesWithAtLeastOne(parameters, expectedCounts)
    ExpectedNumberOfTruncatedGenes(r._1 + r._2)
  }

  def simulate(
    parameters: Parameters,
    expectedCounts: Vector[ExpectedTruncationCount],
    rnd: RandomGenerator
  ): Vector[ObservedTruncationCount] = {
    val rndd = new RandomDataGenerator(rnd)
    expectedCounts.map {
      case (expectedCount) =>
        val l = lambdas(expectedCount, parameters)
        import l._
        (ObservedTruncationCount(((if (rnd.nextDouble < parameters.fractionOfEssentials) rndd.nextPoisson(lambdaHI) else rndd.nextPoisson(lambdaHS))).toInt))
    } toVector
  }

  def logLikelihoodOfTruncationCount(
    parameters: Parameters,
    expectedCount: ExpectedTruncationCount,
    numberOfTruncations: ObservedTruncationCount
  ): Double = {
    assert(numberOfTruncations.value >= 0)

    val l = lambdas(expectedCount, parameters)

    val logLikelihoodTruncationsGivenHI = Poisson.logProbability(numberOfTruncations.value, mean = l.lambdaHI)
    val logLikelihoodTruncationsGivenHS = Poisson.logProbability(numberOfTruncations.value, mean = l.lambdaHS)

    mybiotools.stat.logsumexp(FastMath.log(parameters.fractionOfEssentials) + logLikelihoodTruncationsGivenHI, FastMath.log(1.0 - parameters.fractionOfEssentials) + logLikelihoodTruncationsGivenHS)

  }

  def logLikelihoodOfDataGivenParameters(
    parameters: Parameters,
    data: DataForAbsoluteModel
  ): Double = {
    data.vector.foldLeft(0.0) {
      case (s, d) =>
        s + logLikelihoodOfTruncationCount(parameters, d.expectedTruncations, d.observedTruncations)
    }
  }

  def posteriorOfHaploinsufficient(
    parameters: Parameters,
    expectedCount: ExpectedTruncationCount,
    numberOfTruncations: ObservedTruncationCount
  ): Posteriors =
    RelativeMutationRateModel.posteriorOfFirstComponent(
      lambdas(expectedCount, parameters),
      parameters.fractionOfEssentials,
      numberOfTruncations
    )

  def posteriorOfHaploinsufficient(
    data: DataForAbsoluteModel,
    parameters: Parameters
  ): Vector[Posteriors] = data.vector.map {
    d =>
      posteriorOfHaploinsufficient(parameters, d.expectedTruncations, d.observedTruncations)
  }

  def powerToHaveHighPostieriorOfHaploinsufficiency(
    decisionThresholdOfPosterior: Double,
    data: DataForAbsoluteModel,
    parameters: Parameters,
    simulationSampleSize: Int,
    randomGenerator: RandomGenerator
  ): Vector[Double] = {
    val rnd = new RandomDataGenerator(randomGenerator)
    data.expectedCounts.map { expectedTruncations =>
      val successes = 1 to simulationSampleSize count { i =>
        val count = rnd.nextPoisson(lambdas(expectedTruncations, parameters).lambdaHI).toInt
        posteriorOfHaploinsufficient(parameters, expectedTruncations, ObservedTruncationCount(count)).pHI >= decisionThresholdOfPosterior
      }
      successes.toDouble / simulationSampleSize
    }
  }

  def maximumLikelihood(data: DataForAbsoluteModel): OptimalParameter[Parameters] = {
    val function =
      (p: Seq[Double]) => {
        val v = -1.0 * logLikelihoodOfDataGivenParameters(Parameters(p(0), p(1), p(2)), data)
        v
      }

    val optimum = minimizeN(
      function,
      List(0.25, -0.6, 0.75),
      List(0.0001, -0.9999, 0.000),
      List(0.9999, 0.9999, 1.0)
    )
    OptimalParameter(Parameters(optimum(0), optimum(1), optimum(2)), function(optimum))
  }

  def em(data: DataForAbsoluteModel) = {
    import mybiotools.stat.PoissonMixture
    val countsWithExpectedSynonym = data.vector.map { case d => (d.observedTruncations.value, d.expectedTruncations.value) }
    val estimatedParameters = PoissonMixture.emKComponentPoissonWeighted(countsWithExpectedSynonym, List(PoissonMixture.Parameters(5, 0.3), PoissonMixture.Parameters(6, 0.7)))
    assert(estimatedParameters(0).mixture + estimatedParameters(1).mixture - 1.0 <= 1E-5, estimatedParameters.toString)
    val lowerMean = if (estimatedParameters(0).mean < estimatedParameters(1).mean) estimatedParameters(0).mean else estimatedParameters(1).mean
    val lowerMeanMixture = if (estimatedParameters(0).mean < estimatedParameters(1).mean) estimatedParameters(0).mixture else estimatedParameters(1).mixture
    val higherMean = if (estimatedParameters(0).mean < estimatedParameters(1).mean) estimatedParameters(1).mean else estimatedParameters(0).mean

    val falseRate = higherMean - 1.0
    val penetrance = higherMean - lowerMean
    val optimum = Parameters(lowerMeanMixture, falseRate, penetrance)
    val logLik = logLikelihoodOfDataGivenParameters(optimum, data)
    OptimalParameter(optimum, logLik)

  }

  def cumulativeDistribution(
    parameters: Parameters,
    expected: Vector[ExpectedTruncationCount],
    location: ObservedTruncationCount
  ): Double = {

    def logCumulativeProbability(
      parameters: Parameters,
      expectedCount: ExpectedTruncationCount,
      numberOfTruncations: ObservedTruncationCount
    ): Double = {

      val l = lambdas(expectedCount, parameters)
      import l._

      val logCdfHI = jdistlib.Poisson.cumulative(numberOfTruncations.value, lambdaHI, true, true)
      val logCdfHS = jdistlib.Poisson.cumulative(numberOfTruncations.value, lambdaHS, true, true)

      mybiotools.stat.logsumexp(
        FastMath.log(parameters.fractionOfEssentials) + logCdfHI,
        FastMath.log(1.0 - parameters.fractionOfEssentials) + logCdfHS
      )

    }

    math.exp(mybiotools.stat.logsumexp(expected.map { d =>
      logCumulativeProbability(parameters, d, location)
    }: _*)) / expected.size
  }

  def cdfFit(data: DataForAbsoluteModel): OptimalParameter[Parameters] = {

    val expectedcounts = data.expectedCounts

    val cdf =
      (param: Seq[Double], loc: Double) => cumulativeDistribution(Parameters(param(0), param(1), param(2)), expectedcounts, ObservedTruncationCount(loc.toInt))

    val (optim, minimum) = mybiotools.stat.FitCDF.fitCDF(
      data.observedCounts.map(_.value.toDouble),
      cdf,
      List(0.25, -0.6, 0.75),
      List(0.0001, -0.9999, 0.000),
      List(0.9999, 0.9999, 1.0)
    )

    OptimalParameter(Parameters(optim(0), optim(1), optim(2)), minimum)
  }

  def cdfFitNoPenetrance(data: DataForAbsoluteModel): OptimalParameter[Parameters] = {

    val expectedcounts = data.expectedCounts

    val cdf =
      (param: Seq[Double], loc: Double) => cumulativeDistribution(Parameters(param(0), param(1), 1.0 - 1E-6), expectedcounts, ObservedTruncationCount(loc.toInt))

    val (optim, minimum) = mybiotools.stat.FitCDF.fitCDF(
      data.observedCounts.map(_.value.toDouble),
      cdf,
      List(0.25, -0.6),
      List(0.0001, -0.9999),
      List(0.9999, 0.9999)
    )

    OptimalParameter(Parameters(optim(0), optim(1), 1.0 - 1E-6), minimum)
  }

  def cdfFitOnlyError(data: DataForAbsoluteModel): OptimalParameter[Parameters] = {

    val expectedcounts = data.expectedCounts

    val cdf =
      (param: Seq[Double], loc: Double) => cumulativeDistribution(Parameters(1E-6, param(0), 1.0), expectedcounts, ObservedTruncationCount(loc.toInt))

    val (optim, minimum) = mybiotools.stat.FitCDF.fitCDF(
      data.observedCounts.map(_.value.toDouble),
      cdf,
      List(-0.6),
      List(-0.9999),
      List(0.9999)
    )

    OptimalParameter(Parameters(1E-6, optim(0), 1.0), minimum)
  }

  def downsamplingFit(data: DataForAbsoluteModel, rnd: RandomGenerator): OptimalParameter[Parameters] = {

    def predictedTruncations(rescaled: Vector[ExpectedTruncationCount], p: Parameters) = {
      val (hi, hs) = predictNumberOfGenesWithAtLeastOne(p, rescaled)
      hi + hs
    }

    val referencePoints = 1 to 10 flatMap (x => List(200000, 250000, 300000, 350000, 400000, data.totalSynonyms)) map (x => TotalNumberOfSynonymVariants(x))

    val rescaledToReferencepoints = referencePoints.map(t => data.rescaleExpectationToTotalSynonymCount((t)))

    val observedTruncations = referencePoints.map(x => data.downSampleTruncationCountsToSynonymCount(x, rnd).count(_.value > 0))

    val function =
      (p: Seq[Double]) => {
        val r = referencePoints.zipWithIndex.map { t =>
          val pred = predictedTruncations(rescaledToReferencepoints(t._2), Parameters(p(0), p(2), p(1)))
          val o: Int = observedTruncations(t._2)
          (pred - o) * (pred - o)
        }.sum
        r
      }

    val optimum = minimizeN(function, List(0.15, 0.8, 0.15), List(0.0001, 0.0000, -0.99999), List(0.9999, 1.0, 0.9999999))
    OptimalParameter(Parameters(optimum(0), optimum(2), optimum(1)), function(optimum))

  }

}