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
import dispensability.RelativeMutationRateModel.Parameters
import dispensability.DownsamplingHelpers.GeneId

case class EstimateEssentialsBatchResult(sumOfSquares: Double)

object RelativeSimulation {

  private def countTruncatedGenes(counts: Array[ObservedTruncationCount]) = TotalNumberOfTruncatedGenes(counts.count(_.value > 0))

  def simulateGeneCounts(
    variants: TotalNumberOfTruncations,
    parameters: Parameters,
    fullGenome: MultinomialRandomGenerator[GeneId],
    haploSufficientGenome: MultinomialRandomGenerator[GeneId]
  ): Array[ObservedTruncationCount] = {

    val falseCalls: Int = (new BinomialDistribution(variants.value, parameters.noise)).sample
    val trueCalls = variants.value - falseCalls

    val falseSample: Array[Int] = fullGenome.sample(falseCalls)
    val trueSample: Array[Int] = haploSufficientGenome.sample(trueCalls)
    assert(falseSample.size == trueSample.size)
    val joined = falseSample zip trueSample map (x => ObservedTruncationCount(x._1 + x._2))

    joined
  }

  def simulateNumberOfObservedGenes(
    variants: TotalNumberOfTruncations,
    parameters: Parameters,
    fullGenome: MultinomialRandomGenerator[GeneId],
    haploSufficientGenome: MultinomialRandomGenerator[GeneId]
  ): TotalNumberOfTruncatedGenes = {

    val joined = simulateGeneCounts(variants, parameters, fullGenome, haploSufficientGenome)

    countTruncatedGenes(joined)

  }

  def randomSelectNonEssentials(
    density: Vector[(GeneId, NormalizedProbTruncation)],
    parameters: Parameters,
    rnd: RandomGenerator
  ): Vector[(GeneId, NormalizedProbTruncation)] = {
    val numberOfEssentials = (density.size * parameters.fractionOfEssentials).toInt
    val last = RandomShuffler.shuffle(density, rnd).drop(numberOfEssentials)
    val sum = last.map(_._2.value).sum
    last.map(x => x._1 -> NormalizedProbTruncation(x._2.value / sum))
  }

  def squaredDiffGeneCount(estimated: Double, expectedGenes: TotalNumberOfTruncatedGenes) =
    (estimated - expectedGenes.value) * (estimated - expectedGenes.value)

  def simulationGeneCounts(
    fullNeutralDensity: ScaledProbabilities,
    parameters: Parameters,
    replicasOfEssentialDice: Int,
    replicasOfVariantDice: Int,
    variants: Seq[TotalNumberOfTruncations],
    commonsrnd: RandomGenerator
  ): Map[TotalNumberOfTruncations, IndexedSeq[Array[ObservedTruncationCount]]] = {
    val genes: Vector[(GeneId, NormalizedProbTruncation)] = fullNeutralDensity.vector.zipWithIndex.map(x => GeneId(x._2) -> x._1)

    val fullGenome = new MultinomialRandomGenerator(commonsrnd, false, genes.map(x => x._1 -> x._2.value): _*)

    (1 to replicasOfEssentialDice flatMap { (r1: Int) =>
      val haploSufficientGenome = new MultinomialRandomGenerator(commonsrnd, false, randomSelectNonEssentials(genes, parameters, commonsrnd).map(x => x._1 -> x._2.value): _*)

      1 to replicasOfVariantDice flatMap { (r2: Int) =>

        variants map {
          case (variants) =>
            variants -> simulateGeneCounts(variants, parameters, fullGenome, haploSufficientGenome)
        }
      }
    }).groupBy(_._1).map(x => x._1 -> x._2.map(_._2))
  }

  def simulation(
    fullNeutralDensity: ScaledProbabilities,
    parameters: Parameters,
    replicasOfEssentialDice: Int,
    replicasOfVariantDice: Int,
    variants: Seq[TotalNumberOfTruncations],
    commonsrnd: RandomGenerator
  ): Map[TotalNumberOfTruncations, Seq[TotalNumberOfTruncatedGenes]] = {
    val genes: Vector[(GeneId, NormalizedProbTruncation)] = fullNeutralDensity.vector.zipWithIndex.map(x => GeneId(x._2) -> x._1)

    val fullGenome = new MultinomialRandomGenerator(commonsrnd, false, genes.map(x => x._1 -> x._2.value): _*)

    (1 to replicasOfEssentialDice flatMap { (r1: Int) =>
      val haploSufficientGenome = new MultinomialRandomGenerator(commonsrnd, false, randomSelectNonEssentials(genes, parameters, commonsrnd).map(x => x._1 -> x._2.value): _*)

      1 to replicasOfVariantDice flatMap { (r2: Int) =>

        variants map {
          case (variants) =>
            variants -> simulateNumberOfObservedGenes(variants, parameters, fullGenome, haploSufficientGenome)
        }
      }
    }).groupBy(_._1).map(x => x._1 -> x._2.map(_._2))
  }

  // def replicateRandomEssentialConfigurations(
  //   fullNeutralDensity: ScaledProbabilities,
  //   parameters:Parameters,
  //   replicasOfEssentialDice: Int,
  //   replicasOfVariantDice: Int,
  //   observed: Seq[(TotalNumberOfTruncations, TotalNumberOfTruncatedGenes)],
  //   commonsrnd: RandomGenerator): Seq[EstimateEssentialsBatchResult] = {
  //   val genes : Vector[(GeneId,NormalizedProbTruncation)] = fullNeutralDensity.vector.zipWithIndex.map(x => GeneId(x._2)->x._1)
  //   val fullGenome = new MultinomialRandomGenerator(commonsrnd, false, genes: _*)

  //   1 to replicasOfEssentialDice flatMap { r1 =>
  //     val haploSufficientGenome = new MultinomialRandomGenerator(commonsrnd, false, randomSelectNonEssentials(genes, parameters.fractionOfEssentials, parameters.penetrance, commonsrnd): _*)

  //     1 to replicasOfVariantDice map { r2 =>

  //       EstimateEssentialsBatchResult((observed.map {
  //         case (variants, referenceGeneCount) =>
  //           squaredDiffGeneCount(
  //             simulateNumberOfObservedGenes(variants, parameters, fullGenome, haploSufficientGenome), 
  //             referenceGeneCount)
  //       }).sum)
  //     }
  //   }
  // }

}