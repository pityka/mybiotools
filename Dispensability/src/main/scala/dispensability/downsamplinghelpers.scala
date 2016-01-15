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

object DownsamplingHelpers {

  def downSampleTruncationCountsToTruncationCount(
    observedCounts: Vector[ObservedTruncationCount],
    targetTruncationCount: TotalNumberOfTruncations,
    rnd: RandomGenerator
  ): Vector[ObservedTruncationCount] = {
    if (targetTruncationCount.value == 0) observedCounts.map(x => ObservedTruncationCount(0))
    else {
      val fakeVariantList: Stream[GeneId] = DownsamplingHelpers.makeFakeVariantsFromCounts(observedCounts)
      val subset: Seq[GeneId] = RandomShuffler.reservoirSampling(fakeVariantList, targetTruncationCount.value, rnd)
      DownsamplingHelpers.group(subset.toArray.map(_.value)).toVector.map(x => ObservedTruncationCount(x))
    }
  }

  /* The ith position of the return array contains the number of elements which equal i in the input array.
  *
  * Equivalent to x.groupBy(x => x).map(x => x._1 -> x._2.size).toMap. Index is stored in the array index.
   */
  def group(x: Array[Int]): Array[Int] = {
    val max = x.max
    val min = 0
    val array = Array.ofDim[Int](max + 1)
    var i = 0
    val s = x.size
    while (i < s) {
      array(x(i)) += 1
      i += 1
    }
    array
  }

  def makeReference[T <: AnnotationWithGene[T]](
    variants: Seq[VariantWithSumGenotype[T]],
    downSamples: List[TotalNumberOfTruncations],
    rnd: RandomGenerator
  ): Seq[(TotalNumberOfTruncations, TotalNumberOfTruncatedGenes)] = {
    downSamples.map { targetVariants =>
      targetVariants -> numberOfObservedGenesInDownSampledVariantList(variants, targetVariants, rnd)
    } :+ (TotalNumberOfTruncations(variants.size) -> numberOfGenesObserved(variants))
  }

  def numberOfGenesObserved[T <: AnnotationWithGene[T]](variants: Seq[VariantWithSumGenotype[T]]) = TotalNumberOfTruncatedGenes(variants.map(_.annotation.gene.hgnc).distinct.size)

  case class GeneId(value: Int) extends AnyVal
  def makeFakeVariantsFromCounts(in: Vector[ObservedTruncationCount]): Stream[GeneId] = in.toStream.zipWithIndex.flatMap {
    case ((numvar), geneidx) =>
      0 until numvar.value map { i => GeneId(geneidx) }
  }

  def numberOfObservedGenesInDownSampledVariantList[T <: AnnotationWithGene[T]](
    variants: Seq[VariantWithSumGenotype[T]],
    targetVariants: TotalNumberOfTruncations,
    rnd: RandomGenerator
  ): TotalNumberOfTruncatedGenes =
    numberOfGenesObserved(RandomShuffler.reservoirSampling(variants, targetVariants.value, rnd))

}