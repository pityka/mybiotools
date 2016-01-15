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

import mybiotools.gwascommons.associationresults._
import mybiotools.gwascommons._
import mybiotools.stringstore._
import mybiotools.eq._

import java.io.File
import mybiotools._
import org.saddle._
import mybiotools.gwascommons.genotypedata._

object LDPruning {

  case class LDPoint[T <: HasName](snp1: T, snp2: T, rsq: Double) {
    def _1 = snp1
    def _2 = snp2
    def _3 = rsq
  }

  def calculateLDWithPlink[T <: HasName with HasGenomicLocation](
    bedTrunk: String,
    snps: Traversable[T],
    ldThreshold: Double
  ): Option[Frame[String, String, Double]] = {

    val tmpfilesnps = TempFile.createTempFile(".snplist")
    mybiotools.writeToFile(tmpfilesnps.getAbsolutePath, snps.map(_.name).mkString("\n"))

    val tmpfileplinkout = TempFile.createTempFile(".plinkout")

    val plinkcmd = s"plink --bfile " + bedTrunk + " --extract " + tmpfilesnps.getAbsolutePath + " --r2 --ld-window 10000  --ld-window-kb 10000 --silent --out " + tmpfileplinkout + " --noweb --allow-no-sex  --ld-window-r2 " + ldThreshold + " "

    val b = execIsFinishedWithoutError(plinkcmd)

    if (b)
      Some(openSource(tmpfileplinkout.getAbsolutePath + ".ld") { source =>
        val iter = readSNPCorrelationsFromPlinkWithNames(source)
        readSNPCorrelations(iter)
      })
    else None

  }

  /**
   * LD pruning of loci
   *
   * Calculates r2 snp correlations of the supplied genotype files,
   * then prunes the association results based on these correlations
   * Only those snps are used who pass a p value filter. This makes computation feasible.
   * If a snp is not present in the genomicmap, it is assumed to be in zero ld with everything
   *
   * @param bedTrunks relative path of the genotype files trunk split by chromosome
   * @param assoc association results should be sorted by p-value if needed!
   * @param ldThreshold consider two snps independent if their r2 is lower than this threshold
   * @return pruned association results
   */
  def pruneAndLDFromSortedLoci[T <: HasName with HasGenomicLocation](
    bedTrunks: Map[String, File],
    assoc: Vector[T],
    ldThreshold: Double
  ): Vector[T] = {
    if (assoc.size == 0) Vector()
    else {
      if (ldThreshold == 1.0) assoc
      else {
        val assocByChr: Map[String, Iterable[T]] = assoc.groupBy(_.genomicLocation.chromosome)

        val ldmap = assocByChr.map {
          case (chr, loci) =>
            calculateLDWithPlink(
              bedTrunks(chr).getAbsolutePath,
              loci,
              ldThreshold
            )
        }.filter(_.isDefined).map(_.get)

        if (ldmap.size > 0)
          prune(ldmap.reduce { (_ concat _) }, assoc.toVector, ldThreshold)
        else Vector()
      }
    }
  }

  def prune[T <: HasName, K](
    correlationMatrix: Frame[String, String, Double],
    sorted: Seq[T],
    threshold: Double
  ): Vector[T] = {
    val pf = new PartialFunction[(String, String), Double] {
      def apply(t: (String, String)) = correlationMatrix.firstCol(t._1).first(t._2).get

      def isDefinedAt(t: (String, String)) = correlationMatrix.colIx.contains(t._1) && correlationMatrix.rowIx.contains(t._2) && !correlationMatrix.firstCol(t._1).first(t._2).isNA
    }
    prune(pf, sorted, threshold)
  }

  // if correlationMatrix does not has a key, then that key is in <0.2 ld with every other
  def prune[T <: HasName, K](
    correlationMatrix: PartialFunction[(String, String), Double],
    sorted: Seq[T],
    threshold: Double
  ): Vector[T] = {

    val liftedCorrelationFunction = correlationMatrix.lift

    @scala.annotation.tailrec
    def recurse(part1: Seq[T], part2: Vector[T]): Vector[T] = {
      if (part1.size == 0) part2
      else {
        val head = part1.head
        val filtered = {

          val ldwithhead: Seq[T] = part1.drop(1).filter(s => liftedCorrelationFunction((head.name, s.name)).getOrElse(0.0) >= threshold).seq

          part1.drop(1).filterNot(x => ldwithhead.contains(x))

        }
        recurse(filtered, part2 :+ head)
      }
    }

    recurse(sorted, Vector())
  }

  def pruneIteratorWithoutGenomicMap[T <: HasName](
    dosages: Iterator[Tuple2[T, Array[Float]]],
    missingValue: Float,
    maxDistanceInNumberOfSNPs: Int,
    threshold: Double,
    keepSNPs: Set[String8],
    removeSNPsInLDWithThese: Seq[Array[Float]]
  )(implicit ord: Ordering[GenomicLocation]): Iterator[Tuple2[T, Array[Float]]] = {

    val tail = collection.mutable.Queue[Array[Float]]()

    dosages.filterNot {
      case (pdsr, array) =>

        if (!tail.isEmpty && tail.size >= maxDistanceInNumberOfSNPs) {
          tail.dequeue
        }

        val discard = ((tail.reverse.exists(ar => mybiotools.stat.RSquared.rsquared(ar, array, missingValue) > threshold)) ||
          removeSNPsInLDWithThese.exists(ar => mybiotools.stat.RSquared.rsquared(ar, array, missingValue) > threshold)) && !keepSNPs.contains(pdsr.name)

        if (!discard) {
          tail.enqueue(array)
        }

        discard

    }
  }

  def pruneIterator[T <: HasName](
    dosages: Iterator[Tuple2[T, Array[Float]]],
    map: GenomicMap,
    missingValue: Float,
    maxDistance: Int,
    threshold: Double,
    keepSNPs: Set[String8],
    removeSNPsInLDWithThese: Seq[Array[Float]]
  )(implicit ord: Ordering[GenomicLocation]): Iterator[Tuple2[T, Array[Float]]] = {

    var last: Option[GenomicLocation] = None

    val tail = collection.mutable.Queue[(GenomicLocation, Array[Float])]()

    dosages.filterNot {
      case (pdsr, array) =>

        val gl = map.get(pdsr.name) match {
          case Some(x) => x
          case None => scala.util.Try(GenomicLocation(pdsr.name)).toOption.get
        }

        if (last.isDefined) {
          assert(ord.lteq(last.get, gl), s"chromosome order required for LD pruning. $last should be lteq $gl")
          last = Some(gl)
        } else {
          last = Some(gl)
        }

        if (!tail.isEmpty && (tail.head._1.chromosome != gl.chromosome || (gl.basePairPosition - tail.head._1.basePairPosition > maxDistance))) {
          tail.dequeue
        }

        val discard = ((tail.reverse.exists(ar => mybiotools.stat.RSquared.rsquared(ar._2, array, missingValue) > threshold)) ||
          removeSNPsInLDWithThese.exists(ar => mybiotools.stat.RSquared.rsquared(ar, array, missingValue) > threshold)) && !keepSNPs.contains(pdsr.name)

        if (!discard) {
          tail.enqueue(gl -> array)
        }

        discard

    }
  }

  def readSNPCorrelationsFromPlinkWithNames(s: scala.io.Source): Iterator[LDPoint[OnlyName]] =
    s.getLines.drop(1).map { line =>
      val spl = mybiotools.fastSplitSeparator(line, ' ')
      val gla = OnlyName(StringStore(new String(spl(2))))
      val glb = OnlyName(StringStore(new String(spl(5))))
      val r2 = spl(6).toFloat
      LDPoint(gla, glb, r2)
    }

  def readSNPCorrelations[T <: HasName](iter: Iterator[LDPoint[T]]): Frame[String, String, Double] = {
    val map = collection.mutable.Map[T, collection.mutable.Map[T, Double]]()
    iter.foreach { elem =>
      map.get(elem._1) match {
        case None => map.update(elem._1, collection.mutable.Map(elem._2 -> elem._3))
        case Some(x) => x.update(elem._2, elem._3)
      }
      map.get(elem._2) match {
        case None => map.update(elem._2, collection.mutable.Map(elem._1 -> elem._3))
        case Some(x) => x.update(elem._1, elem._3)
      }
    }
    Frame(
      map.mapValues(x => Series(x.toSeq.map(y => (y._1.name.value, y._2.toDouble)): _*)).toSeq.map(y => (y._1.name.value, y._2)): _*
    )
  }

}