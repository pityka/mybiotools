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

package mybiotools.gwascommons.genotypedata

import mybiotools._
import mybiotools.stringstore._
import mybiotools.gwascommons._
import mybiotools.gwascommons.gwas._
import scala.io.Source
import org.saddle._
import java.io.RandomAccessFile
import mybiotools.eq._
import java.io.File

case class MergeDosageReport(mismatchingSNPs: List[IndexedSeq[PDosageFileRowSummary]], mergedIterator: SNPMajorIterators)

object SNPMajorMerge {
  val MissingValue = -9f
  def merge(
    inputs: IndexedSeq[SNPMajorQuery],
    minimumSNPCoverage: Double = 0.0,
    includeSNPs: Set[String8] = Set[String8](),
    excludeSNPs: Set[String8] = Set[String8](),
    includeIndividuals: Set[Individual] = Set[Individual](),
    excludeIndividuals: Set[Individual] = Set[Individual](),
    minimumMAF: Double = 0.0,
    genomicMap: GenomicMap = Map(),
    blockSize: Int = 1000,
    sortByCHR: Boolean = true
  ): MergeDosageReport = {

    val individuals = inputs.map(_.individuals)

    val individualIndices: IndexedSeq[Map[Individual, Int]] = individuals.map(inds => inds.zipWithIndex.toMap)

    def individualMeetsInclusionCriteria(ind: Individual): Boolean = {
      if (includeIndividuals.size > 0) {
        if (!excludeIndividuals.contains(ind) && includeIndividuals.contains(ind)) true
        else false
      } else if (excludeIndividuals.size > 0) {
        !excludeIndividuals.contains(ind)
      } else true
    }

    def snpNameMeetsInclusionCriteria(snpName: String8) = {
      if (includeSNPs.size > 0) {
        if (!excludeSNPs.contains(snpName) && includeSNPs.contains(snpName)) true
        else false
      } else if (excludeSNPs.size > 0) {
        !excludeSNPs.contains(snpName)
      } else true
    }

    def snpMeetsInclusionCriteria(pdosagerow: PDosageFileRowSummary, numberOfIncludedIndividuals: Int): Boolean = {
      val snpName = pdosagerow.snpName

      val inclusionfilters = snpNameMeetsInclusionCriteria(snpName)

      if (inclusionfilters) {
        val snpCoverage = if (numberOfIncludedIndividuals == 0) -1.0 else pdosagerow.count.toDouble / numberOfIncludedIndividuals.toDouble
        snpCoverage >= minimumSNPCoverage && pdosagerow.maf >= minimumMAF
      } else false
    }

    val mergedIndividuals: IndexedSeq[Individual] = individualIndices.map(_.keys).flatten.distinct.filter(individualMeetsInclusionCriteria(_)).sortBy(ind => individualIndices.head.get(ind).getOrElse(1)).toIndexedSeq

    val mergedIndividualsWithIndex = mergedIndividuals.zipWithIndex
    val mergedIndividualsWithIndexProjected = mergedIndividualsWithIndex.map(_._2).toList

    val mergedIndividualIndices =
      individualIndices.map { fileIndividualIndex =>
        val ar = Array.fill(mergedIndividuals.size)(-1)
        mergedIndividualsWithIndex.foreach {
          case (mind, midx) =>
            fileIndividualIndex.get(mind).foreach { fidx =>
              ar(midx) = fidx
            }
        }
        ar
      }

    def getDosageArray(snp: String8): Array[Float] = {
      var b = false
      var i = 0
      val returnArray = Array.fill(mergedIndividuals.size)(MissingValue)

      var individualsNotDone = mergedIndividualsWithIndexProjected

      while (!b && i < inputs.size) {
        val x = inputs(i)
        x.query(snp).foreach {
          case (pdosagerow, dosagesReadFromReader) =>

            var tmp: List[Int] = Nil

            val fileindex = mergedIndividualIndices(i)

            individualsNotDone.foreach { ind =>
              val individualIndexInJustReadArray = fileindex(ind)
              if (individualIndexInJustReadArray >= 0) {
                val candidate = dosagesReadFromReader(individualIndexInJustReadArray)
                if (candidate != inputs(i).missingValue && !candidate.isNaN) {
                  returnArray(ind) = candidate
                } else {
                  tmp = ind :: tmp
                }
              } else {
                tmp = ind :: tmp
              }
            }

            individualsNotDone = tmp

            b = individualsNotDone.isEmpty
        }
        i += 1
      }

      returnArray
    }

    val mismatchedSNPs = scala.collection.mutable.ListBuffer[IndexedSeq[PDosageFileRowSummary]]()

    val mergedSNPs: Seq[Tuple3[String8, String8, String8]] = {
      import mybiotools.gwascommons._

      val afterSort = {
        val beforeSort: Array[String8] = inputs.flatMap(_.allSNPs).distinct.filter(name => snpNameMeetsInclusionCriteria(name)).toArray

        if (genomicMap.size == 0 || !sortByCHR) {

          val snpOrderInFirst = inputs.head.allSNPs.zipWithIndex.toMap

          beforeSort.sortBy((snp: String8) => snpOrderInFirst.get(snp).getOrElse(0))

        } else beforeSort.sortBy(snp => genomicMap.get(snp).getOrElse(GenomicLocation(snp.value)))
      }

      afterSort.map { snp =>

        val snpSummaries: IndexedSeq[PDosageFileRowSummary] = inputs.map(_.query(snp)).filter(_.isDefined).map(_.get._1)

        val grouped = snpSummaries.groupBy { summary => (summary.al1, summary.al2) }

        if (grouped.size > 1) {
          mismatchedSNPs.append(snpSummaries)
        }

        (snp, snpSummaries.head.al1, snpSummaries.head.al2)

      }
    }

    val iter = {

      val snpDataIterator = mergedSNPs.iterator.filter(tup => snpNameMeetsInclusionCriteria(tup._1)).map {
        case (snpName, a1, a2) =>

          val arrayOfDosages = getDosageArray(snpName)
          val snpSummary = PDosageFileRowSummary(snpName, a1, a2, arrayOfDosages, MissingValue)

          (snpSummary, arrayOfDosages)
      }.filter { tup =>
        snpMeetsInclusionCriteria(tup._1, mergedIndividuals.size)
      }

      SNPMajorIterators(
        individuals = mergedIndividuals,
        snpIterator = snpDataIterator,
        duplicatedSNPs = Set(),
        duplicatedIndividuals = Set(),
        missingValue = MissingValue
      )

    }

    MergeDosageReport(mismatchedSNPs.toList, iter)

  }
}