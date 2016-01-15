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

import mybiotools.gwascommons._

import mybiotools._
import mybiotools.stringkeyedmatrix.StringKeyedMatrix
import java.io.File

import formatters._
import Formatters._

import mybiotools.Gender
import mybiotools.stringstore._

// first dimension is indexed by individuals
final class DosageGenotypeData private (
    val snpInfoList: Iterable[LocusData],
    val patientList: Iterable[PatientInformation],
    protected val matrix: StringKeyedMatrix[Float, IndexedSeq[Float]]
) extends GenotypeData[Float] {

  def flip(t: Float) = 2.0f - t

  def toVariantDosage(t: Float) = DosageGenotype(t.toDouble)

  def filterSNPs(f: LocusData => Boolean): DosageGenotypeData = {
    val filtered = (snpInfoList filter f).toSeq
    val filteredstrings = filtered map (_.name)
    val filteredmatrix = matrix.filterDim2(name => filteredstrings.contains(name))
    new DosageGenotypeData(filtered, patientList, filteredmatrix)
  }

  /** TODO: flipping! */
  /*def merge(that: DosageGenotypeData): DosageGenotypeData = {
    val mergedpatients = (this.patientList ++ that.patientList).toSeq.distinct

    val mergedsnps = (this.snpInfoList ++ that.snpInfoList).toSeq.groupBy(_.id).map(_._2.head).map { x =>
      SNPInformation(x.chr, x.bp, x.allele2, x.allele1, x.id, x.maf)
    }

    val mergedmatrix = this.matrix.merge(that.matrix, Float.NaN) { (x, y) =>
      if (x == Float.NaN) y
      else (if (y == Float.NaN) x
      else x)
    }
    new DosageGenotypeData(mergedsnps, mergedpatients, mergedmatrix)
  }*/

  def writeDosage(famwriter: java.io.Writer, mapwriter: java.io.Writer, pdosewriter: java.io.Writer, missingValue: Float) {
    patientList.foreach { pat =>
      val pheno = pat.phenotype match {
        case None => "-9"
        case Some(x) => format(x, PlinkFam)
      }

      famwriter.write(pat.FID + " " + pat.IID + " 0 0 " + format(pat.gender, PlinkFam) + " " + pheno + "\n")
    }

    snpInfoList.foreach { snp =>
      mapwriter.write(snp.genomicLocation.map(_.chromosome).getOrElse(".") + " " + snp.name + " 0 " + snp.genomicLocation.map(_.basePairPosition).getOrElse(".") + " " + snp.alleles.head.toString + " " + snp.alleles.lift.apply(1).getOrElse(snp.alleles.head).toString + "\n")
    }

    pdosewriter.write("SNP A1 A2 ")
    pdosewriter.write(matrix.keys1.map(_ + " 1").mkString(" "))
    pdosewriter.write('\n')

    val decimalFormatter = new java.text.DecimalFormat("0.0#")

    snpInfoList.foreach { snpinfo =>
      val snpName = snpinfo.name
      val a1 = snpinfo.alleles.head.toString
      val a2 = snpinfo.alleles.lift.apply(1).getOrElse(snpinfo.alleles.head).toString
      pdosewriter.write(snpName + " " + a1 + " " + a2)
      matrix.keys1.foreach { ind =>
        val dosage = matrix.get(ind, snpName).getOrElse(missingValue)
        pdosewriter.write(' ')
        pdosewriter.write(decimalFormatter.format(dosage))
      }
      pdosewriter.write('\n')
    }
  }
}

object DosageGenotypeData {

  def fromPlinkDosage(
    pdosage: File,
    map: File,
    fam: File,
    headerLineOfDosage: Boolean = true
  ): DosageGenotypeData = {

    // chr,bp,id
    val snps: GenomicMap = getGenomicMapFromBimFile(map.getAbsolutePath)

    val individuals: IndexedSeq[PatientInformation] = {
      val famContentsList = openSource(fam.getAbsolutePath)(_.getLines.map { l =>
        val spl = fastSplitSetSeparatorIterator(l, Set('\t', ' '))
        val fid = StringStore(new java.lang.String(spl.next))
        val iid = StringStore(new java.lang.String(spl.next))
        val mid = new java.lang.String(spl.next)
        val pid = new java.lang.String(spl.next)

        val gender: Option[Gender] = Gender.fromPlinkInt(spl.next.toInt)
        val phenotype = catchToLeft(spl.next.toDouble) match {
          case Left(_) => None
          case Right(-9.0) => None
          case Right(x) => Some(x)
        }
        PatientInformation(fid, iid, gender, phenotype)
      }.toIndexedSeq)

      val famContents: Map[String8, PatientInformation] = famContentsList.groupBy(x => (x.FID)).map(x => x._1 -> x._2.head)

      if (headerLineOfDosage) {
        val firstLine = openSource(pdosage.getAbsolutePath)(_.getLines.next)
        val spl = fastSplitSetSeparatorIterator(firstLine, Set('\t', ' '))
        spl.next // SNP
        spl.next // A1
        spl.next // A2

        val x: IndexedSeq[PatientInformation] = spl.grouped(2).map(l => famContents(StringStore(l.head))).toIndexedSeq
        x
      } else {
        famContentsList
      }
    }

    case class SNPInformation(
      chr: String,
      bp: Int,
      val allele1: Char,
      val allele2: Char,
      id: String8
    )

    val snpsWithAlleles = scala.collection.mutable.ArrayBuffer[SNPInformation]()

    val genotypeIter = new Iterator[Tuple3[String8, String8, Float]] {

      val lineIter = io.Source.fromFile(pdosage.getAbsolutePath).getLines

      if (headerLineOfDosage) {
        lineIter.next // header row
      }

      var innerIter = fastSplitSetSeparatorIterator(lineIter.next, Set('\t', ' '))

      var individualIndex = -1

      var currentSNP = StringStore(new java.lang.String(innerIter.next)) // SNP
      val al1 = new java.lang.String(innerIter.next) //A1
      val al2 = new java.lang.String(innerIter.next) // A2
      val si = snps(currentSNP)
      snpsWithAlleles.append(SNPInformation(si.chromosome, si.basePairPosition, al1(0), al2(0), currentSNP))

      def hasNext = lineIter.hasNext || innerIter.hasNext

      def next = {
        val nextGeno = if (innerIter.hasNext) {
          individualIndex += 1
          new java.lang.String(innerIter.next)

        } else {
          innerIter = fastSplitSetSeparatorIterator(lineIter.next, Set('\t', ' '))
          currentSNP = StringStore(new java.lang.String(innerIter.next)) // SNP
          innerIter.next //A1
          innerIter.next // A2
          individualIndex = 0
          val si = snps(currentSNP)
          snpsWithAlleles.append(SNPInformation(si.chromosome, si.basePairPosition, al1(0), al2(0), currentSNP))
          innerIter.next
        }
        (currentSNP, individuals(individualIndex).FID, nextGeno.toFloat)
      }

    }

    val matrix = StringKeyedMatrix.fromSortedIterator(genotypeIter, individuals.size).transpose

    val locusdata = snpsWithAlleles.map { s =>

      val af1 = individuals.foldLeft(0.0)((k: Double, x) => k + matrix(x.FID, s.id)) / (2 * individuals.size.toDouble)
      val al1 = SimpleAllele.makeSingleton(s.allele1)
      val al2 = SimpleAllele.makeSingleton(s.allele2)
      val alleles = if (al1 == al2) Seq(al1) else Seq(al1, al2)
      val afs = if (al1 != al2) List(al1 -> af1, al2 -> (1.0 - af1)) else List(al1 -> 1.0)

      LocusData(
        genomicLocation = Some(GenomicLocation(s.bp, s.chr)),
        name = s.id,
        alleles = alleles,
        alleleFrequencies = afs
      )

    }

    new DosageGenotypeData(locusdata.toSeq, individuals, matrix)

  }
}