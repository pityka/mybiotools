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
import mybiotools.bit2seq.Bit2State._
import mybiotools.bit2seq.Bit2State
import mybiotools.bit2seq._
import mybiotools.stringkeyedmatrix.StringKeyedMatrix
import mybiotools.gwascommons._
import mybiotools.stringstore._
import java.io.File

import formatters._
import Formatters._
import Gender._

object GenotypeStates {
  type GenotypeState = Bit2State.Bit2
  val Homozygous1 = S3 // Homozygous for the first allele 
  val Homozygous2 = S2
  val Heterozygous = S4
  val Missing = S1

}

case class PatientInformation(
  FID: String8,
  IID: String8,
  gender: Option[Gender],
  phenotype: Option[Double]
) extends mybiotools.gwascommons.Individual
object PatientInformation {
  def apply(fid: String8, gender: Option[Gender], phenotype: Option[Double]): PatientInformation = PatientInformation(fid, StringStore("1"), gender, phenotype)
}

import GenotypeStates._

trait GenotypeData[T] {

  val snpInfoList: Iterable[LocusData]
  val patientList: Iterable[PatientInformation]
  protected val matrix: StringKeyedMatrix[T, _] // keys1: patient, keys2: snp

  // assert( snpInfoList.map( _.id ) == matrix.keys2 )
  // assert( patientList.map( _.id ) == matrix.keys1 )

  protected lazy val snpInfoMap = Map[String8, LocusData](snpInfoList.map(x => x.name -> x).toSeq: _*)

  protected lazy val patientInfoMap = Map[String8, PatientInformation](patientList.map(x => x.FID -> x).toSeq: _*)
  protected def sortedSNPs = snpInfoMap.values.toSeq.sortBy { x =>
    x.genomicLocation
  }

  protected def sortedPatients = patientList.toIndexedSeq

  def canEqual(other: Any): Boolean = other.isInstanceOf[GenotypeData[_]]

  override def hashCode: Int = 41 * (41 *
    (41 + snpInfoMap.hashCode) + patientInfoMap.hashCode) + matrix.hashCode

  override def equals(that: Any): Boolean = that match {
    case t: GenotypeData[_] => t.canEqual(this) && snpInfoMap == t.snpInfoMap && patientInfoMap == t.patientInfoMap && matrix == t.matrix
    case _ => false
  }

  // override def toString = snpInfoMap.toString

  // apply, iterator mindket iranyba, equals, object es konstructorok
  def getState(patId: String8, snpId: String8) = matrix.get(patId, snpId)

  def flip(t: T): T

  def toVariantDosage(t: T): WithVariantDosage

  def getStateWithRespectToMinor(patId: String8, snpId: String8): Option[WithVariantDosage] = {
    getState(patId, snpId).map { stateWithRespectToAllele1 =>
      val locus = getSNP(snpId).get
      val al1 = locus.alleles.head
      val variant = locus.variant
      val stateWithRespectToMinorAllele = if (variant.isDefined && variant.get != al1) flip(stateWithRespectToAllele1) else stateWithRespectToAllele1
      toVariantDosage(stateWithRespectToMinorAllele)
    }
  }

  def getPatient(patId: String8) = patientInfoMap.get(patId)

  def getSNP(id: String8) = snpInfoMap.get(id)

  def getPatients = patientInfoMap.values.toSet

  def getSNPs = snpInfoMap.values.toSet

  def formatPhenotypes[F <: Format](f: F)(implicit form: Formatter[Any, F]): String = {
    val listOfRowsInTable: List[Map[Symbol, Any]] = patientInfoMap.keys.toList.sorted.map { indiv =>
      var row = Map[Symbol, Any]()
      row += ('gender -> patientInfoMap(indiv).gender)
      row += ('default -> patientInfoMap(indiv).phenotype)
      row += ('FID -> indiv)
      row += ('IID -> 1)
      row
    }
    val k = List('FID, 'IID, 'gender, 'default)
    formatTable(listOfRowsInTable, keys = k, software = f, quoteString = false)
  }

  def formatMap: String = {
    sortedSNPs.map { x =>
      x.toMapLine
    }.mkString("\n") + "\n"
  }

}

final class DiscreteGenotypeData private (
    val snpInfoList: Iterable[LocusData],
    val patientList: Iterable[PatientInformation],
    protected val matrix: StringKeyedMatrix[GenotypeState, _]
) extends GenotypeData[GenotypeState] {

  // override def getState(patId: String, snpId: String): DiscreteGenotypeProto = GenotypeState(super.getSTate(patId, snpId))

  def flip(gs: GenotypeState) = gs match {
    case Homozygous1 => Homozygous2
    case Homozygous2 => Homozygous1
    case x => x
  }

  def toVariantDosage(gs: GenotypeState) = gs match {
    case Homozygous1 => DiscreteGenotypes.HomozygousVar
    case Homozygous2 => DiscreteGenotypes.HomozygousRef
    case Heterozygous => DiscreteGenotypes.Heterozygous
    case Missing => DiscreteGenotypes.Missing
  }

  def writePed(writer: java.io.Writer) {
    val sortedSNPs2 = sortedSNPs
    sortedPatients.foreach { patient =>
      val fid = patient.FID
      val pheno = patient.phenotype match {
        case None => "-9"
        case Some(x) => format(x, PlinkFam)
      }
      val header = List(patient.FID, patient.IID, "0 0", format(patient.gender, PlinkFam), pheno).mkString(" ")
      writer.write(header)
      sortedSNPs2.foreach { snp =>
        val state = matrix(patient.FID, snp.name)
        val stateString = state match {
          case Homozygous1 => snp.alleles.head.nucleotides(0) + " " + snp.alleles.head.nucleotides(0)
          case Homozygous2 => snp.alleles(1).nucleotides(0) + " " + snp.alleles(1).nucleotides(0)
          case Heterozygous => snp.alleles(0).nucleotides(0) + " " + snp.alleles(1).nucleotides(0)
          case Missing => "0 0"
        }
        writer.write(" ")
        writer.write(stateString)

      }
      writer.write("\n")
    }
  }

  def formatPed = {
    val s = new java.io.StringWriter()
    writePed(s)
    s.toString
  }

}

object DiscreteGenotypeData {
  import scala.collection.mutable.ArrayBuffer
  def fromPedMap(ped: File, map: File): DiscreteGenotypeData = {

    case class SNPMappingInformation(val chr: String, val bp: Int, val id: String8)

    val snpsbuffer = ArrayBuffer[SNPMappingInformation]()
    openSource(map.getAbsolutePath)(_.getLines.foreach { x =>
      val spl = fastSplitSetSeparatorIterator(x, Set('\t', ' '))
      val x1 = spl.next
      val x2 = spl.next
      val x3 = spl.next
      val x4 = spl.next
      snpsbuffer.append(SNPMappingInformation(x1, x4.toInt, StringStore(x2)))
    })

    val snps = snpsbuffer.toArray

    val indivBuffer = ArrayBuffer[PatientInformation]()

    case class AFAccum(var al1: Option[Char], var al2: Option[Char], var al1C: Int, var al2C: Int)

    val snpAlleleCounts: Array[AFAccum] = Array.fill(snps.size)(AFAccum(None, None, 0, 0))

    val genotypeIter = new Iterator[Tuple3[String8, String8, GenotypeState]] {

      var locusIter = snps.iterator.zipWithIndex
      val pedIterator = tokenizeReaderIterator(getFileReader(ped), Array(' ', '\n'))

      def hasNext = pedIterator.hasNext

      var newLine = true
      def next = {
        if (newLine) {
          val fid = StringStore(pedIterator.next)
          val iid = StringStore(pedIterator.next)
          val mid = pedIterator.next
          val pid = pedIterator.next

          val gender = Gender.fromPlinkInt(pedIterator.next.toInt)
          val phenotype = catchToLeft(pedIterator.next.toDouble) match {
            case Left(_) => None
            case Right(-9.0) => None
            case Right(x) => Some(x)
          }
          indivBuffer.append(PatientInformation(fid, iid, gender, phenotype))

          newLine = false
        }

        val al1 = pedIterator.next.apply(0)
        val al2 = pedIterator.next.apply(0)

        if ((al2 == '0' || al1 == '0') && al1 != al2) throw new RuntimeException("Bad genotype: either both missing, or neither is missing.")

        val snpNext = locusIter.next
        val snpName = snpNext._1.id
        val snpIndex = snpNext._2

        if (al1 != '0') {
          if (snpAlleleCounts(snpIndex).al1.isEmpty) snpAlleleCounts(snpIndex).al1 = Some(al1)
        }

        if (al1 != '0' && al1 != snpAlleleCounts(snpIndex).al1.get) {
          if (snpAlleleCounts(snpIndex).al2.isEmpty) snpAlleleCounts(snpIndex).al2 = Some(al1)
        }

        if (al2 != '0' && al2 != snpAlleleCounts(snpIndex).al1.get) {
          if (snpAlleleCounts(snpIndex).al2.isEmpty) snpAlleleCounts(snpIndex).al2 = Some(al2)
        }

        val refAl1 = snpAlleleCounts(snpIndex).al1
        val refAl2 = snpAlleleCounts(snpIndex).al2

        if (al1 != '0') {
          if (refAl2.isEmpty) {
            if (al1 != al2 && al1 != refAl1.get && al2 != refAl1.get) throw new RuntimeException("3alleles" + al1 + "," + al2 + "," + refAl1.get)
          } else {
            if ((al1 != refAl1.get && al1 != refAl2.get) || (al2 != refAl1.get && al2 != refAl2.get)) throw new RuntimeException("3alleles" + al1 + "," + al2 + "," + refAl1.get)
          }

        }

        // val observedAlleles = Set( al1, al2 ) ++ ( refAl2 match {
        //           case None => Set()
        //           case Some( x ) => Set( x )
        //         } ) ++ ( refAl1 match {
        //           case None => Set()
        //           case Some( x ) => Set( x )
        //         } ) - '0'
        //         if ( observedAlleles.size > 2 ) throw new RuntimeException( "3alleles "+observedAlleles.toString )

        val ret = (al1, al2) match {
          case ('0', '0') => (indivBuffer.last.FID, snpName, Missing)
          case (x, y) if (y == x && x == refAl1.get) => {
            snpAlleleCounts(snpIndex).al1C += 2
            (indivBuffer.last.FID, snpName, Homozygous1)
          }
          case (x, y) if (x == y && x == refAl2.get) => {
            snpAlleleCounts(snpIndex).al2C += 2
            (indivBuffer.last.FID, snpName, Homozygous2)
          }
          case (x, y) if (x != y) => {
            snpAlleleCounts(snpIndex).al1C += 1
            snpAlleleCounts(snpIndex).al2C += 1
            (indivBuffer.last.FID, snpName, Heterozygous)
          }
        }

        if (snpIndex == snps.size - 1) {
          newLine = true
          locusIter = snps.iterator.zipWithIndex
        }

        ret
      }

    }

    val matrix = StringKeyedMatrix.fromSortedIteratorOfBit2(genotypeIter, snps.size)

    val snps2 = snps.zipWithIndex.map { y =>
      val x = y._1
      val c = y._2
      val al1: SimpleAllele = SimpleAllele.makeSingleton(snpAlleleCounts(c).al1.get)
      val al2: Option[SimpleAllele] = snpAlleleCounts(c).al2.map(x => SimpleAllele.makeSingleton(x))
      val alC1 = snpAlleleCounts(c).al1C
      val alC2 = if (al2.isEmpty) 0.0 else snpAlleleCounts(c).al2C
      val freqOfAl1 = alC1.toDouble / (alC1 + alC2).toDouble
      val alleles = if (al2.isDefined) Seq(al1, al2.get) else Seq(al1)
      val afs = if (al2.isDefined) List(al1 -> freqOfAl1, al2.get -> (1.0 - freqOfAl1)) else List(al1 -> 1.0)
      new LocusData(
        genomicLocation = Some(GenomicLocation(x.bp, x.chr)),
        name = x.id,
        alleles = alleles,
        alleleFrequencies = afs
      )
    }

    new DiscreteGenotypeData(snps2, indivBuffer.toSeq, matrix)

  }

}