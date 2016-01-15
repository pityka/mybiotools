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

package hdfdosage

import scala.io.Source
import java.io.File
import mybiotools.{
  fastSplitSetSeparator,
  fastSplitSetSeparatorIterator,
  openFileWriter,
  fastSplitSeparatorIterator,
  getFileReader,
  openFileOutputStream,
  writeToFile,
  formatDouble2DecimalTo
}
import FileSets._
import mybiotools.stringstore._
import mybiotools.gwascommons._
import mybiotools.gwascommons.genotypedata._
import _root_.ch.systemsx.cisd.hdf5._
import java.io.{ OutputStream, BufferedOutputStream, DataOutputStream, ByteArrayOutputStream, Writer, FileOutputStream, RandomAccessFile }

object IndividualHelpers {
  def fromDTO(dto: PatientDTO) = Individual(dto.familyID, dto.individualID)
  def toDTO(i: Individual) = new PatientDTO(i.familyID, i.individualID)
}

sealed trait DosageFileFormat
case object HDFDosage extends DosageFileFormat
case object PDose extends DosageFileFormat
case object PGenotypeProbabilities extends DosageFileFormat
case object BIGIndividualMajor extends DosageFileFormat
case object BIGSNPMajor extends DosageFileFormat
case object IMPUTEFile extends DosageFileFormat
case object Missingness extends DosageFileFormat
case class TPed(hardcallThreshold: Double, missingValue: Char) extends DosageFileFormat
case object GRMMatrix extends DosageFileFormat

object HDFDosageFile {

  def readIndividualOrderFromAscii(s: scala.io.Source): Array[Individual] = {
    s.getLines.map { line =>
      val spl = fastSplitSetSeparator(line, Set(' ', '\t'))
      Individual(spl(0), spl(1))
    }.toArray
  }

  def readIndividualOrder(reader: IHDF5Reader): Array[Individual] = {

    // val individualCompoundType = reader.compound.getInferredType(classOf[PatientDTO])
    val individualOrder = reader.compound.readArray("/individualorder", classOf[PatientDTO]).map(x => IndividualHelpers.fromDTO(x))

    individualOrder
  }

  def getNumberOfIndividuals(reader: IHDF5Reader): Int = {
    reader.getDataSetInformation("/individualorder").getNumberOfElements.toInt
  }

  def getNumberOfSNPs(reader: IHDF5Reader): Int = {
    reader.getDataSetInformation("/snporder").getNumberOfElements.toInt
  }

  def getSNPCompoundType(reader: IHDF5Reader) = {
    val arraySize: Int = scala.math.min(reader.getDataSetInformation("/snporder").getNumberOfElements.toInt, 1)

    import ch.systemsx.cisd.hdf5.HDF5CompoundMemberMapping._
    try {
      val t1 = reader.compound.getDataSetType(
        "/snporder",
        classOf[SNPDTO],
        mapping("snpName").length(40),
        mapping("allele1").length(7),
        mapping("allele2").length(7),
        mapping("sumAl1Dosage"),
        mapping("sumAl2Dosage"),
        mapping("count"),
        mapping("maf")
      )

      reader.compound.readArrayBlockWithOffset("/snporder", t1, arraySize, 0)
      t1
    } catch {
      case x: Throwable => {
        val t2 = reader.compound.getDataSetType(
          "/snporder",
          classOf[SNPDTOold],
          mapping("snpName").length(41),
          mapping("allele1").length(7),
          mapping("allele2").length(7),
          mapping("sumAl1Dosage"),
          mapping("sumAl2Dosage"),
          mapping("count"),
          mapping("maf")
        )
        reader.compound.readArrayBlockWithOffset("/snporder", t2, arraySize, 0)
        t2
      }
    }

  }

  def readSNPOrderFull(reader: IHDF5Reader): Array[PDosageFileRowSummary] = {

    val snpCompoundType = getSNPCompoundType(reader)

    val arraySize: Int = reader.getDataSetInformation("/snporder").getNumberOfElements.toInt

    val totalIndividualCount: Int = reader.getDataSetInformation("/individualorder").getNumberOfElements.toInt

    val out = Array.ofDim[PDosageFileRowSummary](arraySize)

    (0 until arraySize).grouped(1000000).foreach { g =>
      val begin = g.head

      val chunk: Array[PDosageFileRowSummary] = reader.compound.readArrayBlockWithOffset("/snporder", snpCompoundType, g.size, begin).map(x => x match {
        case x: SNPDTO => PDosageFileRowSummaryHDFHelpers.fromSNPDTO(x, totalIndividualCount)
        case x: SNPDTOold => PDosageFileRowSummaryHDFHelpers.fromSNPDTOold(x, totalIndividualCount)
      })

      System.arraycopy(chunk, 0, out, begin, chunk.size)

    }

    out
  }

  def readSNPInfoPartial(
    reader: IHDF5Reader,
    snps: Set[String8],
    indices: Map[String8, Int],
    snpCompoundType: HDF5CompoundType[_]
  ): IndexedSeq[PDosageFileRowSummary] = {

    val (lowIndex, highIndex) = {
      val x = snps map indices
      (x.min, x.max)
    }

    val arraySize: Int = highIndex - lowIndex + 1

    val totalIndividualCount: Int = reader.getDataSetInformation("/individualorder").getNumberOfElements.toInt

    val out = Array.ofDim[PDosageFileRowSummary](arraySize)

    (0 until arraySize).grouped(1000000).foreach { g =>
      val begin = g.head

      val chunk: Array[PDosageFileRowSummary] = reader.compound.readArrayBlockWithOffset("/snporder", snpCompoundType, g.size, begin + lowIndex).map(x => x match {
        case x: SNPDTO => PDosageFileRowSummaryHDFHelpers.fromSNPDTO(x, totalIndividualCount)
        case x: SNPDTOold => PDosageFileRowSummaryHDFHelpers.fromSNPDTOold(x, totalIndividualCount)
      })

      System.arraycopy(chunk, 0, out, begin, chunk.size)

    }

    out.filter(x => snps.contains(x.snpName))
  }

  def readSNPInfo(snpIdx: Int, reader: IHDF5Reader, snpCompoundType: HDF5CompoundType[_]): PDosageFileRowSummary = {
    // val snpCompoundType = getSNPCompoundType(reader)

    val totalIndividualCount: Int = reader.getDataSetInformation("/individualorder").getNumberOfElements.toInt

    val chunk: Array[PDosageFileRowSummary] = reader.compound.readArrayBlockWithOffset("/snporder", snpCompoundType, 1, snpIdx).map(x => x match {
      case x: SNPDTO => PDosageFileRowSummaryHDFHelpers.fromSNPDTO(x, totalIndividualCount)
      case x: SNPDTOold => PDosageFileRowSummaryHDFHelpers.fromSNPDTOold(x, totalIndividualCount)
    })

    chunk.head
  }

  def readSNPNamesOrder(reader: IHDF5Reader): Array[String8] = {

    val snpCompoundType = getSNPCompoundType(reader)

    val arraySize: Int = reader.getDataSetInformation("/snporder").getNumberOfElements.toInt

    val out = Array.ofDim[String8](arraySize)

    (0 until arraySize).grouped(1000000).foreach { g =>
      val begin = g.head

      val chunk: Array[String8] = reader.compound.readArrayBlockWithOffset("/snporder", snpCompoundType, g.size, begin).map(x => x match {
        case x: SNPDTO => StringStore(new String(x.snpName).trim)
        case x: SNPDTOold => StringStore(new String(x.snpName).trim)
      })

      System.arraycopy(chunk, 0, out, begin, chunk.size)

    }

    out

  }

  case class MergeDosageReport(mismatchedSNPs: Traversable[Traversable[PDosageFileRowSummary]], file: Option[GenotypeFileSet], writeSuccessful: Boolean)

  def mergeSNPMajors(
    inputFilesp: Seq[GenotypeFileSet],
    outputFile: File,
    minimumSNPCoverage: Double = 0.0,
    includeSNPs: Set[String8] = Set[String8](),
    excludeSNPs: Set[String8] = Set[String8](),
    includeIndividuals: Set[Individual] = Set[Individual](),
    excludeIndividuals: Set[Individual] = Set[Individual](),
    bufferSize: Int = 100000,
    outputFormat: DosageFileFormat = HDFDosage,
    minimumMAF: Double = 0.0,
    genomicMap: GenomicMap = Map(),
    blockSize: Int = 1,
    sortByCHR: Boolean = true,
    recodeToRecessive: Boolean = false
  ): MergeDosageReport = {

    val readers = inputFilesp.map { x =>
      FileSets.getRandomAccessFileSet(x)
    }.toIndexedSeq

    try {
      val report = SNPMajorMerge.merge(
        inputs = readers.map(_._1),
        minimumSNPCoverage = minimumSNPCoverage,
        includeSNPs = includeSNPs,
        excludeSNPs = excludeSNPs,
        includeIndividuals = includeIndividuals,
        excludeIndividuals = excludeIndividuals,
        minimumMAF = minimumMAF,
        genomicMap = genomicMap,
        sortByCHR = sortByCHR
      )

      val mismatchedSNPs = report.mismatchingSNPs
      val iter = report.mergedIterator
      val (data, writeSuccessful) = if (mismatchedSNPs.flatten.size == 0) {
        (WriteData.write(
          iter.individuals,
          iter.snpIterator,
          iter.missingValue,
          outputFormat,
          outputFile,
          genomicMap,
          recodeToRecessive,
          blockSize
        ), true)

      } else {
        (None, false)
      }

      MergeDosageReport(mismatchedSNPs.toList, data, writeSuccessful)
    } finally {
      readers.foreach(_._2.close)
    }

  }

  def readSNPOrder(file: File): Seq[PDosageFileRowSummary] = {
    val reader = HDF5Factory.openForReading(file)

    val ret = HDFDosageFile.readSNPOrderFull(reader)

    reader.close
    ret
  }

  def readIndividualOrder(file: File): Seq[Individual] = {
    val reader = HDF5Factory.openForReading(file)

    val ret = HDFDosageFile.readIndividualOrder(reader)

    reader.close
    ret
  }

  @inline
  private def replaceMissing(in: Array[Float]): Array[Float] = {
    val dest = Array.ofDim[Float](in.length)
    Array.copy(in, 0, dest, 0, in.length)
    var i = 0
    while (i < in.size) {
      if (dest(i) == MissingValue) {
        dest(i) = Float.NaN
      }
      i += 1
    }

    dest
  }

  /** Random access reader. Missing values are recoded to Float.NaN */
  class HDFDosageReader(reader: IHDF5Reader, bufferSize: Int)(restriction: String8 => Boolean) extends SNPMajorQuery {

    val missingValue = Float.NaN

    def query(snp: String8) = {
      originalSNPOrder.get(snp).map { i =>
        allSNPInfo(i) -> getSNP(snp)
      }
    }

    def this(file: File, bufferSize: Int)(implicit restriction: String8 => Boolean = _ => true) = this(HDF5Factory.openForReading(file), bufferSize)(restriction)

    val originalSNPOrder: Map[String8, Int] = Map(readSNPNamesOrder(reader).zipWithIndex.toSeq.filter(x => restriction(x._1)): _*)

    val allSNPs = originalSNPOrder.toSeq.sortBy(_._2).map(_._1).toVector

    private val originalNumberOfIndividuals = getNumberOfIndividuals(reader)

    private val buffer = new SNPBuffer(reader, originalSNPOrder, originalNumberOfIndividuals, bufferSize)

    private val snpCompoundType = getSNPCompoundType(reader)

    private val originalIndividualOrder = Map(readIndividualOrder(reader).zipWithIndex: _*)

    def readDosage(snp: String8, individual: Individual): Option[Float] = {

      val individualIndex = originalIndividualOrder.get(individual)

      val result = if (restriction(snp) && originalSNPOrder.contains(snp) && individualIndex.isDefined) {
        synchronized { Some(buffer.getSNP(snp)(individualIndex.get)) }
      } else None

      result.map { r =>
        r match {
          case x if x == MissingValue => Float.NaN
          case x => x
        }
      }
    }

    def getSNP(snp: String8): Array[Float] = if (originalSNPOrder.contains(snp)) replaceMissing(buffer.getSNP(snp)) else throw new UnsupportedOperationException

    def getSNPOption(snp: String8): Array[Option[Float]] = if (originalSNPOrder.contains(snp)) buffer.getSNPOption(snp) else throw new UnsupportedOperationException

    val snps: Iterable[String8] = originalSNPOrder.keys

    def snpInfo(snp: String8) = readSNPInfo(originalSNPOrder(snp), reader, snpCompoundType)

    lazy val allSNPInfo: IndexedSeq[PDosageFileRowSummary] = readSNPOrderFull(reader)

    def snpInfos(snps: Set[String8]) = readSNPInfoPartial(reader, snps.filter(restriction), originalSNPOrder, snpCompoundType)

    val individuals: IndexedSeq[Individual] = originalIndividualOrder.toIndexedSeq.sortBy((x: Tuple2[Individual, Int]) => x._2).map(_._1)

    def close { synchronized { reader.close } }

  }

  /** Iterator over hdfdosage file. Missing valuesa are recoded to Float.NaN */
  class HDFDosageIterator(reader: IHDF5Reader, bufferSize: Int, subset: SNPMajorSubset) extends Iterator[(PDosageFileRowSummary, Array[Float])] {

    val totalIndividualCount: Int = reader.getDataSetInformation("/individualorder").getNumberOfElements.toInt

    def this(file: File, bufferSize: Int, subset: SNPMajorSubset) = this(HDF5Factory.openForReading(file), bufferSize, subset)

    private def readRows: IndexedSeq[Array[Float]] = {
      val currentReadSize = if ((snpIdx + actualBufferSize) < filelength) actualBufferSize else filelength - snpIdx
      reader.float32.readMatrixBlockWithOffset("/dosagematrix", currentReadSize, individuals.size, snpIdx, 0)
    }

    private def readInfo: IndexedSeq[PDosageFileRowSummary] = {
      val currentReadSize = if ((snpIdx + actualBufferSize) < filelength) actualBufferSize else filelength - snpIdx
      reader.compound.readArrayBlockWithOffset("/snporder", snpCompoundType, currentReadSize, snpIdx).map(x => x match {
        case x: SNPDTO => PDosageFileRowSummaryHDFHelpers.fromSNPDTO(x, totalIndividualCount)
        case x: SNPDTOold => PDosageFileRowSummaryHDFHelpers.fromSNPDTOold(x, totalIndividualCount)
      })

    }

    private val snpCompoundType = getSNPCompoundType(reader)

    private val readSize = (reader.getDataSetInformation("/dosagematrix").tryGetChunkSizes match {
      case x if x == null => 1000
      case x: Array[Int] => x.apply(0)
    }) * 100

    private val actualBufferSize = ((bufferSize / readSize) + 1) * readSize

    private val filelength = reader.getDataSetInformation("/snporder").getNumberOfElements.toInt

    private val snpsInDataSet = {

      subset match {
        case Full => filelength
        case FileSubSet(from, to) if from <= filelength - 1 && to <= filelength => to - from
        case FileSubSet(from, to) if from <= filelength - 1 => filelength - from
        case _ => 0
      }
    }

    private val originalNumberOfIndividuals = getNumberOfIndividuals(reader)

    val individuals = readIndividualOrder(reader)

    private def snpIdx = counter + startSNPIdx

    private val startSNPIdx = subset match {
      case Full => 0
      case FileSubSet(from, to) if from <= filelength - 1 => from
      case FileSubSet(from, to) => filelength
    }
    private var counter = 0
    private var currentRows: IndexedSeq[Array[Float]] = if (hasNext) readRows else Vector()
    private var currentSNPInfos: IndexedSeq[PDosageFileRowSummary] = if (hasNext) readInfo else Vector()
    private var currentRowsIdx = 0

    def hasNext = counter < snpsInDataSet

    def close = reader.close

    def next = {

      if (currentRowsIdx == actualBufferSize) {
        currentRows = readRows
        currentSNPInfos = readInfo
        currentRowsIdx = 0
      }

      val snpDescription = currentSNPInfos(currentRowsIdx)
      val dosage: Array[Float] = replaceMissing(currentRows(currentRowsIdx))
      val ret = (snpDescription, dosage)

      currentRowsIdx += 1
      counter += 1

      ret

    }

  }

}

object HDFDosageIterator {

  def getSNPMajorIterator(reader: IHDF5Reader, bufferSize: Int, subset: SNPMajorSubset, filterMarkerNames: Set[String8]): SNPMajorIterators =
    if (filterMarkerNames.isEmpty) {
      val iter = new HDFDosageFile.HDFDosageIterator(reader, bufferSize, subset)

      val individuals = iter.individuals

      SNPMajorIterators(individuals, iter, Set[String8](), Set[Individual](), Float.NaN)
    } else {
      val set = filterMarkerNames
      val dosagereader = new HDFDosageFile.HDFDosageReader(reader, bufferSize = bufferSize)(x => set.contains(x))
      val indx = dosagereader.individuals
      val iter = dosagereader.originalSNPOrder.toSeq.sortBy(_._2).map(_._1).iterator.map { name =>
        scala.util.Try { (dosagereader.snpInfo(name), dosagereader.getSNP(name)) }.toOption
      }.filter(_.isDefined).map(_.get)
      SNPMajorIterators(indx, iter, Set(), Set(), Float.NaN)
    }
}

object HDFDosageFileTestHelpers {
  // These reading methods are for testing only
  def readDosage(file: File, snp: String, individual: Individual): Option[Float] = {
    val reader = HDF5Factory.openForReading(file)

    val snpCompoundType = HDFDosageFile.getSNPCompoundType(reader)

    val totalIndividualCount: Int = reader.getDataSetInformation("/individualorder").getNumberOfElements.toInt

    val snpOrder = reader.compound.readArray("/snporder", snpCompoundType).map(x => x match {
      case x: SNPDTO => PDosageFileRowSummaryHDFHelpers.fromSNPDTO(x, totalIndividualCount)
      case x: SNPDTOold => PDosageFileRowSummaryHDFHelpers.fromSNPDTOold(x, totalIndividualCount)
    })
    val individualOrder = reader.compound.readArray("/individualorder", classOf[PatientDTO]).map(x => IndividualHelpers.fromDTO(x))

    val snpIndex = snpOrder.indexWhere(x => x.snpName.toString == snp)
    val individualIndex = individualOrder.indexOf(individual)

    val result = if (snpIndex >= 0 && individualIndex >= 0) {
      Some(reader.float32.readMatrixBlockWithOffset("/dosagematrix", 1, 1, snpIndex, individualIndex)(0)(0))
    } else None

    reader.close

    result
  }

}

