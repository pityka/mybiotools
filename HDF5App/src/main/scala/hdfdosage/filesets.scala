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

import java.io.File
import mybiotools._
import hdfdosage.HDFDosageFile.HDFDosageReader
import mybiotools.tasks._
import mybiotools.gwascommons._
import _root_.ch.systemsx.cisd.hdf5._
import mybiotools.gwascommons.genotypedata._
import scala.util.Try
import mybiotools.flatindex._
import mybiotools.stringstore._

object FileSets {
  sealed trait GenotypeFileSet {
    def name: String
    def path: File
  }
  case class BGZippedPDose(file: java.io.File, missingValue: Float, fam: Option[File], index: java.io.File) extends GenotypeFileSet {
    def name = file.getName
    def path = file
  }
  case class BGZippedPGenotypeProbabilities(file: java.io.File, missingValue: Float, fam: Option[File], index: java.io.File) extends GenotypeFileSet {
    def name = file.getName
    def path = file
  }
  case class PDose(file: java.io.File, missingValue: Float, fam: Option[File]) extends GenotypeFileSet {
    def name = file.getName
    def path = file
  }
  case class PGenotypeProbabilities(file: java.io.File, missingValue: Float, fam: Option[File]) extends GenotypeFileSet {
    def name = file.getName
    def path = file
  }
  case class TPed(fam: java.io.File, tped: java.io.File, missingValue: Char) extends GenotypeFileSet {
    def name = tped.getName
    def path = tped
  }
  case class HDFDosage(file: java.io.File) extends GenotypeFileSet {
    def name = file.getName
    def path = file
  }
  case class VCFFile(file: java.io.File) extends GenotypeFileSet {
    def name = file.getName
    def path = file
  }
  case class VCFFileWithIndex(file: java.io.File, indexFile: java.io.File) extends GenotypeFileSet {
    def name = file.getName
    def path = file
  }
  case class BedFile(bed: java.io.File, bim: java.io.File, fam: java.io.File) extends GenotypeFileSet {
    def name = bed.getName
    def path = bed
  }

  def parseConfig(config: com.typesafe.config.Config): List[GenotypeFileSet] = {
    val configInstance = config.withFallback(com.typesafe.config.ConfigFactory.parseString("""
     tped =""     
     tfam = ""
     bed = ""
     bim =""
     fam = ""
     pgeno = ""
     pdose = ""
     vcf = ""
     vcfindex = ""
     missing-genotype = -9
     genotypefiles = ""
     genotypefiletype = pdose
     hdfdosage = ""

      """))
    val tpedfile = configInstance.getString("tped")

    val bedfile = configInstance.getString("bed")
    val bimfile = configInstance.getString("bim")
    val famfile = configInstance.getString("fam")

    val tfamfile = configInstance.getString("tfam")

    val pgenofile = configInstance.getString("pgeno")

    val pdosefile = configInstance.getString("pdose")

    val vcf = configInstance.getString("vcf")
    val vcfindex = configInstance.getString("vcfindex")

    val missingGenotypeValue = configInstance.getString("missing-genotype")

    val hdfdosagefile = configInstance.getString("hdfdosage")

    {
      if (bedfile != "" && bimfile != "" && famfile != "") FileSets.BedFile(new File(bedfile), new File(bimfile), new File(famfile)) :: Nil
      else if (tpedfile != "" && tfamfile != "") FileSets.TPed(new File(tfamfile), new File(tpedfile), missingGenotypeValue(0)) :: Nil
      else if (pgenofile != "") {
        val fam = if (tfamfile != "") Some(new File(tfamfile)) else None

        FileSets.PGenotypeProbabilities(new File(pgenofile), missingGenotypeValue.toFloat, fam) :: Nil
      } else if (pdosefile != "") {
        val fam = if (tfamfile != "") Some(new File(tfamfile)) else None

        FileSets.PDose(new File(pdosefile), missingGenotypeValue.toFloat, fam) :: Nil
      } else if (hdfdosagefile != "") { FileSets.HDFDosage(new File(hdfdosagefile)) :: Nil }
      else if (vcf != "" && vcfindex == "") { FileSets.VCFFile(new File(vcf)) :: Nil }
      else if (vcf != "" && vcfindex != "") { FileSets.VCFFileWithIndex(new File(vcf), new File(vcfindex)) :: Nil }

      else configInstance.getString("genotypefiles") match {
        case x if x == "" => Nil
        case x => {
          scala.io.Source.fromFile(x).getLines.toList.map { f =>
            configInstance.getString("genotypefiletype") match {
              case "pdose" => FileSets.PDose(new File(f.split(" ")(0)), missingGenotypeValue.toFloat, Try(f.split(" ")(1)).toOption.map(f => new File(f)))
              case "pgenotypeprobabilities" => FileSets.PGenotypeProbabilities(new File(f.split(" ")(0)), missingGenotypeValue.toFloat, Try(f.split(" ")(1)).toOption.map(f => new File(f)))
              case "tped" => FileSets.TPed(new File(f.split(" ")(0)), new File(f.split(" ")(1)), missingGenotypeValue(0))
              case "hdfdosage" => FileSets.HDFDosage(new File(f))
              case "vcf" if f.split(" ").size == 1 => FileSets.VCFFile(new File(f))
              case "vcf" if f.split(" ").size > 1 => FileSets.VCFFileWithIndex(new File(f.split(" ")(0)), new File(f.split(" ")(1)))
              case "bed" => FileSets.BedFile(new File(f.split(" ")(0)), new File(f.split(" ")(1)), new File(f.split(" ")(2)))
            }
          }
        }
      }
    }
  }

  def openFileSet[T](fs: GenotypeFileSet)(f: SNPMajorIterators => T): T = openFileSet(fs, 0.0, Full, Set())(f)

  def openFileSet[T](fs: GenotypeFileSet, minimumMaf: Double, subset: SNPMajorSubset)(f: SNPMajorIterators => T): T = openFileSet(fs, minimumMaf, subset, Set())(f)

  def openFileSet[T](fs: GenotypeFileSet, minimumMaf: Double, subset: SNPMajorSubset, filterMarkerNames: Set[String])(f: SNPMajorIterators => T): T = openFileSet(fs, minimumMaf, 1.0, subset, filterMarkerNames)(f)

  def openFileSet[T](fs: GenotypeFileSet, minimumMaf: Double, maximumMaf: Double, subset: SNPMajorSubset, filterMarkerNames: Set[String])(f: SNPMajorIterators => T): T = {
    val filterOnMaf = (p: PDosageFileRowSummary) => p.maf >= minimumMaf && p.maf <= maximumMaf
    fs match {
      case BedFile(bed, bim, fam) => {
        openFileInputStream(bed) { is =>
          openSource(fam.getCanonicalPath) { fam =>
            openSource(bim.getCanonicalPath) { bim =>
              f(SNPMajorReaders.getSNPMajorIterator(is, fam, bim, subset, filterMarkerNames).filter(filterOnMaf))
            }
          }
        }
      }
      case PDose(file, missing, fam) => {
        val inds = fam.map(x => openSource(x.getCanonicalPath)(s => getIndividualsFromFamFile(s)))

        openSource(file.getCanonicalPath) { source =>
          f(SNPMajorReaders.getSNPMajorIterator(PDoseFiles(source, missing, inds), subset, filterMarkerNames).filter(filterOnMaf))
        }
      }
      case BGZippedPDose(file, missing, fam, _) => {
        val inds = fam.map(x => openSource(x.getCanonicalPath)(s => getIndividualsFromFamFile(s)))

        openSource(file.getCanonicalPath) { source =>
          f(SNPMajorReaders.getSNPMajorIterator(PDoseFiles(source, missing, inds), subset, filterMarkerNames).filter(filterOnMaf))
        }
      }
      case BGZippedPGenotypeProbabilities(file, missing, fam, _) => {
        val inds = fam.map(x => openSource(x.getCanonicalPath)(s => getIndividualsFromFamFile(s)))

        openSource(file.getCanonicalPath) { source =>
          f(SNPMajorReaders.getSNPMajorIterator(PGenotypeProbabilitiesFiles(source, missing, inds), subset, filterMarkerNames).filter(filterOnMaf))
        }
      }
      case PGenotypeProbabilities(file, missing, fam) => {
        val inds = fam.map(x => openSource(x.getCanonicalPath)(s => getIndividualsFromFamFile(s)))
        openSource(file.getCanonicalPath) { source =>
          f(SNPMajorReaders.getSNPMajorIterator(PGenotypeProbabilitiesFiles(source, missing, inds), subset, filterMarkerNames).filter(filterOnMaf))
        }
      }
      case TPed(fam, tped, missing) => {
        openSource(fam.getCanonicalPath) { famsource =>
          openSource(tped.getCanonicalPath) { tpedsource =>
            f(SNPMajorReaders.getSNPMajorIterator(TPedFiles(famsource, tpedsource, missing), subset, filterMarkerNames).filter(filterOnMaf))
          }
        }
      }
      case HDFDosage(file) => {
        val reader = HDF5Factory.openForReading(file)
        val x = f(HDFDosageIterator.getSNPMajorIterator(reader, subset = subset, bufferSize = 10000, filterMarkerNames = filterMarkerNames.map(x => mybiotools.stringstore.StringStore(x))))
        reader.close
        x

      }
      case VCFFile(file) => {
        val reader = vcfhelpers.VCFHelpers.openVCF(file, None)
        try {
          f(vcfhelpers.VCFHelpers.getSNPMajorIterators(file, subset, filterMarkerNames))
        } finally {
          reader.close
        }

      }
      case VCFFileWithIndex(file, _) => {
        val reader = vcfhelpers.VCFHelpers.openVCF(file, None)
        try {
          f(vcfhelpers.VCFHelpers.getSNPMajorIterators(file, subset, filterMarkerNames))
        } finally {
          reader.close
        }

      }
    }
  }

  def getIndividuals(fs: GenotypeFileSet): Seq[Individual] = fs match {
    case BedFile(bed, bim, fam) => openSource(fam)(getIndividualsFromFamFile)

    case PDose(file, missing, fam) => {
      fam.map(x => openSource(x.getCanonicalPath)(s => getIndividualsFromFamFile(s))).getOrElse {
        openSource(file)(s => SNPMajorReaders.getIndividualsFromSingleDosage(s))
      }
    }
    case PGenotypeProbabilities(file, missing, fam) => {
      fam.map(x => openSource(x.getCanonicalPath)(s => getIndividualsFromFamFile(s))).getOrElse {
        openSource(file)(s => SNPMajorReaders.getIndividualsFromSingleDosage(s))
      }
    }
    case BGZippedPDose(file, missing, fam, _) => {
      fam.map(x => openSource(x.getCanonicalPath)(s => getIndividualsFromFamFile(s))).getOrElse {
        openSource(file)(s => SNPMajorReaders.getIndividualsFromSingleDosage(s))
      }
    }
    case BGZippedPGenotypeProbabilities(file, missing, fam, _) => {
      fam.map(x => openSource(x.getCanonicalPath)(s => getIndividualsFromFamFile(s))).getOrElse {
        openSource(file)(s => SNPMajorReaders.getIndividualsFromSingleDosage(s))
      }
    }
    case TPed(fam, tped, missing) => openSource(fam)(getIndividualsFromFamFile)

    case file =>
      openFileSet(file)(_.individuals)

  }

  def getIteratorFromFileSet[T](fs: GenotypeFileSet, minimumMaf: Double, maximumMaf: Double, subset: SNPMajorSubset, filterMarkerNames: Set[String]): (SNPMajorIterators, { def close(): Unit }) = {
    val filterOnMaf = (p: PDosageFileRowSummary) => p.maf >= minimumMaf && p.maf <= maximumMaf
    fs match {
      case BedFile(bed, bim, fam) => {
        val is = new java.io.BufferedInputStream(new java.io.FileInputStream(bed))
        val fams = scala.io.Source.fromFile(fam)
        val bims = scala.io.Source.fromFile(bim)
        (SNPMajorReaders.getSNPMajorIterator(is, fams, bims, subset, filterMarkerNames), new {
          def close() = {
            is.close
            fams.close
            bims.close
          }
        })

      }
      case PDose(file, missing, fam) => {
        val inds = fam.map(x => openSource(x.getCanonicalPath)(s => getIndividualsFromFamFile(s)))

        val source = createSource(file)
        (SNPMajorReaders.getSNPMajorIterator(PDoseFiles(source, missing, inds), subset, filterMarkerNames).filter(filterOnMaf), source)

      }
      case PGenotypeProbabilities(file, missing, fam) => {
        val inds = fam.map(x => openSource(x.getCanonicalPath)(s => getIndividualsFromFamFile(s)))

        val source = createSource(file)
        (SNPMajorReaders.getSNPMajorIterator(PGenotypeProbabilitiesFiles(source, missing, inds), subset, filterMarkerNames).filter(filterOnMaf), source)

      }
      case BGZippedPDose(file, missing, fam, _) => {
        val inds = fam.map(x => openSource(x.getCanonicalPath)(s => getIndividualsFromFamFile(s)))

        val source = createSource(file)
        (SNPMajorReaders.getSNPMajorIterator(PDoseFiles(source, missing, inds), subset, filterMarkerNames).filter(filterOnMaf), source)

      }
      case BGZippedPGenotypeProbabilities(file, missing, fam, _) => {
        val inds = fam.map(x => openSource(x.getCanonicalPath)(s => getIndividualsFromFamFile(s)))

        val source = createSource(file)
        (SNPMajorReaders.getSNPMajorIterator(PGenotypeProbabilitiesFiles(source, missing, inds), subset, filterMarkerNames).filter(filterOnMaf), source)

      }
      case TPed(fam, tped, missing) => {
        val famsource = createSource(fam)
        val tpedsource = createSource(tped)

        (SNPMajorReaders.getSNPMajorIterator(TPedFiles(famsource, tpedsource, missing), subset, filterMarkerNames).filter(filterOnMaf), new {
          def close() {
            famsource.close
            tpedsource.close
          }
        })

      }
      case HDFDosage(file) => {
        val reader = HDF5Factory.openForReading(file)
        val x = HDFDosageIterator.getSNPMajorIterator(reader, subset = subset, bufferSize = 10000, filterMarkerNames = filterMarkerNames.map(x => mybiotools.stringstore.StringStore(x)))

        (x, reader)

      }
      case VCFFile(file) => {
        val reader = vcfhelpers.VCFHelpers.openVCF(file, None)
        val x = vcfhelpers.VCFHelpers.getSNPMajorIterators(reader, subset, filterMarkerNames)
        (x, reader)
      }
      case VCFFileWithIndex(file, _) => {
        val reader = vcfhelpers.VCFHelpers.openVCF(file, None)
        val x = vcfhelpers.VCFHelpers.getSNPMajorIterators(reader, subset, filterMarkerNames)
        (x, reader)
      }
    }
  }

  def openFileSets[T](fss: Seq[GenotypeFileSet], minimumMaf: Double, maximumMaf: Double, subset: SNPMajorSubset, filterMarkerNames: Set[String])(f: SNPMajorIterators => T): T = {
    val handles = fss.iterator.map(fs =>
      getIteratorFromFileSet(
        fs,
        minimumMaf = minimumMaf,
        maximumMaf = maximumMaf,
        subset = subset,
        filterMarkerNames = filterMarkerNames
      ))
    val iter = SNPMajorReaders.concatenate(
      handles
    )

    try {
      f(iter.get)
    } finally {
      handles.foreach(_._2.close)
    }

  }

  def openRandomAccessFileSets[T](fs: Seq[GenotypeFileSet])(f: SNPMajorQuery => T): T = {
    val handles = fs.map(getRandomAccessFileSet)
    val composite = new CompositeSNPMajorQuery(handles.map(_._1))
    try {
      f(composite)
    } finally {
      handles.foreach(_._2.close)
    }
  }

  def getRandomAccessFileSet[T](fs: GenotypeFileSet): (SNPMajorQuery, { def close(): Unit }) = {
    fs match {
      case BedFile(bed, bim, fam) => {

        val rf = new java.io.RandomAccessFile(bed, "r")
        val fams = scala.io.Source.fromFile(fam)
        val bims = scala.io.Source.fromFile(bim)
        (new RandomAccessBedReader(rf, bims, fams), new {
          def close() = {
            rf.close
            fams.close
            bims.close
          }
        })

      }
      case HDFDosage(file) => {

        val reader = HDF5Factory.openForReading(file)
        val x = new HDFDosageReader(reader, 10000)(x => true)

        (x, reader)

      }
      case VCFFileWithIndex(file, index) => {

        val reader = vcfhelpers.VCFHelpers.openVCF(file, None)
        val map = vcfhelpers.VCFHelpers.getGenomicMap(reader)

        val x = vcfhelpers.VCFHelpers.getSNPMajorQuery(reader, map)
        (x, reader)

      }
      case BGZippedPDose(file, missingValue, fam, index) => {

        val inds = fam.map(x => openSource(x.getCanonicalPath)(s => getIndividualsFromFamFile(s)))
        val idx = openSource(index)(s => readIndex(s, (s: String) => StringStore(s)).toMap)

        val x = new DelimitedSNPMajorQuery(BGZPDose(file, missingValue, inds, idx))
        (x, x)
      }
      case BGZippedPGenotypeProbabilities(file, missingValue, fam, index) => {

        val inds = fam.map(x => openSource(x.getCanonicalPath)(s => getIndividualsFromFamFile(s)))
        val idx = openSource(index)(s => readIndex(s, (s: String) => StringStore(s)).toMap)

        val x = new DelimitedSNPMajorQuery(BGZPGeno(file, missingValue, inds, idx))
        (x, x)
      }
      case PDose(file, missingValue, fam) => {
        val bgzf = TempFile.createTempFile(".blocked.gz")
        openBlockedZippedFileOutputStream(bgzf) { os =>
          openFileInputStreamMaybeZipped(file) { is =>
            com.google.common.io.ByteStreams.copy(is, os)
          }
        }
        val index = BGZippedGenotypeHelper.makeIndex(bgzf)

        val inds = fam.map(x => openSource(x.getCanonicalPath)(s => getIndividualsFromFamFile(s)))

        val x = new DelimitedSNPMajorQuery(BGZPDose(bgzf, missingValue, inds, index))
        (x, x)

      }
      case PGenotypeProbabilities(file, missingValue, fam) => {
        val bgzf = TempFile.createTempFile(".blocked.gz")
        openBlockedZippedFileOutputStream(bgzf) { os =>
          openFileInputStreamMaybeZipped(file) { is =>
            com.google.common.io.ByteStreams.copy(is, os)
          }
        }
        val index = BGZippedGenotypeHelper.makeIndex(bgzf)

        val inds = fam.map(x => openSource(x.getCanonicalPath)(s => getIndividualsFromFamFile(s)))

        val x = new DelimitedSNPMajorQuery(BGZPGeno(bgzf, missingValue, inds, index))
        (x, x)

      }
      case _ => throw new RuntimeException(s"Random access of $fs not supported.")
    }
  }

  def openRandomAccessFileSet[T](fs: GenotypeFileSet)(f: SNPMajorQuery => T): T = {
    fs match {
      case BedFile(bed, bim, fam) => {
        val rf = new java.io.RandomAccessFile(bed, "r")
        try {
          openSource(fam.getCanonicalPath) { fam =>
            openSource(bim.getCanonicalPath) { bim =>
              f(new RandomAccessBedReader(rf, bim, fam))
            }
          }
        } finally {
          rf.close
        }

      }
      case HDFDosage(file) => {
        val reader = HDF5Factory.openForReading(file)
        val dosagereader = new HDFDosageReader(reader, 10000)(x => true)
        try {
          f(dosagereader)
        } finally {
          dosagereader.close
        }

      }
      case VCFFileWithIndex(file, index) => {
        val reader = vcfhelpers.VCFHelpers.openVCF(file, Some(index))
        val map = vcfhelpers.VCFHelpers.getGenomicMap(reader)
        try {
          f(vcfhelpers.VCFHelpers.getSNPMajorQuery(reader, map))
        } finally {

          reader.close
        }

      }
      case BGZippedPDose(file, missingValue, fam, index) => {

        val inds = fam.map(x => openSource(x.getCanonicalPath)(s => getIndividualsFromFamFile(s)))
        val idx = openSource(index)(s => readIndex(s, (s: String) => StringStore(s))).toMap

        val x = new DelimitedSNPMajorQuery(BGZPDose(file, missingValue, inds, idx))
        try {
          f(x)
        } finally {
          x.close
        }
      }
      case BGZippedPGenotypeProbabilities(file, missingValue, fam, index) => {

        val inds = fam.map(x => openSource(x.getCanonicalPath)(s => getIndividualsFromFamFile(s)))
        val idx = openSource(index)(s => readIndex(s, (s: String) => StringStore(s))).toMap

        val x = new DelimitedSNPMajorQuery(BGZPGeno(file, missingValue, inds, idx))
        try {
          f(x)
        } finally {
          x.close
        }
      }
      case PDose(file, missingValue, fam) => {
        val bgzf = TempFile.createTempFile(".blocked.gz")
        openBlockedZippedFileOutputStream(bgzf) { os =>
          openFileInputStreamMaybeZipped(file) { is =>
            com.google.common.io.ByteStreams.copy(is, os)
          }
        }
        val index = BGZippedGenotypeHelper.makeIndex(bgzf)

        val inds = fam.map(x => openSource(x.getCanonicalPath)(s => getIndividualsFromFamFile(s)))

        val x = new DelimitedSNPMajorQuery(BGZPDose(bgzf, missingValue, inds, index))
        try {
          f(x)
        } finally {
          x.close
        }

      }
      case PGenotypeProbabilities(file, missingValue, fam) => {
        val bgzf = TempFile.createTempFile(".blocked.gz")
        openBlockedZippedFileOutputStream(bgzf) { os =>
          openFileInputStreamMaybeZipped(file) { is =>
            com.google.common.io.ByteStreams.copy(is, os)
          }
        }
        val index = BGZippedGenotypeHelper.makeIndex(bgzf)

        val inds = fam.map(x => openSource(x.getCanonicalPath)(s => getIndividualsFromFamFile(s)))

        val x = new DelimitedSNPMajorQuery(BGZPGeno(bgzf, missingValue, inds, index))
        try {
          f(x)
        } finally {
          x.close
        }

      }
      case _ => throw new RuntimeException(s"Random access of $fs not supported.")
    }
  }

}