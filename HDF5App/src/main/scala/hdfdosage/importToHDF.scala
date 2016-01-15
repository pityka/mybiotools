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

import HDFDosageFile._
import io.Source
import mybiotools.{ fastSplitSetSeparator, fastSplitSetSeparatorIterator }
import _root_.ch.systemsx.cisd.hdf5._
import java.io.File
import mybiotools.stringstore._
import mybiotools.gwascommons.genotypedata._
import mybiotools.gwascommons.genotypedata.SNPMajorReaders._
import mybiotools.gwascommons._

object Import {
  case class ConvertPDosageReport(
    duplicatedIndividuals: Set[Individual],
    duplicatedSNPs: Set[String8],
    writeSuccessful: Boolean
  )

  /**
   * Converts a plink --dosage input.txt format=1 compatible ascii file into hdf5.
   *
   * @param pdosage the scala.io.Source to read input from. It is NOT closed.
   * @param missingValue is the input files designated missing value. in hdf file missing value is ALWAYS -9f.
   */
  def convertPDosage(
    pdosage: Source,
    outputFile: File,
    missingValue: Float,
    cleanDuplicatedSNPs: Boolean,
    blockSize: Int = 1000,
    inputFileFormat: DosageFileFormat = PDose,
    individuals: Option[Seq[Individual]] = None,
    minimumMAF: Double = 0.0
  ): ConvertPDosageReport = {
    val f = inputFileFormat match {
      case PDose => PDoseFiles(pdosage, missingValue, individuals)
      case PGenotypeProbabilities => PGenotypeProbabilitiesFiles(pdosage, missingValue, individuals)
      case _ => throw new RuntimeException("Unsupported format")
    }
    convertSNPMajor(f, outputFile, cleanDuplicatedSNPs, blockSize, minimumMAF)
  }

  def convertSNPMajor(
    inputfiles: SNPMajorDosageInputFileSet,
    outputFile: File,
    cleanDuplicatedSNPs: Boolean,
    blockSize: Int = 1000,
    minimumMAF: Double = 0.0
  ): ConvertPDosageReport = {

    val snpMajorIterators = getSNPMajorIterator(inputfiles, Full, Set(), true)

    convertSNPMajorFromIterator(snpMajorIterators, outputFile, cleanDuplicatedSNPs, blockSize, minimumMAF)

  }

  def convertSNPMajorFromIterator(
    inputfiles: SNPMajorIterators,
    outputFile: File,
    cleanDuplicatedSNPs: Boolean,
    blockSize: Int = 1000,
    minimumMAF: Double = 0.0
  ): ConvertPDosageReport = {

    val filterOnMaf = (p: PDosageFileRowSummary) => p.maf >= minimumMAF

    val SNPMajorIterators(individuals,
      iterToHDF1,
      duplicatedSNPs,
      duplicatedIndividuals,
      missingValue
      ) = if (minimumMAF == 0.0) inputfiles else inputfiles.filter(filterOnMaf)

    val writeSuccessful = if ((duplicatedSNPs.size == 0 || cleanDuplicatedSNPs) && duplicatedIndividuals.size == 0) {

      if (outputFile.exists) outputFile.delete
      val hdfWriter = HDF5Factory.open(outputFile)

      val iterToHDF = iterToHDF1.map {
        case (pd, array) =>
          (pd, array.map(x => x match {
            case y if y == missingValue || y.isNaN => hdfdosage.MissingValue
            case y => y
          }))
      }

      HDFFile.writeHDF(hdfWriter, individuals.size, individuals, iterToHDF, blockSize)

      hdfWriter.close
      true
    } else {
      false
    }

    ConvertPDosageReport(duplicatedIndividuals, duplicatedSNPs, writeSuccessful)

  }
}