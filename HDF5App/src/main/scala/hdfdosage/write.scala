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

import mybiotools.gwascommons.genotypedata._
import java.io.File
import mybiotools._
import _root_.ch.systemsx.cisd.hdf5._
import mybiotools.gwascommons._
import scala.util.
  _

object WriteData {

  def writeBlockedPDose(pdosestring: String, missingValue: Float) = {

    val tmp = TempFile.createTempFile("df")
    val indexFile = new File(tmp.getAbsolutePath + ".pidx")
    openBlockedZippedFileWriter(tmp)(wr => wr.write(pdosestring))
    val index = BGZippedGenotypeHelper.makeIndex(tmp)
    openZippedFileWriter(indexFile)(writer => flatindex.writeIndex(index, writer, (s: mybiotools.stringstore.String8) => s.value))
    FileSets.BGZippedPDose(tmp, missingValue, None, indexFile)
  }

  def write(
    mergedIndividuals: Seq[Individual],
    snpDataIterator: Iterator[(PDosageFileRowSummary, Array[Float])],
    missingValue: Float,
    outputFormat: DosageFileFormat,
    outputFile: File,
    genomicMap: GenomicMap = Map(),
    recodeToRecessive: Boolean = false,
    blockSize: Int = 1000,
    grmbatch: Int = 1000,
    grmthreads: Int = 1
  ): Option[FileSets.GenotypeFileSet] = {

    @inline
    def dosage2genotypes(dosage: Double) =
      if (dosage <= 1.0) (1.0 - dosage, dosage, 0.0)
      else (0.0, 2.0 - dosage, dosage - 1.0)

    outputFormat match {
      case Missingness => openFileWriter(outputFile) { writer =>
        snpDataIterator.foreach {
          case (pdosagesum, floats) =>
            writer.write(s"${pdosagesum.snpName} ${pdosagesum.count.toDouble / mergedIndividuals.size} ${pdosagesum.count} ${mergedIndividuals.size}\n")
        }
        None
      }
      case HDFDosage => {

        @inline
        def replaceMissing(in: Array[Float]): Array[Float] = in.map(x => x match {
          case x if x == missingValue || x.isNaN => hdfdosage.MissingValue
          case x => x
        })

        val hdfWriter = HDF5Factory.open(outputFile)

        HDFFile.writeHDF(hdfWriter, mergedIndividuals.size, mergedIndividuals, if (missingValue == hdfdosage.MissingValue) snpDataIterator else snpDataIterator.map(x => x._1 -> replaceMissing(x._2)), blockSize)

        hdfWriter.file.flush
        hdfWriter.close
        Some(FileSets.HDFDosage(outputFile))
      }
      case PDose => {
        import mybiotools.flatindex._
        import mybiotools.stringstore._

        openBlockedZippedFileWriter(outputFile) { writer =>
          DosageWriter.writePDose(writer, mergedIndividuals, snpDataIterator, recodeToRecessive)
        }
        val indexFile = new File(outputFile.getAbsolutePath + ".pidx")
        val index: collection.Map[String8, Long] = BGZippedGenotypeHelper.makeIndex(outputFile).toMap
        openZippedFileWriter(indexFile)(writer => writeIndex(index, writer, (s: String8) => s.value))
        Some(FileSets.BGZippedPDose(outputFile, missingValue, None, indexFile))
      }
      case PGenotypeProbabilities => {
        openFileWriter(outputFile) { writer =>
          writer.write("SNP A1 A2 ")
          writer.write(mergedIndividuals.map(_.toLine).mkString(" "))
          writer.write('\n')

          val sb = new java.lang.StringBuffer(4)

          snpDataIterator.foreach { tuple =>
            val arrayOfDosages = tuple._2
            val snpName = tuple._1.snpName
            val a1 = tuple._1.al1
            val a2 = tuple._1.al2
            writer.write(snpName + " " + a1 + " " + a2)
            arrayOfDosages.foreach { dosage =>

              val (paa, paA, pAA) = dosage2genotypes(dosage.toDouble)

              writer.write(' ')
              sb.setLength(0)
              formatDouble2DecimalTo(sb, paa)
              writer.write(sb.toString)
              writer.write(' ')
              sb.setLength(0)
              formatDouble2DecimalTo(sb, paA)
              writer.write(sb.toString)
            }
            writer.write('\n')
          }
        }
        Some(FileSets.PGenotypeProbabilities(outputFile, missingValue, None))
      }
      case IMPUTEFile => {
        writeToFile(outputFile.getCanonicalPath + ".individuals", mergedIndividuals.map(_.toLine).mkString("\n"))
        openFileWriter(outputFile) { writer =>

          val sb = new java.lang.StringBuffer(4)

          snpDataIterator.foreach { tuple =>
            val arrayOfDosages = tuple._2
            val snpName = tuple._1.snpName
            val a1 = tuple._1.al1
            val a2 = tuple._1.al2
            writer.write(snpName + " . . " + a1 + " " + a2)
            arrayOfDosages.foreach { dosage =>

              val (paa, paA, pAA) = dosage2genotypes(dosage.toDouble)

              writer.write(' ')
              sb.setLength(0)
              formatDouble2DecimalTo(sb, paa)
              writer.write(sb.toString)
              writer.write(' ')
              sb.setLength(0)
              formatDouble2DecimalTo(sb, paA)
              writer.write(sb.toString)
              writer.write(' ')
              sb.setLength(0)
              formatDouble2DecimalTo(sb, pAA)
              writer.write(sb.toString)
            }
            writer.write('\n')
          }
        }
        None
      }
      case TPed(hardcall, missing) => {

        // Write .big and .bim        
        openFileWriter(new File(outputFile.getCanonicalPath + ".bim")) { writer =>
          TPedFile.writeTPED(new File(outputFile.getCanonicalPath + ".tped"), new File(outputFile.getCanonicalPath + ".tfam"), writer, mergedIndividuals, snpDataIterator, genomicMap, (x => true), hardcall, missing)
        }
        Some(FileSets.TPed(new File(outputFile.getCanonicalPath + ".tfam"), new File(outputFile.getCanonicalPath + ".tped"), missing))
      }
      case BIGSNPMajor => {

        // Write .big and .bim        
        openFileWriter(new File(outputFile.getCanonicalPath + ".bim")) { writer =>
          BIGFile.writeBIG(new File(outputFile.getCanonicalPath + ".big"), new File(outputFile.getCanonicalPath + ".fam"), writer, mergedIndividuals, snpDataIterator, genomicMap, recodeToRecessive)
        }
        None
      }
      case GRMMatrix => {
        val (grm, snpcount) = GRM.getGRMFromAutosomes(
          dosages = snpDataIterator,
          individuals = mergedIndividuals,
          missingValue = MissingValue,
          batchSize = grmbatch,
          threads = grmthreads
        )
        writeToFile(outputFile.getCanonicalPath + ".snpcount", snpcount.toString)
        openFileWriter(new File(outputFile.getCanonicalPath + ".grm.id")) { idwriter =>
          mybiotools.openZippedFileWriter(new File(outputFile.getCanonicalPath + ".grm.gz")) { grmwriter =>

            GRM.write(grm, writerGRM = grmwriter, writerIDs = idwriter)
          }
        }
        openFileWriter(new File(outputFile.getCanonicalPath + ".fastlmmsim")) { writer =>
          GRM.writeFastlmm(grm, writer)
        }

        val grmplot = GRM.plotGRMToFile(grm, new File(outputFile.getCanonicalPath + ".grm.png"))

        val pca = mybiotools.pcanew.pcaFromGRM(grm)

        val frame = pca.evecToFrame
        import org.saddle._
        import org.saddle.io.CsvImplicits._

        openFileWriter(new File(outputFile.getCanonicalPath + ".evec")) { writer =>
          FrameToPlink.writeFrameToPlink(frame.mapColIndex(i => "PCA_" + i).colAt(0 to 10 toArray), "-9", writer)
        }

        mybiotools.writeToFile(
          new File(outputFile.getCanonicalPath + ".eval").getAbsolutePath,
          pca.eigenValues.sorted.reverse.mkString("\n")
        )

        mybiotools.pcanew.plotPCAResultToFile(pca, 1, 2, new File(outputFile.getCanonicalPath + ".pcaplot.1:2.png"))
        mybiotools.pcanew.plotPCAResultToFile(pca, 1, 3, new File(outputFile.getCanonicalPath + ".pcaplot.1:3.png"))
        mybiotools.pcanew.plotPCAResultToFile(pca, 2, 3, new File(outputFile.getCanonicalPath + ".pcaplot.2:3.png"))
        None
      }
      case BIGIndividualMajor => {
        throw new UnsupportedOperationException("individual major file is not supported")
        // openFileWriter(new File(outputFile.getCanonicalPath + ".bim")) { bimwriter =>
        //   BIGFile.writeBIGIndividualMajor(
        //     new File(outputFile.getCanonicalPath + ".big"),
        //     new File(outputFile.getCanonicalPath + ".indlist"),
        //     bimwriter,
        //     mergedIndividuals,
        //     snpDataIterator)
        // }
      }
    }
  }
}