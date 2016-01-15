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

import org.scalatest.FunSuite
import scala.io.Source
import java.io.File
import hdfdosage._
import HDFDosageFile._
import HDFDosageFileTestHelpers._
import java.io.StringReader
import mybiotools.exec
import Import._

import mybiotools.stringstore._
import mybiotools.gwascommons.Individual
import mybiotools.gwascommons.genotypedata._

class ConvertTPedTestSuite extends FunSuite {

  implicit def string2string8(s: String) = new mybiotools.stringstore.String8(s)

  test("0x1") {

    val tped = Source.fromString("""""".stripMargin)
    val tfam = Source.fromString("""|F1 I1 0 0 -9 -9""".stripMargin)

    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest2126tped0.h5")
    val report = Import.convertSNPMajor(
      TPedFiles(tfam, tped, '0'),
      tmpfile, false, blockSize = 1
    )

    expectResult(ConvertPDosageReport(Set(), Set(), true))(report)

    expectResult(None) { readDosage(tmpfile, "snp1", Individual("F1", "I1")) }
    expectResult(None) { readDosage(tmpfile, "snp2", Individual("F1", "I1")) }
    expectResult(None) { readDosage(tmpfile, "snp3", Individual("F1", "I1")) }

    expectResult(None) { readDosage(tmpfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(tmpfile, "snp132", Individual("F1", "I1")) }

    expectResult(List(Individual("F1", "I1"))) { readIndividualOrder(tmpfile).toList }

    expectResult(Nil) { readSNPOrder(tmpfile).toList }
  }

  test("3x1 missing full") {

    val tped = Source.fromString("""chr1 snp1 0 1 T T
            |chr2 snp2 0 2 0 0
            |chr3 snp3 0 3 A A""".stripMargin)
    val tfam = Source.fromString("""|F1 I1 0 0 -9 -9""".stripMargin)

    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest2126tped.h5")
    val report = Import.convertSNPMajor(
      TPedFiles(tfam, tped, '0'),
      tmpfile, false, blockSize = 1
    )

    expectResult(ConvertPDosageReport(Set(), Set(), true))(report)

    expectResult(Some(2.0f)) { readDosage(tmpfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(-9f)) { readDosage(tmpfile, "snp2", Individual("F1", "I1")) }
    expectResult(Some(2.0f)) { readDosage(tmpfile, "snp3", Individual("F1", "I1")) }

    expectResult(None) { readDosage(tmpfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(tmpfile, "snp132", Individual("F1", "I1")) }

    expectResult(List(Individual("F1", "I1"))) { readIndividualOrder(tmpfile).toList }

    expectResult(List(
      PDosageFileRowSummary("snp1", "T", "0", 2.0f, 0.0f, 1, 0.0f, 0),
      PDosageFileRowSummary("snp2", "?", "0", 0.0f, 0.0f, 0, Float.NaN, 1),
      PDosageFileRowSummary("snp3", "A", "0", 2.0f, 0.0f, 1, 0.0f, 0)
    ).toString) { readSNPOrder(tmpfile).toList.toString }
  }

  test("3x1") {

    val tped = Source.fromString("""chr1 snp1 0 1 T T
            |chr2 snp2 0 2 A T
            |chr3 snp3 0 3 A A""".stripMargin)
    val tfam = Source.fromString("""|F1 I1 0 0 -9 -9""".stripMargin)

    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest2126tped.h5")
    val report = Import.convertSNPMajor(
      TPedFiles(tfam, tped, '0'),
      tmpfile, false, blockSize = 1
    )

    expectResult(ConvertPDosageReport(Set(), Set(), true))(report)

    expectResult(Some(2.0f)) { readDosage(tmpfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(1.0f)) { readDosage(tmpfile, "snp2", Individual("F1", "I1")) }
    expectResult(Some(2.0f)) { readDosage(tmpfile, "snp3", Individual("F1", "I1")) }

    expectResult(None) { readDosage(tmpfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(tmpfile, "snp132", Individual("F1", "I1")) }

    expectResult(List(Individual("F1", "I1"))) { readIndividualOrder(tmpfile).toList }

    expectResult(List(
      PDosageFileRowSummary("snp1", "T", "0", 2.0f, 0.0f, 1, 0.0f, 0),
      PDosageFileRowSummary("snp2", "A", "T", 1.0f, 1.0f, 1, 0.5f, 0),
      PDosageFileRowSummary("snp3", "A", "0", 2.0f, 0.0f, 1, 0.0f, 0)
    )) { readSNPOrder(tmpfile).toList }
  }
  test("3x2") {

    val tped = Source.fromString("""chr1 snp1 0 1 T T A A
            |chr2 snp2 0 2 A T A A
            |chr3 snp3 0 3 A T T T""".stripMargin)
    val tfam = Source.fromString("""|F1 I1 0 0 -9 -9
      |F2 I2 0 0 -9 -9""".stripMargin)

    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest2126tped2.h5")
    val report = Import.convertSNPMajor(
      TPedFiles(tfam, tped, '0'),
      tmpfile, false, blockSize = 1
    )

    expectResult(ConvertPDosageReport(Set(), Set(), true))(report)

    expectResult(Some(2.0f)) { readDosage(tmpfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(1.0f)) { readDosage(tmpfile, "snp2", Individual("F1", "I1")) }
    expectResult(Some(1.0f)) { readDosage(tmpfile, "snp3", Individual("F1", "I1")) }

    expectResult(Some(0.0f)) { readDosage(tmpfile, "snp1", Individual("F2", "I2")) }
    expectResult(Some(2.0f)) { readDosage(tmpfile, "snp2", Individual("F2", "I2")) }
    expectResult(Some(0.0f)) { readDosage(tmpfile, "snp3", Individual("F2", "I2")) }

    expectResult(None) { readDosage(tmpfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(tmpfile, "snp132", Individual("F1", "I1")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"))) { readIndividualOrder(tmpfile).toList }

    expectResult(List(
      PDosageFileRowSummary("snp1", "T", "A", 2.0f, 2.0f, 2, 0.5f, 0),
      PDosageFileRowSummary("snp2", "A", "T", 3.0f, 1.0f, 2, 0.25f, 0),
      PDosageFileRowSummary("snp3", "A", "T", 1.0f, 3.0f, 2, 0.25f, 0)
    )) { readSNPOrder(tmpfile).toList }
  }

  test("3x2 with missing") {

    val tped = Source.fromString("""chr1 snp1 0 1 T T A A
            |chr2 snp2 0 2 K A A T
            |chr3 snp3 0 3 A T T T""".stripMargin)
    val tfam = Source.fromString("""|F1 I1 0 0 -9 -9
      |F2 I2 0 0 -9 -9""".stripMargin)

    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest2126tped2missing.h5")
    val report = Import.convertSNPMajor(
      TPedFiles(tfam, tped, 'K'),
      tmpfile, false, blockSize = 1
    )

    expectResult(ConvertPDosageReport(Set(), Set(), true))(report)

    expectResult(Some(2.0f)) { readDosage(tmpfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(-9f)) { readDosage(tmpfile, "snp2", Individual("F1", "I1")) }
    expectResult(Some(1.0f)) { readDosage(tmpfile, "snp3", Individual("F1", "I1")) }

    expectResult(Some(0.0f)) { readDosage(tmpfile, "snp1", Individual("F2", "I2")) }
    expectResult(Some(1.0f)) { readDosage(tmpfile, "snp2", Individual("F2", "I2")) }
    expectResult(Some(0.0f)) { readDosage(tmpfile, "snp3", Individual("F2", "I2")) }

    expectResult(None) { readDosage(tmpfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(tmpfile, "snp132", Individual("F1", "I1")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"))) { readIndividualOrder(tmpfile).toList }

    expectResult(List(
      PDosageFileRowSummary("snp1", "T", "A", 2.0f, 2.0f, 2, 0.5f, 0),
      PDosageFileRowSummary("snp2", "A", "T", 1.0f, 1.0f, 1, 0.5f, 1),
      PDosageFileRowSummary("snp3", "A", "T", 1.0f, 3.0f, 2, 0.25f, 0)
    )) { readSNPOrder(tmpfile).toList }
  }

  test("hapmap") {

    val tped = Source.fromFile(getClass.getResource("/").getPath + "hdf/hapmap1.tped")
    val tfam = Source.fromFile(getClass.getResource("/").getPath + "hdf/hapmap1.tfam")

    val bim = getClass.getResource("/").getPath + "hdf/hapmap1.bim"

    val gmap = mybiotools.gwascommons.getGenomicMapFromBimFile(bim)

    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftesthapmap.h5")
    val tmpfiletped = new File(getClass.getResource("/").getPath + "/hdftesthapmap2.tped")
    val report = Import.convertSNPMajor(
      TPedFiles(tfam, tped, '0'),
      tmpfile, false, blockSize = 1
    )

    expectResult(ConvertPDosageReport(Set(), Set(), true))(report)

    val inds = readIndividualOrder(tmpfile).toList
    val snps = readSNPOrder(tmpfile).toList

    mergeSNPMajors(List(FileSets.HDFDosage(tmpfile)), tmpfiletped, outputFormat = TPed(0.1, '0'), genomicMap = gmap)

    val outtped = Source.fromFile(tmpfiletped.getPath + ".tped")
    val outtfam = Source.fromFile(tmpfiletped.getPath + ".tfam")

    expectResult(tfam.reset.mkString)(outtfam.reset.mkString)
    tped.reset.getLines.toList zip outtped.reset.getLines.toList foreach { lines =>
      expectResult(lines._1)(lines._2)
    }
    expectResult(mybiotools.getCheckSum(tmpfiletped.getPath + ".tped"))(mybiotools.getCheckSum(getClass.getResource("/").getPath + "hdf/hapmap1.tped"))

  }

  ignore("hapmap random access measure time") {

    val tped = Source.fromFile(getClass.getResource("/").getPath + "hdf/hapmap1.tped")
    val tfam = Source.fromFile(getClass.getResource("/").getPath + "hdf/hapmap1.tfam")

    val bim = getClass.getResource("/").getPath + "hdf/hapmap1.bim"

    val gmap = mybiotools.gwascommons.getGenomicMapFromBimFile(bim)

    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftesthapmap.h5")
    val tmpfiletped = new File(getClass.getResource("/").getPath + "/hdftesthapmap2.tped")
    val report = Import.convertSNPMajor(
      TPedFiles(tfam, tped, '0'),
      tmpfile, false, blockSize = 100
    )

    expectResult(ConvertPDosageReport(Set(), Set(), true))(report)

    val inds = readIndividualOrder(tmpfile).toList
    val snps = readSNPOrder(tmpfile).toList

    val buffer = SNPBuffer.fromFile(tmpfile, 100)

    val t1 = System.nanoTime
    snps.foreach { s =>
      buffer.getSNP(s.snpName)
    }
    val diff1 = (System.nanoTime - t1) / 1E9

    println("XXX" + diff1)

    val t2 = System.nanoTime
    snps.reverse.foreach { s =>
      buffer.getSNP(s.snpName)
    }
    val diff2 = (System.nanoTime - t2) / 1E9

    println("YYY" + diff2)
  }

}