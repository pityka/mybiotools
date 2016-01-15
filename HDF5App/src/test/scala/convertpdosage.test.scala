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
import mybiotools.gwascommons.genotypedata.PDosageFileRowSummary

class ConvertPDosageHDFTestSuite extends FunSuite {

  implicit def string2string8(s: String) = new mybiotools.stringstore.String8(s)

  test("2x2") {

    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp2 G C 1.2 -8""".stripMargin)

    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest26.h5")
    val report = Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    expectResult(ConvertPDosageReport(Set(), Set(), true))(report)

    expectResult(Some(1.0f)) { readDosage(tmpfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(1.3f)) { readDosage(tmpfile, "snp1", Individual("F2", "I2")) }
    expectResult(Some(1.2f)) { readDosage(tmpfile, "snp2", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(tmpfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(tmpfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(tmpfile, "snp132", Individual("F1", "I1")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"))) { readIndividualOrder(tmpfile).toList }

    expectResult(List(
      PDosageFileRowSummary("snp1", "A", "T", 2.3f, 1.7f, 2, 0.425f, 0),
      PDosageFileRowSummary("snp2", "G", "C", 1.2f, 0.79999995f, 1, 0.39999998f, 1)
    )) { readSNPOrder(tmpfile).toList }
  }

  test("2x2 with multichar alleles") {

    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 AA T2 1.0 1.3
            |snp2 G2 Cc 1.2 -8""".stripMargin)

    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest26231.h5")
    val report = Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    expectResult(ConvertPDosageReport(Set(), Set(), true))(report)

    expectResult(Some(1.0f)) { readDosage(tmpfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(1.3f)) { readDosage(tmpfile, "snp1", Individual("F2", "I2")) }
    expectResult(Some(1.2f)) { readDosage(tmpfile, "snp2", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(tmpfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(tmpfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(tmpfile, "snp132", Individual("F1", "I1")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"))) { readIndividualOrder(tmpfile).toList }

    expectResult(List(
      PDosageFileRowSummary("snp1", "AA", "T2", 2.3f, 1.7f, 2, 0.425f, 0),
      PDosageFileRowSummary("snp2", "G2", "Cc", 1.2f, 0.79999995f, 1, 0.39999998f, 1)
    )) { readSNPOrder(tmpfile).toList }
  }

  test("0x0") {
    val pdosage = Source.fromString("""SNP A1 A2""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest27.h5")
    val report = Import.convertPDosage(pdosage, tmpfile, MissingValue, false, blockSize = 1)

    expectResult(ConvertPDosageReport(Set(), Set(), true))(report)

    expectResult(None) { readDosage(tmpfile, "snp1", Individual("F1", "I1")) }
    expectResult(None) { readDosage(tmpfile, "snp1", Individual("F2", "I2")) }
    expectResult(None) { readDosage(tmpfile, "snp2", Individual("F1", "I1")) }
    expectResult(None) { readDosage(tmpfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(tmpfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(tmpfile, "snp132", Individual("F1", "I1")) }

    expectResult(Nil) { readIndividualOrder(tmpfile).toList }

    expectResult(Nil) { readSNPOrder(tmpfile).toList }
  }

  test("2x0") {
    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest28.h5")
    val report = Import.convertPDosage(pdosage, tmpfile, MissingValue, false, blockSize = 1)

    expectResult(ConvertPDosageReport(Set(), Set(), true))(report)

    expectResult(None) { readDosage(tmpfile, "snp1", Individual("F1", "I1")) }
    expectResult(None) { readDosage(tmpfile, "snp1", Individual("F2", "I2")) }
    expectResult(None) { readDosage(tmpfile, "snp2", Individual("F1", "I1")) }
    expectResult(None) { readDosage(tmpfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(tmpfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(tmpfile, "snp132", Individual("F1", "I1")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"))) { readIndividualOrder(tmpfile).toList }

    expectResult(Nil) { readSNPOrder(tmpfile).toList }
  }

  test("2x2 all missing") {
    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T -9 -9
            |snp2 G C -9 -9""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest29.h5")
    val report = Import.convertPDosage(pdosage, tmpfile, MissingValue, false, blockSize = 1)

    expectResult(ConvertPDosageReport(Set(), Set(), true))(report)

    expectResult(Some(MissingValue)) { readDosage(tmpfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(tmpfile, "snp1", Individual("F2", "I2")) }
    expectResult(Some(MissingValue)) { readDosage(tmpfile, "snp2", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(tmpfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(tmpfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(tmpfile, "snp132", Individual("F1", "I1")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"))) { readIndividualOrder(tmpfile).toList }

    expectResult(
      List(
        PDosageFileRowSummary("snp1", "A", "T", 0.0f, 0.0f, 0, Float.NaN, 2).toString,
        PDosageFileRowSummary("snp2", "G", "C", 0.0f, 0.0f, 0, Float.NaN, 2).toString
      )
    ) { readSNPOrder(tmpfile).toList.map(_.toString) }
  }

  test("2x2 duplicated individual") {
    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp2 G C 1.2 -9""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest30.h5")
    val report = Import.convertPDosage(pdosage, tmpfile, MissingValue, false, blockSize = 1)

    expectResult(ConvertPDosageReport(Set(Individual("F1", "I1")), Set(), false))(report)

  }

  test("2x2 duplicated snp") {
    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F1 F2
            |snp1 A T 1.0 1.3
            |snp1 G C 1.2 -9""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest31.h5")
    val report = Import.convertPDosage(pdosage, tmpfile, MissingValue, false, blockSize = 1)

    expectResult(ConvertPDosageReport(Set(), Set("snp1"), false))(report)

  }

  test("2x2 duplicated snp , clean") {
    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp1 G C 1.2 -9""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest3dddddrer1.h5")
    val report = Import.convertPDosage(pdosage, tmpfile, MissingValue, true, blockSize = 1)

    expectResult(ConvertPDosageReport(Set(), Set("snp1"), true))(report)

    expectResult(Some(1.2f)) { readDosage(tmpfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(-9f)) { readDosage(tmpfile, "snp1", Individual("F2", "I2")) }
    expectResult(None) { readDosage(tmpfile, "snp2", Individual("F1", "I1")) }
    expectResult(None) { readDosage(tmpfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(tmpfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(tmpfile, "snp132", Individual("F1", "I1")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"))) { readIndividualOrder(tmpfile).toList }

    expectResult(
      List(
        PDosageFileRowSummary("snp1", "G", "C", 1.2f, 0.79999995f, 1, 0.39999998f, 1).toString
      )
    ) { readSNPOrder(tmpfile).toList.map(_.toString) }
  }

  test("merge dosage hdf: merge 1 file") {
    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp2 G C 1.2 -9""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest32.h5")
    Import.convertPDosage(pdosage, tmpfile, MissingValue, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestmerged33.h5")
    outfile.delete

    val report = mergeSNPMajors(IndexedSeq(FileSets.HDFDosage(tmpfile)), outfile)

    expectResult(MergeDosageReport(Nil, Some(FileSets.HDFDosage(outfile)), true))(report)

    expectResult(Some(1.0f)) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(1.3f)) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(outfile, "snp132", Individual("F1", "I1")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"))) { readIndividualOrder(outfile).toList }

    expectResult(List(
      PDosageFileRowSummary("snp1", "A", "T", 2.3f, 1.7f, 2, 0.425f, 0),
      PDosageFileRowSummary("snp2", "G", "C", 1.2f, 0.79999995f, 1, 0.39999998f, 1)
    )) { readSNPOrder(outfile).toList }
  }

  test("merge 1 empty") {

    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest34.h5")
    Import.convertPDosage(pdosage, tmpfile, MissingValue, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestmerged35.h5")

    val report = mergeSNPMajors(IndexedSeq(FileSets.HDFDosage(tmpfile)), outfile)

    expectResult(MergeDosageReport(Nil, Some(FileSets.HDFDosage(outfile)), true))(report)

    expectResult(None) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(outfile, "snp132", Individual("F1", "I1")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"))) { readIndividualOrder(outfile).toList }

    expectResult(Nil) { readSNPOrder(outfile).toList }

  }

  test("merge 1 empty 0x0 ") {

    val pdosage = Source.fromString("""SNP A1 A2""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest36.h5")
    Import.convertPDosage(pdosage, tmpfile, MissingValue, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestmerged37.h5")

    val report = mergeSNPMajors(IndexedSeq(FileSets.HDFDosage(tmpfile)), outfile)

    expectResult(MergeDosageReport(Nil, Some(FileSets.HDFDosage(outfile)), true))(report)

    expectResult(None) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(outfile, "snp132", Individual("F1", "I1")) }

    expectResult(Nil) { readIndividualOrder(outfile).toList }

    expectResult(Nil) { readSNPOrder(outfile).toList }

  }

  test("merge 2 empty 2x0") {
    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest38.h5")
    Import.convertPDosage(pdosage, tmpfile, MissingValue, false, blockSize = 1)

    val pdosage2 = Source.fromString("""SNP A1 A2 F3 I3 F4 I4""".stripMargin)
    val tmpfile2 = new File(getClass.getResource("/").getPath + "/hdftest239.h5")
    Import.convertPDosage(pdosage2, tmpfile2, MissingValue, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestmerged40.h5")

    val report = mergeSNPMajors(IndexedSeq(FileSets.HDFDosage(tmpfile), FileSets.HDFDosage(tmpfile2)), outfile)

    expectResult(MergeDosageReport(Nil, Some(FileSets.HDFDosage(outfile)), true))(report)

    expectResult(None) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(outfile, "snp132", Individual("F1", "I1")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"), Individual("F3", "I3"), Individual("F4", "I4"))) { readIndividualOrder(outfile).toList }

    expectResult(Nil) { readSNPOrder(outfile).toList }
  }

  test("merge 2 empty 0x0") {
    val pdosage = Source.fromString("""SNP A1 A2""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest41.h5")
    Import.convertPDosage(pdosage, tmpfile, MissingValue, false, blockSize = 1)

    val pdosage2 = Source.fromString("""SNP A1 A2""".stripMargin)
    val tmpfile2 = new File(getClass.getResource("/").getPath + "/hdftest242.h5")
    Import.convertPDosage(pdosage2, tmpfile2, MissingValue, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestmerged43.h5")

    val report = mergeSNPMajors(IndexedSeq(FileSets.HDFDosage(tmpfile), FileSets.HDFDosage(tmpfile2)), outfile)

    expectResult(MergeDosageReport(Nil, Some(FileSets.HDFDosage(outfile)), true))(report)

    expectResult(None) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(outfile, "snp132", Individual("F1", "I1")) }

    expectResult(Nil) { readIndividualOrder(outfile).toList }

    expectResult(Nil) { readSNPOrder(outfile).toList }
  }

  test("sort by genomic order") {
    import mybiotools.gwascommons._
    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp2 G C 1.2 1.5""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest1231.h5")
    val outfile = new File(getClass.getResource("/").getPath + "/hdftest1231123.h5")
    Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)
    val report = mergeSNPMajors(Vector(FileSets.HDFDosage(tmpfile)), outfile, genomicMap = Map(StringStore("snp1") -> GenomicLocation(1, 2), StringStore("snp2") -> GenomicLocation(1, 1)))

    expectResult(MergeDosageReport(Nil, Some(FileSets.HDFDosage(outfile)), true))(report)

    expectResult(List(
      PDosageFileRowSummary("snp2", "G", "C", 2.7f, 1.3f, 2, 0.325f, 0),
      PDosageFileRowSummary("snp1", "A", "T", 2.3f, 1.7f, 2, 0.425f, 0)
    )) { readSNPOrder(outfile).toList }
  }

  test("merge 1 nonempty with 1 empty (2x0)") {

    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp2 G C 1.2 -8""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest44.h5")
    Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    val pdosage2 = Source.fromString("""SNP A1 A2 F3 I3 F4 I4""".stripMargin)
    val tmpfile2 = new File(getClass.getResource("/").getPath + "/hdftest245.h5")
    Import.convertPDosage(pdosage2, tmpfile2, -8f, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestmerged46.h5")

    val report = mergeSNPMajors(IndexedSeq(FileSets.HDFDosage(tmpfile), FileSets.HDFDosage(tmpfile2)), outfile)

    expectResult(MergeDosageReport(Nil, Some(FileSets.HDFDosage(outfile)), true))(report)

    expectResult(Some(1.0f)) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(1.3f)) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(outfile, "snp132", Individual("F1", "I1")) }

    expectResult(Some(MissingValue)) { readDosage(outfile, "snp1", Individual("F3", "I3")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp1", Individual("F4", "I4")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp2", Individual("F3", "I3")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp2", Individual("F4", "I4")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"), Individual("F3", "I3"), Individual("F4", "I4"))) { readIndividualOrder(outfile).toList }

    expectResult(List(
      PDosageFileRowSummary("snp1", "A", "T", 2.3f, 1.7f, 2, 0.425f, 2),
      PDosageFileRowSummary("snp2", "G", "C", 1.2f, 0.79999995f, 1, 0.39999998f, 3)
    )) { readSNPOrder(outfile).toList }
  }

  test("merge 2 nonempty with same SNPs") {

    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp2 G C 1.2 -8""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest37.h5")
    Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    val pdosage2 = Source.fromString("""SNP A1 A2 F3 I3 F4 I4
        |snp1 A T 1.8 0.3
            |snp2 G C 0.2 1.2""".stripMargin)
    val tmpfile2 = new File(getClass.getResource("/").getPath + "/hdftest248.h5")
    Import.convertPDosage(pdosage2, tmpfile2, -8f, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestmerged49.h5")

    val report = mergeSNPMajors(IndexedSeq(FileSets.HDFDosage(tmpfile), FileSets.HDFDosage(tmpfile2)), outfile)

    expectResult(MergeDosageReport(Nil, Some(FileSets.HDFDosage(outfile)), true))(report)

    expectResult(Some(1.0f)) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(1.3f)) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(outfile, "snp132", Individual("F1", "I1")) }

    expectResult(Some(1.8f)) { readDosage(outfile, "snp1", Individual("F3", "I3")) }
    expectResult(Some(0.3f)) { readDosage(outfile, "snp1", Individual("F4", "I4")) }
    expectResult(Some(0.2f)) { readDosage(outfile, "snp2", Individual("F3", "I3")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp2", Individual("F4", "I4")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"), Individual("F3", "I3"), Individual("F4", "I4"))) { readIndividualOrder(outfile).toList }

    expectResult(List(
      PDosageFileRowSummary("snp1", "A", "T", 4.4f, 3.6000001f, 4, 0.45f, 0),
      PDosageFileRowSummary("snp2", "G", "C", 2.6000001f, 3.3999999f, 3, 0.43333334f, 1)
    )) { readSNPOrder(outfile).toList }

  }

  test("merge 2 nonempty with different SNPs, same patients") {

    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp2 G C 1.2 -8""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest50.h5")
    Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    val pdosage2 = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
        |snp3 A T 1.8 0.3
        |snp4 G C 0.2 1.2""".stripMargin)
    val tmpfile2 = new File(getClass.getResource("/").getPath + "/hdftest251.h5")
    Import.convertPDosage(pdosage2, tmpfile2, -8f, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestmerged52.h5")

    val report = mergeSNPMajors(IndexedSeq(FileSets.HDFDosage(tmpfile), FileSets.HDFDosage(tmpfile2)), outfile)

    expectResult(MergeDosageReport(Nil, Some(FileSets.HDFDosage(outfile)), true))(report)

    expectResult(Some(1.0f)) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(1.3f)) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(outfile, "snp132", Individual("F1", "I1")) }

    expectResult(Some(1.8f)) { readDosage(outfile, "snp3", Individual("F1", "I1")) }
    expectResult(Some(0.3f)) { readDosage(outfile, "snp3", Individual("F2", "I2")) }
    expectResult(Some(0.2f)) { readDosage(outfile, "snp4", Individual("F1", "I1")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp4", Individual("F2", "I2")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"))) { readIndividualOrder(outfile).toList }

    expectResult(List(
      PDosageFileRowSummary("snp1", "A", "T", 2.3f, 1.7f, 2, 0.425f, 0),
      PDosageFileRowSummary("snp3", "A", "T", 2.1f, 1.9000001f, 2, 0.47500002f, 0),

      PDosageFileRowSummary("snp4", "G", "C", 1.4000001f, 2.6f, 2, 0.35000002f, 0),
      PDosageFileRowSummary("snp2", "G", "C", 1.2f, 0.79999995f, 1, 0.39999998f, 1)
    )) { readSNPOrder(outfile).toList }

  }

  test("merge 2 nonempty with different SNPs, different patients") {

    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp2 G C 1.2 -8""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest53.h5")
    Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    val pdosage2 = Source.fromString("""SNP A1 A2 F3 I3 F4 I4
        |snp3 A T 1.8 0.3
        |snp4 G C 0.2 1.2""".stripMargin)
    val tmpfile2 = new File(getClass.getResource("/").getPath + "/hdftest254.h5")
    Import.convertPDosage(pdosage2, tmpfile2, -8f, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestmerged55.h5")

    val report = mergeSNPMajors(IndexedSeq(FileSets.HDFDosage(tmpfile), FileSets.HDFDosage(tmpfile2)), outfile)

    expectResult(MergeDosageReport(Nil, Some(FileSets.HDFDosage(outfile)), true))(report)

    expectResult(Some(1.0f)) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(1.3f)) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(outfile, "snp132", Individual("F1", "I1")) }

    expectResult(Some(MissingValue)) { readDosage(outfile, "snp3", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp3", Individual("F2", "I2")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp4", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp4", Individual("F2", "I2")) }

    expectResult(Some(MissingValue)) { readDosage(outfile, "snp1", Individual("F3", "I3")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp2", Individual("F3", "I3")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp1", Individual("F4", "I4")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp2", Individual("F4", "I4")) }

    expectResult(Some(1.8f)) { readDosage(outfile, "snp3", Individual("F3", "I3")) }
    expectResult(Some(0.2f)) { readDosage(outfile, "snp4", Individual("F3", "I3")) }
    expectResult(Some(0.3f)) { readDosage(outfile, "snp3", Individual("F4", "I4")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp4", Individual("F4", "I4")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"), Individual("F3", "I3"), Individual("F4", "I4"))) { readIndividualOrder(outfile).toList }

    expectResult(List(
      PDosageFileRowSummary("snp1", "A", "T", 2.3f, 1.7f, 2, 0.425f, 2),
      PDosageFileRowSummary("snp3", "A", "T", 2.1f, 1.9000001f, 2, 0.47500002f, 2),
      PDosageFileRowSummary("snp4", "G", "C", 1.4000001f, 2.6f, 2, 0.35000002f, 2),
      PDosageFileRowSummary("snp2", "G", "C", 1.2f, 0.79999995f, 1, 0.39999998f, 3)
    )) { readSNPOrder(outfile).toList }

  }

  test("merge 3 nonempty") {

    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp2 G C 1.2 -8""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest56.h5")
    Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    val pdosage2 = Source.fromString("""SNP A1 A2 F3 I3 F4 I4
        |snp3 A T 1.8 0.3
        |snp4 G C 0.2 1.2""".stripMargin)
    val tmpfile2 = new File(getClass.getResource("/").getPath + "/hdftest256.h5")
    Import.convertPDosage(pdosage2, tmpfile2, -8f, false, blockSize = 1)

    val pdosage3 = Source.fromString("""SNP A1 A2 F5 I5 F6 I6
        |snp5 A T 1.8 0.3
        |snp6 G C 0.2 1.2""".stripMargin)
    val tmpfile3 = new File(getClass.getResource("/").getPath + "/hdftest357.h5")
    Import.convertPDosage(pdosage3, tmpfile3, -8f, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestmerged58.h5")

    val report = mergeSNPMajors(IndexedSeq(FileSets.HDFDosage(tmpfile), FileSets.HDFDosage(tmpfile2), FileSets.HDFDosage(tmpfile3)), outfile)

    expectResult(MergeDosageReport(Nil, Some(FileSets.HDFDosage(outfile)), true))(report)

    expectResult(Some(1.0f)) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(1.3f)) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(outfile, "snp132", Individual("F1", "I1")) }

    expectResult(Some(MissingValue)) { readDosage(outfile, "snp3", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp3", Individual("F2", "I2")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp4", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp4", Individual("F2", "I2")) }

    expectResult(Some(MissingValue)) { readDosage(outfile, "snp5", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp5", Individual("F2", "I2")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp6", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp6", Individual("F2", "I2")) }

    expectResult(Some(MissingValue)) { readDosage(outfile, "snp1", Individual("F3", "I3")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp2", Individual("F3", "I3")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp1", Individual("F4", "I4")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp2", Individual("F4", "I4")) }

    expectResult(Some(MissingValue)) { readDosage(outfile, "snp5", Individual("F3", "I3")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp6", Individual("F3", "I3")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp5", Individual("F4", "I4")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp6", Individual("F4", "I4")) }

    expectResult(Some(MissingValue)) { readDosage(outfile, "snp1", Individual("F5", "I5")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp2", Individual("F6", "I6")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp1", Individual("F5", "I5")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp2", Individual("F6", "I6")) }

    expectResult(Some(MissingValue)) { readDosage(outfile, "snp3", Individual("F5", "I5")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp4", Individual("F6", "I6")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp3", Individual("F5", "I5")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp4", Individual("F6", "I6")) }

    expectResult(Some(1.8f)) { readDosage(outfile, "snp3", Individual("F3", "I3")) }
    expectResult(Some(0.2f)) { readDosage(outfile, "snp4", Individual("F3", "I3")) }
    expectResult(Some(0.3f)) { readDosage(outfile, "snp3", Individual("F4", "I4")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp4", Individual("F4", "I4")) }

    expectResult(Some(0.2f)) { readDosage(outfile, "snp6", Individual("F5", "I5")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp6", Individual("F6", "I6")) }
    expectResult(Some(1.8f)) { readDosage(outfile, "snp5", Individual("F5", "I5")) }
    expectResult(Some(0.3f)) { readDosage(outfile, "snp5", Individual("F6", "I6")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"), Individual("F3", "I3"), Individual("F4", "I4"), Individual("F5", "I5"), Individual("F6", "I6"))) { readIndividualOrder(outfile).toList }

    expectResult(List(
      PDosageFileRowSummary("snp1", "A", "T", 2.3f, 1.7f, 2, 0.425f, 4),
      PDosageFileRowSummary("snp3", "A", "T", 2.1f, 1.9000001f, 2, 0.47500002f, 4),
      PDosageFileRowSummary("snp4", "G", "C", 1.4000001f, 2.6f, 2, 0.35000002f, 4),
      PDosageFileRowSummary("snp5", "A", "T", 2.1f, 1.9000001f, 2, 0.47500002f, 4),
      PDosageFileRowSummary("snp6", "G", "C", 1.4000001f, 2.6f, 2, 0.35000002f, 4),
      PDosageFileRowSummary("snp2", "G", "C", 1.2f, 0.79999995f, 1, 0.39999998f, 5)
    )) { readSNPOrder(outfile).toList }

  }

  test("merge the same") {

    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp2 G C 1.2 -8""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest59.h5")
    Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    val pdosage2 = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp2 G C 1.2 -8""".stripMargin)
    val tmpfile2 = new File(getClass.getResource("/").getPath + "/hdftest260.h5")
    Import.convertPDosage(pdosage2, tmpfile2, -8f, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestmerged61.h5")

    val report = mergeSNPMajors(IndexedSeq(FileSets.HDFDosage(tmpfile), FileSets.HDFDosage(tmpfile2)), outfile)

    expectResult(MergeDosageReport(Nil, Some(FileSets.HDFDosage(outfile)), true))(report)

    expectResult(Some(1.0f)) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(1.3f)) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(outfile, "snp132", Individual("F1", "I1")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"))) { readIndividualOrder(outfile).toList }

    expectResult(List(
      PDosageFileRowSummary("snp1", "A", "T", 2.3f, 1.7f, 2, 0.425f, 0),
      PDosageFileRowSummary("snp2", "G", "C", 1.2f, 0.79999995f, 1, 0.39999998f, 1)
    )) { readSNPOrder(outfile).toList }

  }

  test("merge 2 with overlapping snps and overlapping individuals, matching snp overlap") {

    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp2 G C 1.2 -8""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest63.h5")
    Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    val pdosage2 = Source.fromString("""SNP A1 A2 F2 I2 F4 I4
        |snp2 G C -8 0.3
        |snp4 G C 0.2 1.2""".stripMargin)
    val tmpfile2 = new File(getClass.getResource("/").getPath + "/hdftest264.h5")
    Import.convertPDosage(pdosage2, tmpfile2, -8f, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestmerged65.h5")

    val report = mergeSNPMajors(IndexedSeq(FileSets.HDFDosage(tmpfile), FileSets.HDFDosage(tmpfile2)), outfile)

    expectResult(MergeDosageReport(Nil, Some(FileSets.HDFDosage(outfile)), true))(report)

    expectResult(Some(1.0f)) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(1.3f)) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(outfile, "snp132", Individual("F1", "I1")) }

    expectResult(None) { readDosage(outfile, "snp3", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp3", Individual("F2", "I2")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp4", Individual("F1", "I1")) }
    expectResult(Some(0.2f)) { readDosage(outfile, "snp4", Individual("F2", "I2")) }

    expectResult(None) { readDosage(outfile, "snp1", Individual("F3", "I3")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F3", "I3")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp1", Individual("F4", "I4")) }
    expectResult(Some(0.3f)) { readDosage(outfile, "snp2", Individual("F4", "I4")) }

    expectResult(None) { readDosage(outfile, "snp3", Individual("F3", "I3")) }
    expectResult(None) { readDosage(outfile, "snp4", Individual("F3", "I3")) }
    expectResult(None) { readDosage(outfile, "snp3", Individual("F4", "I4")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp4", Individual("F4", "I4")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"), Individual("F4", "I4"))) { readIndividualOrder(outfile).toList }

    expectResult(List(
      PDosageFileRowSummary("snp1", "A", "T", 2.3f, 1.7f, 2, 0.425f, 1),
      PDosageFileRowSummary("snp4", "G", "C", 1.4000001f, 2.6f, 2, 0.35000002f, 1),
      PDosageFileRowSummary("snp2", "G", "C", 1.5f, 2.5f, 2, 0.375f, 1)
    )) { readSNPOrder(outfile).toList }

  }

  test("merge 2 with mismatching snps") {

    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp2 G C 1.2 -8""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest66.h5")
    Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    val pdosage2 = Source.fromString("""SNP A1 A2 F2 I2 F4 I4
        |snp2 C G -8 0.3
        |snp4 G C 0.2 1.2""".stripMargin)
    val tmpfile2 = new File(getClass.getResource("/").getPath + "/hdftest267.h5")
    Import.convertPDosage(pdosage2, tmpfile2, -8f, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestmerged68.h5")

    val report = mergeSNPMajors(IndexedSeq(FileSets.HDFDosage(tmpfile), FileSets.HDFDosage(tmpfile2)), outfile)

    expectResult(MergeDosageReport(List(Vector(PDosageFileRowSummary("snp2", "G", "C", 1.2f, 0.79999995f, 1, 0.39999998f, 1), PDosageFileRowSummary("snp2", "C", "G", 0.3f, 1.7f, 1, 0.15f, 1))), None, false))(report)

  }

  test("merge 2 with missing values written (not leaky)") {

    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2 F3 I3 F4 I4
            |snp1 A T 1.0 1.3 -8 -8
            |snp2 G C 1.2 -8 -8 -8
            |snp3 A T -8 -8 -8 -8
            |snp4 G C -8 -8 -8 -8""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest25.h5")
    Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    val pdosage2 = Source.fromString("""SNP A1 A2 F3 I3 F4 I4 F1 I1 F2 I2
        |snp3 A T 1.8 0.3 -8 -8
        |snp4 G C 0.2 1.2 -8 -8
        |snp1 A T -8 -8 -8 -8
        |snp2 G C -8 -8 -8 -8""".stripMargin)
    val tmpfile2 = new File(getClass.getResource("/").getPath + "/hdftest224.h5")
    Import.convertPDosage(pdosage2, tmpfile2, -8f, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestmerged2323.h5")

    val report = mergeSNPMajors(IndexedSeq(FileSets.HDFDosage(tmpfile), FileSets.HDFDosage(tmpfile2)), outfile)

    expectResult(MergeDosageReport(Nil, Some(FileSets.HDFDosage(outfile)), true))(report)

    expectResult(Some(1.0f)) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(1.3f)) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(outfile, "snp132", Individual("F1", "I1")) }

    expectResult(Some(MissingValue)) { readDosage(outfile, "snp3", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp3", Individual("F2", "I2")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp4", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp4", Individual("F2", "I2")) }

    expectResult(Some(MissingValue)) { readDosage(outfile, "snp1", Individual("F3", "I3")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp2", Individual("F3", "I3")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp1", Individual("F4", "I4")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp2", Individual("F4", "I4")) }

    expectResult(Some(1.8f)) { readDosage(outfile, "snp3", Individual("F3", "I3")) }
    expectResult(Some(0.2f)) { readDosage(outfile, "snp4", Individual("F3", "I3")) }
    expectResult(Some(0.3f)) { readDosage(outfile, "snp3", Individual("F4", "I4")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp4", Individual("F4", "I4")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"), Individual("F3", "I3"), Individual("F4", "I4"))) { readIndividualOrder(outfile).toList }

    expectResult(Set(
      PDosageFileRowSummary("snp1", "A", "T", 2.3f, 1.7f, 2, 0.425f, 2),
      PDosageFileRowSummary("snp2", "G", "C", 1.2f, 0.79999995f, 1, 0.39999998f, 3),
      PDosageFileRowSummary("snp3", "A", "T", 2.1f, 1.9000001f, 2, 0.47500002f, 2),
      PDosageFileRowSummary("snp4", "G", "C", 1.4000001f, 2.6f, 2, 0.35000002f, 2)
    )) { readSNPOrder(outfile).toSet }
  }

  test("merge 2 with 6 snp") {

    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2 F3 I3
            |snp1 A T 1.0 1.3 2.0
            |snp2 G C 1.2 -8 0.1 
            |snp3 A T -8 -8 0.3
            |snp4 G C -8 2.0 1.2
            |snp5 G A 2.0 1.0 0.5
            |snp6 A G 1.1 2.0 -8
            |snp7 A T 1.0 2.0 1.0""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest22.h5")
    Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    val pdosage2 = Source.fromString("""SNP A1 A2 F4 I4 F5 I5 F6 I6
        |snp1 A T 1.8 0.3 -8
        |snp2 G C 0.2 1.2 -8
        |snp3 A T -8 -8 -8
        |snp4 G C -8 -8 -8
        |snp5 G A 1.0 2.0 1.0
        |snp6 A G 0.0 1.2 2.0""".stripMargin)
    val tmpfile2 = new File(getClass.getResource("/").getPath + "/hdftest221.h5")
    Import.convertPDosage(pdosage2, tmpfile2, -8f, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestmerged2320.h5")

    val report = mergeSNPMajors(IndexedSeq(FileSets.HDFDosage(tmpfile), FileSets.HDFDosage(tmpfile2)), outfile)

    expectResult(MergeDosageReport(Nil, Some(FileSets.HDFDosage(outfile)), true))(report)

    expectResult(Set(
      PDosageFileRowSummary("snp1", "A", "T", 6.4f, 3.6000001f, 5, 0.36f, 1),
      PDosageFileRowSummary("snp2", "G", "C", 2.7f, 5.2999997f, 4, 0.3375f, 2),
      PDosageFileRowSummary("snp3", "A", "T", 0.3f, 1.7f, 1, 0.15f, 5),
      PDosageFileRowSummary("snp4", "G", "C", 3.2f, 0.79999995f, 2, 0.19999999f, 4),
      PDosageFileRowSummary("snp5", "G", "A", 7.5f, 4.5f, 6, 0.375f, 0),
      PDosageFileRowSummary("snp6", "A", "G", 6.3f, 3.6999998f, 5, 0.37f, 1),
      PDosageFileRowSummary("snp7", "A", "T", 4.0f, 2.0f, 3, 0.3333333f, 3)
    )) { readSNPOrder(outfile).toSet }

    expectResult(Some(MissingValue)) { readDosage(outfile, "snp1", Individual("F6", "I6")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp2", Individual("F6", "I6")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp3", Individual("F6", "I6")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp4", Individual("F6", "I6")) }
    expectResult(Some(1.0f)) { readDosage(outfile, "snp5", Individual("F6", "I6")) }
    expectResult(Some(2.0f)) { readDosage(outfile, "snp6", Individual("F6", "I6")) }

    expectResult(Some(0.3f)) { readDosage(outfile, "snp1", Individual("F5", "I5")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp2", Individual("F5", "I5")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp3", Individual("F5", "I5")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp4", Individual("F5", "I5")) }
    expectResult(Some(2.0f)) { readDosage(outfile, "snp5", Individual("F5", "I5")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp6", Individual("F5", "I5")) }

    expectResult(Some(1.8f)) { readDosage(outfile, "snp1", Individual("F4", "I4")) }
    expectResult(Some(0.2f)) { readDosage(outfile, "snp2", Individual("F4", "I4")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp3", Individual("F4", "I4")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp4", Individual("F4", "I4")) }
    expectResult(Some(1.0f)) { readDosage(outfile, "snp5", Individual("F4", "I4")) }
    expectResult(Some(0.0f)) { readDosage(outfile, "snp6", Individual("F4", "I4")) }

    expectResult(Some(1.0f)) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp3", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp4", Individual("F1", "I1")) }
    expectResult(Some(2.0f)) { readDosage(outfile, "snp5", Individual("F1", "I1")) }
    expectResult(Some(1.1f)) { readDosage(outfile, "snp6", Individual("F1", "I1")) }

    expectResult(Some(1.3f)) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp3", Individual("F2", "I2")) }
    expectResult(Some(2.0f)) { readDosage(outfile, "snp4", Individual("F2", "I2")) }
    expectResult(Some(1.0f)) { readDosage(outfile, "snp5", Individual("F2", "I2")) }
    expectResult(Some(2.0f)) { readDosage(outfile, "snp6", Individual("F2", "I2")) }

    expectResult(Some(2.0f)) { readDosage(outfile, "snp1", Individual("F3", "I3")) }
    expectResult(Some(0.1f)) { readDosage(outfile, "snp2", Individual("F3", "I3")) }
    expectResult(Some(0.3f)) { readDosage(outfile, "snp3", Individual("F3", "I3")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp4", Individual("F3", "I3")) }
    expectResult(Some(0.5f)) { readDosage(outfile, "snp5", Individual("F3", "I3")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp6", Individual("F3", "I3")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"), Individual("F3", "I3"), Individual("F4", "I4"), Individual("F5", "I5"), Individual("F6", "I6"))) { readIndividualOrder(outfile).toList.sortBy(_.familyID) }

  }

  test("merge 2 with 6 snp + exclude snp + exclude individual") {

    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2 F3 I3
            |snp1 A T 1.0 1.3 2.0
            |snp2 G C 1.2 -8 0.1 
            |snp3 A T -8 -8 0.3
            |snp4 G C -8 2.0 1.2
            |snp5 G A 2.0 1.0 0.5
            |snp6 A G 1.1 2.0 -8
            |snp7 A T 1.0 2.0 1.0""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest22.h5")
    Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    val pdosage2 = Source.fromString("""SNP A1 A2 F4 I4 F5 I5 F6 I6
        |snp1 A T 1.8 0.3 -8
        |snp2 G C 0.2 1.2 -8
        |snp3 A T -8 -8 -8
        |snp4 G C -8 -8 -8
        |snp5 G A 1.0 2.0 1.0
        |snp6 A G 0.0 1.2 2.0""".stripMargin)
    val tmpfile2 = new File(getClass.getResource("/").getPath + "/hdftest221.h5")
    Import.convertPDosage(pdosage2, tmpfile2, -8f, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestmerged2320.h5")

    val report = mergeSNPMajors(IndexedSeq(FileSets.HDFDosage(tmpfile), FileSets.HDFDosage(tmpfile2)), outfile, excludeSNPs = Set("snp5"), excludeIndividuals = Set(Individual("F4", "I4")))

    expectResult(MergeDosageReport(Nil, Some(FileSets.HDFDosage(outfile)), true))(report)

    expectResult(Set(
      PDosageFileRowSummary("snp1", "A", "T", 4.6f, 3.4f, 4, 0.425f, 1),
      PDosageFileRowSummary("snp2", "G", "C", 2.5f, 3.5f, 3, 0.4166667f, 2),
      PDosageFileRowSummary("snp3", "A", "T", 0.3f, 1.7f, 1, 0.15f, 4),
      PDosageFileRowSummary("snp4", "G", "C", 3.2f, 0.79999995f, 2, 0.19999999f, 3),
      PDosageFileRowSummary("snp6", "A", "G", 6.3f, 1.6999999f, 4, 0.21249998f, 1),
      PDosageFileRowSummary("snp7", "A", "T", 4.0f, 2.0f, 3, 0.3333333f, 2)
    )) { readSNPOrder(outfile).toSet }

    expectResult(Some(MissingValue)) { readDosage(outfile, "snp1", Individual("F6", "I6")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp2", Individual("F6", "I6")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp3", Individual("F6", "I6")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp4", Individual("F6", "I6")) }
    expectResult(None) { readDosage(outfile, "snp5", Individual("F6", "I6")) }
    expectResult(Some(2.0f)) { readDosage(outfile, "snp6", Individual("F6", "I6")) }

    expectResult(Some(0.3f)) { readDosage(outfile, "snp1", Individual("F5", "I5")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp2", Individual("F5", "I5")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp3", Individual("F5", "I5")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp4", Individual("F5", "I5")) }
    expectResult(None) { readDosage(outfile, "snp5", Individual("F5", "I5")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp6", Individual("F5", "I5")) }

    expectResult(None) { readDosage(outfile, "snp1", Individual("F4", "I4")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F4", "I4")) }
    expectResult(None) { readDosage(outfile, "snp3", Individual("F4", "I4")) }
    expectResult(None) { readDosage(outfile, "snp4", Individual("F4", "I4")) }
    expectResult(None) { readDosage(outfile, "snp5", Individual("F4", "I4")) }
    expectResult(None) { readDosage(outfile, "snp6", Individual("F4", "I4")) }

    expectResult(Some(1.0f)) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp3", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp4", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp5", Individual("F1", "I1")) }
    expectResult(Some(1.1f)) { readDosage(outfile, "snp6", Individual("F1", "I1")) }

    expectResult(Some(1.3f)) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp3", Individual("F2", "I2")) }
    expectResult(Some(2.0f)) { readDosage(outfile, "snp4", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp5", Individual("F2", "I2")) }
    expectResult(Some(2.0f)) { readDosage(outfile, "snp6", Individual("F2", "I2")) }

    expectResult(Some(2.0f)) { readDosage(outfile, "snp1", Individual("F3", "I3")) }
    expectResult(Some(0.1f)) { readDosage(outfile, "snp2", Individual("F3", "I3")) }
    expectResult(Some(0.3f)) { readDosage(outfile, "snp3", Individual("F3", "I3")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp4", Individual("F3", "I3")) }
    expectResult(None) { readDosage(outfile, "snp5", Individual("F3", "I3")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp6", Individual("F3", "I3")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"), Individual("F3", "I3"), Individual("F5", "I5"), Individual("F6", "I6"))) { readIndividualOrder(outfile).toList.sortBy(_.familyID) }

  }

  test("merge 2 with 6 snp + exclude snp based on snpminimumcoverage") {

    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2 F3 I3
            |snp1 A T 1.0 1.3 2.0
            |snp2 G C 1.2 -8 0.1 
            |snp3 A T -8 -8 0.3
            |snp4 G C -8 2.0 1.2
            |snp5 G A 2.0 1.0 0.5
            |snp6 A G 1.1 2.0 -8
            |snp7 A T 1.0 2.0 1.0""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest22.h5")
    Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    val pdosage2 = Source.fromString("""SNP A1 A2 F4 I4 F5 I5 F6 I6
        |snp1 A T 1.8 0.3 -8
        |snp2 G C 0.2 1.2 -8
        |snp3 A T -8 -8 -8
        |snp4 G C -8 -8 -8
        |snp5 G A 1.0 2.0 1.0
        |snp6 A G 0.0 1.2 2.0""".stripMargin)
    val tmpfile2 = new File(getClass.getResource("/").getPath + "/hdftest221.h5")
    Import.convertPDosage(pdosage2, tmpfile2, -8f, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestmerged2320.h5")

    val report = mergeSNPMajors(IndexedSeq(FileSets.HDFDosage(tmpfile), FileSets.HDFDosage(tmpfile2)), outfile, minimumSNPCoverage = 1.0)

    expectResult(MergeDosageReport(Nil, Some(FileSets.HDFDosage(outfile)), true))(report)

    expectResult(Set(
      PDosageFileRowSummary("snp5", "G", "A", 7.5f, 4.5f, 6, 0.375f, 0)
    )) { readSNPOrder(outfile).toSet }

    expectResult(None) { readDosage(outfile, "snp1", Individual("F6", "I6")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F6", "I6")) }
    expectResult(None) { readDosage(outfile, "snp3", Individual("F6", "I6")) }
    expectResult(None) { readDosage(outfile, "snp4", Individual("F6", "I6")) }
    expectResult(Some(1.0f)) { readDosage(outfile, "snp5", Individual("F6", "I6")) }
    expectResult(None) { readDosage(outfile, "snp6", Individual("F6", "I6")) }

    expectResult(None) { readDosage(outfile, "snp1", Individual("F5", "I5")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F5", "I5")) }
    expectResult(None) { readDosage(outfile, "snp3", Individual("F5", "I5")) }
    expectResult(None) { readDosage(outfile, "snp4", Individual("F5", "I5")) }
    expectResult(Some(2.0f)) { readDosage(outfile, "snp5", Individual("F5", "I5")) }
    expectResult(None) { readDosage(outfile, "snp6", Individual("F5", "I5")) }

    expectResult(None) { readDosage(outfile, "snp1", Individual("F4", "I4")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F4", "I4")) }
    expectResult(None) { readDosage(outfile, "snp3", Individual("F4", "I4")) }
    expectResult(None) { readDosage(outfile, "snp4", Individual("F4", "I4")) }
    expectResult(Some(1.0f)) { readDosage(outfile, "snp5", Individual("F4", "I4")) }
    expectResult(None) { readDosage(outfile, "snp6", Individual("F4", "I4")) }

    expectResult(None) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp3", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp4", Individual("F1", "I1")) }
    expectResult(Some(2.0f)) { readDosage(outfile, "snp5", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp6", Individual("F1", "I1")) }

    expectResult(None) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp3", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp4", Individual("F2", "I2")) }
    expectResult(Some(1.0f)) { readDosage(outfile, "snp5", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp6", Individual("F2", "I2")) }

    expectResult(None) { readDosage(outfile, "snp1", Individual("F3", "I3")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F3", "I3")) }
    expectResult(None) { readDosage(outfile, "snp3", Individual("F3", "I3")) }
    expectResult(None) { readDosage(outfile, "snp4", Individual("F3", "I3")) }
    expectResult(Some(0.5f)) { readDosage(outfile, "snp5", Individual("F3", "I3")) }
    expectResult(None) { readDosage(outfile, "snp6", Individual("F3", "I3")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"), Individual("F3", "I3"), Individual("F4", "I4"), Individual("F5", "I5"), Individual("F6", "I6"))) { readIndividualOrder(outfile).toList.sortBy(_.familyID) }

  }

  test("merge 2 with 6 snp + include snp + include individual") {

    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2 F3 I3
            |snp1 A T 1.0 1.3 2.0
            |snp2 G C 1.2 -8 0.1 
            |snp3 A T -8 -8 0.3
            |snp4 G C -8 2.0 1.2
            |snp5 G A 2.0 1.0 0.5
            |snp6 A G 1.1 2.0 -8
            |snp7 A T 1.0 2.0 1.0""".stripMargin)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest22.h5")
    Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    val pdosage2 = Source.fromString("""SNP A1 A2 F4 I4 F5 I5 F6 I6
        |snp1 A T 1.8 0.3 -8
        |snp2 G C 0.2 1.2 -8
        |snp3 A T -8 -8 -8
        |snp4 G C -8 -8 -8
        |snp5 G A 1.0 2.0 1.0
        |snp6 A G 0.0 1.2 2.0""".stripMargin)
    val tmpfile2 = new File(getClass.getResource("/").getPath + "/hdftest221.h5")
    Import.convertPDosage(pdosage2, tmpfile2, -8f, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestmerged2320.h5")

    val report = mergeSNPMajors(IndexedSeq(FileSets.HDFDosage(tmpfile), FileSets.HDFDosage(tmpfile2)), outfile, includeSNPs = Set("snp5"), includeIndividuals = Set(Individual("F4", "I4")))

    expectResult(MergeDosageReport(Nil, Some(FileSets.HDFDosage(outfile)), true))(report)

    expectResult(Set(
      PDosageFileRowSummary("snp5", "G", "A", 1f, 1f, 1, 0.5f, 0)
    )) { readSNPOrder(outfile).toSet }

    expectResult(None) { readDosage(outfile, "snp1", Individual("F6", "I6")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F6", "I6")) }
    expectResult(None) { readDosage(outfile, "snp3", Individual("F6", "I6")) }
    expectResult(None) { readDosage(outfile, "snp4", Individual("F6", "I6")) }
    expectResult(None) { readDosage(outfile, "snp5", Individual("F6", "I6")) }
    expectResult(None) { readDosage(outfile, "snp6", Individual("F6", "I6")) }

    expectResult(None) { readDosage(outfile, "snp1", Individual("F5", "I5")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F5", "I5")) }
    expectResult(None) { readDosage(outfile, "snp3", Individual("F5", "I5")) }
    expectResult(None) { readDosage(outfile, "snp4", Individual("F5", "I5")) }
    expectResult(None) { readDosage(outfile, "snp5", Individual("F5", "I5")) }
    expectResult(None) { readDosage(outfile, "snp6", Individual("F5", "I5")) }

    expectResult(None) { readDosage(outfile, "snp1", Individual("F4", "I4")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F4", "I4")) }
    expectResult(None) { readDosage(outfile, "snp3", Individual("F4", "I4")) }
    expectResult(None) { readDosage(outfile, "snp4", Individual("F4", "I4")) }
    expectResult(Some(1f)) { readDosage(outfile, "snp5", Individual("F4", "I4")) }
    expectResult(None) { readDosage(outfile, "snp6", Individual("F4", "I4")) }

    expectResult(None) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp3", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp4", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp5", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp6", Individual("F1", "I1")) }

    expectResult(None) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp3", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp4", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp5", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp6", Individual("F2", "I2")) }

    expectResult(None) { readDosage(outfile, "snp1", Individual("F3", "I3")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F3", "I3")) }
    expectResult(None) { readDosage(outfile, "snp3", Individual("F3", "I3")) }
    expectResult(None) { readDosage(outfile, "snp4", Individual("F3", "I3")) }
    expectResult(None) { readDosage(outfile, "snp5", Individual("F3", "I3")) }
    expectResult(None) { readDosage(outfile, "snp6", Individual("F3", "I3")) }

    expectResult(List(Individual("F4", "I4"))) { readIndividualOrder(outfile).toList.sortBy(_.familyID) }

  }

  test("merge big") {

    val pdosage = Source.fromFile(getClass.getResource("/hdf/IMP.pdosage").getPath)
    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftestbig19.h5")
    Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    println("convert done.")

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestbig218.h5")

    val report = mergeSNPMajors(IndexedSeq(FileSets.HDFDosage(tmpfile)), outfile)

    val diff = exec(List("h5diff", "-d", "0.00001", outfile.getCanonicalPath, tmpfile.getCanonicalPath))(println)()

    expectResult(0)(diff)

  }

  test("merge big 2") {

    import _root_.ch.systemsx.cisd.hdf5._

    val test1 = Source.fromFile(getClass.getResource("/hdf/test1.short.pdose").getPath)
    val test1hdf = new File(getClass.getResource("/").getPath + "/hdftestbig317.h5")
    Import.convertPDosage(test1, test1hdf, MissingValue, false, blockSize = 1)

    val test2 = Source.fromFile(getClass.getResource("/hdf/test2.short.pdose").getPath)
    val test2hdf = new File(getClass.getResource("/").getPath + "/hdftestbig416.h5")
    Import.convertPDosage(test2, test2hdf, MissingValue, false, blockSize = 1)

    val merged = new File(getClass.getResource("/").getPath + "/hdftestbig5merged.h5")

    mergeSNPMajors(IndexedSeq(FileSets.HDFDosage(test1hdf), FileSets.HDFDosage(test2hdf)), merged, blockSize = 1)

    val individuals1 = readIndividualOrder(test1hdf).toArray
    val individuals2 = readIndividualOrder(test2hdf).toArray
    val snps1 = readSNPOrder(test1hdf).toArray
    val snps2 = readSNPOrder(test2hdf).toArray

    val readerMerged = new HDFDosageReader(merged, 500000)
    val readerTest1 = new HDFDosageReader(test1hdf, 500000)
    val readerTest2 = new HDFDosageReader(test2hdf, 500000)

    individuals1.foreach { ind =>
      snps1.foreach { snp =>
        val original = readerTest1.readDosage(snp.snpName, ind)
        val copied = readerMerged.readDosage(snp.snpName, ind)
        expectResult(true)(math.abs(copied.get - original.get) < 0.00001)
      }
    }

    individuals2.foreach { ind =>
      snps2.foreach { snp =>
        val original = readerTest2.readDosage(snp.snpName, ind)
        val copied = readerMerged.readDosage(snp.snpName, ind)
        expectResult(true)(math.abs(copied.get - original.get) < 0.00001)
      }
    }

  }

  test("merge big, 2 files with different set of snps") {

    import _root_.ch.systemsx.cisd.hdf5._

    val test1 = Source.fromFile(getClass.getResource("/hdf/test1.short.pdose").getPath)
    val test1hdf = new File(getClass.getResource("/").getPath + "/hdftestbig315.h5")
    Import.convertPDosage(test1, test1hdf, MissingValue, false, blockSize = 1)

    val test2 = Source.fromFile(getClass.getResource("/hdf/test2.short.snprenamed.pdose").getPath)
    val test2hdf = new File(getClass.getResource("/").getPath + "/hdftestbig414.h5")
    Import.convertPDosage(test2, test2hdf, MissingValue, false, blockSize = 1)

    val merged = new File(getClass.getResource("/").getPath + "/hdftestbig5merged13.h5")

    mergeSNPMajors(IndexedSeq(FileSets.HDFDosage(test1hdf), FileSets.HDFDosage(test2hdf)), merged, blockSize = 1)

    val individuals1 = readIndividualOrder(test1hdf).toArray
    val individuals2 = readIndividualOrder(test2hdf).toArray
    val snps1 = readSNPOrder(test1hdf).toArray
    val snps2 = readSNPOrder(test2hdf).toArray

    val readerMerged = new HDFDosageReader(merged, 5000)
    val readerTest1 = new HDFDosageReader(test1hdf, 5000)
    val readerTest2 = new HDFDosageReader(test2hdf, 5000)

    individuals1.foreach { ind =>
      snps1.foreach { snp =>
        val original = readerTest1.readDosage(snp.snpName, ind)
        val copied = readerMerged.readDosage(snp.snpName, ind)
        expectResult(true)(math.abs(copied.get - original.get) < 0.00001)
      }
    }

    individuals2.foreach { ind =>
      snps2.foreach { snp =>
        val original = readerTest2.readDosage(snp.snpName, ind)
        val copied = readerMerged.readDosage(snp.snpName, ind)
        expectResult(true)(math.abs(copied.get - original.get) < 0.00001)
      }
    }

  }

  test("subset no filter") {

    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp2 G C 1.2 -8""".stripMargin)

    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest.h5")

    Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestsubsetted12.h5")

    HDFDosageFile.mergeSNPMajors(
      FileSets.HDFDosage(tmpfile) :: Nil,
      outfile,
      0.0,
      Set[String8](),
      Set[String8](),
      Set[Individual](),
      Set[Individual]()
    )

    expectResult(Some(1.0f)) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(1.3f)) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(Some(MissingValue)) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(outfile, "snp132", Individual("F1", "I1")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"))) { readIndividualOrder(outfile).toList }

    expectResult(List(
      PDosageFileRowSummary("snp1", "A", "T", 2.3f, 1.7f, 2, 0.425f, 0),
      PDosageFileRowSummary("snp2", "G", "C", 1.2f, 0.79999995f, 1, 0.39999998f, 1)
    )) { readSNPOrder(outfile).toList }

  }

  test("subset exclude ind") {

    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp2 G C 1.2 -8""".stripMargin)

    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest.h5")

    Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestsubsetted11.h5")

    HDFDosageFile.mergeSNPMajors(
      FileSets.HDFDosage(tmpfile) :: Nil,
      outfile,
      0.0,
      includeSNPs = Set[String8](),
      excludeSNPs = Set[String8](),
      includeIndividuals = Set[Individual](),
      excludeIndividuals = Set[Individual](Individual("F1", "I1"))
    )

    expectResult(None) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(1.3f)) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(outfile, "snp132", Individual("F1", "I1")) }

    expectResult(List(Individual("F2", "I2"))) { readIndividualOrder(outfile).toList }

    expectResult(List(
      PDosageFileRowSummary("snp1", "A", "T", 1.3f, 0.70000005f, 1, 0.35000002f, 0)
    ).toString) { readSNPOrder(outfile).toList.toString }

  }

  test("subset include ind") {
    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp2 G C 1.2 -8""".stripMargin)

    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest.h5")

    Import.convertPDosage(pdosage, tmpfile, -8f, false)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestsubsetted9.h5")

    HDFDosageFile.mergeSNPMajors(
      FileSets.HDFDosage(tmpfile) :: Nil,
      outfile,
      0.0,
      includeSNPs = Set[String8](),
      excludeSNPs = Set[String8](),
      includeIndividuals = Set[Individual](Individual("F2", "I2")),
      excludeIndividuals = Set[Individual]()
    )

    expectResult(None) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(1.3f)) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(outfile, "snp132", Individual("F1", "I1")) }

    expectResult(List(Individual("F2", "I2"))) { readIndividualOrder(outfile).toList }

    expectResult(List(
      PDosageFileRowSummary("snp1", "A", "T", 1.3f, 0.70000005f, 1, 0.35000002f, 0)
    ).toString) { readSNPOrder(outfile).toList.toString }
  }

  test("subset include and exclude") {
    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp2 G C 1.2 -8""".stripMargin)

    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest.h5")

    Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestsubsetted8.h5")

    HDFDosageFile.mergeSNPMajors(
      FileSets.HDFDosage(tmpfile) :: Nil,
      outfile,
      0.0,
      includeSNPs = Set[String8](),
      excludeSNPs = Set[String8](),
      includeIndividuals = Set[Individual](Individual("F2", "I2")),
      excludeIndividuals = Set[Individual](Individual("F2", "I2"))
    )

    expectResult(None) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(outfile, "snp132", Individual("F1", "I1")) }

    expectResult(List()) { readIndividualOrder(outfile).toList }

    expectResult(List()) { readSNPOrder(outfile).toList }
  }

  test("subset exclude snp") {
    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp2 G C 1.2 -8""".stripMargin)

    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest.h5")

    Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestsubsetted7.h5")

    HDFDosageFile.mergeSNPMajors(
      FileSets.HDFDosage(tmpfile) :: Nil,
      outfile,
      0.0,
      includeSNPs = Set[String8](),
      excludeSNPs = Set[String8]("snp2"),
      includeIndividuals = Set[Individual](),
      excludeIndividuals = Set[Individual]()
    )

    expectResult(Some(1.0f)) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(1.3f)) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(outfile, "snp132", Individual("F1", "I1")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"))) { readIndividualOrder(outfile).toList }

    expectResult(List(
      PDosageFileRowSummary("snp1", "A", "T", 2.3f, 1.7f, 2, 0.425f, 0)
    )) { readSNPOrder(outfile).toList }
  }

  test("subset include snp") {

    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp2 G C 1.2 -8""".stripMargin)

    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest.h5")

    Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestsubsetted6.h5")

    HDFDosageFile.mergeSNPMajors(
      FileSets.HDFDosage(tmpfile) :: Nil,
      outfile,
      0.0,
      includeSNPs = Set[String8]("snp1"),
      excludeSNPs = Set[String8](),
      includeIndividuals = Set[Individual](),
      excludeIndividuals = Set[Individual]()
    )

    expectResult(Some(1.0f)) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(1.3f)) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(outfile, "snp132", Individual("F1", "I1")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"))) { readIndividualOrder(outfile).toList }

    expectResult(List(
      PDosageFileRowSummary("snp1", "A", "T", 2.3f, 1.7f, 2, 0.425f, 0)
    )) { readSNPOrder(outfile).toList }

  }

  test("subset include exclude snp") {
    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp2 G C 1.2 -8
            |""".stripMargin)

    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest.h5")

    Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestsubsetted5.h5")

    HDFDosageFile.mergeSNPMajors(
      FileSets.HDFDosage(tmpfile) :: Nil,
      outfile,
      0.0,
      includeSNPs = Set[String8]("snp1"),
      excludeSNPs = Set[String8]("snp1"),
      includeIndividuals = Set[Individual](),
      excludeIndividuals = Set[Individual]()
    )

    expectResult(None) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(outfile, "snp132", Individual("F1", "I1")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"))) { readIndividualOrder(outfile).toList }

    expectResult(Nil) { readSNPOrder(outfile).toList }
  }

  test("subset snp by coverage with all individuals") {
    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp2 G C 1.2 -8
            |""".stripMargin)

    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest.h5")

    Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestsubsetted4.h5")

    HDFDosageFile.mergeSNPMajors(
      FileSets.HDFDosage(tmpfile) :: Nil,
      outfile,
      0.6,
      includeSNPs = Set[String8](),
      excludeSNPs = Set[String8](),
      includeIndividuals = Set[Individual](),
      excludeIndividuals = Set[Individual]()
    )

    expectResult(Some(1.0f)) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(1.3f)) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(outfile, "snp132", Individual("F1", "I1")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"))) { readIndividualOrder(outfile).toList }

    expectResult(List(
      PDosageFileRowSummary("snp1", "A", "T", 2.3f, 1.7f, 2, 0.425f, 0)
    )) { readSNPOrder(outfile).toList }
  }

  test("subset snp by MAF with all individuals") {
    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 2.0 2.0
            |snp2 G C 1.2 1.0
            |""".stripMargin)

    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest.h5")

    Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestsubsetted4.h5")

    HDFDosageFile.mergeSNPMajors(
      FileSets.HDFDosage(tmpfile) :: Nil,
      outfile,
      0.0,
      includeSNPs = Set[String8](),
      excludeSNPs = Set[String8](),
      includeIndividuals = Set[Individual](),
      excludeIndividuals = Set[Individual](),
      minimumMAF = 0.05
    )

    expectResult(None) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(Some(1.2f)) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(Some(1.0f)) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(outfile, "snp132", Individual("F1", "I1")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"))) { readIndividualOrder(outfile).toList }

    expectResult(List(
      PDosageFileRowSummary("snp2", "G", "C", 2.2f, 1.8f, 2, 0.45f, 0)
    )) { readSNPOrder(outfile).toList }
  }

  test("subset snp by coverage with individual exclude fitler") {
    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2 F3 I3
            |snp1 A T 1.0 1.3 2.0
            |snp2 G C 1.2 -8 1.2
            |""".stripMargin)

    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest.h5")

    Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestsubsetted3.h5")

    HDFDosageFile.mergeSNPMajors(
      FileSets.HDFDosage(tmpfile) :: Nil,
      outfile,
      0.55,
      includeSNPs = Set[String8]("snp1"),
      excludeSNPs = Set[String8](),
      includeIndividuals = Set[Individual](),
      excludeIndividuals = Set[Individual](Individual("F3", "I3"))
    )

    expectResult(Some(1.0f)) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    expectResult(Some(1.3f)) { readDosage(outfile, "snp1", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I1")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F2", "I2")) }
    expectResult(None) { readDosage(outfile, "snp2", Individual("F1", "I3")) }
    expectResult(None) { readDosage(outfile, "snp132", Individual("F1", "I1")) }

    expectResult(None) { readDosage(outfile, "snp2", Individual("F3", "I3")) }
    expectResult(None) { readDosage(outfile, "snp1", Individual("F3", "I3")) }

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"))) { readIndividualOrder(outfile).toList }

    expectResult(List(
      PDosageFileRowSummary("snp1", "A", "T", 2.3f, 1.7f, 2, 0.425f, 0)
    )) { readSNPOrder(outfile).toList }
  }

  test("subset: no filter, output in pdose") {
    val str = """SNP A1 A2 F1 I1 F2 I2 F3 I3
            |snp1 A T 1 1.3 2
            |snp2 G C 1.2 -9 1.2
            |""".stripMargin
    val pdosage = Source.fromString(str)

    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest8596.h5")

    Import.convertPDosage(pdosage, tmpfile, -9f, false, blockSize = 1)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestsubsetted2.h5")

    HDFDosageFile.mergeSNPMajors(
      FileSets.HDFDosage(tmpfile) :: Nil,
      outfile,
      0.0,
      includeSNPs = Set[String8](),
      excludeSNPs = Set[String8](),
      includeIndividuals = Set[Individual](),
      excludeIndividuals = Set[Individual](),
      bufferSize = 1000,
      outputFormat = hdfdosage.PDose,
      minimumMAF = 0.0,
      genomicMap = Map(),
      blockSize = 2,
      sortByCHR = true
    )

    expectResult(mybiotools.openSource(outfile)(_.mkString))(str)

  }

  ignore("1x20M conver") {
    mybiotools.openSource(getClass.getResource("/hdf/long.pdose.gz").getPath) { pdosage =>
      val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest3234234.h5")
      val rep1 = Import.convertPDosage(pdosage, tmpfile, MissingValue, false, blockSize = 1000)

      expectResult(ConvertPDosageReport(Set(), Set(), true))(rep1)

      expectResult(Some(1.0f)) { readDosage(tmpfile, "snp20000000", Individual("F1", "I1")) }
    }

  }

  ignore("1x20M merge") {
    val in = new java.io.File(getClass.getResource("/hdf/long.h5").getPath)

    val outfile = new File(getClass.getResource("/").getPath + "/hdftestmerged3sddddfdsf3.h5")

    val report = mergeSNPMajors(IndexedSeq(FileSets.HDFDosage(in)), outfile)

    // expectResult(Some(1.0f)) { readDosage(outfile, "snp1", Individual("F1", "I1")) }
    // expectResult(Some(1.0f)) { readDosage(outfile, "snp20000000", Individual("F1", "I1")) }

    // expectResult(List(Individual("F1", "I1"))) { readIndividualOrder(outfile).toList }

    expectResult(20000000) { readSNPOrder(outfile).size }
  }

}