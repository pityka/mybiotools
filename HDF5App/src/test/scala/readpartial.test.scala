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
import _root_.ch.systemsx.cisd.hdf5._
import mybiotools.gwascommons.Individual
import mybiotools.gwascommons.genotypedata.PDosageFileRowSummary

class HDFDosageReadPartialInfoTestSuite extends FunSuite {

  implicit def string2string8(s: String) = new mybiotools.stringstore.String8(s)

  test("4x2") {

    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp2 G C 1.2 0.0
            |snp3 G C 1.2 2.0
            |snp4 A T 1.3 1.0""".stripMargin)

    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftestreadpartial.h5")
    val report = Import.convertPDosage(pdosage, tmpfile, -8f, false)

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"))) { readIndividualOrder(tmpfile).toList }

    expectResult(List(
      PDosageFileRowSummary("snp1", "A", "T", 2.3f, 1.7f, 2, 0.425f, 0),
      PDosageFileRowSummary("snp2", "G", "C", 1.2f, 2.8f, 2, 0.3f, 0),
      PDosageFileRowSummary("snp3", "G", "C", 3.2f, 0.79999995f, 2, 0.19999999f, 0),
      PDosageFileRowSummary("snp4", "A", "T", 2.3f, 1.7f, 2, 0.425f, 0)
    )) { readSNPOrder(tmpfile).toList }

    val reader = HDF5Factory.openForReading(tmpfile)
    val idx = readSNPNamesOrder(reader).zipWithIndex.toMap
    val result = readSNPInfoPartial(reader, Set("snp2", "snp4"), idx, getSNPCompoundType(reader))

    val result2 = readSNPInfoPartial(reader, Set("snp2"), idx, getSNPCompoundType(reader))

    expectResult(List(PDosageFileRowSummary("snp2", "G", "C", 1.2f, 2.8f, 2, 0.3f, 0), PDosageFileRowSummary("snp4", "A", "T", 2.3f, 1.7f, 2, 0.425f, 0)))(result.toList)

    expectResult(List(PDosageFileRowSummary("snp2", "G", "C", 1.2f, 2.8f, 2, 0.3f, 0)))(result2.toList)
  }

  test("4x2 dosagereader") {

    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 1.0 1.3
            |snp2 G C 1.2 0.0
            |snp3 G C 1.2 2.0
            |snp4 A T 1.3 1.0""".stripMargin)

    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftestreadpartial.h5")
    val report = Import.convertPDosage(pdosage, tmpfile, -8f, false)

    expectResult(List(Individual("F1", "I1"), Individual("F2", "I2"))) { readIndividualOrder(tmpfile).toList }

    expectResult(List(
      PDosageFileRowSummary("snp1", "A", "T", 2.3f, 1.7f, 2, 0.425f, 0),
      PDosageFileRowSummary("snp2", "G", "C", 1.2f, 2.8f, 2, 0.3f, 0),
      PDosageFileRowSummary("snp3", "G", "C", 3.2f, 0.79999995f, 2, 0.19999999f, 0),
      PDosageFileRowSummary("snp4", "A", "T", 2.3f, 1.7f, 2, 0.425f, 0)
    )) { readSNPOrder(tmpfile).toList }

    val reader = new HDFDosageReader(tmpfile, 10)(x => List[String8]("snp1", "snp3").contains(x))

    intercept[UnsupportedOperationException] {
      reader.getSNP("snp2")
    }
    expectResult(reader.getSNP("snp1").toList)(List(1.0f, 1.3f))

    val reader1 = new HDFDosageReader(tmpfile, 10)(x => List[String8]("snp2", "snp3", "snp4").contains(x))
    expectResult(reader1.getSNP("snp2").toList)(List(1.2f, 0.0f))
    expectResult(reader1.getSNP("snp3").toList)(List(1.2f, 2.0f))
    expectResult(reader1.getSNP("snp4").toList)(List(1.3f, 1.0f))
  }

}