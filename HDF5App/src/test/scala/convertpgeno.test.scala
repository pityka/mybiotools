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

class ConvertPGenotypeProbTestSuite extends FunSuite {
  implicit def string2string8(s: String) = new mybiotools.stringstore.String8(s)

  test("2x2") {

    val pdosage = Source.fromString("""SNP A1 A2 F1 I1 F2 I2
            |snp1 A T 0.0 1.0 0.5 0.3
            |snp2 G C 0.4 0.4 -8 -8""".stripMargin)

    val tmpfile = new File(getClass.getResource("/").getPath + "/hdftest2126.h5")
    val report = Import.convertPDosage(pdosage, tmpfile, -8f, false, blockSize = 1, inputFileFormat = PGenotypeProbabilities)

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
}