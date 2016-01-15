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
import hdfdosage._
import TPedFile._
import java.io.ByteArrayOutputStream
import java.io.File
import mybiotools.readBinaryFile
import mybiotools.gwascommons.Individual
import mybiotools.gwascommons.genotypedata.PDosageFileRowSummary

class TPedTestSuite extends FunSuite {

  test("0x0") {

    val numberOfIndividuals = 0
    val dosageArrays = List[Tuple2[PDosageFileRowSummary, Array[Float]]]()
    val tmpfile = File.createTempFile("bio", ".big")
    val tmpfile2 = File.createTempFile("bio", ".fam")

    writeTPED(tmpfile, tmpfile2, new java.io.StringWriter(), List(), dosageArrays.iterator, Map(), (x => true), 0.1, '0')

    val result = io.Source.fromFile(tmpfile.getCanonicalPath).mkString

    expectResult(result)("")
  }

  test("2x2") {

    val numberOfIndividuals = 2
    val dosageArrays = List[Tuple2[PDosageFileRowSummary, Array[Float]]](
      (PDosageFileRowSummary("snp1", "a", "t", Array(1f, 2f), -9f), Array(1f, 2f)),
      (PDosageFileRowSummary("snp2", "a", "t", Array(0f, 1.2f), -9f), Array(0f, 1.2f))
    )
    val tmpfile = File.createTempFile("bio", ".big")
    val tmpfile2 = File.createTempFile("bio", ".fam")

    writeTPED(tmpfile, tmpfile2, new java.io.StringWriter(), List(Individual("F1", "I1"), Individual("F2", "I2")), dosageArrays.iterator, Map(), (x => true), 0.5, '0')

    val result = io.Source.fromFile(tmpfile.getCanonicalPath).mkString

    expectResult(""". snp1 0 . t a a a
|. snp2 0 . t t a t
|""".stripMargin)(result)

  }

}