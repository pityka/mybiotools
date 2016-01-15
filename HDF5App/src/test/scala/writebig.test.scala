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
import BIGFile._
import java.io.ByteArrayOutputStream
import java.io.File
import mybiotools.readBinaryFile
import mybiotools.gwascommons.Individual
import mybiotools.gwascommons.genotypedata.PDosageFileRowSummary

class BIGTestSuite extends FunSuite {

  test("0x0") {

    val numberOfIndividuals = 0
    val dosageArrays = List[Tuple2[PDosageFileRowSummary, Array[Float]]]()
    val tmpfile = File.createTempFile("bio", ".big")
    val tmpfile2 = File.createTempFile("bio", ".fam")

    writeBIG(tmpfile, tmpfile2, new java.io.StringWriter(), List(), dosageArrays.iterator, Map())

    val result = readBinaryFile(tmpfile.getCanonicalPath)

    println(result.deep)

    val rgBuf: Array[Byte] = result.slice(0, 32)
    val data: Array[Byte] = result.slice(32, result.length)

    expectResult(rgBuf.deep)(Array[Byte](
      98, 100, 48, 49,
      48, 48, 26, 1,
      0, 0, 0, 0, // num snp
      0, 0, 0, 0, // num ind
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0
    ).deep)

    expectResult(data.deep)(Array[Byte]().deep)
  }

  test("2x2") {

    val numberOfIndividuals = 2
    val dosageArrays = List[Tuple2[PDosageFileRowSummary, Array[Float]]](
      (PDosageFileRowSummary("snp1", "a", "t", Array(1f, 2f), -9f), Array(1f, 2f)),
      (PDosageFileRowSummary("snp2", "a", "t", Array(0f, 1.2f), -9f), Array(0f, 1.2f))
    )
    val tmpfile = File.createTempFile("bio", ".big")
    val tmpfile2 = File.createTempFile("bio", ".fam")

    writeBIG(tmpfile, tmpfile2, new java.io.StringWriter(), List(Individual("F1", "I1"), Individual("F2", "I2")), dosageArrays.iterator, Map())

    val result = readBinaryFile(tmpfile.getCanonicalPath)

    println(result.deep)

    val rgBuf: Array[Byte] = result.slice(0, 32)
    val data: Array[Byte] = result.slice(32, result.length)

    expectResult(rgBuf.deep)(Array[Byte](98, 100, 48, 49,
      48, 48, 26, 1,
      2, 0, 0, 0, // num snp
      2, 0, 0, 0, // num ind
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0).deep)

    expectResult(data.deep)({
      val bs = new ByteArrayOutputStream()
      val stream = new java.io.DataOutputStream(bs)
      stream.writeFloat(BIGFile.convertFloatToLittleEndian(1f))
      stream.writeFloat(BIGFile.convertFloatToLittleEndian(2f))
      stream.writeFloat(BIGFile.convertFloatToLittleEndian(0.0f))
      stream.writeFloat(BIGFile.convertFloatToLittleEndian(1.2f))
      bs.toByteArray
    }.deep)
  }

  // test("2x2 individual major") {

  //   val numberOfIndividuals = 2
  //   val dosageArrays = List[Tuple2[PDosageFileRowSummary, Array[Float]]](
  //     (PDosageFileRowSummary("snp1", "a", "t", Array(1f, 2f), -9f), Array(1f, 2f)),
  //     (PDosageFileRowSummary("snp2", "a", "t", Array(0f, 1.2f), -9f), Array(0f, 1.2f)))
  //   val tmpfile = File.createTempFile("bio", ".big")
  //   val tmpfile2 = File.createTempFile("bio", ".indlist")

  //   writeBIGSNPMajor(
  //     tmpfile,
  //     tmpfile2,
  //      new java.io.StringWriter(),
  //       Vector(Individual("F1","I1"),Individual("F2","I2")),
  //        dosageArrays.iterator)

  //   val result = readBinaryFile(tmpfile.getCanonicalPath)

  //   println(result.deep)

  //   val rgBuf: Array[Byte] = result.slice(0, 32)
  //   val data: Array[Byte] = result.slice(32, result.length)

  //   expectResult(rgBuf.deep)(Array[Byte](98, 100, 48, 49,
  //     48, 48, 26, 1,
  //     0, 0, 0, 2, // num snp
  //     0, 0, 0, 2, // num ind
  //     0, 0, 0, 0,
  //     0, 0, 0, 0,
  //     0, 0, 0, 0,
  //     0, 0, 0, 0
  //   ).deep
  //   )

  //   expectResult(data.deep)({
  //     val bs = new ByteArrayOutputStream()
  //     val stream = new java.io.DataOutputStream(bs)
  //     stream.writeFloat(1f)
  //     stream.writeFloat(2f)
  //     stream.writeFloat(0.0f)
  //     stream.writeFloat(1.2f)
  //     bs.toByteArray
  //   }.deep)
  // }

}