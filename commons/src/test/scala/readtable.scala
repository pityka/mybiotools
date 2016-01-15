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

import mybiotools._

class ReadTableTestSuite extends FunSuite {

  test("Reading simple table as flat 0") {
    val tab = readTable(Source.fromURL(getClass.getResource("/testtable.txt")), sep = " ", header = false)
    val exp = List(
      Map('V1 -> "1", 'V2 -> "abcd", 'V3 -> "jkl;"),
      Map('V1 -> "2", 'V2 -> "qwer", 'V3 -> "poiu"),
      Map('V1 -> "3", 'V2 -> "1234", 'V3 -> "mnb")
    )
    assertResult(exp) { tab }
  }

  test("Reading simple table as flat") {
    val tab = readTable(Source.fromURL(getClass.getResource("/testtable_header.txt")), sep = ",")
    val exp = List(
      Map('header1 -> "1", 'header2 -> "abcd", 'header3 -> "jkl;"),
      Map('header1 -> "2", 'header2 -> "qwer", 'header3 -> "poiu"),
      Map('header1 -> "3", 'header2 -> "1234", 'header3 -> "mnb")
    )
    assertResult(exp) { tab }
  }

  test("Reading simple table as map") {
    val tab = readTableAsMap[String](Source.fromURL(getClass.getResource("/testtable_header.txt")), sep = ",", key = 'header1)
    val exp = Map(
      "1" -> Map('header1 -> "1", 'header2 -> "abcd", 'header3 -> "jkl;"),
      "2" -> Map('header1 -> "2", 'header2 -> "qwer", 'header3 -> "poiu"),
      "3" -> Map('header1 -> "3", 'header2 -> "1234", 'header3 -> "mnb")
    )
    assertResult(exp) { tab }
  }

  test("Reading simple table as flat with whitespace") {
    val tab = readTable(Source.fromURL(getClass.getResource("/testtable_space.txt")), sep = " ")
    val exp = List(
      Map('header1 -> "1", 'header2 -> "abcd", 'header3 -> "jkl;"),
      Map('header1 -> "2", 'header2 -> "qwer", 'header3 -> "poiu"),
      Map('header1 -> "3", 'header2 -> "1234", 'header3 -> "mnb")
    )
    assertResult(exp) { tab }
  }

  test("Reading simple table as map with whitespace") {
    val tab = readTableAsMap[String](Source.fromURL(getClass.getResource("/testtable_space.txt")), sep = " ", key = 'header1)
    val exp = Map(
      "1" -> Map('header1 -> "1", 'header2 -> "abcd", 'header3 -> "jkl;"),
      "2" -> Map('header1 -> "2", 'header2 -> "qwer", 'header3 -> "poiu"),
      "3" -> Map('header1 -> "3", 'header2 -> "1234", 'header3 -> "mnb")
    )
    assertResult(exp) { tab }
  }

  test("Reading not complete rectangular table as flat.") {
    intercept[RuntimeException] {
      val tab = readTable(Source.fromURL(getClass.getResource("/testtable_space.txt")), sep = "d")
    }
  }

  test("Reading not complete rectangular table as map.") {
    intercept[RuntimeException] {
      val tab = readTableAsMap[String](Source.fromURL(getClass.getResource("/testtable_space.txt")), sep = "d", key = 'header1)
    }
  }

  test("Reading not complete rectangular table. Another") {
    intercept[RuntimeException] {
      val tab = readTable(Source.fromURL(getClass.getResource("/testtable_space_wrong.txt")), sep = " ")
    }
  }

  test("Reading leaky table as flat.") {
    val tab = readTable(Source.fromURL(getClass.getResource("/testtable_header_leaky.txt")), sep = ",")
    val exp = List(
      Map('header1 -> "1", 'header2 -> "", 'header3 -> ""),
      Map('header1 -> "2", 'header2 -> "", 'header3 -> "poiu"),
      Map('header1 -> "3", 'header2 -> "1234", 'header3 -> "mnb")
    )
    assertResult(exp) { tab }
  }

  test("Reading leaky table as map.") {
    val tab = readTableAsMap[String](Source.fromURL(getClass.getResource("/testtable_header_leaky.txt")), sep = ",", key = 'header1)
    val exp = Map(
      "1" -> Map('header1 -> "1", 'header2 -> "", 'header3 -> ""),
      "2" -> Map('header1 -> "2", 'header2 -> "", 'header3 -> "poiu"),
      "3" -> Map('header1 -> "3", 'header2 -> "1234", 'header3 -> "mnb")
    )
    assertResult(exp) { tab }
  }

  test("Reading simple table as flat where with WhereKeyIn") {
    val tab = readTable(Source.fromURL(getClass.getResource("/testtable_header.txt")), sep = ",", key = Some('header1), whereKeyIn = Some(List("1", "2")))
    val exp = List(
      Map('header1 -> "1", 'header2 -> "abcd", 'header3 -> "jkl;"),
      Map('header1 -> "2", 'header2 -> "qwer", 'header3 -> "poiu")
    )
    assertResult(exp) { tab }
  }

  test("Reading leaky table as flat where with WhereKeyIn") {
    val tab = readTable(Source.fromURL(getClass.getResource("/testtable_header.txt")), sep = ",", key = Some('header2), whereKeyIn = Some(List("1234")))
    val exp = List(
      Map('header1 -> "3", 'header2 -> "1234", 'header3 -> "mnb")
    )
    assertResult(exp) { tab }
  }

  test("readTokenizer") {
    import java.io.StringReader
    assertResult(IndexedSeq("1", "2", "3")) { tokenizeReader(new StringReader("1,2,3"), ',') }
    assertResult(IndexedSeq("1", "2", "3")) { tokenizeReader(new StringReader("1,2,,,,3"), ',') }
    assertResult(IndexedSeq("1", "2", "3")) { tokenizeReader(new StringReader(",,,1,,2,,,3,,,"), ',') }
    assertResult(IndexedSeq("1", "2", "3")) { tokenizeReader(new StringReader(",1,2,3"), ',') }
    assertResult(IndexedSeq("1", "2", "3")) { tokenizeReader(new StringReader(",,,,,,1,,,,,2,,,,,3,,,,,,"), ',') }
    assertResult(IndexedSeq("1", "2", "3")) { tokenizeReader(new StringReader("1,,,,,,,,,,,2,,,,,,,,,,,3"), ',') }
    assertResult(IndexedSeq()) { tokenizeReader(new StringReader(""), ',') }
    assertResult(IndexedSeq()) { tokenizeReader(new StringReader(",,,,,"), ',') }
    assertResult(IndexedSeq()) { tokenizeReader(new StringReader(","), ',') }
    assertResult(IndexedSeq("1")) { tokenizeReader(new StringReader(",1"), ',') }
    assertResult(IndexedSeq("1")) { tokenizeReader(new StringReader("1,"), ',') }
    assertResult(IndexedSeq("  1")) { tokenizeReader(new StringReader("  1,,,,,,"), ',') }
    assertResult(IndexedSeq("1  ")) { tokenizeReader(new StringReader("1  ,,,,,"), ',') }
    assertResult(IndexedSeq("1")) { tokenizeReader(new StringReader(",,1,,,,"), ',') }
  }
}