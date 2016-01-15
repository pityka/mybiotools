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

import mybiotools._

class CommonsTestSuite extends FunSuite {

  test("fdr") {
    val p = List(0.01, 0.032, 0.07, 0.07, 0.2, 0.38, 0.48, 0.49, 0.6, 0.68, 0.74, 0.97).map(_.toFloat)
    assertResult(0.032f) { highestSignificantPValueByFDR(0.2f, p, p.size) }
  }

  test("zipBy 1 ") {
    val l1 = List("aaa", "baaa", "cddd", "dddd")
    val l2 = List("a111", "b111", "c111", "d111")
    val zipped = zipBy(l1, l2)(x => x(0))
    assertResult(List("aaa" -> "a111", "baaa" -> "b111", "cddd" -> "c111", "dddd" -> "d111"))(zipped)
  }

  test("zipBy 2 ") {
    val l1 = List("aaa", "baaa", "cddd")
    val l2 = List("a111", "b111", "c111", "d111")
    val zipped = zipBy(l1, l2)(x => x(0))
    assertResult(List("aaa" -> "a111", "baaa" -> "b111", "cddd" -> "c111"))(zipped)
  }

  test("zipBy 3 ") {
    val l1 = List("aaa", "baaa", "cddd", "dddd")
    val l2 = List("b111", "c111", "d111")
    val zipped = zipBy(l1, l2)(x => x(0))
    assertResult(List("b111" -> "baaa", "c111" -> "cddd", "d111" -> "dddd"))(zipped)
  }

  test("zipBy 4 ") {
    val l1 = List("aaa", "baaa", "cddd", "dddd").reverse
    val l2 = List("b111", "c111", "d111")
    val zipped = zipBy(l1, l2)(x => x(0))
    assertResult(List("b111" -> "baaa", "c111" -> "cddd", "d111" -> "dddd"))(zipped)
  }

  test("fileisempty") {
    val fr = new File(getClass.getResource("raxml.tree").getPath)

    assertResult(false)(fileIsEmpty(fr))

    val fr2 = File.createTempFile("bio", "")

    new java.io.FileOutputStream(fr2).close()
    assertResult(true)(fileIsEmpty(fr2))
    fr2.delete
  }

}