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

import mybiotools._
import mybiotools.mymatrix._

class MyMatrixTestSuite extends FunSuite {

  test("trivial") {
    val m = MyMatrix(Array(Array(8, 9, 5), Array(9, 8, 6), Array(10, 16, 12)), Seq(1, 2, 3), false)
    assertResult(m(1, 1)) { 8 }
    assertResult(m(1, 2)) { 9 }
    assertResult(m(1, 3)) { 5 }
    assertResult(m(2, 1)) { 9 }
    assertResult(m(2, 2)) { 8 }
    assertResult(m(2, 3)) { 6 }
    assertResult(m(3, 1)) { 10 }
    assertResult(m(3, 2)) { 16 }
    assertResult(m(3, 3)) { 12 }
    assertResult(m.keys) { Seq(1, 2, 3) }
  }

  test("named") {
    val m = MyMatrix(Array(Array(8, 9, 5), Array(9, 8, 6), Array(10, 16, 12)), Seq(6, 4, 8), false)
    assertResult(m(6, 6)) { 8 }
    assertResult(m(6, 4)) { 9 }
    assertResult(m(6, 8)) { 5 }
    assertResult(m(4, 6)) { 9 }
    assertResult(m(4, 4)) { 8 }
    assertResult(m(4, 8)) { 6 }
    assertResult(m(8, 6)) { 10 }
    assertResult(m(8, 4)) { 16 }
    assertResult(m(8, 8)) { 12 }
    assertResult(m.keys) { Seq(6, 4, 8) }
  }

  test("Equality") {
    val m = MyMatrix(Array(Array(8, 9, 5), Array(9, 8, 6), Array(5, 6, 2)), Seq(1, 2, 3), false)
    val m2 = MyMatrix(Array(Array(8, 9, 5), Array(9, 8, 6), Array(5, 6, 2)), Seq(1, 2, 3), false)
    val m3 = MyMatrix(Array(Array(1, 9, 5), Array(9, 8, 6), Array(5, 6, 2)), Seq(1, 2, 3), false)

    assertResult(true) { m2 == m }
    assertResult(false) { m == m3 }
  }

  test("Symmetric") {
    val m = MyMatrix(Array(Array(1, 0, 0), Array(2, 3, 0), Array(4, 5, 6)), Seq(1, 2, 3), true)
    val m2 = MyMatrix(Array(Array(1, 2, 4), Array(2, 3, 5), Array(4, 5, 6)), Seq(1, 2, 3), false)

    assertResult(true) { m == m2 }
  }

  test("pairwise matrix") {
    val alignment = FastaSequenceData("cucc3" -> "atgc", "cucc2" -> "atgca", "cucc4" -> "a")
    val m = computePairwiseMatrix[SequenceKey, Int, String](alignment) { (s1: String, s2: String) => s1.size + s2.size }
    val expected = MyMatrix(Array(Array(8, 9, 5), Array(9, 10, 6), Array(5, 6, 2)), Seq("cucc3", "cucc2", "cucc4"), false)
    assertResult(expected) { m }
  }

  test("mapvalues") {
    val m = MyMatrix(Array(Array(8, 9, 5), Array(9, 8, 6), Array(5, 6, 2)), Seq(1, 2, 3), false)
    val m3 = MyMatrix(Array(Array(-8, -9, -5), Array(-9, -8, -6), Array(-5, -6, -2)), Seq(1, 2, 3), false)
    val m2 = m.mapValues { x => -1 * x }
    assertResult(false) { m eq m2 }
    assertResult { false } { m == m2 }
    assertResult(m3) { m2 }
    assertResult(-9) { m3(1, 2) }
  }

  test("mapvaluesInPlace") {
    val m = MyMatrix(Array(Array(8, 9, 5), Array(9, 8, 6), Array(5, 6, 2)), Seq(1, 2, 3), false)
    val m3 = MyMatrix(Array(Array(-8, -9, -5), Array(-9, -8, -6), Array(-5, -6, -2)), Seq(1, 2, 3), false)
    m.mapValuesInPlace { x => -1 * x }
    assertResult(m) { m3 }
  }

}