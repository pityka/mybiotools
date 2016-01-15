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

class ContingencyTableTestSuite extends FunSuite {

  test("easy") {
    val x1 = Seq(Some(true), Some(true), Some(true), Some(false), Some(false)).zipWithIndex.map(_.swap).toMap
    val x2 = Seq(Some(true), Some(true), Some(true), Some(false), Some(false)).zipWithIndex.map(_.swap).toMap
    val c = ContingencyTable2x2(3, 0, 0, 2)
    assertResult(c) { contingencyTable(x1, x2) }
  }

  test("kereszt") {
    val x1 = Seq(Some(true), Some(true), Some(true), Some(false), Some(false)).zipWithIndex.map(_.swap).toMap
    val x2 = Seq(Some(false), Some(false), Some(false), Some(true), Some(true)).zipWithIndex.map(_.swap).toMap
    val c = ContingencyTable2x2(0, 2, 3, 0)
    assertResult(c) { contingencyTable(x1, x2) }
  }

  test("Unknown") {
    val x1 = Seq(Some(true), Some(true), Some(true), Some(false), None).zipWithIndex.map(_.swap).toMap
    val x2 = Seq(Some(false), Some(false), Some(false), Some(true), Some(true), Some(true)).zipWithIndex.map(_.swap).toMap
    val c = ContingencyTable2x2(0, 1, 3, 0)
    assertResult(c) { contingencyTable(x1, x2) }
  }

  test("Mixed") {
    val x1 = Seq(Some(false), Some(true), Some(true), Some(true), Some(false), None).zipWithIndex.map(_.swap).toMap
    val x2 = Seq(Some(false), Some(true), Some(false), Some(false), Some(true), Some(true), Some(true)).zipWithIndex.map(_.swap).toMap
    val c = ContingencyTable2x2(1, 1, 2, 1)
    assertResult(c) { contingencyTable(x1, x2) }
  }

}