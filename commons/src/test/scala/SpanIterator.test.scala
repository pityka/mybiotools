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

class SpanIteratorTestSuite extends FunSuite {

  test("empty") {
    val res = new SpanIterator(List[Boolean]().iterator, (x: Boolean) => x).toList
    assertResult(Nil)(res)
  }

  test("monochrome") {
    val res = new SpanIterator(List[Boolean](true, true, true, true).iterator, (x: Boolean) => x).toList
    assertResult(List(Seq(true, true, true, true)))(res)
  }

  test("monochrome2") {
    val res = new SpanIterator(List[Boolean](false, false, false, false).iterator, (x: Boolean) => x).toList
    assertResult(List(Seq(false, false, false, false)))(res)
  }

  test("1 elem") {
    val res = new SpanIterator(List[Boolean](true).iterator, (x: Boolean) => x).toList
    assertResult(List(Seq(true)))(res)
  }

  test("1 elem 2") {
    val res = new SpanIterator(List[Boolean](false).iterator, (x: Boolean) => x).toList
    assertResult(List(Seq(false)))(res)
  }

  test("2 elem") {
    val res = new SpanIterator(List[Boolean](true, true).iterator, (x: Boolean) => x).toList
    assertResult(List(Seq(true, true)))(res)
  }

  test("2 elem 2") {
    val res = new SpanIterator(List[Boolean](true, false).iterator, (x: Boolean) => x).toList
    assertResult(List(Seq(true), Seq(false)))(res)
  }

  test("2 elem 3") {
    val res = new SpanIterator(List[Boolean](false, true).iterator, (x: Boolean) => x).toList
    assertResult(List(Seq(false), Seq(true)))(res)
  }

  test("2 elem 4") {
    val res = new SpanIterator(List[Boolean](false, false).iterator, (x: Boolean) => x).toList
    assertResult(List(Seq(false, false)))(res)
  }

  test("3 elem") {
    val res = new SpanIterator(List[Boolean](false, false, true).iterator, (x: Boolean) => x).toList
    assertResult(List(Seq(false, false), Seq(true)))(res)
  }

  test("3 elem 2") {
    val res = new SpanIterator(List[Boolean](true, false, true).iterator, (x: Boolean) => x).toList
    assertResult(List(Seq(true), Seq(false), Seq(true)))(res)
  }

  test("3 elem 3") {
    val res = new SpanIterator(List[Boolean](true, false, false).iterator, (x: Boolean) => x).toList
    assertResult(List(Seq(true), Seq(false, false)))(res)
  }

}