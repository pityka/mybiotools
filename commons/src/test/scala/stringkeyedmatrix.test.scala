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
import mybiotools.stringkeyedmatrix._
import mybiotools.stringstore._

class StringKeyedMatrixTestSuite extends FunSuite {

  implicit def string2string8(s: String): String8 = StringStore(s)

  test("trivial, fromUnsortedIterable") {
    val m = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", 8),
      ("1", "2", 9),
      ("1", "3", 5),
      ("2", "1", 9),
      ("2", "2", 8),
      ("2", "3", 6),
      ("3", "1", 10),
      ("3", "2", 16),
      ("3", "3", 12)
    ))
    assertResult(m("1", "1")) { 8 }
    assertResult(m("1", "2")) { 9 }
    assertResult(m("1", "3")) { 5 }
    assertResult(m("2", "1")) { 9 }
    assertResult(m("2", "2")) { 8 }
    assertResult(m("2", "3")) { 6 }
    assertResult(m("3", "1")) { 10 }
    assertResult(m("3", "2")) { 16 }
    assertResult(m("3", "3")) { 12 }
    assertResult(m.keys1) { Seq[String8]("1", "2", "3") }
    assertResult(m.keys2) { Seq[String8]("1", "2", "3") }
  }

  test("filterDim2") {
    val m: StringKeyedMatrix[Int, IndexedSeq[Int]] = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", 8),
      ("1", "2", 9),
      ("1", "3", 5),
      ("2", "1", 9),
      ("2", "2", 8),
      ("2", "3", 6),
      ("3", "1", 10),
      ("3", "2", 16),
      ("3", "3", 12)
    ))
    assertResult(m("1", "1")) { 8 }
    assertResult(m("1", "2")) { 9 }
    assertResult(m("1", "3")) { 5 }
    assertResult(m("2", "1")) { 9 }
    assertResult(m("2", "2")) { 8 }
    assertResult(m("2", "3")) { 6 }
    assertResult(m("3", "1")) { 10 }
    assertResult(m("3", "2")) { 16 }
    assertResult(m("3", "3")) { 12 }
    assertResult(m.keys1) { Seq[String8]("1", "2", "3") }
    assertResult(m.keys2) { Seq[String8]("1", "2", "3") }

    val n = m filterDim2 (_ == "1")
    assertResult(n("1", "1")) { 8 }
    assertResult(n("2", "1")) { 9 }

    assertResult(n("3", "1")) { 10 }

    assertResult(n.keys1) { Seq[String8]("1", "2", "3") }
    assertResult(n.keys2) { Seq[String8]("1") }
    intercept[java.util.NoSuchElementException](n("3", "2"))
    intercept[java.util.NoSuchElementException](n("3", "3"))
    intercept[java.util.NoSuchElementException](n("2", "3"))
    intercept[java.util.NoSuchElementException](n("2", "2"))
    intercept[java.util.NoSuchElementException](n("1", "2"))
    intercept[java.util.NoSuchElementException](n("1", "3"))

  }

  test("filterDim1") {
    val m: StringKeyedMatrix[Int, IndexedSeq[Int]] = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", 8),
      ("1", "2", 9),
      ("1", "3", 5),
      ("2", "1", 9),
      ("2", "2", 8),
      ("2", "3", 6),
      ("3", "1", 10),
      ("3", "2", 16),
      ("3", "3", 12)
    ))
    assertResult(m("1", "1")) { 8 }
    assertResult(m("1", "2")) { 9 }
    assertResult(m("1", "3")) { 5 }
    assertResult(m("2", "1")) { 9 }
    assertResult(m("2", "2")) { 8 }
    assertResult(m("2", "3")) { 6 }
    assertResult(m("3", "1")) { 10 }
    assertResult(m("3", "2")) { 16 }
    assertResult(m("3", "3")) { 12 }
    assertResult(m.keys1) { Seq[String8]("1", "2", "3") }
    assertResult(m.keys2) { Seq[String8]("1", "2", "3") }

    val n = m filterDim1 (_ == "1")
    assertResult(n("1", "1")) { 8 }
    assertResult(n("1", "2")) { 9 }
    assertResult(n.keys1) { Seq[String8]("1") }
    assertResult(n.keys2) { Seq[String8]("1", "2", "3") }
    intercept[java.util.NoSuchElementException](n("3", "2"))
    intercept[java.util.NoSuchElementException](n("3", "3"))
    intercept[java.util.NoSuchElementException](n("2", "3"))
    intercept[java.util.NoSuchElementException](n("2", "2"))
    intercept[java.util.NoSuchElementException](n("2", "1"))
    intercept[java.util.NoSuchElementException](n("3", "1"))

  }

  test("trivial, fromSortedIterator") {
    val m = StringKeyedMatrix.fromSortedIterator(List[(String8, String8, Int)](
      ("1", "1", 8),
      ("1", "2", 9),
      ("1", "3", 5),
      ("2", "1", 9),
      ("2", "2", 8),
      ("2", "3", 6),
      ("3", "1", 10),
      ("3", "2", 16),
      ("3", "3", 12)
    ).iterator, 3)
    assertResult(m("1", "1")) { 8 }
    assertResult(m("1", "2")) { 9 }
    assertResult(m("1", "3")) { 5 }
    assertResult(m("2", "1")) { 9 }
    assertResult(m("2", "2")) { 8 }
    assertResult(m("2", "3")) { 6 }
    assertResult(m("3", "1")) { 10 }
    assertResult(m("3", "2")) { 16 }
    assertResult(m("3", "3")) { 12 }
    assertResult(m.keys1) { Seq[String8]("1", "2", "3") }
    assertResult(m.keys2) { Seq[String8]("1", "2", "3") }
  }

  test("trivial, fromSortedIteratorWithKeySeqs") {

    val m = StringKeyedMatrix.fromSortedIteratorWithKeySeqs(List(
      8,
      9,
      5,
      9,
      8,
      6,
      10,
      16,
      12
    ).iterator, List[String8]("1", "2", "3"), List[String8]("4", "5", "6"))
    assertResult(m("1", "4")) { 8 }
    assertResult(m("1", "5")) { 9 }
    assertResult(m("1", "6")) { 5 }
    assertResult(m("2", "4")) { 9 }
    assertResult(m("2", "5")) { 8 }
    assertResult(m("2", "6")) { 6 }
    assertResult(m("3", "4")) { 10 }
    assertResult(m("3", "5")) { 16 }
    assertResult(m("3", "6")) { 12 }
    assertResult(m.keys1) { Seq[String8]("1", "2", "3") }
    assertResult(m.keys2) { Seq[String8]("4", "5", "6") }
  }

  test("trivial, fromMapofTuples") {
    val m = StringKeyedMatrix.fromMapofTuples(Map[(String8, String8), Int](
      (StringStore("1"), StringStore("1")) -> 8,
      (StringStore("1"), StringStore("2")) -> 9,
      (StringStore("1"), StringStore("3")) -> 5,
      (StringStore("2"), StringStore("1")) -> 9,
      (StringStore("2"), StringStore("2")) -> 8,
      (StringStore("2"), StringStore("3")) -> 6,
      (StringStore("3"), StringStore("1")) -> 10,
      (StringStore("3"), StringStore("2")) -> 16,
      (StringStore("3"), StringStore("3")) -> 12
    ))
    assertResult(m("1", "1")) { 8 }
    assertResult(m("1", "2")) { 9 }
    assertResult(m("1", "3")) { 5 }
    assertResult(m("2", "1")) { 9 }
    assertResult(m("2", "2")) { 8 }
    assertResult(m("2", "3")) { 6 }
    assertResult(m("3", "1")) { 10 }
    assertResult(m("3", "2")) { 16 }
    assertResult(m("3", "3")) { 12 }
    assertResult(m.keys1) { Seq[String8]("1", "2", "3") }
    assertResult(m.keys2) { Seq[String8]("1", "2", "3") }
  }

  test("trivial, fromMapOfMaps") {
    val m = StringKeyedMatrix.fromMapofMaps(Map[String8, Map[String8, Int]](
      StringStore("1") -> Map[String8, Int](StringStore("1") -> 8, StringStore("2") -> 9, StringStore("3") -> 5),
      StringStore("2") -> Map[String8, Int](StringStore("1") -> 9, StringStore("2") -> 8, StringStore("3") -> 6),
      StringStore("3") -> Map[String8, Int](StringStore("1") -> 10, StringStore("2") -> 16, StringStore("3") -> 12)
    ))
    assertResult(m("1", "1")) { 8 }
    assertResult(m("1", "2")) { 9 }
    assertResult(m("1", "3")) { 5 }
    assertResult(m("2", "1")) { 9 }
    assertResult(m("2", "2")) { 8 }
    assertResult(m("2", "3")) { 6 }
    assertResult(m("3", "1")) { 10 }
    assertResult(m("3", "2")) { 16 }
    assertResult(m("3", "3")) { 12 }
    assertResult(m.keys1) { Seq[String8]("1", "2", "3") }
    assertResult(m.keys2) { Seq[String8]("1", "2", "3") }
  }

  test("Equality") {
    val m = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", 8),
      ("1", "2", 9),
      ("1", "3", 5),
      ("2", "1", 9),
      ("2", "2", 8),
      ("2", "3", 6),
      ("3", "1", 10),
      ("3", "2", 16),
      ("3", "3", 12)
    ))
    val m2 = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", 8),
      ("3", "2", 16),
      ("1", "2", 9),
      ("1", "3", 5),
      ("2", "2", 8),
      ("2", "3", 6),
      ("3", "1", 10),
      ("2", "1", 9),
      ("3", "3", 12)
    ))
    val m3 = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", 81),
      ("1", "2", 91),
      ("1", "3", 52),
      ("2", "1", 91),
      ("2", "2", 8),
      ("2", "3", 6),
      ("3", "1", 102),
      ("3", "2", 161),
      ("3", "3", 12)
    ))

    assertResult(true) { m2 == m }
    assertResult(false) { m == m3 }
    assertResult(false) { m == "sfdsadf" }
  }
  //   

  // 

  //   
  //   
  //   
  test("mapvalues") {
    val m = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", 8),
      ("1", "2", 9),
      ("1", "3", 5),
      ("2", "1", 9),
      ("2", "2", 8),
      ("2", "3", 6),
      ("3", "1", 10),
      ("3", "2", 16),
      ("3", "3", 12)
    ))
    val m3 = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", -8),
      ("1", "2", -9),
      ("1", "3", -5),
      ("2", "1", -9),
      ("2", "2", -8),
      ("2", "3", -6),
      ("3", "1", -10),
      ("3", "2", -16),
      ("3", "3", -12)
    ))
    val m2 = m.mapValues { x => -1 * x }
    assertResult(false) { m eq m2 }
    assertResult { false } { m == m2 }
    assertResult(m3) { m2 }
    assertResult(-9) { m3("1", "2") }
    assertResult(9)(m("1", "2"))
  }

  test("merge non overlap") {
    val m = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", 8),
      ("1", "2", 9),
      ("2", "1", 9),
      ("2", "2", 8)
    ))

    val m2 = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("11", "11", 8),
      ("11", "12", 9),
      ("12", "11", 9),
      ("12", "12", 8)
    ))

    val m3 = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", 8),
      ("1", "2", 9),
      ("2", "1", 9),
      ("2", "2", 8),
      ("11", "11", 8),
      ("11", "12", 9),
      ("12", "11", 9),
      ("12", "12", 8),
      ("11", "1", 0),
      ("11", "2", 0),
      ("12", "1", 0),
      ("12", "2", 0),
      ("1", "11", 0),
      ("2", "12", 0),
      ("1", "12", 0),
      ("2", "11", 0)
    ))
    assertResult(m3) { m.merge(m2, 0)((x1, x2) => 100) }

  }

  test("merge widen") {
    val m = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", 11),
      ("1", "2", 12),
      ("2", "1", 21),
      ("2", "2", 22)
    ))

    val m2 = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "3", 13),
      ("2", "3", 23)
    ))

    val m3 = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", 11),
      ("1", "2", 12),
      ("2", "1", 21),
      ("2", "2", 22),
      ("1", "3", 13),
      ("2", "3", 23)
    ))
    assertResult(m3) { m.merge(m2, 0)((x1, x2) => 100) }
  }

  test("merge heighten") {
    val m = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", 11),
      ("1", "2", 12),
      ("2", "1", 21),
      ("2", "2", 22)
    ))

    val m2 = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("3", "1", 31),
      ("3", "2", 32)
    ))

    val m3 = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", 11),
      ("1", "2", 12),
      ("2", "1", 21),
      ("2", "2", 22),
      ("3", "1", 31),
      ("3", "2", 32)
    ))
    assertResult(m3) { m.merge(m2, 0)((x1, x2) => 100) }
  }

  test("merge complete overlap 1") {
    val m = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", 11),
      ("1", "2", 12),
      ("2", "1", 21),
      ("2", "2", 22)
    ))

    val m2 = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", 11),
      ("1", "2", 12),
      ("2", "1", 21),
      ("2", "2", 22)
    ))

    val m3 = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", 100),
      ("1", "2", 100),
      ("2", "1", 100),
      ("2", "2", 100)
    ))
    assertResult(m3) { m.merge(m2, 0)((x1, x2) => 100) }
  }

  test("merge complete overlap 2") {
    val m = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", 10),
      ("1", "2", 10),
      ("2", "1", 10),
      ("2", "2", 10)
    ))

    val m2 = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", 20),
      ("1", "2", 20),
      ("2", "1", 20),
      ("2", "2", 20)
    ))

    val m3 = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", 10),
      ("1", "2", 10),
      ("2", "1", 10),
      ("2", "2", 10)
    ))
    assertResult(m3) { m.merge(m2, 0)((x1, x2) => x1) }
  }

  test("merge complete overlap 3") {
    val m = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", 10),
      ("1", "2", 10),
      ("2", "1", 10),
      ("2", "2", 10)
    ))

    val m2 = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", 20),
      ("1", "2", 20),
      ("2", "1", 20),
      ("2", "2", 20)
    ))

    val m3 = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", 20),
      ("1", "2", 20),
      ("2", "1", 20),
      ("2", "2", 20)
    ))
    assertResult(m3) { m.merge(m2, 0)((x1, x2) => x2) }
  }

  test("merge partial overlap") {
    val m = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", 10),
      ("1", "2", 10),
      ("2", "1", 10),
      ("2", "2", 10)
    ))

    val m2 = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("3", "1", 20),
      ("3", "2", 20),
      ("2", "1", 20),
      ("2", "2", 20),
      ("2", "3", 20),
      ("3", "3", 20)
    ))

    val m3 = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Int)](
      ("1", "1", 10),
      ("1", "2", 10),
      ("2", "1", 100),
      ("2", "2", 100),
      ("3", "1", 20),
      ("3", "2", 20),
      ("1", "3", 0),
      ("2", "3", 20),
      ("3", "3", 20)
    ))
    assertResult(m3) { m.merge(m2, 0)((x1, x2) => 100) }
  }

  // test( "huge" ) {
  //   // 1000*1000
  //   val n = 10000
  //   // val elems = Array.fill(n*n)(RNA.S1)
  //   
  //   val d  = Array.fill(n)(RNA(Seq.fill(n)(Base(BaseValue.S1)):_*))
  //   
  //   val keys1 = for ( i <- 0 to n - 1 ) yield i.toString
  //   val keys2 = for ( i <- 0 to n - 1 ) yield i.toString
  //   println( "start" )
  // 
  //   val m = StringKeyedMatrix.fromArray( d, keys1, keys2 )
  //   assertResult( n )( m.dimension1 )
  //   assertResult( 0 )( m( "0", "0" ) )
  //   // val a = Array.fill(100000000)(1.toByte)
  //   // var b : Byte = 0
  //   // a.foreach(x => (b += x).z)
  //   // println(b)
  // }

  test("Bit2 matrix, with normal storage") {
    import mybiotools.bit2seq._
    import mybiotools.bit2seq.Bit2State._
    val m = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Value)](
      ("1", "1", S1),
      ("1", "2", S2),
      ("2", "1", S3),
      ("2", "2", S4)
    ))
    assertResult(m("1", "1")) { S1 }
    assertResult(m("1", "2")) { S2 }
    assertResult(m("2", "1")) { S3 }
    assertResult(m("2", "2")) { S4 }
  }

  test("Bit2 matrix, with bitset storage") {
    import mybiotools.bit2seq._
    import mybiotools.bit2seq.Bit2State._
    val m = StringKeyedMatrix.fromSortedIteratorOfBit2(List[(String8, String8, Value)](
      ("1", "1", S1),
      ("1", "2", S2),
      ("2", "1", S3),
      ("2", "2", S4)
    ).iterator, 2)
    assertResult(m("1", "1")) { S1 }
    assertResult(m("1", "2")) { S2 }
    assertResult(m("2", "1")) { S3 }
    assertResult(m("2", "2")) { S4 }
    assertResult(Seq[String8]("1", "2")) { m.keys1 }
    assertResult(Seq[String8]("1", "2")) { m.keys2 }
    assertResult("StringKeyedMatrix[mybiotools.bit2seq.Bit2Buffer](2,ArrayBuffer(1, 2),ArrayBuffer(1, 2))")(m.toString)
  }

  test("Bit2 matrix,  mapValues") {
    import mybiotools.bit2seq._
    import mybiotools.bit2seq.Bit2State._
    val m = StringKeyedMatrix.fromSortedIteratorOfBit2(List[(String8, String8, Value)](
      ("1", "1", S1),
      ("1", "2", S2),
      ("2", "1", S3),
      ("2", "2", S4)
    ).iterator, 2)
    assertResult(m("1", "1")) { S1 }
    assertResult(m("1", "2")) { S2 }
    assertResult(m("2", "1")) { S3 }
    assertResult(m("2", "2")) { S4 }
    assertResult(Seq[String8]("1", "2")) { m.keys1 }
    assertResult(Seq[String8]("1", "2")) { m.keys2 }
    val m2 = StringKeyedMatrix.fromSortedIteratorOfBit2(List[(String8, String8, Value)](
      ("1", "1", S4),
      ("1", "2", S4),
      ("2", "1", S4),
      ("2", "2", S4)
    ).iterator, 2)
    assertResult(m2) { m.mapValues(x => S4) }
  }

  test("Bit2 matrix,  mapValues 2") {
    import mybiotools.bit2seq._
    import mybiotools.bit2seq.Bit2State._
    import Bit2Buffer._
    val m = StringKeyedMatrix.fromSortedIterator(List[(String8, String8, Int)](
      ("1", "1", 1),
      ("1", "2", 2),
      ("2", "1", 3),
      ("2", "2", 4)
    ).iterator, 2)
    assertResult(m("1", "1")) { 1 }
    assertResult(m("1", "2")) { 2 }
    assertResult(m("2", "1")) { 3 }
    assertResult(m("2", "2")) { 4 }
    assertResult(Seq[String8]("1", "2")) { m.keys1 }
    assertResult(Seq[String8]("1", "2")) { m.keys2 }
    val m2 = StringKeyedMatrix.fromSortedIteratorOfBit2(List[(String8, String8, Value)](
      ("1", "1", S4),
      ("1", "2", S4),
      ("2", "1", S4),
      ("2", "2", S4)
    ).iterator, 2)
    val m3 = m.mapValues[Bit2, Bit2Buffer](x => S4)
    assertResult(m2) { m3 }
    assertResult(m2.toString) { m3.toString }
  }

  test("Bit2 matrix, with bitset ,merge") {
    import mybiotools.bit2seq._
    import mybiotools.bit2seq.Bit2State._
    val m = StringKeyedMatrix.fromSortedIteratorOfBit2(List[(String8, String8, Value)](
      ("1", "1", S1),
      ("1", "2", S2),
      ("2", "1", S3),
      ("2", "2", S4)
    ).iterator, 2)
    assertResult(m("1", "1")) { S1 }
    assertResult(m("1", "2")) { S2 }
    assertResult(m("2", "1")) { S3 }
    assertResult(m("2", "2")) { S4 }
    assertResult(Seq[String8]("1", "2")) { m.keys1 }
    assertResult(Seq[String8]("1", "2")) { m.keys2 }
    val m2 = m.merge(m, S1)((x1, x2) => x1)
    val m3 = StringKeyedMatrix.fromUnsortedIterable(List[(String8, String8, Value)](
      ("1", "1", S1),
      ("1", "2", S2),
      ("2", "1", S3),
      ("2", "2", S4)
    ))
    assertResult(m("1", "1")) { S1 }
    assertResult(m("1", "2")) { S2 }
    assertResult(m("2", "1")) { S3 }
    assertResult(m("2", "2")) { S4 }
    val m4 = m.merge(m3, S1)((x1, x2) => x1)
    // println( m2 )
    // println( m3 )
    // println( m4 )
  }

  // test("serialization to json") {
  //   val a = StringKeyedMatrix.fromMapofMaps(Map(
  //     StringStore("1") -> Map(StringStore("A") -> 1, StringStore("B") -> 2, StringStore("C") -> 3),
  //     StringStore("2") -> Map(StringStore("A") -> 4, StringStore("B") -> 5, StringStore("C") -> 6),
  //     StringStore("3") -> Map(StringStore("A") -> 1, StringStore("B") -> 2, StringStore("C") -> 3)
  //   ))
  //   import sjson.json._
  //   import DefaultProtocol._
  //   import JsonSerialization._
  //   import dispatch.json._
  //   import Protocols._

  //   val ser = mybiotools.Serialization.jsonSerialize(a)
  //   // println(ser.toString)
  //   val deser = mybiotools.Serialization.jsonDeserialize[StringKeyedMatrix[Int, IndexedSeq[Int]]](ser)
  //   assertResult(a)(deser)

  // }

  // test("serialization to bson") {
  //   val a = StringKeyedMatrix.fromMapofMaps(Map(
  //     StringStore("1") -> Map(StringStore("A") -> (1, 2), StringStore("B") -> (1, 2), StringStore("C") -> (1, 2)),
  //     StringStore("2") -> Map(StringStore("A") -> (1, 2), StringStore("B") -> (1, 2), StringStore("C") -> (1, 2)),
  //     StringStore("3") -> Map(StringStore("A") -> (1, 2), StringStore("B") -> (1, 2), StringStore("C") -> (1, 2))
  //   ))
  //   import sjson.json._
  //   import DefaultProtocol._
  //   import JsonSerialization._
  //   import dispatch.json._
  //   import Protocols._

  //   val ser = mybiotools.Serialization.bsonSerialize(a, 2048)
  //   val deser = mybiotools.Serialization.bsonDeserialize[StringKeyedMatrix[(Int, Int), IndexedSeq[(Int, Int)]]](ser)
  //   // println(ser)

  //   assertResult(a)(deser)

  // }
  // val iter = new Iterator[Tuple3[String,String,Bit2]] { var i = 0; var j = 0;  val rnd = util.Random; val n = 1000; val m = 1000;  def hasNext = i*j < n*m; def next = { if (j == m ) { j = 0; i += 1} else {j +=1};  (i.toString,j.toString,Bit2State.fromInt(rnd.nextInt( 4 ))) } }

}