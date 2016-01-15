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

package mybiotools
package tabular

import org.scalatest.FunSpec
import org.scalatest.Matchers

class TabSpec extends FunSpec with Matchers {
  import mybiotools.tabular._
  import mybiotools.tabular.TabSerializationPrivate._
  import mybiotools.tabular.RProtocol._
  import mybiotools.Gender._

  describe("Serialization of R primitives") {
    it("should serialize a Gender.Male as \"Male\" ") {
      fromCell[Gender](toCell[Gender](Male)) should equal(Male)
      toCell[Gender](Male) should equal(Cell("Male"))
    }
    it("should serialize a Gender.Male as \"Female\" ") {
      fromCell[Gender](toCell[Gender](Male)) should equal(Male)
      toCell[Gender](Male) should equal(Cell("Male"))
    }
    it("should serialize an None to NA") {
      fromCell[Option[Int]](toCell[Option[Int]](None)) should equal(None)
      toCell[Option[Int]](None) should equal(Cell("NA"))
    }
    it("should serialize a Some(1) to 1") {
      fromCell[Option[Int]](toCell[Option[Int]](Some(1))) should equal(Some(1))
      toCell[Option[Int]](Some(1)) should equal(Cell("1"))
    }
    it("should understand yes,T,True,true,1 as true") {
      fromCell[Boolean](toCell[Boolean](true)) should equal(true)
      toCell[Boolean](true) should equal(Cell("true"))
      fromCell[Boolean](Cell("true")) should equal(true)
      fromCell[Boolean](Cell("T")) should equal(true)
      fromCell[Boolean](Cell("True")) should equal(true)
      fromCell[Boolean](Cell("1")) should equal(true)
      fromCell[Boolean](Cell("yes")) should equal(true)
    }
    it("should undsertand no,F,False,false,0 as false") {
      fromCell[Boolean](toCell[Boolean](false)) should equal(false)
      toCell[Boolean](false) should equal(Cell("false"))
      fromCell[Boolean](Cell("false")) should equal(false)
      fromCell[Boolean](Cell("F")) should equal(false)
      fromCell[Boolean](Cell("False")) should equal(false)
      fromCell[Boolean](Cell("0")) should equal(false)
      fromCell[Boolean](Cell("no")) should equal(false)
    }
  }

  describe("serialization of rows") {
    it("should serialize a Seq[Int] to a Row") {
      toRow[Seq[Int]](List(1, 2, 3), None) should equal(Row(Vector(Cell("1"), Cell("2"), Cell("3"))))
      fromRow[Seq[Int]](toRow[Seq[Int]](List(1, 2, 3), None), None) should equal(Seq(1, 2, 3))
    }
  }

  describe("serialization of tables") {
    it("should serialize a Seq[Seq[Int]] to a Table without header") {
      toTable[Seq[Seq[Int]]](List(List(1, 2), List(3, 4))) should equal(Table(Vector(Row(Vector(Cell("1"), Cell("2"))), Row(Vector(Cell("3"), Cell("4")))), None))
      fromTable[Seq[Seq[Int]]](Table(Vector(Row(Vector(Cell("1"), Cell("2"))), Row(Vector(Cell("3"), Cell("4")))), None)) should equal(Seq(Seq(1, 2), Seq(3, 4)))
      fromTable[Seq[Seq[Int]]](toTable[Seq[Seq[Int]]](List(List(1, 2), List(3, 4)))) should equal(Seq(Seq(1, 2), Seq(3, 4)))
    }
    it("should serialize a Tuple2[Seq[Seq[Int]],Seq[String]] to a Table with header") {
      toTable[Tuple2[Seq[Seq[Int]], Seq[String]]]((List(List(1, 2), List(3, 4)), List("A", "B"))) should equal(Table(Vector(Row(Vector(Cell("1"), Cell("2"))), Row(Vector(Cell("3"), Cell("4")))), Some(Vector("A", "B"))))
      fromTable[Tuple2[Seq[Seq[Int]], Seq[String]]](toTable[Tuple2[Seq[Seq[Int]], Seq[String]]]((List(List(1, 2), List(3, 4)), List("A", "B")))) should equal((List(List(1, 2), List(3, 4)), List("A", "B")))
    }
    it("should serialize a Tuple2[Map[Int,Seq[Int]],Option[IndexedSeq[String]]]") {
      implicit val helper = TabSerialization.selectKeyFromHeader[Int]("B")
      fromTable[Tuple2[Map[Int, Seq[Int]], IndexedSeq[String]]](toTable[Tuple2[Map[Int, Seq[Int]], IndexedSeq[String]]]((Map(2 -> Vector(1, 2), 4 -> Vector(3, 4)), Vector("A", "B")))) should equal((Map(2 -> Vector(1, 2), 4 -> Vector(3, 4)), Vector("A", "B")))
    }
  }
  describe("tuple1 serialization") {
    type T1 = Tuple1[Option[Double]]
    it("should serialize a Product1 into a Row, by column index") {
      val p = Tuple1(Some(1.0))
      fromRow[T1](toRow[T1](p, None), None) should equal(p)
      toRow[T1](p, None) should equal(Row(Vector(Cell("1.0"))))
    }

    it("should serialize a Seq[Product1] into a Table") {
      val p: Tuple1[Option[Double]] = Tuple1(Some(34))
      val q: Tuple1[Option[Double]] = Tuple1(None)
      fromTable[Seq[T1]](toTable[Seq[T1]](List(p, q))) should equal(p :: q :: Nil)
      // toTable[Seq[T3]](List(p, q), Vector("age", "name", "gender")) should equal(Table(Vector(Row(Vector(Cell("Joe"), Cell("34"), Cell("Male"))), Row(Vector(Cell("Jane"), Cell("35"), Cell("Female")))), Some(Vector("age", "name", "gender"))))
      // toTable[Seq[T3]](List(p, q), Vector("age", "whatever", "gender")) should equal(Table(Vector(Row(Vector(Cell("Joe"), Cell("34"), Cell("Male"))), Row(Vector(Cell("Jane"), Cell("35"), Cell("Female")))), Some(Vector("age", "whatever", "gender"))))
    }
  }
  describe("tuple serialization") {
    type T3 = Tuple3[String, Int, Gender]
    it("should serialize a Product3 into a Row, by column index") {
      val p = ("Joe", 34, Male)
      fromRow[T3](toRow[T3](p, None), None) should equal(p)
      toRow[T3](p, None) should equal(Row(Vector(Cell("Joe"), Cell("34"), Cell("Male"))))
    }
    it("should serialize a Product3 into a Row, by headers") {
      val p = ("Joe", 34, Male)
      evaluating { fromRow[T3](Row(Vector(Cell("34"), Cell("Joe"), Cell("Male"))), None) should equal(p) } should produce[java.lang.NumberFormatException]
      toRow[T3](p, None) should equal(Row(Vector(Cell("Joe"), Cell("34"), Cell("Male"))))
      toRow[T3](p, Some(Vector("age", "name", "gender"))) should equal(Row(Vector(Cell("Joe"), Cell("34"), Cell("Male"))))
    }
    it("should serialize a Seq[Product3] into a Table") {
      val p = ("Joe", 34, Male)
      val q = ("Jane", 35, Female)
      fromTable[Seq[T3]](toTable[Seq[T3]](List(p, q))) should equal(p :: q :: Nil)
      toTable[Seq[T3]](List(p, q), Vector("age", "name", "gender")) should equal(Table(Vector(Row(Vector(Cell("Joe"), Cell("34"), Cell("Male"))), Row(Vector(Cell("Jane"), Cell("35"), Cell("Female")))), Some(Vector("age", "name", "gender"))))
      toTable[Seq[T3]](List(p, q), Vector("age", "whatever", "gender")) should equal(Table(Vector(Row(Vector(Cell("Joe"), Cell("34"), Cell("Male"))), Row(Vector(Cell("Jane"), Cell("35"), Cell("Female")))), Some(Vector("age", "whatever", "gender"))))
    }
  }

  describe("column format") {

    it("tuple1 to saddle vec") {
      fromTable[Tuple1[Vector[Int]]](toTable[Tuple1[Vector[Int]]](Tuple1(Vector(1, 2)))) should equal(Tuple1(Vector(1, 2)))
      toTable[Tuple1[Vector[Int]]](Tuple1(Vector(1, 2))) should equal(Table(Vector(Row(Vector(Cell("1"))), Row(Vector(Cell("2")))), Some(Vector("V1"))))

      fromTable[(Tuple1[Vector[Int]], Seq[String])](toTable[(Tuple1[Vector[Int]], Seq[String])]((Tuple1(Vector(1, 2))), List("A1"))) should equal((Tuple1(Vector(1, 2))), List("A1"))
    }

    it("tuple2 to saddle vec") {
      val p = (Vector(1, 2), Seq(Vector("a", "b")))
      fromTable[(Vector[Int], Seq[Vector[String]])](toTable[(Vector[Int], Seq[Vector[String]])](p)) should equal(p)
      toTable[(Vector[Int], Seq[Vector[String]])](p) should equal(Table(Vector(Row(Vector(Cell("1"), Cell("a"))), Row(Vector(Cell("2"), Cell("b")))), Some(Vector("V1", "V2"))))

      fromTable[((Vector[Int], Seq[Vector[String]]), Seq[String])](toTable[((Vector[Int], Seq[Vector[String]]), Seq[String])]((p, List("A1", "A2")))) should equal(((Vector(1, 2), Vector(Vector("a", "b")))), Vector("A1", "A2"))
    }
  }

  describe("tuple serialization with sequence remainder") {
    type T3 = Tuple3[String, Int, Seq[Int]]
    it("should serialize a Product3 into a Row, by column index") {
      val p = ("Joe", 34, List(1, 2, 3))
      fromRow[T3](toRow[T3](p, None), None) should equal(p)
      toRow[T3](p, None) should equal(Row(Vector(Cell("Joe"), Cell("34"), Cell("1"), Cell("2"), Cell("3"))))
    }
    it("should serialize a Product3 into a Row, by headers") {
      val p = ("Joe", 34, List(1, 2, 3))
      evaluating { fromRow[T3](Row(Vector(Cell("34"), Cell("Joe"), Cell("1"), Cell("2"), Cell("3"))), None) should equal(p) } should produce[java.lang.NumberFormatException]
      toRow[T3](p, None) should equal(Row(Vector(Cell("Joe"), Cell("34"), Cell("1"), Cell("2"), Cell("3"))))
      toRow[T3](p, Some(Vector("age", "name", "gender"))) should equal(Row(Vector(Cell("Joe"), Cell("34"), Cell("1"), Cell("2"), Cell("3"))))
    }
    it("should serialize a Seq[Product3] into a Table") {
      val p = ("Joe", 34, List(1, 2, 3))
      val q = ("Jane", 35, List(4, 5, 6))
      fromTable[Seq[T3]](toTable[Seq[T3]](List(p, q))) should equal(p :: q :: Nil)
      toTable[Seq[T3]](List(p, q)) should equal(Table(Vector(Row(Vector(Cell("Joe"), Cell("34"), Cell("1"), Cell("2"), Cell("3"))), Row(Vector(Cell("Jane"), Cell("35"), Cell("4"), Cell("5"), Cell("6")))), Some(Vector("V1", "V2", "V3", "V4", "V5"))))
      toTable[Seq[T3]](List(p, q), Vector("age", "whatever", "gender")) should equal(Table(Vector(Row(Vector(Cell("Joe"), Cell("34"), Cell("1"), Cell("2"), Cell("3"))), Row(Vector(Cell("Jane"), Cell("35"), Cell("4"), Cell("5"), Cell("6")))), Some(Vector("age", "whatever", "gender", "V4", "V5"))))
    }
    it("should serialize a Seq[Product3] into a Table with header") {
      val p = ("Joe", 34, List(1, 2, 3))
      val q = ("Jane", 35, List(4, 5, 6))
      val header = Vector("name", "age", "k1", "k2", "k3")
      fromTable[(Seq[T3], Seq[String])](toTable[(Seq[T3], Seq[String])]((List(p, q), header))) should equal((p :: q :: Nil, header))
      toTable[(Seq[T3], Seq[String])]((List(p, q), header)) should equal(Table(Vector(Row(Vector(Cell("Joe"), Cell("34"), Cell("1"), Cell("2"), Cell("3"))), Row(Vector(Cell("Jane"), Cell("35"), Cell("4"), Cell("5"), Cell("6")))), Some(Vector("name", "age", "k1", "k2", "k3"))))
      evaluating {
        toTable[(Seq[T3], Seq[String])](List(p, q), Vector("age", "whatever", "gender")) should equal(Table(Vector(Row(Vector(Cell("Joe"), Cell("34"), Cell("1"), Cell("2"), Cell("3"))), Row(Vector(Cell("Jane"), Cell("35"), Cell("4"), Cell("5"), Cell("6")))), Some(Vector("name", "age", "k1", "k2", "k3"))))
      } should produce[AssertionError]
    }
  }
  describe("product serialization") {

    case class Person(name: String, age: Int, gender: Gender)
    implicit val PersonFormat = asProduct3("name", "age", "gender")(Person.apply)(Person.unapply(_).get)

    it("should serialize a Product3 into a Row, by column index") {
      val p = Person("Joe", 34, Male)
      fromRow[Person](toRow[Person](p, None), None) should equal(p)
      toRow[Person](p, None) should equal(Row(Vector(Cell("Joe"), Cell("34"), Cell("Male"))))
    }
    it("should serialize a Product3 into a Row, by headers") {
      val p = Person("Joe", 34, Male)
      fromRow[Person](Row(Vector(Cell("34"), Cell("Joe"), Cell("Male"))), Some(Vector("age", "name", "gender"))) should equal(p)
      toRow[Person](p, None) should equal(Row(Vector(Cell("Joe"), Cell("34"), Cell("Male"))))
      toRow[Person](p, Some(Vector("age", "name", "gender"))) should equal(Row(Vector(Cell("34"), Cell("Joe"), Cell("Male"))))
    }
    it("should serialize a Product1 into a Row, by headers") {
      case class A(a: String)
      implicit val AFormat = wrap("asdf")(A.apply)(A.unapply(_).get)
      val p = A("a")
      fromRow[A](Row(Vector(Cell("a"))), Some(Vector("asdf"))) should equal(p)
      toRow[A](p, None) should equal(Row(Vector(Cell("a"))))
      toRow[A](p, Some(Vector("asdf"))) should equal(Row(Vector(Cell("a"))))
    }
    it("should serialize a Seq[Product3] into a Table") {
      val p = Person("Joe", 34, Male)
      val q = Person("Jane", 35, Female)
      fromTable[Seq[Person]](toTable[Seq[Person]](List(p, q))) should equal(p :: q :: Nil)
      toTable[Seq[Person]](List(p, q), Vector("age", "name", "gender")) should equal(Table(Vector(Row(Vector(Cell("34"), Cell("Joe"), Cell("Male"))), Row(Vector(Cell("35"), Cell("Jane"), Cell("Female")))), Some(Vector("age", "name", "gender"))))
      toTable[Seq[Person]](List(p, q)) should equal(Table(Vector(Row(Vector(Cell("Joe"), Cell("34"), Cell("Male"))), Row(Vector(Cell("Jane"), Cell("35"), Cell("Female")))), Some(Vector("name", "age", "gender"))))
    }
    it("should produce an error on incorrect header specification") {
      val p = Person("Joe", 34, Male)
      evaluating { toTable[Seq[Person]](List(p), Vector("BAD", "name", "gender")) should equal(Table(Vector(Row(Vector(Cell("34"), Cell("Joe"), Cell("Male"))), Row(Vector(Cell("35"), Cell("Jane"), Cell("Female")))), Some(Vector("age", "name", "gender")))) } should produce[AssertionError]
    }
  }

  describe("csv parser") {
    it("should parse simple csv, without header") {
      val txt = "abc,\"de\ns\"\"f\",ghi\r\n1,2,3"
      TableParser.parse(txt, false) should equal(Table(Vector(Row(Vector(Cell("abc"), Cell("de\ns\"f"), Cell("ghi"))), Row(Vector(Cell("1"), Cell("2"), Cell("3")))), None))
    }
    it("should parse simple csv, withheader") {
      val txt = "A,B,C\r\nabc,\"de\ns\"\"f\",ghi\r\n1,2,3"
      TableParser.parse(txt, true) should equal(Table(Vector(Row(Vector(Cell("abc"), Cell("de\ns\"f"), Cell("ghi"))), Row(Vector(Cell("1"), Cell("2"), Cell("3")))), Some(Vector("A", "B", "C"))))
    }
    it("should parse simple csv, withheader, with trailing CRLF") {
      val txt = "A,B,C\r\nabc,\"de\ns\"\"f\",ghi\r\n1,2,3\r\n"
      TableParser.parse(txt, true) should equal(Table(Vector(Row(Vector(Cell("abc"), Cell("de\ns\"f"), Cell("ghi"))), Row(Vector(Cell("1"), Cell("2"), Cell("3")))), Some(Vector("A", "B", "C"))))
    }
    it("should parse the empty string as empty table") {
      val txt = ""
      TableParser.parse(txt, false) should equal(Table(Vector(), None))
    }
    it("should parse a one column one line document without header") {
      val txt = "A"
      TableParser.parse(txt, false) should equal(Table(Vector(Row(Vector(Cell("A")))), None))
    }
    it("should parse a one column one line document with header") {
      val txt = "A"
      TableParser.parse(txt, true) should equal(Table(Vector(), Some(Vector("A"))))
    }
    it("should parse simple csv, withheader, with 3 (etc) trailing CRLF") {
      val txt = "A,B,C\r\nabc,\"de\ns\"\"f\",ghi\r\n1,2,3\r\n\r\n\r\n"
      TableParser.parse(txt, true) should equal(Table(Vector(Row(Vector(Cell("abc"), Cell("de\ns\"f"), Cell("ghi"))), Row(Vector(Cell("1"), Cell("2"), Cell("3")))), Some(Vector("A", "B", "C"))))
    }

  }

  describe("simple whitespace table parser") {
    it("should parse without header") {
      val txt = "abc desf ghi\r\n1   2   3"
      SimpleWhiteSpaceTableParser.parse(txt, false) should equal(Table(Vector(Row(Vector(Cell("abc"), Cell("desf"), Cell("ghi"))), Row(Vector(Cell("1"), Cell("2"), Cell("3")))), None))
    }
    it("should parse , withheader") {
      val txt = "A B C\r\nabc desf ghi\r\n1 2 3"
      SimpleWhiteSpaceTableParser.parse(txt, true) should equal(Table(Vector(Row(Vector(Cell("abc"), Cell("desf"), Cell("ghi"))), Row(Vector(Cell("1"), Cell("2"), Cell("3")))), Some(Vector("A", "B", "C"))))
    }
    it("should parse simple csv, withheader, with trailing CRLF") {
      val txt = "A   B\t\tC\r\nabc     desf ghi\r\n1 2\t\t3\r\n"
      SimpleWhiteSpaceTableParser.parse(txt, true) should equal(Table(Vector(Row(Vector(Cell("abc"), Cell("desf"), Cell("ghi"))), Row(Vector(Cell("1"), Cell("2"), Cell("3")))), Some(Vector("A", "B", "C"))))
    }
    it("should parse the empty string as empty table") {
      val txt = ""
      SimpleWhiteSpaceTableParser.parse(txt, false) should equal(Table(Vector(), None))
    }
    it("should parse a one column one line document without header") {
      val txt = "A"
      SimpleWhiteSpaceTableParser.parse(txt, false) should equal(Table(Vector(Row(Vector(Cell("A")))), None))
    }
    it("should parse a one column one line document with header") {
      val txt = "A"
      SimpleWhiteSpaceTableParser.parse(txt, true) should equal(Table(Vector(), Some(Vector("A"))))
    }
    it("should parse simple csv, withheader, with 3 (etc) trailing CRLF") {
      val txt = "A B C\r\nabc desf ghi\r\n1 2 3\r\n\r\n\r\n"
      SimpleWhiteSpaceTableParser.parse(txt, true) should equal(Table(Vector(Row(Vector(Cell("abc"), Cell("desf"), Cell("ghi"))), Row(Vector(Cell("1"), Cell("2"), Cell("3")))), Some(Vector("A", "B", "C"))))
    }

  }

  describe("table writer") {
    it("should write a simple table with header") {
      val table = Table(Vector(Row(Vector(Cell("abc"), Cell("de\ns\"f"), Cell("ghi"))), Row(Vector(Cell("1"), Cell("2"), Cell("3")))), Some(Vector("A", "B", "C")))
      TableWriter.write(table) should equal("A,B,C\r\nabc,\"de\ns\"\"f\",ghi\r\n1,2,3")
    }
    it("should write a simple table without header") {
      val table = Table(Vector(Row(Vector(Cell("abc"), Cell("de\ns\"f"), Cell("ghi"))), Row(Vector(Cell("1"), Cell("2"), Cell("3")))), None)
      TableWriter.write(table) should equal("abc,\"de\ns\"\"f\",ghi\r\n1,2,3")
    }
  }
  describe("a user example ") {
    it("should deserialize 2 columns of numbers") {
      val txt = "1,2\r\n4,5"
      TabSerialization.fromTab[Seq[Seq[Int]]](TabValue(txt, false)) should equal(Seq(Seq(1, 2), Seq(4, 5)))

    }
  }
  describe("lazy table reader") {
    it("scala combinator parsers are not lazy, so this is")(pending)
  }

}

