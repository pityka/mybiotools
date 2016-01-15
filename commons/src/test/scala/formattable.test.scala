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
import mybiotools.Gender
import mybiotools.formatters.Formatters._
import mybiotools.formatters._

class FormatTableTestSuite extends FunSuite {

  test("Formatting of GenderData") {
    val male = Some(Male)
    val female = Some(Female)
    val unknown = None
    assertResult("1") { format(male, PlinkFam) }
    assertResult("1") { format(Male, PlinkFam) }
    assertResult("2") { format(female, PlinkFam) }
    assertResult("-9") { format(unknown, PlinkFam) }
    assertResult("1") { format(1, PlinkFam) }
    assertResult("abcd") { format("abcd", PlinkFam) }
    assertResult("List(1)") { format(List(1), PlinkFam) }

    assertResult("Male") { format(male, RFormat) }
    assertResult("Male") { format(Male, RFormat) }
    assertResult("Female") { format(female, RFormat) }
    assertResult("NA") { format(unknown, RFormat) }
    assertResult("1") { format(1, RFormat) }
    assertResult("abcd") { format("abcd", RFormat) }
    assertResult("List(1)") { format(List(1), RFormat) }
  }

  test("Formatting of BinaryData") {
    val affected = Some(true)
    val control = Some(false)
    val missing = None
    assertResult("2") { format(affected, PlinkFam) }
    assertResult("1") { format(control, PlinkFam) }
    assertResult("2") { format(true, PlinkFam) }
    assertResult("1") { format(false, PlinkFam) }
    assertResult("-9") { format(missing, PlinkFam) }

    assertResult("T") { format(affected, RFormat) }
    assertResult("F") { format(control, RFormat) }
    assertResult("T") { format(true, RFormat) }
    assertResult("F") { format(false, RFormat) }
    assertResult("NA") { format(missing, RFormat) }
  }

  test("Format of simple table.") {
    val table: FlatTable[Symbol, _] = List(
      Map('a -> 1, 'b -> "Name1", 'c -> Some(Male), 'd -> Some(true)),
      Map('a -> 2, 'b -> "Name2", 'c -> Some(Female), 'd -> None),
      Map('a -> 3, 'b -> "Name3", 'c -> None, 'd -> Some(false))
    )
    val str = formatTable(table, quoteString = false)
    val strexp = """|a,b,c,d
                    |1,Name1,Male,T
                    |2,Name2,Female,NA
                    |3,Name3,NA,F""".stripMargin
    assertResult(strexp) { str }
  }

  test("Format of simple table with ordered keys.") {
    val table: FlatTable[Symbol, _] = List(
      Map('id -> 1, 'name -> "Name1", 'gender -> Some(Male), 'affected -> Some(true)),
      Map('id -> 2, 'name -> "Name2", 'gender -> Some(Female), 'affected -> None),
      Map('id -> 3, 'name -> "Name3", 'gender -> None, 'affected -> Some(false))
    )
    val str = formatTable(table, keys = List('name, 'id), quoteString = false)
    val strexp = """|name,id
                       |Name1,1
                       |Name2,2
                       |Name3,3""".stripMargin
    assertResult(strexp) { str }
  }

  test("Format of table with missing values (leaky).") {
    val table: FlatTable[Symbol, _] = List(
      Map('name -> "Name1", 'gender -> Some(Male), 'affected -> Some(true)),
      Map('id -> 2, 'name -> "Name2", 'affected -> None),
      Map('id -> 3, 'name -> "Name3", 'gender -> None)
    )
    val str = formatTable(table, keys = List('id, 'name, 'gender, 'affected), quoteString = false)
    val strexp = """|id,name,gender,affected
                     |NA,Name1,Male,T
                     |2,Name2,NA,NA
                     |3,Name3,NA,NA""".stripMargin
    assertResult(strexp) { str }
  }

  test("Format of table with missing values (leaky). Another.") {
    val table: FlatTable[Symbol, _] = List(
      Map('name -> "Name1", 'gender -> Some(Male), 'affected -> Some(true), 'birth -> "A long ago"),
      Map('id -> 2, 'name -> "Name2", 'affected -> None),
      Map('id -> 3, 'name -> "Name3", 'gender -> None)
    )
    val str = formatTable(table, keys = List('id, 'name, 'gender, 'affected, 'birth), quoteString = false)
    val strexp = """|id,name,gender,affected,birth
                     |NA,Name1,Male,T,A long ago
                     |2,Name2,NA,NA,NA
                     |3,Name3,NA,NA,NA""".stripMargin
    assertResult(strexp) { str }
  }

  test("Format of table with String quoting.") {
    val table: FlatTable[Symbol, _] = List(
      Map('name -> "Name1", 'gender -> Some(Male), 'affected -> Some(true), 'birth -> "A long ago"),
      Map('id -> 2, 'name -> "Name2", 'affected -> None),
      Map('id -> 3, 'name -> "Name3", 'gender -> None)
    )
    val str = formatTable(table, keys = List('id, 'name, 'gender, 'affected, 'birth), quoteString = true)
    val strexp = """|id,name,gender,affected,birth
                     |NA,"Name1",Male,T,"A long ago"
                     |2,"Name2",NA,NA,NA
                     |3,"Name3",NA,NA,NA""".stripMargin
    assertResult(strexp) { str }
  }

  test("Format of table with missing key.") {
    val table: FlatTable[Symbol, _] = List(
      Map('name -> "Name1", 'gender -> Some(Male), 'affected -> Some(true), 'birth -> "A long ago"),
      Map('id -> 2, 'name -> "Name2", 'affected -> None),
      Map('id -> 3, 'name -> "Name3", 'gender -> None)
    )
    val str = formatTable(table, keys = List('nosuchkey), quoteString = true)
    val strexp = """|nosuchkey
     |NA
     |NA
     |NA""".stripMargin
    assertResult(strexp) { str }
  }

  test("Format of simple table for Plink.") {
    val table: FlatTable[Symbol, _] = List(
      Map('a -> 1, 'b -> "Name1", 'c -> Some(Male), 'd -> Some(true)),
      Map('a -> 2, 'b -> "Name2", 'c -> Some(Female), 'd -> None),
      Map('a -> 3, 'b -> "Name3", 'c -> None, 'd -> Some(false))
    )
    val str = formatTable(table, quoteString = false, software = PlinkCovar)
    val strexp = """|a b c d
                     |1 Name1 1 1
                     |2 Name2 0 -9
                     |3 Name3 -9 0""".stripMargin
    assertResult(strexp) { str }
  }

  test("Format of simple table for Plink with categorical variables.") {
    val set = Seq('a, 'b, 'c, 'd, 'e)
    val table: FlatTable[Symbol, _] = List(
      Map('a -> 1, 'b -> "Name1", 'c -> Some(Male), 'd -> Some(true), 'e -> new CategoricalData(Some('a), set)),
      Map('a -> 2, 'b -> "Name2", 'c -> Some(Female), 'd -> None, 'e -> new CategoricalData(Some('b), set)),
      Map('a -> 3, 'b -> "Name3", 'c -> None, 'd -> Some(false), 'e -> new CategoricalData(None, set))
    )
    val str = formatTable(table, quoteString = false, software = PlinkCovar)
    val strexp = """|a b c d e_a e_b e_c e_d
                     |1 Name1 1 1 1 0 0 0
                     |2 Name2 0 -9 0 1 0 0
                     |3 Name3 -9 0 -9 -9 -9 -9""".stripMargin
    assertResult(strexp) { str }
  }

  test("Format of simple table for R with categorical variables.") {
    val set = Seq('a, 'b, 'c, 'd, 'e)
    val table: FlatTable[Symbol, _] = List(
      Map('a -> 1, 'b -> "Name1", 'c -> Some(Male), 'd -> Some(true), 'e -> new CategoricalData(Some('a), set)),
      Map('a -> 2, 'b -> "Name2", 'c -> Some(Female), 'd -> None, 'e -> new CategoricalData(Some('b), set)),
      Map('a -> 3, 'b -> "Name3", 'c -> None, 'd -> Some(false), 'e -> new CategoricalData(None, set))
    )
    val str = formatTable(table, quoteString = false, software = RFormat)
    val strexp = """|a,b,c,d,e
|1,Name1,Male,T,a
|2,Name2,Female,NA,b
|3,Name3,NA,F,NA""".stripMargin
    assertResult(strexp) { str }
  }
}

