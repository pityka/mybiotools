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

package mybiotools.stringstore

import org.scalatest.FunSpec
import org.scalatest.Matchers
import mybiotools.stringstore._

class StringStoreSpec extends FunSpec with Matchers {

  describe("source with iter 8") {
    it("sample") {
      val source = scala.io.Source.fromString("abc\ndef\nghi")
      source.getLines8.toList.map(_.value) should equal(source.reset.getLines.toList)
    }
    it("large") {
      val string = (1 to 100 map ((i: Int) => (1 to 100000 mkString))).mkString("\n")
      val source = scala.io.Source.fromString(string)
      source.getLines8.toList.map(_.value) should equal(source.reset.getLines.toList)
    }
  }

  describe("substring") {
    it("value") {
      val base = s8"0123456789"
      base should equal(s8"0123456789")
      base should not equal (s8"0123456788")
      base.substring(0, 10) should equal(base)
      base.substring(0, 0) should equal(s8"")
      base.substring(0, 1) should equal(s8"0")
      base.substring(0, 1).value should equal("0")

      base.substring(0, 3) should equal(s8"012")
      base.substring(0, 3).value should equal("012")

      base.substring(2, 7) should equal(s8"23456")

      base.substring(2, 7) should not equal (s8"34567")

      base.substring(2, 7).value should equal("23456")
      base.substring(2, 7).substring(3, 5) should equal(s8"56")
    }
  }

  describe("interpolation") {
    it("s8") {
      s8"abcd".isInstanceOf[String8] should equal(true)
    }
  }

  describe("String8") {
    it("equality") {
      val s1 = new String8("abcd")
      val s2 = new String8("abcd")
      val s3 = new String8("abcdadf")

      (s1 == s2) should equal(true)
      (s1 != s3) should equal(true)
      (s1 ne s2) should equal(true)
      (s1 ne s3) should equal(true)
      (s1 eq s1) should equal(true)
      (s1 == s1) should equal(true)
    }
    it("value") {
      val s = "sdfad"
      val s1 = new String8(s)
      s1.value should equal(s)
    }
  }

  describe("Intern table") {
    it("equality") {
      val s1 = StringStore("abcd")
      val s2 = StringStore("abcd")
      val s3 = StringStore("qewr")

      (s1 == s2) should equal(true)
      (s1 != s3) should equal(true)
      (s1 eq s2) should equal(true)
      (s1 ne s3) should equal(true)
      (s1 eq s1) should equal(true)
      (s1 == s1) should equal(true)

    }
  }

  describe("comparison") {
    it("comparison") {
      val s1 = StringStore("abcd")
      val s2 = StringStore("abcd")
      val s3 = StringStore("abcf")
      val s4 = StringStore("abcc")
      val s5 = StringStore("abc")

      (String8Ordering.gteq(s1, s2)) should equal(true)
      (String8Ordering.lteq(s1, s2)) should equal(true)
      String8Ordering.gt(s1, s5) should equal(true)
      String8Ordering.gt(s1, s4) should equal(true)
      String8Ordering.lt(s1, s3) should equal(true)
      String8Ordering.lt(s5, s4) should equal(true)
      String8Ordering.lt(s5, s3) should equal(true)
    }
  }

  describe("performance") {
    it("1M string x 6 parallel should finish in 20sec") {
      val t1 = System.nanoTime()
      val x = (1 to 6).par.map { x =>
        for (i <- 1 to 1000000) yield { mybiotools.stringstore.StringStore("rs" + i) }
      }
      val t2 = System.nanoTime()

      val tsec = (t2 - t1) / 1E9

      println(tsec)

      (tsec < 20.0) should equal(true)
    }
  }

}