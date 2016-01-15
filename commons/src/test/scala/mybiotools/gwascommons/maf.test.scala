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
package gwascommons

import gwascommons._
import org.scalatest.FunSpec
import org.scalatest.Matchers

class MAFSpec extends FunSpec with Matchers {
  import gwascommons._
  import MAFReader._

  describe("MAF ") {
    it("empty") {
      val str = """|# whatever
    				 |# whatever
    				 |""".stripMargin
      MAFReader.readMAF(io.Source.fromString(str)).toList should equal(Nil)
    }
    it("1 empty par") {
      val str = """|# whatever
    				 |# whatever
    				 |a 
    				 |
    				 |""".stripMargin
      MAFReader.readMAF(io.Source.fromString(str)).toList should equal(List(Paragraph(Nil)))
    }
    it("1 par with 1 seq") {
      val str = """|# whatever
    				 |# whatever
    				 |a 
    				 |s name 0 3 + 4 ATG
    				 |
    				 |""".stripMargin
      MAFReader.readMAF(io.Source.fromString(str)).toList should equal(List(Paragraph(List(SLine("name", 0, 3, PositiveStrand, 4, "ATG")))))
    }
    it("1 par with 2 seq") {
      val str = """|# whatever
    				 |# whatever
    				 |a 
    				 |s name 0 3 + 4 ATG
    				 |s name2 0 3 + 4 ATC
    				 |
    				 |""".stripMargin
      MAFReader.readMAF(io.Source.fromString(str)).toList should equal(List(Paragraph(List(SLine("name", 0, 3, PositiveStrand, 4, "ATG"), SLine("name2", 0, 3, PositiveStrand, 4, "ATC")))))
    }
    it("2 par with 2 seq") {
      val str = """|# whatever
    				 |# whatever
    				 |a 
    				 |s name 0 3 + 4 ATG
    				 |s name2 0 3 + 4 ATC
    				 |
    				 |a 
    				 |s name 0 3 + 4 ATG
    				 |s name2 0 3 + 4 ATC
    				 |
    				 |""".stripMargin
      MAFReader.readMAF(io.Source.fromString(str)).toList should equal(List(Paragraph(List(SLine("name", 0, 3, PositiveStrand, 4, "ATG"), SLine("name2", 0, 3, PositiveStrand, 4, "ATC"))), Paragraph(List(SLine("name", 0, 3, PositiveStrand, 4, "ATG"), SLine("name2", 0, 3, PositiveStrand, 4, "ATC")))))
    }
  }
}