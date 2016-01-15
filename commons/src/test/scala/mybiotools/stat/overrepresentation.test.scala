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

package mybiotools.stat

import HWE._

import org.scalatest.FunSpec
import org.scalatest.Matchers

class OverrepresentationSpec extends FunSpec with Matchers {

  describe("hypergeometric test") {
    it("R 1") {
      OverRepresentation.hypergeometricTest(total = 1, marked = 1, draws = 1, markedDraws = 1) should equal(1.0)
    }
    it("R 2") {
      OverRepresentation.hypergeometricTest(total = 1, marked = 1, draws = 1, markedDraws = 0) should equal(1.0)
    }
    it("R 3") {
      OverRepresentation.hypergeometricTest(total = 1, marked = 0, draws = 1, markedDraws = 0) should equal(1.0)
    }
    it("R 4") {
      OverRepresentation.hypergeometricTest(total = 2, marked = 1, draws = 1, markedDraws = 1) should equal(0.4999999999999999)
    }
    it("R 5") {
      OverRepresentation.hypergeometricTest(total = 40, marked = 10, draws = 1, markedDraws = 1) should equal(0.25000000000000255)
    }
    it("R 6") {
      OverRepresentation.hypergeometricTest(total = 40, marked = 10, draws = 10, markedDraws = 3) should equal(0.4850554843813657)
    }
    it("enrichment should produce a low p-value") {
      OverRepresentation.hypergeometricTest(total = 40, marked = 8, draws = 10, markedDraws = 8) should equal(5.85139904025353E-7)
    }

    it("depletion should produce a high p-value") {
      OverRepresentation.hypergeometricTest(total = 40, marked = 8, draws = 20, markedDraws = 1) should equal(0.9983619983619983)
    }
    it("expected should produce a high p-value") {
      OverRepresentation.hypergeometricTest(total = 40, marked = 8, draws = 20, markedDraws = 4) should equal(0.6526176526176533)
    }

  }

}