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
package search

import org.scalatest.FunSpec
import org.scalatest.Matchers

class GoldenSectionSpec extends FunSpec with Matchers {

  describe("x  * (x + 15)") {
    it("should find minimum from -100 500") {
      val m = findMinimum(-100.0, 500.0, 0.0000001, 1000) { x => x * (x + 15) }
      m._2 should equal(25)
      math.abs(m._1 - (-7.5)) should be < 0.001
    }
    it("out of domain") {
      val m = findMinimum(2, 5.0, 0.0000001, 100) { x => x * x }
      // m._2 should equal(25)
      math.abs(m._1 - (2.0)) should be < 0.001
    }
  }
  describe("bisect") {
    it("should find root of y=x") {
      (bisect(-10d, 10d, 5d, 10, 1E-3, 1E-5)(x => x)) should equal(0.0)
    }
    it("should find root of y=x+1") {
      (bisect(-10d, 10d, 5d, 10, 1E-3, 1E-5)(x => x + 1)) should equal(-0.99609375)
    }
  }

}