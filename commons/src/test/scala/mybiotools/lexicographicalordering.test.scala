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

import org.scalatest.FunSpec
import org.scalatest.Matchers

import mybiotools.LexicographicalOrder._

class LexicographicalOrderingSpec extends FunSpec with Matchers {

  describe("comparison") {
    it("comparison") {
      val ordering = lexicographicalOrdering[Int]
      val s1 = List(1, 2, 3, 4)
      val s2 = List(1, 2, 3, 4)
      val s3 = List(1, 2, 3, 5)
      val s4 = List(1, 2, 3, 3)
      val s5 = List(1, 2, 3)

      (ordering.gteq(s1, s2)) should equal(true)
      (ordering.lteq(s1, s2)) should equal(true)
      ordering.gt(s1, s5) should equal(true)
      ordering.gt(s1, s4) should equal(true)
      ordering.lt(s1, s3) should equal(true)
      ordering.lt(s5, s4) should equal(true)
      ordering.lt(s5, s3) should equal(true)
    }
  }

}