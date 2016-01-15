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

package mybiotools.sequence

import org.scalatest.FunSpec
import org.scalatest.Matchers

class RollingHashSpec extends FunSpec with Matchers {

  describe("reorderbytes from string") {
    it("ACTG") {
      val f = reorderBytesFromString("ACTG-N")
      ("ACTG-N".getBytes("US-ASCII").map(f).toSet) should equal(Set(1, 2, 3, 4, 5, 6))
    }
    it("ABCDEFGHIJKLMNOPQRSTUVW") {
      val f = reorderBytesFromString("ABCDEFGHIJKLMNOPQRSTUVW")
      ("ABCDEFGHIJKLMNOPQRSTUVW".getBytes("US-ASCII").map(f).toSet) should equal(1 to "ABCDEFGHIJKLMNOPQRSTUVW".size toSet)
    }
  }

  describe("rolling hash") {
    it("0123") {
      val rh = new RollingHash(4)
      val array = Array[Byte](0, 1, 2, 3)
      rh.full(Array[Byte](0)) should equal(0)
      rh.full(Array[Byte](1)) should equal(1)
      rh.full(Array[Byte](2)) should equal(2)
      rh.full(Array[Byte](3)) should equal(3)
      rh.full(Array[Byte](4)) should equal(4)
      rh.full(Array[Byte](1, 0)) should equal(4)

      rh.full(Array[Byte](0, 0)) should equal(0)
      rh.full(Array[Byte](1, 1)) should equal(5)
      rh.full(Array[Byte](1, 2)) should equal(6)
      rh.full(Array[Byte](1, 3)) should equal(7)
      rh.full(Array[Byte](1, 4)) should equal(8)
      rh.full(Array[Byte](2, 0)) should equal(8)

      rh.full(Array[Byte](2, 1)) should equal(9)
      rh.full(Array[Byte](3, 1)) should equal(13)
      rh.full(Array[Byte](4, 1)) should equal(17)

    }
    it("0123 mapped") {
      val rh = new RollingHash(4, (x: Byte) => (x - 5).toByte)
      val array = Array[Byte](0, 1, 2, 3)
      rh.full(Array[Byte](5 + 0)) should equal(0)
      rh.full(Array[Byte](5 + 1)) should equal(1)
      rh.full(Array[Byte](5 + 2)) should equal(2)
      rh.full(Array[Byte](5 + 3)) should equal(3)
      rh.full(Array[Byte](5 + 4)) should equal(4)
      rh.full(Array[Byte](5 + 1, 5 + 0)) should equal(4)

      rh.full(Array[Byte](5 + 0, 5 + 0)) should equal(0)
      rh.full(Array[Byte](5 + 1, 5 + 1)) should equal(5)
      rh.full(Array[Byte](5 + 1, 5 + 2)) should equal(6)
      rh.full(Array[Byte](5 + 1, 5 + 3)) should equal(7)
      rh.full(Array[Byte](5 + 1, 5 + 4)) should equal(8)
      rh.full(Array[Byte](5 + 2, 5 + 0)) should equal(8)

      rh.full(Array[Byte](5 + 2, 5 + 1)) should equal(9)
      rh.full(Array[Byte](5 + 3, 5 + 1)) should equal(13)
      rh.full(Array[Byte](5 + 4, 5 + 1)) should equal(17)

    }
    it("update") {
      val rh = new RollingHash(4)
      rh.shift(0, 1, 0, 1) should equal(1)
      rh.shift(1, 1, 1, 0) should equal(0)
      rh.shift(2, 1, 2, 3) should equal(3)
      rh.shift(2, 1, 2, 2) should equal(2)
      rh.shift(2, 1, 2, 1) should equal(1)
      rh.shift(2, 1, 2, 0) should equal(0)
      rh.shift(0, 2, 0, 1) should equal(1)
      rh.shift(1, 2, 5, 1) should equal(5)
      rh.shift(1, 2, 5, 0) should equal(4)
      rh.shift(1, 2, 5, 2) should equal(6)
      rh.shift(1, 2, 5, 3) should equal(7)
      rh.shift(1, 2, 5, 4) should equal(8)
      rh.full(Array[Byte](1, 1, 1)) should equal(21)
      rh.shift(1, 3, 21, 4) should equal(24)

    }

    it("update mapped ") {
      val rh = new RollingHash(4, (x: Byte) => (x - 5).toByte)
      rh.shift(5 + 0, 1, 0, 5 + 1) should equal(1)
      rh.shift(5 + 1, 1, 1, 5 + 0) should equal(0)
      rh.shift(5 + 2, 1, 2, 5 + 3) should equal(3)
      rh.shift(5 + 2, 1, 2, 5 + 2) should equal(2)
      rh.shift(5 + 2, 1, 2, 5 + 1) should equal(1)
      rh.shift(5 + 2, 1, 2, 5 + 0) should equal(0)
      rh.shift(5 + 0, 2, 0, 5 + 1) should equal(1)
      rh.shift(5 + 1, 2, 5, 5 + 1) should equal(5)
      rh.shift(5 + 1, 2, 5, 5 + 0) should equal(4)
      rh.shift(5 + 1, 2, 5, 5 + 2) should equal(6)
      rh.shift(5 + 1, 2, 5, 5 + 3) should equal(7)
      rh.shift(5 + 1, 2, 5, 5 + 4) should equal(8)

    }
    ignore("rolling hash without a prime modulo takes only the suffix of the string into account") {
      new RollingHash(256).full(Array[Byte](1, 2, 3, 5, 6, 7, 8, 9)) should equal(new RollingHash(256).full(Array[Byte](6, 7, 8, 9)))
    }
  }
}