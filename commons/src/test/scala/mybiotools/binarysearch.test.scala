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
import mybiotools.gwascommons.Individual

class BinarySearchSpec extends FunSpec with Matchers {

  describe("cpu time on 1 .. 8") {
    val v = Array(1, 2, 3, 4, 5, 6, 7, 8)

    it("should find 2 in 3 iterations") {
      var counter = 0

      val less = (x: Int) => { counter += 1; x < 2 }
      val greater = (x: Int) => { counter += 1; x > 2 }
      (binarySearch(v, less, greater)) should equal(Right(1, 2))
      counter should equal(3)
    }
    it("should find 6 in 2 iterations") {
      var counter = 0

      val less = (x: Int) => { counter += 1; x < 6 }
      val greater = (x: Int) => { counter += 1; x > 6 }
      (binarySearch(v, less, greater)) should equal(Right(5, 6))
      counter should equal(4)
    }

  }

  describe("on Array(1,2,3,4,5,_,7,8,9,10)") {
    val v = Array(1, 2, 3, 4, 5, 7, 8, 9, 10)

    it("should find 5") {
      val less = (_: Int) < 5
      val greater = (_: Int) > 5
      (binarySearch(v, less, greater)) should equal(Right(4, 5))
    }
    it("should find 1") {
      val less = (_: Int) < 1
      val greater = (_: Int) > 1
      (binarySearch(v, less, greater)) should equal(Right(0, 1))
    }
    it("should find 10") {
      val less = (_: Int) < 10
      val greater = (_: Int) > 10
      (binarySearch(v, less, greater)) should equal(Right(8, 10))
    }
    it("should find 9") {
      val less = (_: Int) < 9
      val greater = (_: Int) > 9
      (binarySearch(v, less, greater)) should equal(Right(7, 9))
    }
    it("should find 2") {
      val less = (_: Int) < 2
      val greater = (_: Int) > 2
      (binarySearch(v, less, greater)) should equal(Right(1, 2))
    }
    it("should not find 11") {
      val less = (_: Int) < 11
      val greater = (_: Int) > 11
      (binarySearch(v, less, greater)) should equal(Left(8, 9))
    }
    it("should not find 0") {
      val less = (_: Int) < 0
      val greater = (_: Int) > 0
      (binarySearch(v, less, greater)) should equal(Left(-1, 0))
    }
    it("should not find 6") {
      val less = (_: Int) < 6
      val greater = (_: Int) > 6
      (binarySearch(v, less, greater)) should equal(Left(4, 5))
    }
  }

  describe("on view 4 5 7 8") {
    val v = Array(1, 2, 3, 4, 5, 7, 8, 9, 10).view(3, 7)
    it("should find 8") {
      val less = (_: Int) < 8
      val greater = (_: Int) > 8
      (binarySearch(v, less, greater)) should equal(Right(3, 8))
    }
    it("should find 7") {
      val less = (_: Int) < 7
      val greater = (_: Int) > 7
      (binarySearch(v, less, greater)) should equal(Right(2, 7))
    }
    it("should find 5") {
      val less = (_: Int) < 5
      val greater = (_: Int) > 5
      (binarySearch(v, less, greater)) should equal(Right(1, 5))
    }
    it("should find 4") {
      val less = (_: Int) < 4
      val greater = (_: Int) > 4
      (binarySearch(v, less, greater)) should equal(Right(0, 4))
    }
  }

  describe("on reversed view 8 7 5 4") {
    val v = Array(1, 2, 3, 4, 5, 7, 8, 9, 10).view(3, 7).reverse
    it("should find 8") {
      val less = (_: Int) > 8
      val greater = (_: Int) < 8
      (binarySearch(v, less, greater)) should equal(Right(0, 8))
    }
    it("should find 7") {
      val less = (_: Int) > 7
      val greater = (_: Int) < 7
      (binarySearch(v, less, greater)) should equal(Right(1, 7))
    }
    it("should find 5") {
      val less = (_: Int) > 5
      val greater = (_: Int) < 5
      (binarySearch(v, less, greater)) should equal(Right(2, 5))
    }
    it("should find 4") {
      val less = (_: Int) > 4
      val greater = (_: Int) < 4
      (binarySearch(v, less, greater)) should equal(Right(3, 4))
    }
  }

  describe(" on a Array 1 3 2  find  even ") {
    val v = Array(1, 3, 2)
    val less = (_: Int) % 2 != 0
    val greater = (_: Int) % 2 == 0
    it("should find the borderline ") {
      (binarySearch(v, less, greater)) should equal(Left(1, 2))
    }
  }

  describe(" on a Array 1 2  find even ") {
    val v = Array(1, 2)
    val less = (_: Int) % 2 != 0
    val greater = (_: Int) % 2 == 0
    it("should find the borderline ") {
      (binarySearch(v, less, greater)) should equal(Left(0, 1))
    }
  }

  describe(" on a Array 1 1 1 2 2 2 find even ") {
    val v = Array(1, 1, 1, 2, 2, 2)
    val less = (_: Int) % 2 != 0
    val greater = (_: Int) % 2 == 0
    it("should find the borderline ") {
      (binarySearch(v, less, greater)) should equal(Left(2, 3))
    }
  }

  describe(" on a Array 2 2 2 find even ") {
    val v = Array(2, 2, 2)
    val less = (_: Int) % 2 != 0
    val greater = (_: Int) % 2 == 0
    it("should find the borderline ") {
      (binarySearch(v, less, greater)) should equal(Left(-1, 0))
    }
  }

  describe(" on a Array 1 1 1 find even ") {
    val v = Array(1, 1, 1)
    val less = (_: Int) % 2 != 0
    val greater = (_: Int) % 2 == 0
    it("should find the borderline ") {
      (binarySearch(v, less, greater)) should equal(Left(2, 3))
    }
  }

  describe(" on a Array 1 2 3  the border between 1's and 3's ") {
    val v = Array(1, 2, 3)
    val less = (_: Int) < 3
    val greater = (_: Int) > 1
    it("should find the borderline ") {
      (binarySearch(v, less, greater)) should equal(Left(0, 1))
    }
  }

  describe(" on a Array 1 1  3 2 2 2  find  the border between 1 and 2 ") {
    val v = Array(1, 1, 3, 2, 2, 2)
    val less = (_: Int) < 2
    val greater = (_: Int) > 1
    it("should find the borderline ") {
      (binarySearch(v, less, greater)) should equal(Left(1, 2))
    }
  }

  describe(" on a Array 2 2 2 find even jumpDirectional") {
    val v = Array(2, 2, 2)
    val odd = (_: Int) % 2 != 0
    it("should find the borderline ") {
      (jumpDirectionalSearch(v, odd, true)) should equal(None)
    }
  }

  describe("jumpDirectionalSearch find first even") {
    val even = (_: Int) % 2 == 0

    it("on (1,2) ") {
      val v = Array(1, 2)

      jumpDirectionalSearch(v, even, true) should equal(Some((1, 2)))
    }
    it("on (1,1) ") {
      val v = Array(1, 1)

      jumpDirectionalSearch(v, even, true) should equal(None)
    }

    it("on (2,2) ") {
      val v = Array(2, 2)

      jumpDirectionalSearch(v, even, true) should equal(Some(0, 2))
    }

    it("on (1,1,1,1,1,2) ") {
      val v = Array(1, 1, 1, 1, 1, 2)

      jumpDirectionalSearch(v, even, true) should equal(Some(5, 2))
    }

    it("on (1,1,1,1,1,2,2,2,2,2,2,2,2) ") {
      val v = Array(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2)

      jumpDirectionalSearch(v, even, true) should equal(Some(3, 2))
    }
    it("on (1,1,1,1,1) ") {
      val v = Array(1, 1, 1, 1, 1)

      jumpDirectionalSearch(v, even, true) should equal(None)
    }
    it("on () ") {
      val v = Array[Int]()

      jumpDirectionalSearch(v, even, true) should equal(None)
    }
  }

  describe("jumpDirectionalSearch find first even REVERSE") {
    val even = (_: Int) % 2 == 0

    it("on (1,2) ") {
      val v = Array(1, 2)

      jumpDirectionalSearch(v, even, false) should equal(Some((1, 2)))
    }
    it("on (1,1) ") {
      val v = Array(1, 1)

      jumpDirectionalSearch(v, even, false) should equal(None)
    }

    it("on (2,2) ") {
      val v = Array(2, 2)

      jumpDirectionalSearch(v, even, false) should equal(Some(1, 2))
    }

    it("on (1,1,1,1,1,2) ") {
      val v = Array(1, 1, 1, 1, 1, 2)

      jumpDirectionalSearch(v, even, false) should equal(Some(5, 2))
    }

    it("on (2,1,1,1,1,1) ") {
      val v = Vector(2, 1, 1, 1, 1, 1)

      jumpDirectionalSearch(v, even, false) should equal(Some(0, 2))

    }

    it("on (2,2,1,1,1,1) ") {
      val v = Vector(2, 2, 1, 1, 1, 1)

      jumpDirectionalSearch(v, even, false) should equal(Some(1, 2))

    }
    it("on (1,1,1,1,1) ") {
      val v = Array(1, 1, 1, 1, 1)

      jumpDirectionalSearch(v, even, false) should equal(None)
    }
    it("on () ") {
      val v = Array[Int]()

      jumpDirectionalSearch(v, even, false) should equal(None)
    }
    it("on (0,9,10,11,31,41) first below 10") {
      val v = Array(0, 9, 10, 11, 31, 41)
      jumpDirectionalSearch(
        v,
        (_: Int) <= 10,
        forward = false
      ) should equal(Some(2, 10))
    }
    it("on (0,9,10,11,31,41) first below 40") {
      val v = Array(0, 9, 10, 11, 31, 41)
      jumpDirectionalSearch(
        v,
        (_: Int) <= 40,
        forward = false
      ) should equal(Some(4, 31))
    }
  }

}