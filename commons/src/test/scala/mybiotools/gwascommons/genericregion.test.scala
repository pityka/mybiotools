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
import org.scalatest.PrivateMethodTester
import org.scalatest.Matchers
import org.scalatest.prop.Checkers
// import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.BooleanOperators
// import org.scalacheck.Gen._
import org.scalacheck.{ Arbitrary, Gen }

class GenericGenomicRegionSpec extends FunSpec with Matchers with PrivateMethodTester with Checkers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(minSuccessful = 1000, maxDiscarded = 10000)

  implicit def arbRegion[Region] =
    Arbitrary {
      for {
        chr <- Gen.choose(1, 22)
        from <- Gen.choose(0, 1000)
        to <- Gen.choose(from, 1000)
      } yield {
        Region("xy" + chr.toString, from, to)
      }
    }

  val gl4 = GenomicLocation(4, "zsdf")
  val gl0 = GenomicLocation(1, "zsdf")
  val gl10 = GenomicLocation(10, "zsdf")
  val gl11 = GenomicLocation(11, "zsdf")
  val gl32 = GenomicLocation(32, "zsdf")

  describe("Region") {

    it("chunkToMaxSize has the same size as inputs") {
      check((a: Region, b: Int) => (b > 0) ==> (a.chunkToMaxSize(b).map(_.size).sum == a.size))
    }

    it("chunkToMaxSize chunks to smaller chunks") {
      check((a: Region, b: Int) => (b > 0) ==> (a.chunkToMaxSize(b).forall(_.size <= b)))
    }

    it("extendbefore is greater") {
      check { (a: Region, extension: Int) =>
        (extension >= 0 && scala.math.abs(extension) < 100000) ==> {
          (a.extendBefore(extension).size == a.size + extension) || (a.from - extension < 0)
        }
      }
    }
    it("extendafter is greater") {
      check { (a: Region, extension: Int) =>
        (extension >= 0 && scala.math.abs(extension) < 100000) ==> {
          (a.extendAfter(extension).size == (a.size + extension))
        }
      }
    }
    it("collapse does not increases size") {
      check { (a: Region, b: Region) =>
        a.collapse(b, 0).map(_.size <= (a.size + b.size)).getOrElse(true)
      }
    }
    it("collapse keeps size if not overlap") {
      check { (a: Region, b: Region) =>
        (a.to <= b.from) ==> {
          val x: Boolean = (a.collapse(b, 0)).map(x => x.size == (a.size + b.size)).getOrElse(true)
          x
        }
      }
    }
    it("collapse joins") {
      check { (a: Region, extension: Int) =>
        (extension >= 0 && extension < 10000) ==> {
          val x: Boolean = (a.collapse(Region(a.chromosome, a.to, a.to + extension), 0)).map(x => x.size == (a.size + extension) && x.from == a.from && x.to == (a.to + extension)).getOrElse(true)
          x
        }
      }
    }

    val big = Region("zsdf", 0, 10)
    val small = Region("zsdf", 2, 5)
    val smaller = Region("zsdf", 3, 4)
    val small2 = Region("zsdf", 5, 8)
    val smaller2 = Region("zsdf", 3, 7)
    val big8_16 = Region("zsdf", 8, 16)

    it("[0,10) contains 3")(big contains gl4 should equal(true))
    it("[0,10) contains 0")(big contains gl0 should equal(true))
    it("[0,10) does contains 9")(big contains gl10 should equal(true))
    it("[0,10) does not contain 10")(big contains gl11 should equal(false))
    it("[3,4) contains 3")(smaller contains gl4 should equal(true))
    it("[3,4) not contains 4")(smaller contains GenomicLocation(5, "zsdf") should equal(false))

    it("[0,10) contains [0,10)")(big contains big should equal(true))
    it("[0,10) contains [2,5)")(big contains small should equal(true))
    it("[0,10) contains [3,4)")(big contains smaller should equal(true))
    it("[0,10) contains [5,8)")(big contains small2 should equal(true))
    it("[0,10) contains [3,7)")(big contains smaller2 should equal(true))
    it("[0,10) NOT contain (8,16)")(big contains big8_16 should equal(false))
    it("[0,10) NOT contain (8,16) reverse")(big8_16 contains big should equal(false))

    it("[2,5) contains [3,4)")(small contains smaller should equal(true))
    it("[3,4) NOT contains [2,5)")(smaller contains small should equal(false))

    it("[2,5) U [5,8) == [2,8)")(small.collapse(small2, 0).get should equal(Region("zsdf", 2, 8)))
    it("[5,8) U [2,5) == [2,8)")(small2.collapse(small, 0).get should equal(Region("zsdf", 2, 8)))
    it("(5,8) U (5,8) == [5,8)")(small2.collapse(small2, 0).get should equal(Region("zsdf", 5, 8)))

    it("[2,6) U [6,8) == [2,8)")((Region("zsdf", 2, 6)).collapse(Region("zsdf", 6, 8), 0).get should equal(Region("zsdf", 2, 8)))
    it("[2,5) U [6,8) != [2,8)")(small.collapse(Region("zsdf", 6, 8), 0) should equal(None))
    it("[6,8) U [2,5) != [2,8)")(Region("zsdf", 6, 8).collapse(small, 0) should equal(None))
    it("[6,8) U [2,6) == [2,8)")(Region("zsdf", 6, 8).collapse(Region("zsdf", 2, 6), 0) should equal(Some(Region("zsdf", 2, 8))))

  }

  describe("Region intersects") {
    it("[0,10) intersects [9,10)") {
      Region("zsdf", 0, 10) intersects Region("zsdf", 9, 10) should be(true)
    }
    it("[0,10) intersects [9,11)") {
      Region("zsdf", 0, 10) intersects Region("zsdf", 9, 11) should be(true)
    }
    it("[0,10) does not [9,9)") {
      Region("zsdf", 0, 10) intersects Region("zsdf", 9, 9) should be(false)
    }
    it("[10,15) intersects [9,11)") {
      Region("zsdf", 10, 15) intersects Region("zsdf", 9, 11) should be(true)
    }
    it("[10,15) not [9,10)") {
      Region("zsdf", 10, 15) intersects Region("zsdf", 9, 10) should be(false)
    }
    it("[10,15) not [11,13)") {
      Region("zsdf", 10, 15) intersects Region("zsdf", 11, 13) should be(true)
    }
  }

  describe("Region chunk") {
    val r = Region("zsdf", 0, 100)
    it("no need for chunking") {
      r.chunkToMaxSize(1000) should equal(List(r))
    }
    it("special sizes") {

      r.chunkToMaxSize(100) should equal(List(r))
      r.chunkToMaxSize(101) should equal(List(r))
      r.chunkToMaxSize(99) should equal(List(Region("zsdf", 0, 99), Region("zsdf", 99, 100)).reverse)
      evaluating(r.chunkToMaxSize(0)) should produce[IllegalArgumentException]
    }
  }

  describe("Region subtract") {
    val rr = Region("zsdf", 10, 20)
    val left = Region("zsdf", 1, 9)
    val right = Region("zsdf", 21, 25)
    val leftTouch = Region("zsdf", 1, 11)
    var rightTouch = Region("zsdf", 19, 22)
    val leftOverlap = Region("zsdf", 5, 15)
    val rightOverlap = Region("zsdf", 15, 25)
    val middle = Region("zsdf", 12, 15)
    val middleTouchLeft = Region("zsdf", 10, 14)
    val middleTouchRight = Region("zsdf", 14, 20)
    val over = Region("zsdf", 0, 100)
    it("rr - left")(rr subtract left should equal(List(rr)))
    it("rr - right")(rr subtract right should equal(List(rr)))
    it("rr - leftTouch")(rr subtract leftTouch should equal(List(Region("zsdf", 11, 20))))
    it("rr - rightTouch")(rr subtract rightTouch should equal(List(Region("zsdf", 10, 19))))
    it("rr - leftOverlap")(rr subtract leftOverlap should equal(List(Region("zsdf", 15, 20))))
    it("rr - rightOverlap")(rr subtract rightOverlap should equal(List(Region("zsdf", 10, 15))))
    it("rr - middle")(rr subtract middle should equal(List(Region("zsdf", 10, 12), Region("zsdf", 15, 20))))
    it("rr - middleTouchLeft")(rr subtract middleTouchLeft should equal(List(Region("zsdf", 14, 20))))
    it("rr - middleTouchRight")(rr subtract middleTouchRight should equal(List(Region("zsdf", 10, 14))))
    it("rr - over")(rr subtract over should equal(Nil))
    it("rr - rr")(rr subtract rr should equal(Nil))
  }

  describe("Region subtract same chromosome") {
    val rr = List(Region("zsdf", 10, 30), Region("zsdf", 40, 60), Region("zsdf", 90, 96))
    val rr2 = List(Region("zsdf", 12, 14), Region("zsdf", 8, 12), Region("zsdf", 0, 5), Region("zsdf", 18, 42), Region("zsdf", 45, 50), Region("zsdf", 60, 80)).sortBy(_.from)

  }

}
