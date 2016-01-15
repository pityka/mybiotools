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
import mybiotools.stringstore._
import org.scalacheck.{ Arbitrary, Gen }

class GenomicRegionSpec extends FunSpec with Matchers with PrivateMethodTester with Checkers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(minSuccessful = 1000, maxDiscarded = 10000)

  implicit def arbRegion[Region] =
    Arbitrary {
      for {
        chr <- Gen.choose(1, 22)
        from <- Gen.choose(0, 1000)
        to <- Gen.choose(from, 1000)
      } yield {
        Region(chr, from, to)
      }
    }

  val gl4 = GenomicLocation(4, 0)
  val gl0 = GenomicLocation(1, 0)
  val gl10 = GenomicLocation(10, 0)
  val gl11 = GenomicLocation(11, 0)
  val gl32 = GenomicLocation(32, 0)

  describe("Region") {

    it("chunkToMaxSize has the same size as inputs") {
      check((a: Region, b: Int) => (b > 0) ==> (a.chunkToMaxSize(b).map(_.size).sum == a.size))
    }

    it("chunkToMaxSize chunks to smaller chunks") {
      check((a: Region, b: Int) => (b > 0) ==> (a.chunkToMaxSize(b).forall(_.size <= b)))
    }

    it("chunkToMaxSize chunks to smaller chunks, RegionSet 10") {
      check { (a: List[Region]) =>

        val rs = GenericRegionSet(a)
        val res = rs.chunkToMaxSize(10)
        val x = res.forall(_.map(_.size).sum <= 10)
        if (!x) { println(res) }

        x

      }
    }

    it("chunkToMaxSize chunks to smaller chunks, RegionSet 30") {
      check { (a: List[Region]) =>

        val rs = GenericRegionSet(a)
        rs.chunkToMaxSize(30).forall(_.map(_.size).sum <= 30)

      }
    }

    it("chunkToMaxSize chunks to smaller chunks, RegionSet 2") {
      check { (a: List[Region]) =>

        val rs = GenericRegionSet(a)
        rs.chunkToMaxSize(2).forall(_.map(_.size).sum <= 2)

      }
    }

    it("chunkToMaxSize has the same size as inputs, RegionSet 10") {
      check { (a: List[Region]) =>
        val max = 10
        val rs = GenericRegionSet(a)
        val asum = rs.chunkToMaxSize(max).map(_.map(_.size).sum).sum
        val bsum = Region.collapse[String8, GenomicLocation, Region](a, 0).map(_.size).sum
        if (asum != bsum) { println(rs + "," + rs.chunkToMaxSize(max) + "," + a + " " + a.map(_.size) + " " + asum + " " + bsum) }
        asum == bsum

      }
    }

    it("chunkToMaxSize has the same size as inputs, RegionSet, random") {
      check { (a: List[Region], max: Int) =>
        ((max > 1)) ==> {
          val rs = GenericRegionSet(a)
          val asum = rs.chunkToMaxSize(max).map(_.map(_.size).sum).sum
          val bsum = Region.collapse[String8, GenomicLocation, Region](a, 0).map(_.size).sum
          if (asum != bsum) { println(rs + "," + rs.chunkToMaxSize(max) + "," + a + " " + a.map(_.size) + " " + asum + " " + bsum) }
          asum == bsum
        }
      }
    }

    it("chunkToMaxSize has the same size as inputs, RegionSet 30") {
      check { (a: List[Region]) =>
        val max = 30
        val rs = GenericRegionSet(a)
        val asum = rs.chunkToMaxSize(max).map(_.map(_.size).sum).sum
        val bsum = Region.collapse[String8, GenomicLocation, Region](a, 0).map(_.size).sum
        if (asum != bsum) { println(rs + "," + rs.chunkToMaxSize(max) + "," + a + " " + a.map(_.size) + " " + asum + " " + bsum) }
        asum == bsum

      }
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

    val big = Region(0, 0, 10)
    val small = Region(0, 2, 5)
    val smaller = Region(0, 3, 4)
    val small2 = Region(0, 5, 8)
    val smaller2 = Region(0, 3, 7)
    val big8_16 = Region(0, 8, 16)

    it("[0,10) contains 3")(big contains gl4 should equal(true))
    it("[0,10) contains 0")(big contains gl0 should equal(true))
    it("[0,10) does contains 9")(big contains gl10 should equal(true))
    it("[0,10) does not contain 10")(big contains gl11 should equal(false))
    it("[3,4) contains 3")(smaller contains gl4 should equal(true))
    it("[3,4) not contains 4")(smaller contains GenomicLocation(5, 0) should equal(false))

    it("[0,10) contains [0,10)")(big contains big should equal(true))
    it("[0,10) contains [2,5)")(big contains small should equal(true))
    it("[0,10) contains [3,4)")(big contains smaller should equal(true))
    it("[0,10) contains [5,8)")(big contains small2 should equal(true))
    it("[0,10) contains [3,7)")(big contains smaller2 should equal(true))
    it("[0,10) NOT contain (8,16)")(big contains big8_16 should equal(false))
    it("[0,10) NOT contain (8,16) reverse")(big8_16 contains big should equal(false))

    it("[2,5) contains [3,4)")(small contains smaller should equal(true))
    it("[3,4) NOT contains [2,5)")(smaller contains small should equal(false))

    it("[2,5) U [5,8) == [2,8)")(small.collapse(small2, 0).get should equal(Region(0, 2, 8)))
    it("[5,8) U [2,5) == [2,8)")(small2.collapse(small, 0).get should equal(Region(0, 2, 8)))
    it("(5,8) U (5,8) == [5,8)")(small2.collapse(small2, 0).get should equal(Region(0, 5, 8)))

    it("[2,6) U [6,8) == [2,8)")((Region(0, 2, 6)).collapse(Region(0, 6, 8), 0).get should equal(Region(0, 2, 8)))
    it("[2,5) U [6,8) != [2,8)")(small.collapse(Region(0, 6, 8), 0) should equal(None))
    it("[6,8) U [2,5) != [2,8)")(Region(0, 6, 8).collapse(small, 0) should equal(None))
    it("[6,8) U [2,6) == [2,8)")(Region(0, 6, 8).collapse(Region(0, 2, 6), 0) should equal(Some(Region(0, 2, 8))))

    it("list collapse") {
      Region.collapse[String8, GenomicLocation, Region](List(small, small2), 0) should equal(List(Region(0, 2, 8)))
    }
    it("list collapse same chromosome keeps order") {
      val list = List(Region(0, 1, 2), small, small2, Region(0, 12, 13), Region(0, 15, 16))

      Region.collapseSameChromosome[String8, GenomicLocation, Region](
        list, 0
      ) should equal(List(Region(0, 1, 8), Region(0, 12, 13), Region(0, 15, 16)))
    }

    it("list collapse same chromosome should assert order") {
      val list = List(Region(0, 113, 222), small, small2, Region(0, 12, 13), Region(0, 15, 16))

      evaluating { Region.collapseSameChromosome[String8, GenomicLocation, Region](list, 0) } should produce[AssertionError]
    }
    it("list collapse different chrs") {
      Region.collapse[String8, GenomicLocation, Region](List(small, small2, Region(1, 1, 2)), 0).toSet should equal(Set(Region(0, 2, 8), Region(1, 1, 2)))
    }

  }

  describe("Region intersects") {
    it("[0,10) intersects [9,10)") {
      Region(0, 0, 10) intersects Region(0, 9, 10) should be(true)
    }
    it("[0,10) intersects [9,11)") {
      Region(0, 0, 10) intersects Region(0, 9, 11) should be(true)
    }
    it("[0,10) does not [9,9)") {
      Region(0, 0, 10) intersects Region(0, 9, 9) should be(false)
    }
    it("[10,15) intersects [9,11)") {
      Region(0, 10, 15) intersects Region(0, 9, 11) should be(true)
    }
    it("[10,15) not [9,10)") {
      Region(0, 10, 15) intersects Region(0, 9, 10) should be(false)
    }
    it("[10,15) not [11,13)") {
      Region(0, 10, 15) intersects Region(0, 11, 13) should be(true)
    }
  }

  describe("Region chunk") {
    val r = Region(0, 0, 100)
    it("no need for chunking") {
      r.chunkToMaxSize(1000) should equal(List(r))
    }
    it("special sizes") {

      r.chunkToMaxSize(100) should equal(List(r))
      r.chunkToMaxSize(101) should equal(List(r))
      r.chunkToMaxSize(99) should equal(List(Region(0, 0, 99), Region(0, 99, 100)).reverse)
      evaluating(r.chunkToMaxSize(0)) should produce[IllegalArgumentException]
    }
  }

  describe("Regionset chunk") {
    val r = GenericRegionSet(List(Region(0, 0, 100), Region(0, 110, 200)))
    it("no need for chunking") {
      r.chunkToMaxSize(1000) should equal(List(List(Region(0, 0, 100), Region(0, 110, 200)).reverse))
    }
    it("special sizes") {
      r.chunkToMaxSize(100).toList should equal(List(List(Region(0, 0, 100)), List(Region(0, 110, 200))))
      r.chunkToMaxSize(90).toList should equal(List(List(Region(0, 90, 100)), List(Region(0, 0, 90)), List(Region(0, 110, 200))))
      r.chunkToMaxSize(190).toList should equal(List(List(Region(0, 0, 100), Region(0, 110, 200)).reverse))
      r.chunkToMaxSize(60).toList should equal(List(List(Region(0, 60, 100)), List(Region(0, 0, 60)), List(Region(0, 170, 200)), List(Region(0, 110, 170))))
      evaluating(r.chunkToMaxSize(0)) should produce[IllegalArgumentException]
    }

  }

  describe("Regionset contains region") {
    it("[10,20) U [25,30) no intersect (0,10]") {
      GenericRegionSet(List(Region(0, 10, 20), Region(0, 25, 30)).reverse) intersects Region(0, 0, 10) should be(false)
    }

    it("[10,20) U [25,30) intersect (0,11]") {
      GenericRegionSet(List(Region(0, 10, 20), Region(0, 25, 30)).reverse) intersects Region(0, 0, 11) should be(true)
    }
    it("[10,20) U [25,30) not intersect (20,21]") {
      GenericRegionSet(List(Region(0, 10, 20), Region(0, 25, 30)).reverse) intersects Region(0, 20, 21) should be(false)
    }
    it("[10,20) U [25,30) not intersect (20,25]") {
      GenericRegionSet(List(Region(0, 10, 20), Region(0, 25, 30)).reverse) intersects Region(0, 20, 25) should be(false)
    }
    it("[10,20) U [25,30)  intersect (20,26]") {
      GenericRegionSet(List(Region(0, 10, 20), Region(0, 25, 30)).reverse) intersects Region(0, 20, 26) should be(true)
    }
    it("[10,20) U [25,30] intersect (19,24]") {
      GenericRegionSet(List(Region(0, 10, 20), Region(0, 25, 30)).reverse) intersects Region(0, 19, 24) should be(true)
    }
    it("[10,20) U [25,30] not intersect (19,19]") {
      GenericRegionSet(List(Region(0, 10, 20), Region(0, 25, 30)).reverse) intersects Region(0, 19, 19) should be(false)
    }
  }

  describe("Region subtract") {
    val rr = Region(0, 10, 20)
    val left = Region(0, 1, 9)
    val right = Region(0, 21, 25)
    val leftTouch = Region(0, 1, 11)
    var rightTouch = Region(0, 19, 22)
    val leftOverlap = Region(0, 5, 15)
    val rightOverlap = Region(0, 15, 25)
    val middle = Region(0, 12, 15)
    val middleTouchLeft = Region(0, 10, 14)
    val middleTouchRight = Region(0, 14, 20)
    val over = Region(0, 0, 100)
    it("rr - left")(rr subtract left should equal(List(rr)))
    it("rr - right")(rr subtract right should equal(List(rr)))
    it("rr - leftTouch")(rr subtract leftTouch should equal(List(Region(0, 11, 20))))
    it("rr - rightTouch")(rr subtract rightTouch should equal(List(Region(0, 10, 19))))
    it("rr - leftOverlap")(rr subtract leftOverlap should equal(List(Region(0, 15, 20))))
    it("rr - rightOverlap")(rr subtract rightOverlap should equal(List(Region(0, 10, 15))))
    it("rr - middle")(rr subtract middle should equal(List(Region(0, 10, 12), Region(0, 15, 20))))
    it("rr - middleTouchLeft")(rr subtract middleTouchLeft should equal(List(Region(0, 14, 20))))
    it("rr - middleTouchRight")(rr subtract middleTouchRight should equal(List(Region(0, 10, 14))))
    it("rr - over")(rr subtract over should equal(Nil))
    it("rr - rr")(rr subtract rr should equal(Nil))
  }

  describe("Region subtract same chromosome") {
    val rr = List(Region(0, 10, 30), Region(0, 40, 60), Region(0, 90, 96))
    val rr2 = List(Region(0, 12, 14), Region(0, 8, 12), Region(0, 0, 5), Region(0, 18, 42), Region(0, 45, 50), Region(0, 60, 80)).sortBy(_.from)

    it("should subtract to sorted lists of regions") {
      val subtracted = Region.subtractSameChromosome[String8, GenomicLocation, Region](rr, rr2)

      val expected = List(
        Region(0, 14, 18),
        Region(0, 42, 45),
        Region(0, 50, 60),
        Region(0, 90, 96)
      )
      subtracted should equal(expected)
    }

  }

  describe("RegionSet: [2,5) U [5,8) U [10,20)") {
    val r2_5 = Region(0, 2, 5)
    val r5_8 = Region(0, 5, 8)
    val r10_20 = Region(0, 10, 20)
    val rs = GenericRegionSet(List(r2_5, r5_8, r10_20))

    val r0_1 = Region(0, 0, 1)
    val r30_35 = Region(0, 30, 35)
    val r15_20 = Region(0, 15, 20)
    val rs2 = GenericRegionSet(List(r0_1, r30_35, r15_20))

    val map = PrivateMethod[Map[Int, IndexedSeq[Region]]]('map)
    val res = rs invokePrivate map()

    it("map repr") {

      res should equal(Map(StringStore("0") -> Vector(Region(0, 2, 8), Region(0, 10, 20))))
    }

    it("contains 4")(rs contains gl4 should equal(true))
    it("contains 0")(rs contains gl0 should equal(false))
    it("contains 10")(rs contains gl10 should equal(false))
    it("contains 11")(rs contains gl11 should equal(true))

    it("rs + rs1") {
      val merged = rs + rs2
      val expected = Map(StringStore("0") -> Vector(Region(0, 0, 1), Region(0, 2, 8), Region(0, 10, 20), Region(0, 30, 35)))
      val res = merged invokePrivate map()
      res should equal(expected)
    }
    it("rs - rs1") {
      val rs4 = rs + GenericRegionSet(Region(1, 0, 10) :: Nil)
      val rs3 = rs2 + GenericRegionSet(Region(2, 0, 10) :: Nil)

      val expected = Map(StringStore("0") -> Vector(
        Region(0, 2, 8), Region(0, 10, 15)
      ), StringStore("1") -> Vector(Region(1, 0, 10)))

      val substracted = rs4 - rs3
      val res = substracted invokePrivate map()
      res should equal(expected)
    }
  }

  describe("regionset maskto") {
    val rset = GenericRegionSet(List(Region(1, 0, 10), Region(1, 30, 40)))

    case class GL(genomicLocation: GenomicLocation) extends HasGenomicLocation
    val vec = Vector(
      GL(GenomicLocation(1, 1)),
      GL(GenomicLocation(9, 1)),
      GL(GenomicLocation(10, 1)),
      GL(GenomicLocation(11, 1)),
      GL(GenomicLocation(41, 1)),
      GL(GenomicLocation(31, 1))
    )

    it("simple ") {
      val masked = rset.maskTo(vec.groupBy(_.genomicLocation.chromosomeAsT).mapValues(_.toVector.sortBy(_.genomicLocation.basePairPosition)))
      val expected = Map(s8"1" -> Vector(
        GL(GenomicLocation(1, 1)),
        GL(GenomicLocation(9, 1)),
        GL(GenomicLocation(10, 1)),
        GL(GenomicLocation(31, 1))
      ))
      masked should equal(expected)
    }
    it("empty") {
      val rset2 = GenericRegionSet(List(Region(1, 12, 16)))
      val masked = rset2.maskTo(vec.groupBy(_.genomicLocation.chromosomeAsT).mapValues(_.toVector.sortBy(_.genomicLocation.basePairPosition)))
      masked should equal(Map(s8"1" -> Vector()))
    }
    it("1") {
      val rset = GenericRegionSet(List(Region(1, 25, 35)))
      val masked = rset.maskTo(vec.groupBy(_.genomicLocation.chromosomeAsT).mapValues(_.toVector.sortBy(_.genomicLocation.basePairPosition)))
      masked should equal(Map(s8"1" -> Vector(GL(GenomicLocation(31, 1)))))
    }
    it("empty2") {
      val rset = GenericRegionSet(List(Region(1, 50, 52)))
      val masked = rset.maskTo(vec.groupBy(_.genomicLocation.chromosomeAsT).mapValues(_.toVector.sortBy(_.genomicLocation.basePairPosition)))
      masked should equal(Map(s8"1" -> Vector()))
    }

  }

}
