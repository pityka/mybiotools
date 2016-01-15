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

import mybiotools.gwascommons._
import mybiotools.gwascommons.associationresults._
import LDPruning._
import mybiotools.stringstore._

import org.scalatest.FunSpec
import org.scalatest.Matchers

import org.saddle._

class LDPruningSpec extends FunSpec with Matchers {

  val as1 = Locus(StringStore("rs1"), GenomicLocation(1, 0))
  val as2 = Locus(StringStore("rs2"), GenomicLocation(2, 0))
  val as3 = Locus(StringStore("rs3"), GenomicLocation(12, 0))
  val as4 = Locus(StringStore("rs4"), GenomicLocation(17, 0))

  val correlationMatrix = Frame(
    as1.name.value -> Series(as1.name.value -> 1.0, as2.name.value -> 0.1, as3.name.value -> 0.5, as4.name.value -> 0.1),
    as2.name.value -> Series(as1.name.value -> 0.1, as2.name.value -> 1.0, as3.name.value -> 0.8, as4.name.value -> 0.9),
    as3.name.value -> Series(as1.name.value -> 0.5, as2.name.value -> 0.8, as3.name.value -> 1.0, as4.name.value -> 0.1),
    as4.name.value -> Series(as1.name.value -> 0.1, as2.name.value -> 0.9, as3.name.value -> 0.1, as4.name.value -> 1.0)
  )

  val assocresults = List(as1, as2, as3, as4)

  describe("prune iterator") {

    case class A(name: String8) extends HasName
    val list = List(
      A(s8"1") -> Array(0.0f, 1.0f, 2.0f, 3.0f, 4.0f),
      A(s8"2") -> Array(4.0f, 3.0f, 2.0f, 1.0f, 0.0f),
      A(s8"3") -> Array(10.0f, 11.0f, 12.0f, 13.0f, 14.0f),
      A(s8"4") -> Array(-10.0f, 21.0f, 22.0f, -33.0f, 4.0f),
      A(s8"5") -> Array(0.0f, 0.0f, 0.0f, 0.0f, 0.1f)
    )
    // val rsq = list.combinations(2).map(l => (l(0)._1, l(1)._1) -> mybiotools.stat.RSquared.rsquared(l(0)._2, l(1)._2, -9f)).toList
    // println(rsq)
    val map = collection.Map(
      (s8"1") -> GenomicLocation(1, 1),
      (s8"2") -> GenomicLocation(2, 1),
      (s8"3") -> GenomicLocation(3, 1),
      (s8"4") -> GenomicLocation(4, 1),
      (s8"5") -> GenomicLocation(5, 1)
    )
    val map2 = collection.Map(
      (s8"1") -> GenomicLocation(10, 1),
      (s8"2") -> GenomicLocation(20, 1),
      (s8"3") -> GenomicLocation(30, 1),
      (s8"4") -> GenomicLocation(40, 1),
      (s8"5") -> GenomicLocation(50, 1)
    )
    it("small example") {
      pruneIterator(list.iterator, map, -9f, 1000, 1.0, Set(), Nil).toList.map(_._1).toSet.size should equal(5)
      pruneIterator(list.iterator, map, -9f, 1000, 0.3, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "4"))
      pruneIterator(list.iterator, map, -9f, 1000, 0.03, Set(), Nil).toList.map(_._1.name.value) should equal(List("1"))
      pruneIterator(list.iterator, map, -9f, 1000, 0.003, Set(), Nil).toList.map(_._1.name.value) should equal(List("1"))
      pruneIterator(list.iterator, map, -9f, 1000, 0.6, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "4", "5"))

      pruneIterator(list.iterator, map, -9f, 1000, 0.6, Set(), List(Array(0.0f, 1.0f, 2.0f, 3.0f, 4.0f))).toList.map(_._1.name.value) should equal(List("4", "5"))

      pruneIterator(list.iterator, map, -9f, 1, 1.0, Set(), Nil).toList.map(_._1).toSet.size should equal(5)
      pruneIterator(list.iterator, map, -9f, 1, 0.3, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "3", "4", "5"))
      pruneIterator(list.iterator, map, -9f, 1, 0.03, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "3", "5"))
      pruneIterator(list.iterator, map, -9f, 1, 0.003, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "3", "5"))
      pruneIterator(list.iterator, map, -9f, 1, 0.6, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "3", "4", "5"))

      pruneIterator(list.iterator, map, -9f, 2, 1.0, Set(), Nil).toList.map(_._1).toSet.size should equal(5)
      pruneIterator(list.iterator, map, -9f, 2, 0.3, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "4", "5"))
      pruneIterator(list.iterator, map, -9f, 2, 0.03, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "4", "5"))
      pruneIterator(list.iterator, map, -9f, 2, 0.003, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "4"))
      pruneIterator(list.iterator, map, -9f, 2, 0.6, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "4", "5"))

      pruneIterator(list.iterator, map2, -9f, 1000, 1.0, Set(), Nil).toList.map(_._1).toSet.size should equal(5)
      pruneIterator(list.iterator, map2, -9f, 1000, 0.3, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "4"))
      pruneIterator(list.iterator, map2, -9f, 1000, 0.03, Set(), Nil).toList.map(_._1.name.value) should equal(List("1"))
      pruneIterator(list.iterator, map2, -9f, 1000, 0.003, Set(), Nil).toList.map(_._1.name.value) should equal(List("1"))
      pruneIterator(list.iterator, map2, -9f, 1000, 0.6, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "4", "5"))

      pruneIterator(list.iterator, map2, -9f, 10, 1.0, Set(), Nil).toList.map(_._1).toSet.size should equal(5)
      pruneIterator(list.iterator, map2, -9f, 10, 0.3, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "3", "4", "5"))
      pruneIterator(list.iterator, map2, -9f, 10, 0.03, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "3", "5"))
      pruneIterator(list.iterator, map2, -9f, 10, 0.003, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "3", "5"))
      pruneIterator(list.iterator, map2, -9f, 10, 0.6, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "3", "4", "5"))

      pruneIterator(list.iterator, map2, -9f, 15, 1.0, Set(), Nil).toList.map(_._1).toSet.size should equal(5)
      pruneIterator(list.iterator, map2, -9f, 15, 0.3, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "3", "4", "5"))
      pruneIterator(list.iterator, map2, -9f, 15, 0.03, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "3", "5"))
      pruneIterator(list.iterator, map2, -9f, 15, 0.003, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "3", "5"))
      pruneIterator(list.iterator, map2, -9f, 15, 0.6, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "3", "4", "5"))

      pruneIterator(list.iterator, map2, -9f, 2, 1.0, Set(), Nil).toList.map(_._1).toSet.size should equal(5)
      pruneIterator(list.iterator, map2, -9f, 2, 0.3, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "2", "3", "4", "5"))
      pruneIterator(list.iterator, map2, -9f, 2, 0.03, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "2", "3", "4", "5"))
      pruneIterator(list.iterator, map2, -9f, 2, 0.003, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "2", "3", "4", "5"))
      pruneIterator(list.iterator, map2, -9f, 2, 0.6, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "2", "3", "4", "5"))

      pruneIterator(list.iterator, map2, -9f, 20, 1.0, Set(), Nil).toList.map(_._1).toSet.size should equal(5)
      pruneIterator(list.iterator, map2, -9f, 20, 0.3, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "4", "5"))
      pruneIterator(list.iterator, map2, -9f, 20, 0.03, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "4", "5"))
      pruneIterator(list.iterator, map2, -9f, 20, 0.003, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "4"))
      pruneIterator(list.iterator, map2, -9f, 20, 0.6, Set(), Nil).toList.map(_._1.name.value) should equal(List("1", "4", "5"))

    }
  }

  describe("ldpruning") {
    it("small example") {
      val x = prune(correlationMatrix, assocresults, 0.2)
      x should equal(Vector(as1, as2))
    }
  }

  describe("readSNPCorrelations") {
    it("trivial") {
      val iter = List(
        LDPoint(as1, as2, 0.5f)
      ).iterator
      val expected = Frame(
        as1.name.value -> Series(as2.name.value -> 0.5),
        as2.name.value -> Series(as1.name.value -> 0.5)
      )
      val t = readSNPCorrelations(iter)
      t.sortedCIx.sortedRIx should equal(expected.sortedCIx.sortedRIx)
    }

    it("small example") {
      val iter = List(
        LDPoint(as1, as1, 1.0),
        LDPoint(as1, as2, 0.1),
        LDPoint(as1, as3, 0.5),
        LDPoint(as1, as4, 0.1),
        LDPoint(as2, as1, 0.1),
        LDPoint(as2, as2, 1.0),
        LDPoint(as2, as3, 0.8),
        LDPoint(as2, as4, 0.9),
        LDPoint(as3, as1, 0.5),
        LDPoint(as3, as2, 0.8),
        LDPoint(as3, as3, 1.0),
        LDPoint(as3, as4, 0.1),
        LDPoint(as4, as1, 0.1),
        LDPoint(as4, as2, 0.9),
        LDPoint(as4, as3, 0.1),
        LDPoint(as4, as4, 1.0)
      ).iterator

      val t = readSNPCorrelations(iter).sortedCIx.sortedRIx
      t should equal(correlationMatrix)
    }

    it("small example lower triangle") {
      val list = List(
        LDPoint(as1, as1, 1.0),
        LDPoint(as1, as2, 0.1),
        LDPoint(as1, as3, 0.5),
        LDPoint(as1, as4, 0.1),
        LDPoint(as2, as2, 1.0),
        LDPoint(as2, as3, 0.8),
        LDPoint(as2, as4, 0.9),
        LDPoint(as3, as3, 1.0),
        LDPoint(as3, as4, 0.1),
        LDPoint(as4, as4, 1.0)
      )

      readSNPCorrelations(list.iterator).sortedCIx.sortedRIx should equal(correlationMatrix)
    }

    //   it("small example lower triangle 2") {
    //     val list = List(
    //       LDPoint(GenomicLocation(1, 0), GenomicLocation(1, 0), 1.0f),
    //       LDPoint(GenomicLocation(1, 0), GenomicLocation(12, 0), 0.5f),
    //       LDPoint(GenomicLocation(2, 0), GenomicLocation(1, 0), 0.1f),
    //       LDPoint(GenomicLocation(2, 0), GenomicLocation(2, 0), 1.0f),
    //       LDPoint(GenomicLocation(2, 0), GenomicLocation(12, 0), 0.8f),
    //       LDPoint(GenomicLocation(2, 0), GenomicLocation(17, 0), 0.9f),
    //       LDPoint(GenomicLocation(12, 0), GenomicLocation(12, 0), 1.0f),
    //       LDPoint(GenomicLocation(12, 0), GenomicLocation(17, 0), 0.1f),
    //       LDPoint(GenomicLocation(17, 0), GenomicLocation(17, 0), 1.0f),
    //       LDPoint(GenomicLocation(17, 0), GenomicLocation(1, 0), 0.1f)
    //     )

    //     readSNPCorrelations(list.iterator).foreach { p =>
    //       p._2 should equal(correlationMatrix(p._1))
    //     }

    //     readSNPCorrelations(list.iterator) should equal(correlationMatrix)
    //   }
  }
}