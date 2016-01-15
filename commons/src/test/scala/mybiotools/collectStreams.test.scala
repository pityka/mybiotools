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
import org.scalatest.PrivateMethodTester
import org.scalatest.Matchers
import org.scalatest.prop.Checkers
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.{ Arbitrary, Gen }

class CollectStreamsSpec extends FunSpec with Matchers with PrivateMethodTester with Checkers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(minSuccessful = 1000, maxDiscarded = 10000)

  val mapfun = (x: Int, y: IndexedSeq[Int]) => y.sum

  describe("collect and map iterators") {
    it("empty") {
      collectStreamsAndMap(List(List[(Int, Int)]().iterator))(mapfun).hasNext should be(false)
    }
    it("trivial 1 sample") {
      collectStreamsAndMap(List(List[(Int, Int)](1 -> 3).iterator))(mapfun).next should be(3)
    }
    it("trivial 2 sample") {
      collectStreamsAndMap(List(List[(Int, Int)](1 -> 1).iterator, List(1 -> 3).iterator))(mapfun).toList should be(List(4))
    }
    it("trivial 2 sample, 2") {
      collectStreamsAndMap(List(List[(Int, Int)](1 -> 1, 2 -> 5).iterator, List(1 -> 3).iterator))(mapfun).toList should be(List(4, 5))
    }
    it("no common keys") {
      collectStreamsAndMap(List(List[(Int, Int)](2 -> 5).iterator, List(1 -> 3).iterator))(mapfun).toList should be(List(5, 3))
    }
    it("no common keys 3 sample") {
      collectStreamsAndMap(List(List[(Int, Int)](2 -> 5).iterator, List(1 -> 3).iterator, List(3 -> 5).iterator))(mapfun).toList should be(List(5, 3, 5))
    }
    it("no common keys 3 sample, multiple keys in 1 iterator") {
      collectStreamsAndMap(List(List[(Int, Int)](2 -> 5, 2 -> 5).iterator, List(1 -> 3, 1 -> 3).iterator, List(3 -> 5, 3 -> 5).iterator))(mapfun).toList should be(List(5, 3, 5))
    }
    it("issue 2") {
      collectStreamsAndMap(List(List[(Int, Int)](0 -> 1547901679, 0 -> 1547901679).iterator, List(-1 -> -1, 1 -> 1, 0 -> 0, -1 -> -1, 1 -> 1, 0 -> 0).iterator))(mapfun).toList should be(List(1547901679, -1, 1))
    }
    it("issue 3") {
      collectStreamsAndMap(List(List[(Int, Int)](0 -> 0, 5 -> 0, 1 -> 0, 2 -> 0, 3 -> 0, 4 -> 0, 0 -> 0, 5 -> 0, 1 -> 0, 2 -> 0, 3 -> 0, 4 -> 0).iterator, List(0 -> 0, 5 -> 0, 1 -> 0, 2 -> 0, 3 -> 0, 4 -> 0, 0 -> 0, 5 -> 0, 1 -> 0, 2 -> 0, 3 -> 0, 4 -> 0).iterator))(mapfun).toList should be(List(0, 0, 0, 0, 0, 0))
    }
  }

  describe("generated data") {
    it("same as addmaps, pairwise ") {

      check { (a: Map[Int, Int], b: Map[Int, Int]) =>
        collectStreamsAndMap(List(a.toSeq.iterator, b.toSeq.iterator))(mapfun).toSet ==
          addMaps(a, b)(_ + _).toList.map(_._2).toSet
      }
    }

    it("same as addmaps, pairwise, with dups ") {

      check { (a: Map[Int, Int], b: Map[Int, Int]) =>
        val v1 = collectStreamsAndMap(List((a.toSeq ++ a.toSeq).iterator, (b.toSeq ++ b.toSeq).iterator))(mapfun).toList
        val v2 = addMaps(a, b)(_ + _).toList.map(_._2)
        if (v1.toSet != v2.toSet) {
          println(a + " " + b + " " + v1 + " / " + v2)
          false
        } else true
      }
    }

    it("same as addmaps, pairwise small maps small integers") {
      implicit lazy val smallMap: Arbitrary[Map[Int, Int]] = Arbitrary(Gen.containerOfN[List, (Int, Int)](100, Gen.choose(0, 5).flatMap(g1 => Gen.choose(0, 100).map(g2 => (g1, g2)))).map(_.toMap))
      check { (a: Map[Int, Int], b: Map[Int, Int]) =>
        collectStreamsAndMap(List((a.toSeq ++ a.toSeq).toSeq.iterator, (b.toSeq ++ b.toSeq).iterator))(mapfun).toSet ==
          addMaps(a, b)(_ + _).toList.map(_._2).toSet
      }
    }

    it("same as addmaps, n-wise small maps small integers") {
      implicit lazy val smallMap: Arbitrary[Map[Int, Int]] = Arbitrary(Gen.containerOfN[List, (Int, Int)](100, Gen.choose(0, 5).flatMap(g1 => Gen.choose(0, 100).map(g2 => (g1, g2)))).map(_.toMap))
      check { (a: List[Map[Int, Int]]) =>
        val result = collectStreamsAndMap(a.map(_.toList.iterator))(mapfun).toSet
        if (a.isEmpty) result.isEmpty
        else
          result == a.reduce((x, y) => addMaps(x, y)(_ + _)).toList.map(_._2).toSet
      }
    }

    it("same as addmaps, n-wise small maps small integers dups  ") {
      implicit lazy val smallMap: Arbitrary[Map[Int, Int]] = Arbitrary(Gen.containerOfN[List, (Int, Int)](100, Gen.choose(0, 5).flatMap(g1 => Gen.choose(0, 100).map(g2 => (g1, g2)))).map(_.toMap))
      check { (a: List[Map[Int, Int]]) =>
        val result = collectStreamsAndMap(a.map(x => (x.toSeq ++ x.toSeq ++ x.toSeq).toList.iterator))(mapfun).toSet
        if (a.isEmpty) result.isEmpty
        else
          result == a.reduce((x, y) => addMaps(x, y)(_ + _)).toList.map(_._2).toSet
      }
    }

    it("same as addmaps, n-wise small maps small integers 2") {
      implicit lazy val smallMap: Arbitrary[Map[Int, Int]] = Arbitrary(Gen.containerOfN[List, (Int, Int)](100, Gen.choose(0, 2).flatMap(g1 => Gen.choose(0, 100).map(g2 => (g1, g2)))).map(_.toMap))
      check { (a: List[Map[Int, Int]]) =>
        val result = collectStreamsAndMap(a.map(_.toList.iterator))(mapfun).toSet
        if (a.isEmpty) result.isEmpty
        else
          result == a.reduce((x, y) => addMaps(x, y)(_ + _)).toList.map(_._2).toSet
      }
    }

  }

}