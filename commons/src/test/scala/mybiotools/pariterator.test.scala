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
import collection.immutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global

class ParIteratorSpec extends FunSpec with Matchers {

  describe("test sequence reduce") {
    it("empty") {
      intercept[IllegalArgumentException](ParIterator.reduce(List[Int]().iterator, 1)(_ + _))
      intercept[IllegalArgumentException](ParIterator.reduce(List[Int]().iterator, 10)(_ + _))
    }
    it("1") {
      ParIterator.reduce(List[Int](1).iterator, 1)(_ + _) should equal(1)
      ParIterator.reduce(List[Int](1).iterator, 10)(_ + _) should equal(1)
    }
    it("2") {
      ParIterator.reduce(List[Int](1, 2).iterator, 1)(_ + _) should equal(3)
      ParIterator.reduce(List[Int](1, 2).iterator, 10)(_ + _) should equal(3)
    }
    it("3") {
      ParIterator.reduce(List[Int](1, 2, 3).iterator, 1)(_ + _) should equal(6)
      ParIterator.reduce(List[Int](1, 2, 3).iterator, 10)(_ + _) should equal(6)
    }
    it("100000k") {
      ParIterator.reduce(List[Int](1 to 100000: _*).iterator, 1)(_ + _) should equal(100000 * 100001 / 2)
      ParIterator.reduce(List[Int](1 to 100000: _*).iterator, 10)(_ + _) should equal(100000 * 100001 / 2)
    }
    it("20 slow") {
      val t1 = System.nanoTime
      ParIterator.reduce(List[Int](1 to 20: _*).iterator, 1) { (x, y) => println(x + " " + y); Thread.sleep(1000); println(x + " " + y + "end"); x + y } should equal(20 * 21 / 2)
      val t2 = System.nanoTime
      ParIterator.reduce(List[Int](1 to 20: _*).iterator, 10) { (x, y) => println(x + " " + y); Thread.sleep(1000); println(x + " " + y + "end"); x + y } should equal(20 * 21 / 2)
      val t3 = System.nanoTime
      (t3 - t2) should be < (t2 - t1)
    }

  }

  describe("test sequence") {
    it(" iterator with state 1 ") {
      var l = List[Int]()
      ParIterator.map(List[Int](1 to 1000: _*).iterator.filter { i =>

        l = i :: l
        true
      }, 1)(x => x).toList should equal(1 to 1000 toList)

      l.reverse should equal((1 to 1000).toList)

    }

    it(" iterator with state 10 ") {
      var l = List[Int]()
      ParIterator.map(List[Int](1 to 1000: _*).iterator.filter { i =>

        l = i :: l
        true
      }, 10)(x => x).toList should equal(1 to 1000 toList)

      l.reverse should equal((1 to 1000).toList)

    }

    it("empty") {
      ParIterator.map(List[Int]().iterator, 1)(x => x).toList should equal(Nil)
      ParIterator.map(List[Int]().iterator, 10)(x => x).toList should equal(Nil)
    }
    it("1") {
      ParIterator.map(List[Int](1).iterator, 1)(x => x.toString).toList should equal(List("1"))
      ParIterator.map(List[Int](1).iterator, 10)(x => x.toString).toList should equal(List("1"))
    }
    it("3") {
      ParIterator.map(List[Int](1, 2, 3).iterator, 1)(x => x.toString).toList should equal(List("1", "2", "3"))
      ParIterator.map(List[Int](1, 2, 3).iterator, 10)(x => x.toString).toList should equal(List("1", "2", "3"))
      ParIterator.map(List[Int](1, 2, 3).iterator, 2)(x => x.toString).toList should equal(List("1", "2", "3"))
    }
    it("10k") {
      ParIterator.map(List[Int](1 to 10000: _*).iterator, 1)(x => x.toString).toList should equal(List[Int](1 to 10000: _*).map(_.toString))
      ParIterator.map(List[Int](1 to 10000: _*).iterator, 10)(x => x.toString).toList should equal(List[Int](1 to 10000: _*).map(_.toString))
      ParIterator.map(List[Int](1 to 10000: _*).iterator, 2)(x => x.toString).toList should equal(List[Int](1 to 10000: _*).map(_.toString))
    }
    it("error") {
      intercept[RuntimeException](ParIterator.map(List[Int](1).iterator, 1)(x => throw new RuntimeException("dsfd")).toList)
    }

  }

  describe("test sequence unordered") {
    it("empty") {
      ParIterator.map(List[Int]().iterator, 1, false)(x => x).toList should equal(Nil)
      ParIterator.map(List[Int]().iterator, 10, false)(x => x).toList should equal(Nil)
    }
    it("1") {
      ParIterator.map(List[Int](1).iterator, 1, false)(x => x.toString).toList should equal(List("1"))
      ParIterator.map(List[Int](1).iterator, 10, false)(x => x.toString).toList should equal(List("1"))
    }
    it("3") {
      ParIterator.map(List[Int](1, 2, 3).iterator, 1, false)(x => x.toString).toList should equal(List("1", "2", "3"))
      ParIterator.map(List[Int](1, 2, 3).iterator, 10, false)(x => x.toString).toList.sorted should equal(List("1", "2", "3").sorted)
      ParIterator.map(List[Int](1, 2, 3).iterator, 2, false)(x => x.toString).toList.sorted should equal(List("1", "2", "3").sorted)
    }
    it("10k") {
      ParIterator.map(List[Int](1 to 10000: _*).iterator, 1, false)(x => x.toString).toList.sorted should equal(List[Int](1 to 10000: _*).map(_.toString).sorted)
      ParIterator.map(List[Int](1 to 10000: _*).iterator, 10, false)(x => x.toString).toList.sorted should equal(List[Int](1 to 10000: _*).map(_.toString).sorted)
      ParIterator.map(List[Int](1 to 10000: _*).iterator, 10, false)(x => x.toString).toList shouldNot equal(List[Int](1 to 10000: _*).map(_.toString))
      ParIterator.map(List[Int](1 to 10000: _*).iterator, 2, false)(x => x.toString).toList.sorted should equal(List[Int](1 to 10000: _*).map(_.toString).sorted)
    }
    it("error") {
      intercept[RuntimeException](ParIterator.map(List[Int](1).iterator, 1, false)(x => throw new RuntimeException("dsfd")).toList)
    }

  }
}
