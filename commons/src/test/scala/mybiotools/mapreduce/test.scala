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

package mybiotools.mapreduce

import org.scalatest.FunSpec
import org.scalatest.PrivateMethodTester
import org.scalatest.Matchers
import org.scalatest.prop.Checkers
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.{ Arbitrary, Gen }
import scala.util._

class MapReduceSpec extends FunSpec with Matchers with PrivateMethodTester with Checkers {

  object MR extends MapReduceScheme[Int, Int] {
    val map = (i: Int) => i
    val reduce = (i: Int, x: Int) => i + x
  }

  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(minSuccessful = 10, maxDiscarded = 1000)

  describe("trivi") {
    it("empty") {
      MapReduceTraversal.traverse[Int, Int](List[Int](), MR).toString should equal(Failure(new java.lang.IllegalArgumentException("Empty iterator")).toString)
    }
    it("1, 1 map 1 reduce") {
      (MapReduceTraversal.traverse[Int, Int](List(1), MR, 1, 1)) should equal(Success(1))
    }
    it("1,2,3 1 map 1 reduce") {
      (MapReduceTraversal.traverse[Int, Int](List(1, 2, 3), MR, 1, 1)) should equal(Success(6))
    }
    it("1, 2 map 2 reduce") {
      (MapReduceTraversal.traverse[Int, Int](List(1), MR, 2, 2)) should equal(Success(1))
    }
    it("1-10, 1 map 1 reduce") {
      (MapReduceTraversal.traverse[Int, Int](List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), MR, 1, 1)) should equal(Success(55))
    }

    it("stress slow reduce ") {
      object MR2 extends MapReduceScheme[Int, Int] {
        val map = (i: Int) => i
        val reduce = (i: Int, x: Int) => { Thread.sleep(100); i + x }
      }
      val n = 1000
      (MapReduceTraversal.traverse[Int, Int](1 to n toList, MR2, 1, 1)) should equal(Success(n * (n + 1) / 2))
    }
  }

}