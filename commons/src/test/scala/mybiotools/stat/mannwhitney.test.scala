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
package stat
import MannWhitneyU._

import org.scalatest.FunSpec
import org.scalatest.Matchers

class MannWhitneySpec extends FunSpec with Matchers {

  describe("sum of tied ranks of left") {
    it("(1L,1L,1,1) should be 5") {
      mannWhitneyU(List(1, 1), List(1, 1)).sumRanksOfLeft should equal(5)
    }
    it("(1L,1L,1L,3) should be 6") {
      mannWhitneyU(List(1, 1, 1), List(3)).sumRanksOfLeft should equal(6)
    }
    it("(1L,2L,3L,4,5,6) should be 6") {
      mannWhitneyU(List(1, 2, 3), List(4, 5, 6)).sumRanksOfLeft should equal(6)
    }
    it("(1,2,3,4L,5L,6L) should be 9") {
      mannWhitneyU(List(4, 5, 6), List(1, 2, 3)).sumRanksOfLeft should equal(15)
    }
    it("(1L,2,3L,4,5L,6) should be 3") {
      mannWhitneyU(List(1, 3, 5), List(2, 4, 6)).sumRanksOfLeft should equal(9)
    }
    it("(1L,2L,3L,3,5,6) should be 6.5") {
      mannWhitneyU(List(1, 2, 3), List(3, 5, 6)).sumRanksOfLeft should equal(6.5)
    }
    it("(1L,2,3,3) should be 1") {
      mannWhitneyU(List(1), List(2, 3, 3)).sumRanksOfLeft should equal(1)
    }
    it("(1,2L,3L,3L) should be 9") {
      mannWhitneyU(List(2, 3, 3), List(1)).sumRanksOfLeft should equal(9)
    }
    it("1,2L,2,2L,2L should be 10.5") {
      mannWhitneyU(List(2, 2, 2), List(1, 2)).sumRanksOfLeft should equal(10.5)
    }
  }

  describe("test statistic U1 ") {
    it("(1,2,3,4L,5L,6L) should be 9") {
      mannWhitneyU(List(4, 5, 6), List(1, 2, 3)).uLeft should equal(9)
    }
    it("(1L,2,3L,4,5L,6) should be 3") {
      mannWhitneyU(List(1, 3, 5), List(2, 4, 6)).uLeft should equal(3)
    }
    it("(1L,1L,1,1) should be 2") {
      val w = mannWhitneyU(List(1, 1), List(1, 1))
      w.uLeft should equal(2)
    }
    it("(1L,1L,1L) should be 0") {
      val w = mannWhitneyU(List(1, 1, 1), List(3))
      w.uLeft should equal(0)

    }
    it("(1L,2L,3L,4,5,6) should be 0") {
      val w = mannWhitneyU(List(1, 2, 3), List(4, 5, 6))
      w.uLeft should equal(0)

    }
    it("(1L,2L,3L,3,5,6) should be 0.5") {
      val w = mannWhitneyU(List(1, 2, 3), List(3, 5, 6))
      w.uLeft should equal(0.5)

    }
    it("(1L,2,3,3) should be 0") {
      val w = mannWhitneyU(List(1), List(2, 3, 3))
      w.uLeft should equal(0)

    }
    it("(1,2L,3L,3L) should be 3") {
      val w = mannWhitneyU(List(2, 3, 3), List(1))
      w.uLeft should equal(3)

    }
    it("1,2L,2,2L,2L should be 4.5") {
      val w = mannWhitneyU(List(2, 2, 2), List(1, 2))
      w.uLeft should equal(4.5)
    }
    it("200 random number should yield 4682") {
      val rand = new util.Random(1)
      val x1 = 1 to 100 map (x => rand.nextDouble)
      val x2 = 1 to 100 map (x => rand.nextDouble)
      val w = mannWhitneyU(x1, x2)
      w.uLeft should equal(4682)
    }
  }

  describe(" p value by normal approximation") {

    val rand = new util.Random(1)
    val x1 = 1 to 100 map (x => rand.nextDouble)
    val x2 = 1 to 100 map (x => rand.nextDouble)
    it("200 random number") {

      val w = mannWhitneyU(x1, x2)
      w.uLeft should equal(4682)
      val p = mannWhitneyUTestXLessThanY(x1, x2)
      math.abs(p.pValue - 0.2189) should be < 0.001
    }
    it("200 random number reverse") {

      val w = mannWhitneyU(x2, x1)

      w.uLeft should equal(5318)
      val p = mannWhitneyUTestXLessThanY(x2, x1)
      math.abs(p.pValue - 0.7818) should be < 0.001
    }
  }
  describe("apache commons test cases") {
    val exampleData = Array[Double](20, 17, 30, 42.3, 17, 50, Double.NegativeInfinity, 17);
    val tiesFirst = Array[Double](0, 0, 2, 1, 4);
    val tiesLast = Array[Double](4, 4, 1, 0);
    val multipleTies = Array[Double](3, 2, 5, 5, 6, 6, 1);
    val allSame = Array[Double](0, 0, 0, 0);

    it("case 1 ") {
      mannWhitneyU(exampleData, List(Double.MaxValue)).sumRanksOfLeft should equal(36)
    }
    it("case 2 ") {
      mannWhitneyU(tiesFirst, List(Double.MaxValue)).sumRanksOfLeft should equal(15)
    }
    it("case 3 ") {
      mannWhitneyU(tiesLast, List(Double.MaxValue)).sumRanksOfLeft should equal(10)
    }
    it("case 4 ") {
      mannWhitneyU(multipleTies, List(Double.MaxValue)).sumRanksOfLeft should equal(28)
    }
    it("case 5 ") {
      mannWhitneyU(allSame, List(Double.MaxValue)).sumRanksOfLeft should equal(10)
    }
  }

  //

  describe("benchmark") {

    val rand = new util.Random(1)
    val x1 = 1 to 100000 map (x => rand.nextDouble)
    val x2 = 1 to 100000 map (x => rand.nextDouble)
    ignore("200k random number") {
      val avgtime = (1 to 20 map { i =>
        val time = System.nanoTime
        val w = mannWhitneyU(x1, x2)
        (System.nanoTime - time) / 1E9
      }).sum / 20.0
      println(avgtime)
    }
  }

}