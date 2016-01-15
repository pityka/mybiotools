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

package mybiotools.stat

import org.scalatest.FunSpec
import org.scalatest.PrivateMethodTester
import org.scalatest.Matchers
import org.scalatest.prop.Checkers
import org.scalacheck.Prop._
import org.scalacheck._
import org.apache.commons.math3.random.RandomDataGenerator
import org.apache.commons.math3.distribution.NormalDistribution
import FitCDF._

class FitCDFSpec extends FunSpec with Matchers with PrivateMethodTester with Checkers {

  describe("cdf") {
    it("get cumulative") {
      FitCDF.cumulative(List(1d, 2d, 3d, 4d)) should equal(CumulativeRelativeFrequencies(IndexedSeq(1d -> 0.25, 2d -> 0.5, 3d -> 0.75, 4d -> 1.0)))
      FitCDF.cumulative(List(1d, 1d, 1d, 1d, 1d, 2d, 3d, 4d)) should equal(CumulativeRelativeFrequencies(IndexedSeq(1d -> 0.625, 2d -> 0.75, 3d -> 0.875, 4d -> 1.0)))
      FitCDF.cumulative(List(1d, 1d, 1d, 1d, 1d, 2d, 3d, 4d, 4d, 4d)) should equal(CumulativeRelativeFrequencies(IndexedSeq(1d -> 0.5, 2d -> 0.6, 3d -> 0.7, 4d -> 1.0)))
    }
    it("evaluate cdf") {
      val c = FitCDF.cumulative(List(1d, 1d, 1d, 1d, 1d, 2d, 3d, 4d, 4d, 4d))
      c.cdf(0.0) should equal(0.0)
      c.cdf(0.5) should equal(0.0)
      c.cdf(1.0) should equal(0.5)
      c.cdf(1.1) should equal(0.5)
      c.cdf(1.2) should equal(0.5)
      c.cdf(1.9999) should equal(0.5)
      c.cdf(2d) should equal(0.6)
      c.cdf(2.99d) should equal(0.6)
      c.cdf(3.5d) should equal(0.7)
      c.cdf(4.5d) should equal(1.0)
      c.cdf(45999d) should equal(1.0)
    }

  }

  describe("normal distribution") {
    it("sdfs") {

      val dist = new NormalDistribution(0, 1)
      val data = dist.sample(10000)
      val fitted = FitCDF.fitCDF(
        data,
        (param: Seq[Double], loc: Double) => jdistlib.Normal.cumulative(loc, param(0), param(1)),
        Vector(3, 10),
        Vector(-20, -20),
        Vector(20, 20)
      )

      math.abs(fitted._1(0)) should be < 0.05
      math.abs(fitted._1(1) - 1.0) should be < 0.03

    }

  }

}