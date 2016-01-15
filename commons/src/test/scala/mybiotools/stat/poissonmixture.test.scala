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
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.random.Well19937c
import PoissonMixture.Parameters

class PoissonMixtureSpec extends FunSpec with Matchers with PrivateMethodTester with Checkers {

  describe("poisson mixture") {

    it("trivial") {
      val rnd = new Well19937c(1)
      val data = PoissonMixture.generateK(Seq(1000.0 -> 0.05, 100.0 -> 0.15, 1.0 -> 0.8), 10000, rnd).map(_.toInt)
      val estimates = PoissonMixture.emKComponentPoisson(data.toVector, Vector(Parameters(1.0, 0.13724455318670348), Parameters(370.0, 0.7879467240620615), Parameters(739.0, 0.07480872275123505)))
      estimates should equal(Vector(Parameters(1000.9289940828394, 0.050699999999999974), Parameters(99.5310580204781, 0.14649999999999982), Parameters(0.9930244145490829, 0.8027999999999986)))

    }
    it("weighted") {
      val rnd = new Well19937c(1)
      val weights = 1 to 10000 map (i => math.exp(rnd.nextDouble * 3))
      val data = PoissonMixture.generateKWeighted(Seq(10.0 -> 0.05, 50.0 -> 0.15, 1.0 -> 0.8), weights, rnd).map(_.toInt) zip weights
      val estimates = PoissonMixture.emKComponentPoissonWeightedMultistart(data.toVector, 3, rnd, 10)
      math.abs(estimates(0).mean - 10.0) should be < 0.1
      math.abs(estimates(1).mean - 50.0) should be < 0.5
      math.abs(estimates(2).mean - 1.0) should be < 0.01

      math.abs(estimates(0).mixture - 0.05) should be < 0.01
      math.abs(estimates(1).mixture - 0.15) should be < 0.03
      math.abs(estimates(2).mixture - 0.8) should be < 0.01
    }
  }
}