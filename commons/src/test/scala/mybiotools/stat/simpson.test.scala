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

class SimpsonSpec extends FunSpec with Matchers with PrivateMethodTester with Checkers {

  describe("simpson's rule ") {
    it("trivial") {
      (NewtonCotes.simpson((x: Double) => x, 0.0, 1.0, 4)) should equal(0.5)
      (NewtonCotes.simpson((x: Double) => x * x * x, 0.0, 10.0, 2)) should equal(2500.0)
    }
    it("recursive") {
      (NewtonCotes.recursiveSimpson((x: Double) => x, 0.0, 1.0, 1E-4)) should equal(0.5)
    }

  }

  describe("trapezoid rule ") {
    it("trivial") {
      (NewtonCotes.trapezoid((x: Double) => x, 0.0, 1.0, 4)) should equal(0.5)
      (NewtonCotes.trapezoid((x: Double) => x, 0.0, 10.0, 2)) should equal(50.0)
    }

  }

  describe("Boole's rule ") {
    it("trivial") {
      (NewtonCotes.booles((x: Double) => x, 0.0, 1.0, 4)) should equal(0.5)
      (NewtonCotes.booles((x: Double) => x, 0.0, 10.0, 4)) should equal(50.0)
      (NewtonCotes.booles((x: Double) => x, 0.0, 10.0, 10)) should equal(50.0)
    }

    it("recursive") {
      NumericalIntegration.recursive((x: Double) => x, 0.0, 10.0, 1E-3, 1E-4, 10, NewtonCotes.Booles, 10) should equal(50d)
    }

  }

  describe("Gauss-Legendre ") {
    it("trivial") {
      (GaussLegendre.integrate((x: Double) => x, 0.0, 1.0, 5)) should equal(0.49999999999999994)
      (GaussLegendre.integrate((x: Double) => x * x * x, 0.0, 10.0, 15)) should equal(2500.0000000000005)
    }

  }
}