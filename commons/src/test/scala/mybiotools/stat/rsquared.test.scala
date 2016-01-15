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
import LinearRegression._

import org.scalatest.FunSpec
import org.scalatest.Matchers
import mybiotools.gwascommons.Individual

class RsquaredSpec extends FunSpec with Matchers {

  describe("basics") {

    it("simple") {
      val a1 = Array(-9f, -9f, -9f, 1f, 2f)
      val a2 = Array(1.0f, 2f, 3f, 1f, 2f)
      math.abs(mybiotools.stat.RSquared.rsquared(a1, a2, -9f) - 1.0) should be < 1E-5
    }
    it("simple 2") {
      val a1 = Array(-9f, -9f, -9f, 1f, 2f)
      val a2 = Array(-9f, -9f, -9f, 1f, 2f)
      math.abs(mybiotools.stat.RSquared.rsquared(a1, a2, -9f) - 1.0) should be < 1E-5
    }

    it("simple 3") {
      val a1 = Array(-9f, -9f, -9f, -9f, 2f, 3f)
      val a2 = Array(-9f, -9f, -9f, 1f, 2f, 3f)
      math.abs(mybiotools.stat.RSquared.rsquared(a1, a2, -9f) - 1.0) should be < 1E-5
    }

    it("simple 4") {
      val a1 = Array(-9f, -9f, -9f, 2323f, 2f, 3f)
      val a2 = Array(-9f, -9f, -9f, -9f, 2f, 3f)
      math.abs(mybiotools.stat.RSquared.rsquared(a1, a2, -9f) - 1.0) should be < 1E-5
    }

    it("simple 5") {
      val a1 = Array(123f, 9f, 19f, 2323f, 2f, 3f)
      val a2 = Array(-9f, -9f, -9f, -9f, 2f, 3f)
      math.abs(mybiotools.stat.RSquared.rsquared(a1, a2, -9f) - 1.0) should be < 1E-5
    }

    it("simple 6") {
      val a1 = Array(-9f, -9f, 0f, -9f, -9f, 2f, 3f, -9f, 3f)
      val a2 = Array(12, 11111, 0f, 23, -9f, 2f, -9f, 0, 4f)
      math.abs(mybiotools.stat.RSquared.rsquared(a1, a2, -9f) - 0.96428) should be < 1E-5
    }

    it("dfsdf") {
      val a1 = Array(2.0, 1.0, 1.0, 1.0, 1.0, 2.0, 2.0, 1.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 1.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 1.0, 2.0, 2.0, 2.0, 2.0, 1.0, 2.0, 2.0, 2.0, 2.0, 2.0, 1.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 1.0, 2.0, 2.0, 1.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 0.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 1.0, 2.0, 2.0, 2.0).map(_.toFloat)
      val a2 = Array(-9.0, -9.0, -9.0, -9.0, 2.0, 0.0, 0.0, -9.0, 1.0, 0.0, 0.0, 0.0, -9.0, -9.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 2.0, -9.0, -9.0, -9.0, -9.0, 0.0, -9.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 2.0, 2.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 12.0).map(_.toFloat)
      math.abs(mybiotools.stat.RSquared.rsquared(a1, a2, -9f) - 4.802011409320169E-5) should be < 1E-5

    }

  }
}