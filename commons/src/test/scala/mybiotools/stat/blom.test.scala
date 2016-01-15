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
import BlomTransformation._

import org.scalatest.FunSpec
import org.scalatest.Matchers

class BlomSpec extends FunSpec with Matchers {

  describe("blom transformation") {
    it("1 2 NA 3 4") {
      BlomTransformation(List(1, 2, Double.NaN, 4, 5)).toList.toString should equal(List(-1.0491313979639705, -0.29930691046566704, Double.NaN, 0.29930691046566704, 1.0491313979639705).toString)
    }
    it("1 2 NA 1 3 4") {
      BlomTransformation(List(1, 2, Double.NaN, 1, 4, 5)).toList.toString should equal(List(-0.7916386077433745, 0.0, Double.NaN, -0.7916386077433745, 0.49720057068155393, 1.1797611176118612).toString)
    }
  }

}