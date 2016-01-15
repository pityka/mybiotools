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
import mybiotools.gwascommons._

import org.scalatest.FunSpec
import org.scalatest.Matchers

class LRTSpec extends FunSpec with Matchers {

  describe("simple test with R") {

    it("should equal with R") {
      val l1 = LogLikelihood(-168.7005, 1, 300)
      val l2 = LogLikelihood(-131.3975, 2, 300)
      LikelihoodRatioTest(l1, l2).statistic should equal(74.606)
      LikelihoodRatioTest(l1, l2).pValue should be < 1E-16
    }
    it("should throw exception on df < 1") {
      val l1 = LogLikelihood(-168.7005, 1, 300)
      val l2 = LogLikelihood(-131.3975, 2, 300)
      evaluating(LikelihoodRatioTest(l2, l1)) should produce[RuntimeException]
    }
  }

}