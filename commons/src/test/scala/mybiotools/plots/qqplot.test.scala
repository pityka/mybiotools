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
package plots
import QQPlot._

import org.scalatest.FunSpec
import org.scalatest.Matchers

class QQPlotSpec extends FunSpec with Matchers {

  describe("QQPlot") {
    val random = new scala.util.Random(1)
    it("1k with max") {
      val ps = (1 to 1000) map (x => random.nextDouble / 30.0)
      val plot = QQPlot.plot(ps.iterator)

      mybiotools.plots.show(plot)
    }
    ignore("5M with pruning") {
      val ps = (1 to 5000000) map (x => random.nextDouble)
      val plot = QQPlot.plot(ps.iterator)
      val expected = Array[Byte](-119, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, 73, 72, 68, 82, 0, 0, 0, 10, 0, 0, 0, 10, 8, 2, 0, 0, 0, 2, 80, 88, -22, 0, 0, 0, 13, 73, 68, 65, 84, 120, -100, 99, 96, 24, 5, -92, 3, 0, 1, 54, 0, 1, 86, -47, 127, -56, 0, 0, 0, 0, 73, 69, 78, 68, -82, 66, 96, -126)
      renderToByteArray(plot, "image/png", 10, 10) should equal(expected)
    }
    it("lambda ") {
      val r = new scala.util.Random(1)
      val ps = (1 to 5000000) map (x => r.nextDouble)
      val lambda = QQPlot.computeLambda(ps)
      lambda should equal(1.000618087534595)

    }
    it("empty should not fail ") {
      val ps = (1 to 0) map (x => random.nextDouble)
      val plot = QQPlot.plot(ps.iterator)
    }
  }
}