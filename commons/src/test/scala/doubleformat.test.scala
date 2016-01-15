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

class DoubleFormatSpec extends FunSpec with Matchers {

  describe("two point double formatter") {
    it("should format 0.0 to 0")(formatDouble(0.0) should equal("0"))

    it("should format 1.23999 to 1.24")(formatDouble(1.23999) should equal("1.24"))

    it("should format 1.9 to 1.9")(formatDouble(1.9) should equal("1.9"))

    it("should format 0.0001 to 0")(formatDouble(0.0001) should equal("0"))

    it("should format 0.01 to 0.01")(formatDouble(0.01) should equal("0.01"))

    it("should format 1.16")(formatDouble(1.16) should equal("1.16"))

    it("should format Nan, Infinity") {
      formatDouble(Double.NaN) should equal("NaN")
      formatDouble(Double.NegativeInfinity) should equal("-Infinity")
      formatDouble(Double.PositiveInfinity) should equal("Infinity")
    }

    it("should format (-2,2) by 0.0001 ") {

      val x = (-20000 to 20000) map (i => i / 10000.0)
      val str = x map formatDouble
      val back = str map (_.toDouble)
      (back zip x).foreach { p =>
        val diff = math.abs(p._1 - p._2)
        assert(diff < 0.01, p)
        diff should be < 0.01
      }
    }

    it("Microbenchmark") {
      val runs = 20000000;
      val ds = for (i <- 0 to runs) yield (i * 1E-6)
      // val decimalFormatter = new java.text.DecimalFormat("0.0#")
      val start = System.nanoTime();
      val sb = new java.lang.StringBuffer();
      ds foreach { i =>
        // formatDouble(i);
        formatDouble2DecimalTo(sb, i)
        sb.setLength(0);
        // decimalFormatter.format(i)
      }
      val time = System.nanoTime() - start;
      Console.printf("Took %,d ns per append double%n", time / runs);
    }
  }

}