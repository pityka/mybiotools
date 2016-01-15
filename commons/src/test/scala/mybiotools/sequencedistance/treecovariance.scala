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

package mybiotools.sequencedistance

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.saddle._

class CovarianceSpec extends FunSpec with Matchers {

  describe("simple example") {
    it("sdfs") {
      val dist = Frame(
        "a" -> Series("a" -> 0d, "b" -> 2d, "c" -> 2d, "d" -> 8d, "e" -> 16d, "f" -> 16d, "g" -> 26d, "h" -> 26d, "o" -> 16d, "i" -> 10.1),
        "b" -> Series("a" -> 2d, "b" -> 0d, "c" -> 1d, "d" -> 8d, "e" -> 16d, "f" -> 16d, "g" -> 26d, "h" -> 26d, "o" -> 16d, "i" -> 11.9),
        "c" -> Series("a" -> 2d, "b" -> 1d, "c" -> 0d, "d" -> 8d, "e" -> 16d, "f" -> 16d, "g" -> 26d, "h" -> 26d, "o" -> 16d, "i" -> 11.9),
        "d" -> Series("a" -> 8d, "b" -> 8d, "c" -> 8d, "d" -> 0d, "e" -> 16d, "f" -> 16d, "g" -> 26d, "h" -> 26d, "o" -> 16d, "i" -> 17.9),
        "e" -> Series("a" -> 16d, "b" -> 16d, "c" -> 16d, "d" -> 16d, "e" -> 0d, "f" -> 6d, "g" -> 26d, "h" -> 26d, "o" -> 16d, "i" -> 25.9),
        "f" -> Series("a" -> 16d, "b" -> 16d, "c" -> 16d, "d" -> 16d, "e" -> 6d, "f" -> 0d, "g" -> 26d, "h" -> 26d, "o" -> 16d, "i" -> 25.9),
        "g" -> Series("a" -> 26d, "b" -> 26d, "c" -> 26d, "d" -> 26d, "e" -> 26d, "f" -> 26d, "g" -> 0d, "h" -> 6d, "o" -> 16d, "i" -> 35.9),
        "h" -> Series("a" -> 26d, "b" -> 26d, "c" -> 26d, "d" -> 26d, "e" -> 26d, "f" -> 26d, "g" -> 6d, "h" -> 0d, "o" -> 16d, "i" -> 35.9),
        "o" -> Series("a" -> 16d, "b" -> 16d, "c" -> 16d, "d" -> 16d, "e" -> 16d, "f" -> 16d, "g" -> 16d, "h" -> 16d, "o" -> 0d, "i" -> 25.9),
        "i" -> Series("a" -> 10.1, "b" -> 11.9, "c" -> 11.9, "d" -> 17.9, "e" -> 25.9, "f" -> 25.9, "g" -> 35.9, "h" -> 35.9, "o" -> 25.9, "i" -> 0d)
      )
      val cov = covarianceMatrixFromDistanceMatrix(dist.mapValues(_.toDouble), "o")

      assertResult(cov.at(0, 0).get)(13d)
      assertResult(cov.at(0, 1).get)(12d)
      assertResult(cov.at(0, 2).get)(12d)
      assertResult(cov.at(0, 3).get)(9d)
      assertResult(cov.at(0, 4).get)(5d)
      assertResult(cov.at(0, 5).get)(5d)
      assertResult(cov.at(0, 6).get)(0d)
      assertResult(cov.at(0, 7).get)(0d)

      assertResult(cov.at(1, 0).get)(12d)
      assertResult(cov.at(1, 1).get)(13d)
      assertResult(cov.at(1, 2).get)(12.5)
      assertResult(cov.at(1, 3).get)(9d)
      assertResult(cov.at(1, 4).get)(5d)
      assertResult(cov.at(1, 5).get)(5d)
      assertResult(cov.at(1, 6).get)(0d)
      assertResult(cov.at(1, 7).get)(0d)

      assertResult(cov.at(2, 0).get)(12d)
      assertResult(cov.at(2, 1).get)(12.5)
      assertResult(cov.at(2, 2).get)(13d)
      assertResult(cov.at(2, 3).get)(9d)
      assertResult(cov.at(2, 4).get)(5d)
      assertResult(cov.at(2, 5).get)(5d)
      assertResult(cov.at(2, 6).get)(0d)
      assertResult(cov.at(2, 7).get)(0d)

      assertResult(cov.at(7, 0).get)(0d)
      assertResult(cov.at(7, 1).get)(0d)
      assertResult(cov.at(7, 2).get)(0d)
      assertResult(cov.at(7, 3).get)(0d)
      assertResult(cov.at(7, 4).get)(0d)
      assertResult(cov.at(7, 5).get)(0d)
      assertResult(cov.at(7, 6).get)(10d)
      assertResult(cov.at(7, 7).get)(13d)

      assertResult(cov.at(8, 0).get)(12.899999999999999)
      assertResult(cov.at(8, 1).get)(12d)
      assertResult(cov.at(8, 2).get)(12d)
      assertResult(cov.at(8, 3).get)(9d)
      assertResult(cov.at(8, 4).get)(5d)
      assertResult(cov.at(8, 5).get)(5d)
      assertResult(cov.at(8, 6).get)(0d)
      assertResult(cov.at(8, 7).get)(0d)
      assertResult(cov.at(8, 8).get)(22.9)

    }
  }
}