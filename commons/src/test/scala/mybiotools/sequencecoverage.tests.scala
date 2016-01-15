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

class SequenceCovaregeSpec extends FunSpec with Matchers {

  describe("simple test") {

    it("throw error on empty") {
      // val alignment = FastaSequenceData("cucc" -> "atgc----", "cucc2" -> "atgcatgc", "cucc3" -> "atgcatgc")
      val alignment = FastaSequenceData[SequenceKey, String]()

      evaluating(fastaCoverage(alignment)) should produce[NoSuchElementException]

    }

    it("with 1 sequence") {
      val alignment = FastaSequenceData("cucc" -> "a")

      fastaCoverage(alignment) should equal(Map(0 -> 1.0))

    }

    it("with 2 sequence") {
      val alignment = FastaSequenceData("cucc" -> "a", "cucc2" -> "b")

      fastaCoverage(alignment) should equal(Map(0 -> 1.0))

    }
    it("with 2 sequence, 1 X") {
      val alignment = FastaSequenceData("cucc" -> "X", "cucc2" -> "b")

      fastaCoverage(alignment) should equal(Map(0 -> 0.5))

    }

    it("with 2 sequence, 1 -, 1 X") {
      val alignment = FastaSequenceData("cucc" -> "X", "cucc2" -> "-")

      fastaCoverage(alignment) should equal(Map(0 -> 0.0))

    }

    it("with 2 sequence, 1 -, 1 N") {
      val alignment = FastaSequenceData("cucc" -> "N", "cucc2" -> "-")

      fastaCoverage(alignment, Set('-', 'N')) should equal(Map(0 -> 0.0))

    }

    it("with 2 sequence, 1 -, 1 N, 2 letters") {
      val alignment = FastaSequenceData("cucc" -> "Na", "cucc2" -> "-X")

      fastaCoverage(alignment, Set('-', 'N')) should equal(Map(0 -> 0.0, 1 -> 1.0))

    }
  }
}