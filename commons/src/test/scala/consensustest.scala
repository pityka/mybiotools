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

import org.scalatest.FunSuite
import scala.io.Source

import mybiotools._

class ConsensusTestSuite extends FunSuite {

  test("easy") {
    val alignment = FastaSequenceData(
      "cucc1" -> "atga",
      "cucc2" -> "atac",
      "cucc3" -> "tagc"
    )
    assertResult("ATGC") { makeConsensus(alignment) }
  }

  test("trivi") {
    val alignment = FastaSequenceData(
      "cucc1" -> "atga",
      "cucc2" -> "atga",
      "cucc3" -> "atga"
    )
    assertResult("ATGA") { makeConsensus(alignment) }
  }

  test("hosszabb") {
    val alignment = FastaSequenceData(
      "cucc1" -> "atga",
      "cucc2" -> "atat",
      "cucc2" -> "cttg",
      "cucc2" -> "ctcc",
      "cucc3" -> "ctga"
    )
    assertResult("CTGA") { makeConsensus(alignment) }
  }

}