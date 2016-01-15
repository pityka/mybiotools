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

class FastaOpsTestSuite extends FunSuite {

  test("indicators") {
    val alignment1 = FastaSequenceData("cucc" -> "ATGNAAA", "cucc2" -> "ACCNAAN", "cucc3" -> "AGCGATT", "cucc4" -> "ATANTTT")
    val result = mybiotools.sequence.transformToIndicators(alignment1)(isNucleotide).T.sortedCIx.sortedRIx
    // println(result)
    assertResult(result((1, 'G'), "cucc").raw(0, 0))(Some(false))
    assertResult(result((1, 'T'), "cucc").raw(0, 0))(Some(true))
    assertResult(result((1, 'T'), "cucc2").raw(0, 0))(Some(false))
    assertResult(result((1, 'G'), "cucc2").raw(0, 0))(Some(false))
    assertResult(result((1, 'T'), "cucc3").raw(0, 0))(Some(false))
    assertResult(result((1, 'G'), "cucc3").raw(0, 0))(Some(true))
    assertResult(result((1, 'T'), "cucc4").raw(0, 0))(Some(true))
    assertResult(result((1, 'G'), "cucc4").raw(0, 0))(Some(false))

    assertResult(result.rowIx.toSeq.toSet)(Set(1 -> 'G', 1 -> 'T', 2 -> 'A', 2 -> 'C', 4 -> 'A', 5 -> 'T', 6 -> 'T'))

    assertResult(result((2, 'C'), "cucc").raw(0, 0))(Some(false))
    assertResult(result((2, 'A'), "cucc").raw(0, 0))(Some(false))
    assertResult(result((2, 'C'), "cucc2").raw(0, 0))(Some(true))
    assertResult(result((2, 'A'), "cucc2").raw(0, 0))(Some(false))
    assertResult(result((2, 'C'), "cucc3").raw(0, 0))(Some(true))
    assertResult(result((2, 'A'), "cucc3").raw(0, 0))(Some(false))
    assertResult(result((2, 'C'), "cucc4").raw(0, 0))(Some(false))
    assertResult(result((2, 'A'), "cucc4").raw(0, 0))(Some(true))

    assertResult(result((4, 'A'), "cucc").raw(0, 0))(Some(true))
    assertResult(result((4, 'A'), "cucc2").raw(0, 0))(Some(true))
    assertResult(result((4, 'A'), "cucc3").raw(0, 0))(Some(true))
    assertResult(result((4, 'A'), "cucc4").raw(0, 0))(Some(false))

    assertResult(result((5, 'T'), "cucc").raw(0, 0))(Some(false))
    assertResult(result((5, 'T'), "cucc2").raw(0, 0))(Some(false))
    assertResult(result((5, 'T'), "cucc3").raw(0, 0))(Some(true))
    assertResult(result((5, 'T'), "cucc4").raw(0, 0))(Some(true))

    assertResult(result((6, 'T'), "cucc").raw(0, 0))(Some(false))
    assertResult(result((6, 'T'), "cucc2").raw(0, 0))(None)
    assertResult(result((6, 'T'), "cucc3").raw(0, 0))(Some(true))
    assertResult(result((6, 'T'), "cucc4").raw(0, 0))(Some(true))

  }

  test("ld") {
    val alignment1 = FastaSequenceData("cucc" -> "ATGNAA", "cucc2" -> "ACCNAA", "cucc3" -> "AGCGAT", "cucc4" -> "ATANTT")
    val alignment2 = FastaSequenceData("cucc" -> "ATANTT", "cucc2" -> "ACTNTA", "cucc3" -> "AGAGTT", "cucc4" -> "ACTNTA")
    val result = sequence.calculateLD(alignment1, alignment2)
    assertResult(3.0)(sequence.cramerV(Vector("A", "B", "A"), Vector("B", "A", "B")).chisquare)
    assertResult(4.0)(sequence.cramerV(Vector("B", "A", "B", "B"), Vector("B", "A", "B", "B")).chisquare)
    assertResult(1.0)(sequence.cramerV(Vector("B", "A", "C"), Vector("B", "A", "C")).cramerV)

    assertResult(result.filter(x => x._1 == 0 && x._2 == 0).head._3)(None)
    assertResult(result.filter(x => x._1 == 1 && x._2 == 1).head._3.get)(sequence.cramerV(Vector("T", "C", "G", "T"), Vector("T", "C", "G", "C")))
    assertResult(result.filter(x => x._1 == 1 && x._2 == 2).head._3.get)(sequence.cramerV(Vector("T", "C", "G", "T"), Vector("A", "T", "A", "T")))
    assertResult(result.filter(x => x._1 == 2 && x._2 == 2).head._3.get)(sequence.cramerV(Vector("G", "C", "C", "A"), Vector("A", "T", "A", "T")))

    assertResult(sequence.maskPositionsInSecondWhichAreCorrelatedInTheFirst(alignment1, alignment2, 0.7))(Map("cucc" -> "ANNNTN", "cucc2" -> "ANNNTN", "cucc3" -> "ANNGTN", "cucc4" -> "ANNNTN"))

  }

  test("Delete columns 1 ") {
    val alignment = FastaSequenceData("cucc" -> "atgc----", "cucc2" -> "atgcatgc", "cucc3" -> "atgcatgc")
    val e = FastaSequenceData("cucc" -> "tgc---", "cucc2" -> "tgctgc", "cucc3" -> "tgctgc")
    assertResult(e) { deleteColumnsFromAlignment(alignment, Set(0, 4)) }
  }
  test("Delete columns 2 ") {
    val alignment = FastaSequenceData("cucc" -> "atgc", "cucc2" -> "atgcatgc", "cucc3" -> "atgcatgc")
    val e = FastaSequenceData("cucc" -> "tgc", "cucc2" -> "tgctgc", "cucc3" -> "tgctgc")
    assertResult(e) { deleteColumnsFromAlignment(alignment, Set(0, 4)) }
  }

  test("concat 1") {
    val alignment = FastaSequenceData("cucc" -> "atgc", "cucc2" -> "atgcatgc", "cucc3" -> "atgcatgc")
    val al2 = concatenateFastas(List(alignment, alignment), 'N')

    val exp = FastaSequenceData("cucc" -> "atgc----atgc----", "cucc2" -> "atgcatgcatgcatgc", "cucc3" -> "atgcatgcatgcatgc")

    assertResult(exp)(al2)
  }

  test("concat 2") {
    val alignment = FastaSequenceData("cucc" -> "atgccgta", "cucc2" -> "atgcatgc", "cucc3" -> "atgcatgc")
    val al2 = concatenateFastas(List(alignment, alignment), 'N')

    val exp = FastaSequenceData("cucc" -> "atgccgtaatgccgta", "cucc2" -> "atgcatgcatgcatgc", "cucc3" -> "atgcatgcatgcatgc")

    assertResult(exp)(al2)
  }

  test("concat 3") {
    val alignment = FastaSequenceData("cucc" -> "atgccgta", "cucc2" -> "atgcatgc", "cucc3" -> "aaaabbbb")
    val alignment2 = FastaSequenceData("cucc" -> "aaa", "cucc2" -> "bbb", "cucc3" -> "ccc")
    val al2 = concatenateFastas(List(alignment, alignment2), 'N')

    val exp = FastaSequenceData("cucc" -> "atgccgtaaaa", "cucc2" -> "atgcatgcbbb", "cucc3" -> "aaaabbbbccc")

    assertResult(exp)(al2)
  }
  test("concat 4") {
    val alignment = FastaSequenceData("cucc" -> "abcd", "cucc2" -> "atgcatgc", "cucc3" -> "aaaabbbb")
    val alignment2 = FastaSequenceData("cucc" -> "aaa", "cucc2" -> "bbb", "cucc3" -> "ccc")
    val al2 = concatenateFastas(List(alignment, alignment2), 'N')

    val exp = FastaSequenceData("cucc" -> "abcd----aaa", "cucc2" -> "atgcatgcbbb", "cucc3" -> "aaaabbbbccc")

    assertResult(exp)(al2)
  }

  test("concat 5") {
    val alignment = FastaSequenceData("cucc2" -> "atgcatgc", "cucc3" -> "aaaabbbb")
    val alignment2 = FastaSequenceData("cucc" -> "aaa", "cucc2" -> "bbb", "cucc3" -> "ccc")
    val al2 = concatenateFastas(List(alignment, alignment2), 'N')

    val exp = FastaSequenceData("cucc" -> "NNNNNNNNaaa", "cucc2" -> "atgcatgcbbb", "cucc3" -> "aaaabbbbccc")

    assertResult(exp)(al2)
  }

  test("concat 6") {
    val alignment = FastaSequenceData("cucc2" -> "atgcatg", "cucc3" -> "aaaabbbbb")
    val alignment2 = FastaSequenceData("cucc" -> "aaa", "cucc2" -> "bbb", "cucc3" -> "ccc")
    val al2 = concatenateFastas(List(alignment, alignment2), 'N')

    val exp = FastaSequenceData("cucc" -> "NNNNNNNNNaaa", "cucc2" -> "atgcatg--bbb", "cucc3" -> "aaaabbbbbccc")

    assertResult(exp)(al2)
  }

  test("column 1") {
    val alignment = FastaSequenceData("cucc2" -> "atgc", "cucc" -> "aaaa", "cucc3" -> "atgc")

    val exp = FastaSequenceData("cucc" -> "a", "cucc2" -> "a", "cucc3" -> "a")

    val exp2 = FastaSequenceData("cucc" -> "a", "cucc2" -> "t", "cucc3" -> "t")

    assertResult(exp)(column(alignment, 0))
    assertResult(exp2)(column(alignment, 1))
  }
}