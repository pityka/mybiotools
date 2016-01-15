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

package g2gsequence

import org.scalatest.FunSuite

import java.io.File

import mybiotools._
import mybiotools.gwascommons._

import formatters._

class G2GTestSuite extends FunSuite {
  test("padAlignemtn") {
    val alignment = FastaSequenceData("cucc" -> "atgc", "cucc2" -> "atgcatgc", "cucc3" -> "a")
    val padded = padAlignment(alignment)
    expectResult(FastaSequenceData("cucc" -> "atgc----", "cucc2" -> "atgcatgc", "cucc3" -> "a-------")) { padded }
    expectResult(FastaSequenceData("cucc" -> "atgc", "cucc2" -> "atgcatgc", "cucc3" -> "a")) { alignment }
  }

  test("variable positions") {
    val alignment = FastaSequenceData(
      "ref" -> "aac",
      "cucc" -> "aab",
      "cucc2" -> "cac",
      "cucc3" -> "aab"
    )
    val vars = createBinaryPhenotypesWhereVariable(
      alignment,
      "ref",
      1, 1, List("cucc", "cucc2", "cucc3"), None, None
    )._2

    expectResult(Set(0, 2)) { vars }
  }

  test("createPhenotype, raw") {
    val alignment = FastaSequenceData("ref" -> "a", "cucc" -> "a", "cucc2" -> "c", "cucc3" -> "a")
    val phenotypes = createBinaryPhenotypesWhereVariable(
      alignment,
      "ref",
      1, 1, List("cucc", "cucc2", "cucc3"), None, None
    )._1
    val exp = Map(MutationLabel("ref", 1, 0, "A") -> Map("cucc" -> Some(true), "cucc2" -> Some(false), "cucc3" -> Some(true)))
    expectResult(exp) { phenotypes }
  }

  test("createPhenotype, raw, HomerID") {
    val alignment = FastaSequenceData("AC000004" -> "a", "AC000000" -> "a", "AC000001" -> "c", "AC000002" -> "a")
    val phenotypes = createBinaryPhenotypesWhereVariable(
      alignment,
      "AC000004",
      1, 1, List("AC000000", "AC000001", "AC000002"), None, None
    )._1
    val exp = Map(MutationLabel("AC000004", 1, 0, "A") -> Map("AC000000" -> Some(true), "AC000001" -> Some(false), "AC000002" -> Some(true)))
    expectResult(exp) { phenotypes }
  }

  test("createPhenotype, raw, multiple sites") {
    val alignment = FastaSequenceData("ref" -> "aa", "cucc" -> "aa", "cucc2" -> "cc", "cucc3" -> "aa")
    val phenotypes = createBinaryPhenotypesWhereVariable(
      alignment,
      "ref",
      1, 1, List("cucc", "cucc2", "cucc3"), None, None
    )._1
    val exp = Map(MutationLabel("ref", 1, 0, "A") -> Map("cucc" -> Some(true), "cucc2" -> Some(false), "cucc3" -> Some(true)), MutationLabel("ref", 2, 0, "A") -> Map("cucc" -> Some(true), "cucc2" -> Some(false), "cucc3" -> Some(true)))
    expectResult(exp) { phenotypes }
  }

  test("createPhenotype, raw, multiple sites gap in ref 1") {
    val alignment = FastaSequenceData("ref" -> "-a", "cucc" -> "aa", "cucc2" -> "cc", "cucc3" -> "aa")
    val phenotypes = createBinaryPhenotypesWhereVariable(
      alignment,
      "ref",
      1, 1, List("cucc", "cucc2", "cucc3"), None, None
    )._1
    val exp = Map(MutationLabel("ref", 0, 1, "A") -> Map("cucc" -> Some(true), "cucc2" -> Some(false), "cucc3" -> Some(true)), MutationLabel("ref", 1, 0, "A") -> Map("cucc" -> Some(true), "cucc2" -> Some(false), "cucc3" -> Some(true)))
    expectResult(exp) { phenotypes }
  }

  test("createPhenotype, raw, multiple sites gap in ref 2") {
    val alignment = FastaSequenceData("ref" -> "a-", "cucc" -> "aa", "cucc2" -> "cc", "cucc3" -> "aa")
    val phenotypes = createBinaryPhenotypesWhereVariable(
      alignment,
      "ref",
      1, 1, List("cucc", "cucc2", "cucc3"), None, None
    )._1
    val exp = Map(MutationLabel("ref", 1, 1, "A") -> Map("cucc" -> Some(true), "cucc2" -> Some(false), "cucc3" -> Some(true)), MutationLabel("ref", 1, 0, "A") -> Map("cucc" -> Some(true), "cucc2" -> Some(false), "cucc3" -> Some(true)))
    expectResult(exp) { phenotypes }
  }

  test("createPhenotype, raw, multiple sites with gap in nonref") {
    val alignment = FastaSequenceData("ref" -> "aa", "cucc" -> "a-", "cucc2" -> "c-", "cucc3" -> "a-")
    val phenotypes = createBinaryPhenotypesWhereVariable(
      alignment,
      "ref",
      1, 1, List("cucc", "cucc2", "cucc3"), None, None
    )._1
    val exp = Map(MutationLabel("ref", 1, 0, "A") -> Map("cucc" -> Some(true), "cucc2" -> Some(false), "cucc3" -> Some(true)))
    expectResult(exp) { phenotypes }
  }

  test("createPhenotype, raw 2") {
    val alignment = FastaSequenceData("ref" -> "a", "cucc" -> "a", "cucc2" -> "c", "cucc3" -> "-", "cucc4" -> "-", "cucc5" -> "a")
    val phenotypes = createBinaryPhenotypesWhereVariable(
      alignment,
      "ref",
      1, 1, List("cucc", "cucc2", "cucc3", "cucc4", "cucc5"), None, None
    )._1
    val exp = Map(MutationLabel("ref", 1, 0, "A") -> Map("cucc" -> Some(true), "cucc2" -> Some(false), "cucc3" -> None, "cucc4" -> None, "cucc5" -> Some(true)))
    expectResult(exp) { phenotypes }
  }

  test("createPhenotype, raw, includeSequenceKeys") {
    val alignment = FastaSequenceData("ref" -> "a", "cucc" -> "a", "cucc2" -> "c", "cucc3" -> "-", "cucc4" -> "a", "cucc5" -> "a")
    val phenotypes = createBinaryPhenotypesWhereVariable(
      alignment,
      "ref",
      1, 1, List("cucc", "cucc2", "cucc3", "cucc4"), None, None
    )._1
    val exp = Map(MutationLabel("ref", 1, 0, "A") -> Map("cucc" -> Some(true), "cucc2" -> Some(false), "cucc3" -> None, "cucc4" -> Some(true)))
    expectResult(exp) { phenotypes }
  }

  test("createPhenotype, raw, excludeFilter & includeSequenceKeys") {
    val alignment = FastaSequenceData("ref" -> "aa", "cucc" -> "ac", "cucc2" -> "cc", "cucc3" -> "-c", "cucc4" -> "ac", "cucc5" -> "a")
    val phenotypes = createBinaryPhenotypesWhereVariable(
      alignment,
      "ref",
      1, 1, List("cucc", "cucc2", "cucc3", "cucc4", "cucc5"), None, Some(List(1))
    )._1
    val exp = Map(MutationLabel("ref", 1, 0, "A") -> Map("cucc" -> Some(true), "cucc2" -> Some(false), "cucc3" -> None, "cucc4" -> Some(true), "cucc5" -> Some(true)))
    expectResult(exp) { phenotypes }
  }

  test("createPhenotype, raw, excludeFilter & includeSequenceKeys 2") {
    val alignment = FastaSequenceData("ref" -> "aa", "cucc" -> "ac", "cucc2" -> "cc", "cucc3" -> "-c", "cucc4" -> "ac", "cucc5" -> "a")
    val phenotypes = createBinaryPhenotypesWhereVariable(
      alignment,
      "ref",
      1, 1, List("cucc", "cucc2", "cucc3", "cucc4", "cucc5"), None, Some(List(0, 1))
    )._1
    val exp = Map()
    expectResult(exp) { phenotypes }
  }

  test("createPhenotype, raw, excludeFilter & includeSequenceKeys 3") {
    val alignment = FastaSequenceData("ref" -> "aa", "cucc" -> "ac", "cucc2" -> "cc", "cucc3" -> "-c", "cucc4" -> "ag", "cucc5" -> "a")
    val phenotypes = createBinaryPhenotypesWhereVariable(
      alignment,
      "ref",
      1, 1, List("cucc", "cucc2", "cucc3", "cucc4", "cucc5"), None, Some(List(0))
    )._1
    val exp = Map(MutationLabel("ref", 2, 0, "C") -> Map("cucc" -> Some(true), "cucc2" -> Some(true), "cucc3" -> Some(true), "cucc4" -> Some(false), "cucc5" -> None))
    expectResult(exp) { phenotypes }
  }

  test("createPhenotype, raw, excludeFilter & includeSequenceKeys & maskFilter") {
    val alignment = FastaSequenceData("ref" -> "aaa", "cucc" -> "act", "cucc2" -> "cct", "cucc3" -> "-ct", "cucc4" -> "agg", "cucc5" -> "a-t")
    val phenotypes = createBinaryPhenotypesWhereVariable(
      alignment,
      "ref",
      1, 1, List("cucc", "cucc2", "cucc3", "cucc4", "cucc5"), Some(List(0, 1)), Some(List(0))
    )._1
    val exp = Map(MutationLabel("ref", 2, 0, "C") -> Map("cucc" -> Some(true), "cucc2" -> Some(true), "cucc3" -> Some(true), "cucc4" -> Some(false), "cucc5" -> None))
    expectResult(exp) { phenotypes }
  }

  test("createPhenotype, raw 3, minimumDataCount") {
    val alignment = FastaSequenceData("ref" -> "a", "cucc" -> "a", "cucc2" -> "c", "cucc3" -> "-", "cucc4" -> "-", "cucc5" -> "a")
    val phenotypes = createBinaryPhenotypesWhereVariable(
      alignment,
      "ref",
      3, 1, List("cucc", "cucc2", "cucc3", "cucc4", "cucc5"), None, None
    )._1
    val exp = Map(MutationLabel("ref", 1, 0, "A") -> Map("cucc" -> Some(true), "cucc2" -> Some(false), "cucc3" -> None, "cucc4" -> None, "cucc5" -> Some(true)))
    expectResult(exp) { phenotypes }
  }

  test("createPhenotype, raw 4, minimumDataCount") {
    val alignment = FastaSequenceData("ref" -> "aa", "cucc" -> "a", "cucc2" -> "c", "cucc3" -> "-", "cucc4" -> "-", "cucc5" -> "a")
    val phenotypes = createBinaryPhenotypesWhereVariable(
      alignment,
      "ref",
      4, 1, List("cucc", "cucc2", "cucc3", "cucc4", "cucc5"), None, None
    )._1
    val exp = Map()
    expectResult(exp) { phenotypes }
  }

  test("createPhenotype, raw 5, minorAlleleFrequencyCount") {
    val alignment = FastaSequenceData("ref" -> "a", "cucc" -> "a", "cucc2" -> "c", "cucc3" -> "c", "cucc4" -> "a", "cucc5" -> "a")
    val phenotypes = createBinaryPhenotypesWhereVariable(
      alignment,
      "ref",
      3, 2, List("cucc", "cucc2", "cucc3", "cucc4", "cucc5"), None, None
    )._1
    val exp = Map(MutationLabel("ref", 1, 0, "A") -> Map("cucc" -> Some(true), "cucc2" -> Some(false), "cucc3" -> Some(false), "cucc4" -> Some(true), "cucc5" -> Some(true)))
    expectResult(exp) { phenotypes }
  }

  test("createPhenotype, raw 6, minorAlleleFrequencyCount") {
    val alignment = FastaSequenceData("ref" -> "a", "cucc" -> "a", "cucc2" -> "c", "cucc3" -> "c", "cucc4" -> "a", "cucc5" -> "a")
    val phenotypes = createBinaryPhenotypesWhereVariable(
      alignment,
      "ref",
      3, 3, List("cucc", "cucc2", "cucc3", "cucc4", "cucc5"), None, None
    )._1
    val exp = Map()
    expectResult(exp) { phenotypes }
  }

  test("createPhenotype, raw 7, minorAlleleFrequencyCount & includeSequenceKeys") {
    val alignment = FastaSequenceData("ref" -> "a", "cucc" -> "a", "cucc2" -> "c", "cucc3" -> "c", "cucc4" -> "-", "cucc5" -> "a")
    val phenotypes = createBinaryPhenotypesWhereVariable(
      alignment,
      "ref",
      3, 2, List("cucc", "cucc2", "cucc3", "cucc4", "cucc5"), None, None
    )._1
    val exp = Map(MutationLabel("ref", 1, 0, "A") -> Map("cucc" -> Some(true), "cucc2" -> Some(false), "cucc3" -> Some(false), "cucc4" -> None, "cucc5" -> Some(true)))
    expectResult(exp) { phenotypes }
  }

  test("createPhenotype, raw 8, minorAlleleFrequencyCount & includeSequenceKeys") {
    val alignment = FastaSequenceData("cucc" -> "a", "cucc2" -> "c", "cucc3" -> "c", "cucc4" -> "-", "cucc5" -> "a")
    val phenotypes = createBinaryPhenotypesWhereVariable(
      alignment,
      "ref",
      3, 2, List("cucc", "cucc2", "cucc4", "cucc5"), None, None
    )._1
    val exp = Map()
    expectResult(exp) { phenotypes }
  }

  test("createPhenotype, raw , 3 nucleotides") {
    val alignment = FastaSequenceData("ref" -> "a", "cucc" -> "a", "cucc2" -> "c", "cucc3" -> "c", "cucc4" -> "g", "cucc5" -> "a", "cucc6" -> "g")
    val phenotypes = createBinaryPhenotypesWhereVariable(
      alignment,
      "ref",
      3, 2, List("cucc", "cucc2", "cucc4", "cucc5", "cucc3", "cucc6"), None, None
    )._1
    val exp = Map(
      MutationLabel("ref", 1, 0, "A") -> Map("cucc" -> Some(true), "cucc2" -> Some(false), "cucc3" -> Some(false), "cucc4" -> Some(false), "cucc5" -> Some(true), "cucc6" -> Some(false)),
      MutationLabel("ref", 1, 0, "C") -> Map("cucc" -> Some(false), "cucc2" -> Some(true), "cucc3" -> Some(true), "cucc4" -> Some(false), "cucc5" -> Some(false), "cucc6" -> Some(false)),
      MutationLabel("ref", 1, 0, "G") -> Map("cucc" -> Some(false), "cucc2" -> Some(false), "cucc3" -> Some(false), "cucc4" -> Some(true), "cucc5" -> Some(false), "cucc6" -> Some(true))
    )
    expectResult(exp) { phenotypes }
  }

  test("Phenotype summary Stats 1 ") {
    val phenotype = Map(MutationLabel("ref", 0, 0, "A") -> Map("cucc" -> Some(true), "cucc2" -> Some(false), "cucc3" -> Some(false), "cucc4" -> None, "cucc5" -> Some(true)))
    val exp = Map(MutationLabel("ref", 0, 0, "A") -> (2, 2, 1))
    expectResult(exp) {
      phenotypeSummaryStatistics(
        phenotype,
        List("cucc", "cucc2", "cucc3", "cucc4", "cucc5")
      )
    }
  }

  test("Phenotype summary Stats 2 ") {
    val phenotype = Map(MutationLabel("ref", 0, 0, "A") -> Map("cucc" -> Some(true), "cucc2" -> Some(false), "cucc3" -> Some(false), "cucc4" -> None, "cucc5" -> Some(true)), MutationLabel("ref", 0, 0, "C") -> Map("cucc" -> Some(true), "cucc2" -> Some(true), "cucc3" -> Some(false), "cucc4" -> None, "cucc5" -> Some(true)))
    val exp = Map(MutationLabel("ref", 0, 0, "A") -> (2, 2, 1), MutationLabel("ref", 0, 0, "C") -> (3, 1, 1))

    expectResult(exp) {
      phenotypeSummaryStatistics(
        phenotype,
        List("cucc", "cucc2", "cucc3", "cucc4", "cucc5")
      )
    }
  }

  test("Format phenotype summary stats") {
    val d = Map(MutationLabel("ref", 0, 0, "A") -> (2, 2, 1), MutationLabel("ref", 0, 0, "C") -> (3, 1, 1))
    val exp = """|cases,controls,missings,phenoType
                 |2,2,1,ref_0_0_A
                 |3,1,1,ref_0_0_C""".stripMargin
    expectResult(exp) { formatPhenotypeSummaryStatistics(d) }

  }

  test("Format for Plink") {
    val phenotype = Map(MutationLabel("ref", 0, 0, "A") -> Map("cucc" -> Some(true), "cucc2" -> Some(false), "cucc3" -> Some(false), "cucc4" -> None, "cucc5" -> Some(true)), MutationLabel("ref", 0, 0, "C") -> Map("cucc" -> Some(true), "cucc2" -> Some(true), "cucc3" -> Some(false), "cucc4" -> None, "cucc5" -> Some(true)))
    val exp = """|FID IID ref_0_0_A ref_0_0_C
                        |cucc 1 2 2
                        |cucc2 1 1 2
                        |cucc3 1 1 1
                        |cucc4 1 -9 -9
                        |cucc5 1 2 2""".stripMargin
    expectResult(exp) {
      formatPhenotypesForPlink(
        phenotype,
        List("cucc", "cucc2", "cucc3", "cucc4", "cucc5")
      )
    }
  }

  test("format for Tped tfam") {
    val phenotype = Map(MutationLabel("ref", 0, 0, "A") -> Map("cucc" -> Some(true), "cucc2" -> Some(false), "cucc3" -> Some(false), "cucc4" -> None, "cucc5" -> Some(true)), MutationLabel("ref", 0, 0, "C") -> Map("cucc" -> Some(true), "cucc2" -> Some(true), "cucc3" -> Some(false), "cucc4" -> None, "cucc5" -> Some(true)))

    val exptfam = """|cucc 1 0 0 -9 -9
                  |cucc2 1 0 0 -9 -9
                  |cucc3 1 0 0 -9 -9
                  |cucc4 1 0 0 -9 -9
                  |cucc5 1 0 0 -9 -9""".stripMargin
    val exptped = """|0 ref_0_0_A 0 0 T T A A A A 0 0 T T
                    |0 ref_0_0_C 0 0 T T T T A A 0 0 T T""".stripMargin

    expectResult((exptfam)) {
      formatPhenotypesForTFamTPed(
        phenotype,
        List("cucc", "cucc2", "cucc3", "cucc4", "cucc5")
      )._1
    }
    expectResult((exptped)) {
      formatPhenotypesForTFamTPed(
        phenotype,
        List("cucc", "cucc2", "cucc3", "cucc4", "cucc5")
      )._2
    }
  }

}