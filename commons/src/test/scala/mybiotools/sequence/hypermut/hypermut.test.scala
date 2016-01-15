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

package mybiotools.sequence.hypermut
import org.scalatest.FunSuite

class HyperMutationTestSuite extends FunSuite {

  test("1 missing") {
    expectResult(0) { hyperMutationCount("GAA", "---").mutationEvents }

    expectResult(List()) { hyperMutationCount("GAA", "---").mutatedPositions.toList }

    expectResult(0) { hyperMutationCount("CCGAACC", "-------").mutationEvents }

  }

  test("gaps") {
    expectResult(1) { hyperMutationCount("GCA", "ACA").matchingControlContextPositions.size }
    expectResult(0) { hyperMutationCount("GCA", "NNN").matchingControlContextPositions.size }
    expectResult(0) { hyperMutationCount("GCA", "---").matchingControlContextPositions.size }

    expectResult(1) { hyperMutationCount("GAA", "AAA").mutationEvents }
    expectResult(1) { hyperMutationCount("GAA", "AAA").possibleMutationEvents }
    expectResult(0) { hyperMutationCount("GAA", "GAA").mutationEvents }
    expectResult(1) { hyperMutationCount("GAA", "GAA").possibleMutationEvents }
    expectResult(0) { hyperMutationCount("GAA", "ANA").mutationEvents }
    expectResult(0) { hyperMutationCount("GAA", "ANA").possibleMutationEvents }
    expectResult(0) { hyperMutationCount("GAA", "GNA").mutationEvents }
    expectResult(0) { hyperMutationCount("GAA", "GNA").possibleMutationEvents }

  }

  test(" control") {
    // "([CT][ACGT])|([AG]C)"
    expectResult(0) { hyperMutationCount("GCA", "GCA").controlEvents }
    expectResult(0) { hyperMutationCount("GC", "GC").controlEvents }
    expectResult(1) { hyperMutationCount("GCA", "ACA").controlEvents }

  }

  test("0") {
    expectResult(0) { hyperMutationCount("ATGCATGC", "ATGCATGC").mutationEvents }

    expectResult(List[Int]()) { hyperMutationCount("ATGCATGC", "ATGCATGC").mutatedPositions.toList }

    expectResult(0) { hyperMutationCount("ATGAGGGC", "ATGAGGGC").mutationEvents }

    expectResult(List[Int]()) { hyperMutationCount("ATGAGGGC", "ATGAGGGC").mutatedPositions.toList }

    expectResult(0) { hyperMutationCount("ATGAGGGC", "ATGAGAGC").mutationEvents }

    expectResult(List[Int]()) { hyperMutationCount("ATGAGGGC", "ATGAGAGC").mutatedPositions.toList }

    expectResult(0) { hyperMutationCount("GGA", "AGT").mutationEvents }

    expectResult(List()) { hyperMutationCount("GGA", "AGT").mutatedPositions.toList }

    expectResult(0) { hyperMutationCount("GAA", "AGT").mutationEvents }

    expectResult(List()) { hyperMutationCount("GAA", "AGT").mutatedPositions.toList }

    expectResult(0) { hyperMutationCount("CCGAACC", "CCAGTCC").mutationEvents }

    expectResult(Nil) { hyperMutationCount("CCGAACC", "CCAGTCC").mutatedPositions.toList }

  }
  test("1") {
    expectResult(1) { hyperMutationCount("GAA", "AAA").mutationEvents }

    expectResult(List(0)) { hyperMutationCount("GAA", "AAA").mutatedPositions.toList }

    expectResult(1) { hyperMutationCount("GGG", "AGG").mutationEvents }

    expectResult(List(0)) { hyperMutationCount("GGG", "AGG").mutatedPositions.toList }

    expectResult(1) { hyperMutationCount("GAA", "AAA").mutationEvents }

    expectResult(List(0)) { hyperMutationCount("GAA", "AAA").mutatedPositions.toList }

    expectResult(1) { hyperMutationCount("GGA", "AGA").mutationEvents }

    expectResult(List(0)) { hyperMutationCount("GGA", "AGA").mutatedPositions.toList }

    expectResult(1) { hyperMutationCount("CCGAACC", "CCAAACC").mutationEvents }

    expectResult(List(2)) { hyperMutationCount("CCGAACC", "CCAAACC").mutatedPositions.toList }

    expectResult(1) { hyperMutationCount("GACGAA", "TTTAAA").mutationEvents }
    expectResult(List(3)) { hyperMutationCount("GACGAA", "TTTAAA").mutatedPositions.toList }

  }
  test("more") {
    expectResult(2) { hyperMutationCount("GAAGAA", "AAAAAA").mutationEvents }

    expectResult(List(0, 3)) { hyperMutationCount("GAAGAA", "AAAAAA").mutatedPositions.toList }

    expectResult(2) { hyperMutationCount("GATGAT", "AATAAT").mutationEvents }

    expectResult(List(0, 3)) { hyperMutationCount("GATGAT", "AATAAT").mutatedPositions.toList }

    expectResult(1) { hyperMutationCount("GAAGAA", "AAAAAC").mutationEvents }

    expectResult(List(0)) { hyperMutationCount("GAAGAA", "AAAAAC").mutatedPositions.toList }

    expectResult(2) { hyperMutationCount("GAGGAA", "AAGAAA").mutationEvents }

    expectResult(List(0, 3)) { hyperMutationCount("GAGGAA", "AAGAAA").mutatedPositions.toList }

    expectResult(0) { hyperMutationCount("GGGGGG", "AAAAAA").mutationEvents }

    expectResult(Nil) { hyperMutationCount("GGGGGG", "AAAAAA").mutatedPositions.toList }

    expectResult(0) { hyperMutationCount("GACAAA", "AACAAA").mutationEvents }

    expectResult(Nil) { hyperMutationCount("GGGGGG", "AACAAA").mutatedPositions.toList }

    expectResult(List(0, 8, 16)) { hyperMutationCount("GATATGCGGAGACGACGATATGC", "AATATGCGAAGACGACAATGTGC").mutatedPositions.toList }
  }

}

