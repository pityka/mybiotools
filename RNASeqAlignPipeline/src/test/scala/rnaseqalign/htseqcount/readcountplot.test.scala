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

package rnaseqalign.htseqcount

import org.scalatest.FunSpec
import org.scalatest.PrivateMethodTester
import org.scalatest.Matchers
import org.scalatest.prop.Checkers
// import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.BooleanOperators
// import org.scalacheck.Gen._
import org.scalacheck.{ Arbitrary, Gen }

import HTSeqCount._
import mybiotools.gwascommons._
import java.io.File
import rnaseqalign.Helpers
import scala.collection.immutable.ListMap
import mybiotools.sequence.alignment._

class ReadCountPlotSpec extends FunSpec with Matchers with PrivateMethodTester with Checkers {

  val chr = "sdfs"

  describe("read count plot") {
    val samrecords = List(
      SAMRecordEssentials(alignmentStart = 5, cigar = Cigar(CigarElement(10, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = None, queryName = "XY"),
      SAMRecordEssentials(alignmentStart = 5, cigar = Cigar(CigarElement(10, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = None, queryName = "XY"),
      SAMRecordEssentials(alignmentStart = 25, cigar = Cigar(CigarElement(10, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = None, queryName = "XY"),
      SAMRecordEssentials(alignmentStart = 25, cigar = Cigar(CigarElement(10, M) :: Nil), negativeStrandFlag = true, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = None, queryName = "XY"),
      SAMRecordEssentials(alignmentStart = 5, cigar = Cigar(CigarElement(3, M) :: CigarElement(8, D) :: CigarElement(3, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = None, queryName = "XY")
    )
    it("coverage values") {
      ReadCountPlot.extractCoordinatesFromIterator(samrecords.iterator, Stranded) should equal(Map(Forward -> (Map(4 -> 3, 5 -> 3, 6 -> 3, 7 -> 2, 8 -> 2, 9 -> 2, 10 -> 2, 11 -> 2, 12 -> 2, 13 -> 2, 24 -> 1, 25 -> 1, 26 -> 1, 27 -> 1, 28 -> 1, 29 -> 1, 30 -> 1, 31 -> 1, 32 -> 1, 33 -> 1, 15 -> 1, 16 -> 1, 17 -> 1), Map(7 -> 1, 8 -> 1, 9 -> 1, 10 -> 1, 11 -> 1, 12 -> 1, 13 -> 1, 14 -> 1)), Reverse -> (Map(24 -> 1, 25 -> 1, 26 -> 1, 27 -> 1, 28 -> 1, 29 -> 1, 30 -> 1, 31 -> 1, 32 -> 1, 33 -> 1), Map())))
    }
    ignore("plot") {
      mybiotools.plots.show(ReadCountPlot.plot(samrecords.iterator, Stranded))
    }

  }

}