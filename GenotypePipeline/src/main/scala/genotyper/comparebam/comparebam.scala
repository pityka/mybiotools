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

package genotyper.comparebam

import mybiotools.gwascommons._
import java.io.File
import mybiotools._
import htsjdk.samtools.{ SAMFileReader, SAMFileWriterFactory, SAMSequenceRecord, SAMSequenceDictionary, SAMFileHeader, SAMRecord, SAMTextHeaderCodec }
import scala.collection.JavaConversions._
import scala.util.Try
import mybiotools.stringstore._
import java.nio.{ ByteBuffer, ByteOrder }

sealed trait RC
case class ReadComparison(readName: String, chrMatch: Boolean, startMatch: Boolean, endMatch: Boolean, cigarMatch: Boolean, duplicateFlagMatch: Boolean, secondOfPairMatch: Boolean) extends RC {
  def readsMatch = (chrMatch && startMatch && endMatch && cigarMatch && duplicateFlagMatch && secondOfPairMatch)
}

case class FoundOnlyInOneFile(readName: String) extends RC

case class Summary(numberOfMatching: Int, nonMatching: List[ReadComparison], orphan: List[FoundOnlyInOneFile]) {
  def update(r: RC): Summary = r match {
    case x: FoundOnlyInOneFile => Summary(numberOfMatching, nonMatching, x :: orphan)
    case x: ReadComparison if x.readsMatch => Summary(numberOfMatching + 1, nonMatching, orphan)
    case x: ReadComparison => Summary(numberOfMatching, x :: nonMatching, orphan)
  }
}

object CompareBam extends App {

  def compareSamIterators(sam1: Iterator[SAMRecord], sam2: Iterator[SAMRecord]): Summary = {
    val filter: (SAMRecord => Boolean) = (sam: SAMRecord) => {
      !sam.isSecondaryOrSupplementary &&
        !sam.getReadUnmappedFlag
    }
    val compared: Iterator[RC] = collectStreamsAndMap(List(sam1.filter(filter).map(s => s.getReadName -> s), sam2.filter(filter).map(s => s.getReadName -> s))) {
      case (name, reads) =>
        if (reads.size == 1) FoundOnlyInOneFile(name)
        else compareSAMRecords(reads.head, reads.last)
    }

    compared.foldLeft(Summary(0, Nil, Nil))((x, y) => x.update(y))

  }

  def compareSAMRecords(sam1: SAMRecord, sam2: SAMRecord): ReadComparison = {
    assert(sam1.getReadName == sam2.getReadName)
    val chrEquals = sam1.getReferenceName == sam2.getReferenceName
    val startposEquals = sam1.getAlignmentStart == sam2.getAlignmentStart
    val endposEquals = sam1.getAlignmentEnd == sam2.getAlignmentEnd
    val cigarEquals = sam1.getCigarString == sam2.getCigarString
    val duplicateFlag = sam1.getDuplicateReadFlag == sam2.getDuplicateReadFlag
    val secondOfPair = sam1.getReadPairedFlag && sam2.getReadPairedFlag && sam1.getSecondOfPairFlag == sam2.getSecondOfPairFlag
    ReadComparison(sam1.getReadName, chrEquals, startposEquals, endposEquals, cigarEquals, duplicateFlag, secondOfPair)
  }
  println(args.toList)
  val file1 = args(0)
  val file2 = args(1)
  val reader1 = new SAMFileReader(new File(file1))
  val reader2 = new SAMFileReader(new File(file2))
  reader1.setValidationStringency(htsjdk.samtools.ValidationStringency.SILENT)
  reader2.setValidationStringency(htsjdk.samtools.ValidationStringency.SILENT)
  val iter1 = reader1.iterator
  val iter2 = reader2.iterator

  println(compareSamIterators(iter1, iter2))

}