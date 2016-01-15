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

package viralshortread
import rnaseqalign.htseqcount._

case class BinaryVariantKey(position: ReferencePosition, state: Char) {
  override def toString = position.toString + ":" + state
}

sealed trait ReferencePosition extends Any {
  def ref: Int
}
case class PositionOnReference(ref: Int) extends AnyVal with ReferencePosition {
  override def toString = "ONREF" + ref
}
case class InsertionIntoReference(ref: Int, offsetOnInsertion: Int) extends ReferencePosition {
  override def toString = "INSERTAT" + ref + "+" + offsetOnInsertion
}

/* insertions are wrt the sequence and descending order (last first) */
case class AlignedStringWithStart(start: Int, sequence: String, insertions: Map[Int, String])

object AlignedStringWithStart {
  def apply(sam: SAMRecordEssentials, rawread: String, gap: Char): AlignedStringWithStart =
    {

      val (editedread, insertions) = viralshortread.parseCigar(sam.cigar, rawread, gap)
      val start = sam.alignmentStart - 1 //SAM has 1 based coordinates     
      AlignedStringWithStart(start, editedread, insertions.toMap)
    }
}