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

package vcfhelpers

import htsjdk.tribble._
import mybiotools._

case class QuickTribbleFeature(chr: String, start: Int, end: Int, line: String) extends Feature {
  def getChr = chr
  def getContig = chr
  def getStart = start
  def getEnd = end
}
case class QuickVCFHeader(lines: Seq[String])

object QuickTribbleFeature {
  def write(header: QuickVCFHeader, data: Seq[QuickTribbleFeature], writer: java.io.Writer) = {
    header.lines.foreach { l =>
      writer.write(l)
      writer.write("\n")
    }
    data.foreach { d =>
      writer.write(d.line)
      writer.write("\n")
    }
  }
}

class QuickTribbleVCFCodec extends AsciiFeatureCodec[QuickTribbleFeature](classOf[QuickTribbleFeature]) {
  def canDecode(path: String): Boolean = path.endsWith("vcf")
  def decode(line: String): QuickTribbleFeature = {
    if (line.startsWith("#")) null
    else {
      val spl = fastSplit1WideSeparatorIterator(line, '\t')
      val chrom = spl.next
      val pos = spl.next.toInt
      val id = spl.next
      val ref = spl.next
      val alt = spl.next
      val qual = spl.next
      val filter = spl.next
      val info = spl.next
      val splinfo = fastSplit1WideSeparatorIterator(info, ';')
      val end = splinfo.find(_.startsWith("END="))

      val stop = if (end.isDefined) end.get.substring(4).toInt else (pos + ref.size - 1)
      QuickTribbleFeature(chrom, pos, stop, line)
    }
  }

  def readActualHeader(lineIter: htsjdk.tribble.readers.LineIterator): AnyRef = {
    val lines = collection.mutable.ArrayBuffer[String]()
    while (lineIter.hasNext && lineIter.peek.startsWith("#")) {
      lines.append(lineIter.next)
    }
    QuickVCFHeader(lines.toSeq)
  }
}
