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

package mybiotools.gwascommons

import mybiotools.intervaltree._

object MAFReader {

  sealed trait Strand
  case object PositiveStrand extends Strand
  case object NegativeStrand extends Strand

  case class Paragraph(lines: Seq[SLine])

  case class SLine(name: String, start: Int, size: Int, strand: Strand, sourceSize: Int, sequence: String)

  object SLine {
    def fromLine(line: String) = {
      val spl = mybiotools.fastSplitSeparatorIterator(line, ' ')
      assert(spl.next == "s")
      val name = spl.next
      val start = spl.next.toInt
      val size = spl.next.toInt
      val strand = spl.next match {
        case "+" => PositiveStrand
        case "-" => NegativeStrand
      }
      val sourceSize = spl.next.toInt
      val sequence = spl.next
      SLine(name, start, size, strand, sourceSize, sequence)
    }

  }

  def readMAF(source: scala.io.Source): Iterator[Paragraph] = {
    val it = source.getLines.dropWhile(_.startsWith("#"))
    (new mybiotools.SpanIterator(it, (x: String) => !x.isEmpty)).filterNot(_.head.isEmpty).map { parlines =>
      val slines = parlines.drop(1).map(SLine.fromLine)
      Paragraph(slines)
    }
  }

  def mapToIntervalOfFirstSequence[T](in: Iterator[Paragraph])(f: Seq[SLine] => T): Iterator[(String, IntervalWithPayLoad[T])] =
    in.map { par =>
      val v = f(par.lines)
      val ref = par.lines.head
      val chr = ref.name
      val start = ref.start
      val stop = ref.start + ref.size
      (chr, IntervalWithPayLoad(start, stop, v))
    }

}