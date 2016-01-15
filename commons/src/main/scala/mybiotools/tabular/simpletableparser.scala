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

package mybiotools.tabular

import scala.util.parsing.combinator._
import scala.util.parsing.input.{ Reader, PagedSeqReader, CharArrayReader }
import scala.collection.immutable.PagedSeq

import mybiotools.fastSplitSetSeparator

object SimpleWhiteSpaceTableParser {

  def parse(source: io.Source, header: Boolean): Table = parse(source.getLines, header)

  def parse(lines: Iterator[String], header: Boolean): Table = {

    // fromTable[Tuple2[Seq[Seq[Int]], Seq[String]]]()
    val fields = if (header) {
      val header = lines.next
      Some(fastSplitSetSeparator(header, Set(' ', '\t')))
    } else None

    Table(
      (lines.filterNot(_.size == 0).map { line =>
        fastSplitSetSeparator(line, Set(' ', '\t')).map(x => Cell(x))
      }).toIndexedSeq.map(x => Row(x)),
      fields
    )

  }
  def parse(s: String, header: Boolean): Table = parse(io.Source.fromString(s), header)
}