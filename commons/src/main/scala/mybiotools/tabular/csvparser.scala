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

/**
 * A CSV parser based on RFC4180
 *
 * `http://tools.ietf.org/html/rfc4180`
 *
 * `http://stackoverflow.com/questions/5063022/use-scala-parser-combinator-to-parse-csv-files`
 */
object TableParser {

  private class CSV(separator: Char, headerLine: Boolean) extends RegexParsers {

    object NoneEmptyRow {
      def unapply(s: Seq[Cell]): Option[Seq[Cell]] = if (s.size == 1 && s.head.value == "") None else Some(s)
    }

    override val skipWhitespace = false // meaningful spaces in CSV

    def COMMA = separator.toString
    def DQUOTE = "\""
    def DQUOTE2 = "\"\"" ^^ { case _ => "\"" } // combine 2 dquotes into 1
    def CRLF = "\r\n" | "\n"
    def TXT = "[^\",\r\n]".r
    def SPACES = "[ \t]+".r

    def file: Parser[Table] = repsep(record, CRLF) ^^ {
      case ls => {
        val filtered = ls.filterNot(row => row.cells.size == 1 && row.cells.head.value == "")
        if (headerLine) Table(filtered.tail, Some(filtered.head.cells.map(_.value)))
        else Table(filtered, None)
      }
    }

    def record: Parser[Row] = repsep(field, COMMA) ^^ {
      case ls => Row(ls.toIndexedSeq)
    }

    def field: Parser[Cell] = escaped | nonescaped

    def escaped: Parser[Cell] = {
      ((SPACES?) ~> DQUOTE ~> ((TXT | COMMA | CRLF | DQUOTE2)*) <~ DQUOTE <~ (SPACES?)) ^^ {
        case ls => Cell(ls.mkString(""))
      }
    }

    def nonescaped: Parser[Cell] = (TXT*) ^^ { case ls => Cell(ls.mkString("")) }

    def parse(s: Reader[Char]) = parseAll(file, s) match {
      case Success(res, _) => res
      case e => throw new Exception(e.toString)
    }
  }

  def parse(s: String, headerLine: Boolean)(implicit sepformat: Separator) = {
    new CSV(sepformat.separator, headerLine).parse(new CharArrayReader(s.toArray))
  }
  def parse(s: Reader[Char], headerLine: Boolean)(implicit sepformat: Separator) = {
    new CSV(sepformat.separator, headerLine).parse(s)
  }
}