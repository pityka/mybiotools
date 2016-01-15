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

sealed trait TabValue
object TabValue {
  def apply(s: String, header: Boolean)(implicit sep: Separator): Table = TableParser.parse(s, header)
}
case class Cell(value: String) extends TabValue

// sealed trait CellWithValue[+A] extends Cell {
//   val value: A
// }
// object EmptyCell extends Cell

// case class TextCell(value: String) extends CellWithValue[String]
// case class IntCell(value: Int) extends CellWithValue[Int]
// case class DoubleCell(value: Double) extends CellWithValue[Double]
// case class BoolCell(value: Boolean) extends CellWithValue[Boolean]

case class Row(cells: IndexedSeq[Cell]) extends TabValue

case class Table(rows: Seq[Row], header: Option[IndexedSeq[String]]) extends TabValue {
  assert(rows.map(_.cells.size).toSet.size <= 1, "nonuniform rows" + rows.map(_.cells.size).distinct + rows.groupBy(_.cells.size))
  if (header.isDefined) assert(rows.size == 0 || header.get.size == rows.head.cells.size, header)

}

