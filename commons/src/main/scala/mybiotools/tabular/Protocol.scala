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

object TabSerializationPrivate {

  def toCell[T](o: T)(implicit tjs: Writes[T, Cell]): Cell = tjs.writes(o, None)
  def toTable[T](o: T)(implicit tjs: Writes[T, Table]): Table = tjs.writes(o, None)
  def toTable[T](o: T, header: IndexedSeq[String])(implicit tjs: Writes[T, Table]): Table = tjs.writes(o, Some(header))
  def toRow[T](o: T, header: Option[IndexedSeq[String]])(implicit tjs: Writes[T, Row]): Row = tjs.writes(o, header)

  def fromCell[T](tab: Cell)(implicit fjs: Reads[T]): T = fjs.reads(tab, None)
  def fromTable[T](tab: Table)(implicit fjs: Reads[T]): T = fjs.reads(tab, tab.header)
  def fromRow[T](tab: TabValue, header: Option[IndexedSeq[String]])(implicit fjs: Reads[T]): T = fjs.reads(tab, header)

}

object TabSerialization {
  def toTab[T](o: T)(implicit tjs: Writes[T, Table]): Table = tjs.writes(o, None)
  def toTab[T](o: T, header: IndexedSeq[String])(implicit tjs: Writes[T, Table]): Table = tjs.writes(o, Some(header))

  def fromTab[T](tab: Table)(implicit fjs: Reads[T]): T = fjs.reads(tab, tab.header)

  def selectKeyFromHeader[T](keyName: String): (Seq[T], IndexedSeq[String]) => T = (row: Seq[T], header: IndexedSeq[String]) => row(header.indexOf(keyName))
}

sealed trait Writes[T, K <: TabValue] {
  def writes(o: T, header: Option[IndexedSeq[String]]): K
}

sealed trait Reads[T] {
  def reads(tab: TabValue, header: Option[IndexedSeq[String]]): T
}

sealed trait Format[T, K <: TabValue] extends Writes[T, K] with Reads[T]

trait CellFormat[T] extends Format[T, Cell] {
  def writes(o: T): Cell
  def reads(tab: Cell): T

  override def reads(tab: TabValue, header: Option[IndexedSeq[String]]) = reads(tab.asInstanceOf[Cell])
  override def writes(o: T, header: Option[IndexedSeq[String]]) = writes(o)
}

trait TableFormat[T] extends Format[T, Table] {
  def reads(tab: Table): T
  // def writes(o: T): Table

  // use the Table's header not the parameter
  override def reads(tab: TabValue, header: Option[IndexedSeq[String]]) = reads(tab.asInstanceOf[Table])
  // override def writes(o: T, header: Option[IndexedSeq[String]]) = writes(o)
}

trait RowFormat[T] extends Format[T, Row] {
  def reads(tab: Row, header: Option[IndexedSeq[String]]): T
  override def reads(tab: TabValue, header: Option[IndexedSeq[String]]) = reads(tab.asInstanceOf[Row], header)

  def header(t: T): Option[IndexedSeq[String]]

}

// trait TupleFormat[T] extends RowFormat[T] {
//   def width : Int
// }

case class Separator(separator: Char)

trait Protocol {
  implicit val IntFormat: CellFormat[Int]
  implicit val ShortFormat: CellFormat[Short]
  implicit val LongFormat: CellFormat[Long]
  implicit val BooleanFormat: CellFormat[Boolean]
  implicit val FloatFormat: CellFormat[Float]
  implicit val DoubleFormat: CellFormat[Double]
  implicit val StringFormat: CellFormat[String]
  implicit def OptionFormat[T](implicit fmt: CellFormat[T]): CellFormat[Option[T]]
}

