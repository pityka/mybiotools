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

import TabSerializationPrivate._

trait CollectionFormats extends Protocol {

  implicit def seqRowFormat[T](implicit fmt: CellFormat[T]): RowFormat[Seq[T]] = new RowFormat[Seq[T]] {
    def writes(ts: Seq[T], header: Option[IndexedSeq[String]]) = Row(ts.map(t => toCell(t)(fmt)).toIndexedSeq)
    def reads(tab: Row, header: Option[IndexedSeq[String]]) = tab.cells.map(t => fromCell(t)(fmt))
    def header(ts: Seq[T]) = None
  }

  implicit def seqTableFormat[T](implicit fmt: RowFormat[T]): TableFormat[Seq[T]] = new TableFormat[Seq[T]] {
    def writes(ts: Seq[T], h: Option[IndexedSeq[String]]) = {
      val headerFromRowFormat = if (ts.size == 0) None else fmt.header(ts.head)
      val header = (h, headerFromRowFormat) match {
        case (None, None) => None
        case (None, Some(x)) => Some(x)
        case (Some(y), Some(x)) if x.size > y.size => {
          Some(x.patch(0, y, y.size))
        }
        case (Some(y), Some(x)) => Some(y)
        case (x, None) => x
      }
      Table(ts.map(t => toRow(t, h)(fmt)), header)
    }
    def reads(tab: Table) = tab.rows.map(t => fromRow(t, tab.header)(fmt))

  }

  implicit def tableWithHeaderFormat[T](implicit fmt: TableFormat[T]): TableFormat[Tuple2[T, Seq[String]]] = new TableFormat[Tuple2[T, Seq[String]]] {
    def writes(in: Tuple2[T, Seq[String]], h: Option[IndexedSeq[String]]) = {
      val header = in._2.toIndexedSeq
      val ts = in._1
      val t1 = fmt.writes(ts, Some(header))
      Table(t1.rows, Some(header.toIndexedSeq))
    }
    def reads(tab: Table) = tab match {
      case Table(ts, header) => {
        val t1 = fmt.reads(Table(ts, header), header)
        (t1, header.get.toIndexedSeq)
      }
      // ((ts.map(t => fromRow(t, header)(fmt))), header.get.toIndexedSeq)
    }
  }

  implicit def byKeyWithHeader[T, KEY](implicit fmt: Format[T, Row], getKey: (T, IndexedSeq[String]) => KEY): TableFormat[Tuple2[Map[KEY, T], IndexedSeq[String]]] = new TableFormat[Tuple2[Map[KEY, T], IndexedSeq[String]]] {
    def writes(in: Tuple2[Map[KEY, T], IndexedSeq[String]], h: Option[IndexedSeq[String]]) = {
      val seq = in._1.toSeq.unzip
      val header = in._2
      val rows = seq._2
      Table(rows.map(t => toRow(t, Some(header))(fmt)).toIndexedSeq, Some(header))
    }
    def reads(tab: Table) = tab match {
      case Table(ts, header) => {
        val rows: Seq[T] = ts.map(t => fromRow(t, header)(fmt))
        (rows.map(x => getKey(x, header.get) -> x).toMap, header.get)
      }
    }
  }

}