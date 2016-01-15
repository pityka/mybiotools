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

import scala.util.Try
import TabSerializationPrivate._

object RProtocol extends RProtocolBase {
  implicit val separator = Separator(',')
}

class RProtocolWithCustomSeparator(s: Char) extends RProtocolBase {
  implicit val separator = Separator(s)
}

trait RProtocolBase extends CollectionFormats with Primitives with ProductProtocol with SaddleVecProtocol {

  implicit object BooleanFormat extends CellFormat[Boolean] {
    def writes(o: Boolean) = Cell.apply(o.toString)
    def reads(tab: Cell) = tab match {
      case Cell(n) => n.toLowerCase match {
        case "true" => true
        case "t" => true
        case "1" => true
        case "yes" => true
        case "false" => false
        case "f" => false
        case "0" => false
        case "no" => false
      }
      case _ => throw new RuntimeException("Cell with Double expected")
    }
  }

  implicit def OptionFormat[T](implicit fmt: CellFormat[T]): CellFormat[Option[T]] = new CellFormat[Option[T]] {
    def writes(ot: Option[T]) = ot match {
      case Some(t) => toCell(t)(fmt)
      case None => Cell("NA")
    }
    def reads(cell: Cell) = cell match {
      case Cell("NA") => None
      case x: Cell => Some(fromCell[T](x)(fmt))
    }
  }

  implicit val GenderFormat: CellFormat[mybiotools.Gender] = new CellFormat[mybiotools.Gender] {
    def writes(ot: mybiotools.Gender) = ot match {
      case mybiotools.Male => Cell("Male")
      case mybiotools.Female => Cell("Female")
    }
    def reads(cell: Cell) = cell match {
      case Cell("Male") => mybiotools.Male
      case Cell("Female") => mybiotools.Female
    }
  }
}

class PlinkProtocol(val miss: String) extends PlinkProtocolBase

trait PlinkProtocolBase extends SaddleVecProtocol with CollectionFormats with Primitives with ProductProtocol {

  def miss: String

  lazy val numericMissing = Try(miss.toDouble).toOption

  implicit object BooleanFormat extends CellFormat[Boolean] {
    def writes(o: Boolean) = Cell.apply(o match {
      case true => "2"
      case false => "1"
    })
    def reads(tab: Cell) = tab match {
      case Cell(n) => n match {
        case "2" => true
        case "1" => false
      }
      case _ => throw new RuntimeException("Cell with Double expected")
    }
  }

  implicit def OptionFormat[T](implicit fmt: CellFormat[T]): CellFormat[Option[T]] = new CellFormat[Option[T]] {
    def writes(ot: Option[T]) = ot match {
      case Some(t) => toCell(t)(fmt)
      case None => Cell(miss)
    }
    def reads(cell: Cell) = cell match {
      case Cell(x) if x == miss || numericMissing.map(m => Try(x.toDouble).toOption.map(_ == m).getOrElse(false)).getOrElse(false) => None
      case x: Cell => Some(fromCell[T](x)(fmt))
    }
  }

  implicit val GenderFormat: CellFormat[mybiotools.Gender] = new CellFormat[mybiotools.Gender] {
    def writes(ot: mybiotools.Gender) = ot match {
      case mybiotools.Male => Cell("1")
      case mybiotools.Female => Cell("2")
    }
    def reads(cell: Cell) = cell match {
      case Cell("1") => mybiotools.Male
      case Cell("2") => mybiotools.Female
    }
  }

}