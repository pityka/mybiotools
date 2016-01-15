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

trait Primitives {
  implicit object IntFormat extends CellFormat[Int] {
    def writes(o: Int) = Cell.apply(o.toString)
    def reads(tab: Cell) = tab match {
      case Cell(n) => n.toInt
      case _ => throw new RuntimeException("Cell with Int expected")
    }
  }

  implicit object ShortFormat extends CellFormat[Short] {
    def writes(o: Short) = Cell.apply(o.toString)
    def reads(tab: Cell) = tab match {
      case Cell(n) => n.toShort
      case _ => throw new RuntimeException("Cell with Short expected")
    }
  }

  implicit object LongFormat extends CellFormat[Long] {
    def writes(o: Long) = Cell.apply(o.toString)
    def reads(tab: Cell) = tab match {
      case Cell(n) => n.toLong
      case _ => throw new RuntimeException("Cell with Long expected")
    }
  }

  implicit object FloatFormat extends CellFormat[Float] {
    def writes(o: Float) = Cell.apply(o.toString)
    def reads(tab: Cell) = tab match {
      case Cell(n) => n.toFloat
      case _ => throw new RuntimeException("Cell with Float expected")
    }
  }

  implicit object DoubleFormat extends CellFormat[Double] {
    def writes(o: Double) = Cell.apply(o.toString)
    def reads(tab: Cell) = tab match {
      case Cell(n) => n.toDouble
      case _ => throw new RuntimeException("Cell with Double expected")
    }
  }

  implicit object StringFormat extends CellFormat[String] {
    def writes(o: String) = Cell.apply(o)
    def reads(tab: Cell) = tab match {
      case Cell(n) => n
      case _ => throw new RuntimeException("Cell expected")
    }
  }
}
