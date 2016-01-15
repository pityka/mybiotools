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

package mybiotools.saddlehelpers

import mybiotools._
import gwascommons._
import gwascommons.genotypedata._
import gwascommons.gwas._
import gwascommons.gwas.GWAS._
import org.saddle._
import scala.reflect.ClassTag

case class FrameInSerializableEnvelope[RX, CX, D](rowIx: Vector[RX], colIx: Vector[CX], data: Vector[D]) extends Serializable {
  override val hashCode = 41 * (rowIx.hashCode + 41 * (colIx.hashCode + 41 * (data.hashCode))) + 41

  def canEqual(other: Any): Boolean = other.isInstanceOf[FrameInSerializableEnvelope[RX, CX, D]]

  // I need to override this, because Double.NaN
  override def equals(that: Any): Boolean = that match {
    case t: FrameInSerializableEnvelope[RX, CX, D] => t.canEqual(this) && t.rowIx.toString == this.rowIx.toString && t.colIx.toString == this.colIx.toString && t.data.toString == this.data.toString
    case _ => false
  }
}

case class SeriesInSerializableEnvelope[RX, D](rowIx: Vector[RX], data: Vector[D]) extends Serializable {
  override val hashCode = 41 * (rowIx.hashCode + 41 * ((data.hashCode))) + 41

  def canEqual(other: Any): Boolean = other.isInstanceOf[SeriesInSerializableEnvelope[RX, D]]

  // I need to override this, because Double.NaN
  override def equals(that: Any): Boolean = that match {
    case t: SeriesInSerializableEnvelope[RX, D] => t.canEqual(this) && t.rowIx.toString == this.rowIx.toString && t.data.toString == this.data.toString
    case _ => false
  }
}

case class MatInSerializableEnvelope(numRows: Int, numCols: Int, data: Vector[Double]) extends Serializable {
  override val hashCode = 41 * (numCols.hashCode + 41 * (numRows.hashCode + 41 * ((data.hashCode)))) + 41

  def canEqual(other: Any): Boolean = other.isInstanceOf[MatInSerializableEnvelope]

  // I need to override this, because Double.NaN
  override def equals(that: Any): Boolean = that match {
    case t: MatInSerializableEnvelope => t.canEqual(this) && t.numRows == this.numRows && t.numCols == this.numCols && {
      var b = true
      var i = 0
      while (b && i < t.data.size) {
        if (t.data(i) == this.data(i) ||
          (t.data(i).isNaN && data(i).isNaN) ||
          (t.data(i).isPosInfinity && data(i).isPosInfinity) ||
          (t.data(i).isNegInfinity && data(i).isNegInfinity)) {
          b = true
        } else {
          b = false
        }
        i += 1
      }
      b
    }
    case _ => false
  }

  def unwrap = Mat(numRows, numCols, data.toArray)
}

