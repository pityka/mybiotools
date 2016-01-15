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

package mybiotools

import mybiotools._
import gwascommons._
import gwascommons.genotypedata._
import gwascommons.gwas._
import gwascommons.gwas.GWAS._
import org.saddle._
import scala.reflect.ClassTag

package object saddlehelpers {

  def envelopeToFrame[RX, CX, D](f: FrameInSerializableEnvelope[RX, CX, D])(
    implicit
    rst: ST[RX], rord: Ordering[RX], cst: ST[CX], dst: ST[D], cord: Ordering[CX]
  ): Frame[RX, CX, D] = {
    import f._
    val rowIx = Index(f.rowIx.toArray)
    val colIx = Index(f.colIx.toArray)

    Frame(Mat(rowIx.length, colIx.length, data.toArray), rowIx, colIx)

  }

  def frameToEnvelope[RX, CX, D](f: Frame[RX, CX, D])(implicit ev1: ClassTag[RX], ev2: ClassTag[CX], ev3: ClassTag[D]): FrameInSerializableEnvelope[RX, CX, D] = {
    FrameInSerializableEnvelope(f.rowIx.toSeq.toVector, f.colIx.toSeq.toVector, f.toMat.contents.toVector)
  }

  def envelopeToSeries[RX, D](f: SeriesInSerializableEnvelope[RX, D])(
    implicit
    rst: ST[RX], rord: Ordering[RX], dst: ST[D]
  ): Series[RX, D] = {
    import f._
    val rowIx = Index(f.rowIx.toArray)

    Series(Vec(data.toArray), rowIx)

  }

  def seriesToEnvelope[RX, D](f: Series[RX, D])(implicit ev1: ClassTag[RX], ev3: ClassTag[D]): SeriesInSerializableEnvelope[RX, D] = {
    SeriesInSerializableEnvelope(f.index.toSeq.toVector, f.toVec.toSeq.toVector)
  }

}