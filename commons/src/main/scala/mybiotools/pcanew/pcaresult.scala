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

package mybiotools.pcanew

import mybiotools.gwascommons._

import org.saddle._

case class PCAResultFrames[RX, CX](projectedSampleCoordinates: Frame[RX, Int, Double], eigenValues: Vec[Double], eigenVectors: Frame[CX, Int, Double], loadings: Frame[CX, Int, Double])

case class PCAResult[RX](
    coordinates: Map[RX, Seq[Double]],
    eigenValues: Seq[Double]
) {
  def evecToFrame(implicit o: Ordering[RX], st: ST[RX]): Frame[RX, Int, Double] = {
    Frame(
      coordinates.map {
        case (ind, coords) =>
          ind -> Series(coords: _*)
      }.toSeq: _*
    ).T
  }

  def outliers(sdThreshold: Double, pcIdx: Int): Set[RX] = {
    val loadings = coordinates.map(x => x._1 -> x._2(pcIdx))
    val summary = mybiotools.SummaryStat(loadings.map(_._2))
    loadings.filter(x => x._2 < summary.mean - sdThreshold * summary.stddev ||
      x._2 > summary.mean + sdThreshold * summary.stddev).map(_._1).toSet
  }
}

object PCAResult {
  def fromSmartPCA(evecoutFile: scala.io.Source): PCAResult[Individual] = {
    val lines = evecoutFile.getLines

    val eigenValues = lines.next.replaceFirst("\\s+#", "#").split("\\s+").toList.tail.map(_.toDouble)

    val coordinates = lines.map { line =>
      // read the table of evecs
      val splitted = mybiotools.fastSplitSetSeparator(line, Set(' ', '\t')).toList
      val key: Individual = {
        val spl = splitted.head.split(":")
        Individual(spl(0), spl(1))
      }
      (key -> splitted.drop(1).dropRight(1).map(_.toDouble))
    }.toMap

    new PCAResult(coordinates, eigenValues)
  }
}