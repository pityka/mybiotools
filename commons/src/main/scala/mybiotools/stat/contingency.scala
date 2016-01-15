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

package mybiotools.stat

import org.saddle._

object ContingencyTable {

  def expectedCounts[RX, CX](frame: Frame[RX, CX, Int])(implicit ev1: ST[CX], ev2: ST[RX], ord1: Ordering[CX], ord2: Ordering[RX]): Frame[RX, CX, Double] = {
    val rowTotals = frame.T.sum
    val colTotals = frame.sum
    val total = rowTotals.sum
    frame.map {
      case (r, c, _) =>
        (r, c, rowTotals.first(r).get * colTotals.first(c).get / total.toDouble)
    }
  }

  def observedMinusExpectedSquaredOverExpected[RX, CX](frame: Frame[RX, CX, Int])(implicit ev1: ST[CX], ev2: ST[RX], ord1: Ordering[CX], ord2: Ordering[RX]): Frame[RX, CX, Double] = {
    val expected = expectedCounts(frame)
    val emo = frame - expected
    (emo ** 2) / expected
  }

  def observedMinusExpectedSquaredOverExpectedSigned[RX, CX](frame: Frame[RX, CX, Int], colExpectations: Series[CX, Double])(implicit ev1: ST[CX], ev2: ST[RX], ord1: Ordering[CX], ord2: Ordering[RX]): Frame[RX, CX, Double] = {
    val expected = expectedCounts(frame, colExpectations)
    val emo = frame - expected
    val sign = (emo > 0).mapValues(b => if (b) 1 else -1)
    (emo ** 2) * sign / expected
  }

  def perRowChisqTest[RX, CX](frame: Frame[RX, CX, Int])(implicit ev1: ST[CX], ev2: ST[RX], ord1: Ordering[CX], ord2: Ordering[RX]): Series[RX, ChiSqTestResult] = {
    val omesq = observedMinusExpectedSquaredOverExpected(frame).T
    val chisqsums: Series[RX, Double] = omesq.sum
    val chisqcounts: Series[RX, Int] = omesq.count
    chisqsums.joinMap(chisqcounts) { case (sum, count) => ChiSquareTest(sum, count) }

  }

  def expectedCounts[RX, CX](frame: Frame[RX, CX, Int], colExpectations: Series[CX, Double])(implicit ev1: ST[CX], ev2: ST[RX], ord1: Ordering[CX], ord2: Ordering[RX]): Frame[RX, CX, Double] = {
    assert(math.abs(colExpectations.toVec.sum - 1.0) < 1E-8, s"${colExpectations.toVec.sum}, ${colExpectations}")

    val rowTotals = frame.T.sum
    val total = rowTotals.sum
    frame.map {
      case (r, c, _) =>
        (r, c, rowTotals.first(r).get * colExpectations.first(c).get)
    }
  }

  def perRowChisqTest[RX, CX](frame: Frame[RX, CX, Int], colExpectations: Series[CX, Double])(implicit ev1: ST[CX], ev2: ST[RX], ord1: Ordering[CX], ord2: Ordering[RX]): Series[RX, ChiSqTestResult] = {
    val omesq = {
      val expected = expectedCounts(frame, colExpectations)
      val emo = frame - expected
      (emo ** 2) / expected
    }.T
    val chisqsums: Series[RX, Double] = omesq.sum
    val chisqcounts: Series[RX, Int] = omesq.count
    chisqsums.joinMap(chisqcounts) { case (sum, count) => ChiSquareTest(sum, count) }

  }

}