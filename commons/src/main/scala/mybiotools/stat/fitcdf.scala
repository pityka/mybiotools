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

import mybiotools.plots.ScatterPlot._

object FitCDF {

  def qqplot(
    q1: Double => Double,
    q2: Double => Double,
    min: Double,
    max: Double,
    xlab: String = "",
    ylab: String = "",
    main: String = ""
  ) = {
    val step = (max - min) / 10000
    val range = min to max by step
    mybiotools.plots.ScatterPlot.createScatterPlot(
      range map { i =>
        q1(i) -> q2(i)
      } toIndexedSeq,
      xlab = xlab,
      ylab = ylab,
      main = main
    )
  }

  case class CumulativeRelativeFrequencies(cumulative: IndexedSeq[(Double, Double)]) {
    def cdf(loc: Double): Double = mybiotools.binarySearch[(Double, Double)](
      cumulative,
      (x: (Double, Double)) => x._1 < loc,
      (x: (Double, Double)) => x._1 > loc
    ) match {
        case Right((idx, elem)) => cumulative(idx)._2
        case Left((idx1, idx2)) if idx1 >= 0 => cumulative(idx1)._2
        case Left((idx1, idx2)) if idx1 < 0 => 0.0
      }
  }

  def cumulative(data: Seq[Double]) = {
    val sorted = data.sorted
    val counts = sorted.foldLeft((sorted.head, 0, Map[Double, Int]())) {
      case ((last, count, map), elem) =>
        if (elem == last) {
          val nl = map.get(elem) match {
            case None => map.updated(elem, count + 1)
            case Some(x) => map.updated(elem, x + 1)
          }
          (elem, count + 1, nl)
        } else {
          (elem, count + 1, map.updated(elem, count + 1))
        }
    }
    val cumulative: IndexedSeq[(Double, Double)] = counts._3.map(x => x._1 -> x._2.toDouble / data.size).toSeq.sortBy(_._1).toIndexedSeq
    CumulativeRelativeFrequencies(cumulative)

  }
  def fitCDF(
    data: Seq[Double],
    cdf: (Seq[Double], Double) => Double,
    initial: Seq[Double],
    lowerBounds: Seq[Double],
    upperBounds: Seq[Double],
    usePoints: Seq[Double] = Nil
  ): (Seq[Double], Double) = {
    val distinct = if (usePoints.isEmpty) data.distinct else usePoints.distinct
    val cum = cumulative(data)

    val norm = (param: Seq[Double]) => {
      val r = distinct.foldLeft(0.0) { (acc, x) =>
        val emp = math.log(cum.cdf(x))

        if (!emp.isNaN) {
          val the = math.log(cdf(param, x))
          acc + (emp - the) * (emp - the)
        } else acc

      }
      r
    }

    val optim = minimizeN(norm, initial, lowerBounds, upperBounds, 1E-8, 1E-8)
    val minimum = norm(optim)
    (optim, minimum)

  }
}