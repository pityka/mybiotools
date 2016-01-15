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

object MannWhitneyU {

  trait Mark
  case object Left extends Mark
  case object Right extends Mark

  case class MWUTestStats(uLeft: Double, uRight: Double, sumRanksOfLeft: Double, n1: Long, n2: Long)

  case class MWUTestResult(pValue: Double, z: Double) {
    override def toString = s"MWUTestResult(pValue=$pValue, z=$z)"
  }

  @specialized(Double)
  def mannWhitneyUTestXLessThanY[T](x: Traversable[T], y: Traversable[T])(implicit fd: Ordering[T]): MWUTestResult = {
    val n1 = x.size.toLong
    val n2 = y.size.toLong
    val ts = mannWhitneyU(x, y)
    val uLeft = ts.uLeft

    oneSidedPValueFromTestStat(uLeft, n1, n2)
  }

  @specialized(Double)
  def mannWhitneyUTest[T](x: Traversable[T], y: Traversable[T])(implicit fd: Ordering[T]): MWUTestResult = {
    val n1 = x.size.toLong
    val n2 = y.size.toLong
    val ts = mannWhitneyU(x, y)
    val uLeft = ts.uLeft

    pValueFromTestStat(uLeft, n1, n2)
  }

  private def pValueFromTestStat(uLeft: Double, n1: Long, n2: Long) = {
    val n1n2prod = n1 * n2
    val mu: Double = n1n2prod / 2.0
    val sd: Double = math.sqrt((n1n2prod * (n1 + n2 + 1)) / 12.0)

    val z = (uLeft - mu) / sd

    MWUTestResult(jdistlib.Normal.cumulative(math.abs(z), 0, 1, false, false) * 2, z)
  }

  private def oneSidedPValueFromTestStat(uLeft: Double, n1: Long, n2: Long) = {
    val n1n2prod = n1 * n2
    val mu: Double = n1n2prod / 2.0
    val sd: Double = math.sqrt((n1n2prod * (n1 + n2 + 1)) / 12.0)

    val z = (uLeft - mu) / sd

    MWUTestResult(jdistlib.Normal.cumulative(z, 0, 1, true, false), z)
  }

  @specialized(Double)
  def mannWhitneyUTestXLessThanYPresorted[T](mergedSorted: Seq[(T, Mark)]): MWUTestResult = {

    val ts = mannWhitneyUPreSorted(mergedSorted)
    import ts._

    pValueFromTestStat(uLeft, n1, n2)
  }

  @specialized(Double)
  def mannWhitneyU[T](x: Traversable[T], y: Traversable[T])(implicit fd: Ordering[T]) = {

    if (x.size + y.size <= 2 || x.size == 0 || y.size == 0) throw new RuntimeException("Not enough data")

    val mergedSorted = {
      val ab = scala.collection.mutable.ArrayBuffer[(T, Mark)]()
      x.foreach { z =>
        ab.append((z, Left))
      }
      y.foreach { z =>
        ab.append((z, Right))
      }
      ab.result.sortBy(_._1)
    }
    mannWhitneyUPreSorted(mergedSorted)
  }

  @specialized(Double)
  def mannWhitneyUPreSorted[T](mergedSorted: Seq[(T, Mark)]): MWUTestStats = {

    var n1 = 0L
    var n2 = 0L

    val ranksOfLeft: Double = {
      var rank = 0L
      var sum = 0.0 // accumulates Left ranks
      val iter = mergedSorted.iterator
      val tieBuffer = scala.collection.mutable.ArrayBuffer[(Mark, Long)]() // accumulates tie band
      var elem: Option[(T, Mark)] = None

      if (elem.isEmpty) {
        elem = Some(iter.next)
        elem.get._2 match {
          case Left => { n1 += 1 }
          case Right => { n2 += 1 }
        }
        rank += 1
        tieBuffer.append((elem.get._2, rank))
      }
      var value = elem.get._1
      while (iter.hasNext) {
        elem = Some(iter.next)
        elem.get._2 match {
          case Left => { n1 += 1 }
          case Right => { n2 += 1 }
        }
        if (value == elem.get._1) {
          rank += 1
          tieBuffer.append((elem.get._2, rank))
        } else {

          val rankedTie = if (tieBuffer.length == 1) tieBuffer.head._2 else tieBuffer.map(_._2).sum / tieBuffer.length.toDouble

          tieBuffer.filter(_._1 == Left).foreach { x =>
            sum += rankedTie
          }
          tieBuffer.clear
          value = elem.get._1
          rank += 1
          tieBuffer.append((elem.get._2, rank))
        }
      }
      val rankedTie = if (tieBuffer.length == 1) tieBuffer.head._2 else tieBuffer.map(_._2).sum / tieBuffer.length.toDouble

      tieBuffer.filter(_._1 == Left).foreach { x =>
        sum += rankedTie
      }

      sum
    }

    val uLeft = ranksOfLeft - n1 * (n1 + 1) / 2.0
    val uRight = n1 * n2 - uLeft
    MWUTestStats(uLeft, uRight, ranksOfLeft, n1, n2)
  }

}