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

import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics

/** Descriptive statistics. Like R's summary(). */
case class SummaryStat(p25: Double, p50: Double, p75: Double, mean: Double, stddev: Double, min: Double, max: Double, count: Int, p2_5: Double, p97_5: Double) extends {
  def median = p50

  def toLine = p25 + " " + p50 + " " + p75 + " " + mean + " " + stddev + " " + min + " " + max + " " + count

  def iqr = p75 - p25

  override def toString = "SummaryStat(" +
    "p25: " + p25 + ", " +
    "median: " + p50 + ", " +
    "p75: " + p75 + ", " +
    "mean: " + mean + ", " +
    "stddev: " + stddev + ", " +
    "p2.5: " + p2_5 + ", " +
    "p97.5: " + p97_5 + ", " +
    "min: " + min + ", " +
    "max: " + max + ", " +
    "count: " + count +
    ")"
}

object SummaryStat {

  def apply(ds: DescriptiveStatistics): SummaryStat = SummaryStat(
    p25 = ds.getPercentile(25.0),
    p50 = ds.getPercentile(50.0),
    p75 = ds.getPercentile(75.0),
    mean = ds.getMean,
    stddev = scala.math.sqrt(ds.getVariance),
    min = ds.getMin,
    max = ds.getMax,
    count = ds.getN.toInt,
    p2_5 = ds.getPercentile(2.5),
    p97_5 = ds.getPercentile(97.5)
  )

  def apply[T](ts: Traversable[T])(implicit numeric: Numeric[T]): SummaryStat = {
    val ds = new org.apache.commons.math3.stat.descriptive.DescriptiveStatistics()
    ts.map(x => numeric.toDouble(x)).foreach(x => ds.addValue(x))
    SummaryStat(ds)
  }

  def apply[T](ts: Traversable[T], removeNaN: Boolean)(implicit numeric: Numeric[T]): SummaryStat = {
    val ds = new org.apache.commons.math3.stat.descriptive.DescriptiveStatistics()
    val t = ts.map(x => numeric.toDouble(x))
    (if (removeNaN) t.filter(x => !java.lang.Double.isNaN(x)) else t).foreach(x => ds.addValue(x))
    SummaryStat(ds)
  }

}