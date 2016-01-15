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

import org.apache.commons.math3.stat.ranking.{ NaturalRanking, NaNStrategy }
import cern.jet.stat.Probability

/**
 * Rank based inverse normal transformation
 *
 * From http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2921808/pdf/nihms-218012.pdf Eq.1
 */
object BlomTransformation {

  private val ranker = new NaturalRanking(NaNStrategy.FIXED)

  def apply(in: Seq[Double]): Seq[Double] = {
    val ranks = ranker.rank(in.toArray)

    val n = ranks.size - (ranks.count(_.isNaN))
    val c = 3.0 / 8.0
    val z = ranks.map(r => (r - c) / (n - 2 * c + 1))
    z.map(Probability.normalInverse)
  }

  def withOptions(in: Seq[Option[Double]]): Seq[Option[Double]] = {
    val x = in.map(_.getOrElse(Double.NaN))
    apply(x).map(x => if (x.isNaN) None else Some(x))
  }

}