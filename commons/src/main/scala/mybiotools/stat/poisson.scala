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

import org.apache.commons.math3.util.MathUtils
import org.apache.commons.math3.distribution.SaddlePointExpansionPublic

// This is translated from commons math, because their class initializes other features
object Poisson {
  private val LogTWO_PI = math.log(MathUtils.TWO_PI)
  def pdf(k: Int, mean: Double) = {
    assert(mean > 0.0, "Poisson mean <= 0")
    val ln = logProbability(k, mean)
    if (ln == Double.NegativeInfinity) 0 else math.exp(ln)
  }

  def logProbability(x: Int, mean: Double) = {

    if (x < 0 || x == Int.MaxValue) {
      Double.NegativeInfinity
    } else if (x == 0) {
      -1 * mean
    } else {
      -1 * SaddlePointExpansionPublic.getStirlingError(x) -
        SaddlePointExpansionPublic.getDeviancePart(x, mean) -
        0.5 * LogTWO_PI - 0.5 * math.log(x)
    }

  }

  def cumulative(x: Double, mean: Double) = {
    if (x < 0) {
      0
    } else if (x == Integer.MAX_VALUE) {
      1
    } else org.apache.commons.math3.special.Gamma.regularizedGammaQ(x + 1d, mean, 1e-12,
      10000000)
  }
}

