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

import org.apache.commons.math3.stat.regression.SimpleRegression
import mybiotools._
import mybiotools.gwascommons.Individual
import mybiotools.gwascommons.indorder

object RSquared {

  /** http://bits.stephan-brumme.com/squareRoot.html */
  def fastAndVeryInprecisesqrt(d: Float): Float = {

    java.lang.Float.intBitsToFloat(
      (java.lang.Float.floatToIntBits(d) + (127 << 23)) >>> 1
    )

  }

  def rsquared(x: Array[Float], y: Array[Float], missingValue: Float): Float = {
    if (x.size != y.size) {
      throw new IllegalArgumentException("Vectors must have same length");
    }
    // if (x.size == 0) {
    //   throw new IllegalArgumentException("Vectors must contain data");
    // }

    val N = x.size;
    var sum_sq_x = 0.0f;
    var sum_sq_y = 0.0f;
    var sum_coproduct = 0.0f;
    var mean_x = -1f;
    var mean_y = -1f;
    var meanset = false
    var i = 1
    var j = 2
    while (i <= N) {
      val X = x.apply(i - 1)
      val Y = y.apply(i - 1)
      if (X != missingValue && Y != missingValue && !java.lang.Float.isNaN(X) && !java.lang.Float.isNaN(Y)) {
        if (!meanset) {
          mean_x = X
          mean_y = Y
          meanset = true
        } else {
          val sweep = (j - 1.0f) / j;
          val delta_x = X - mean_x;
          val delta_y = Y - mean_y;
          sum_sq_x += (delta_x * delta_x * sweep);
          sum_sq_y += (delta_y * delta_y * sweep);
          sum_coproduct += (delta_x * delta_y * sweep);
          mean_x += (delta_x / j);
          mean_y += (delta_y / j);
          j += 1
        }
      }
      i += 1
    }
    if (!meanset) Float.NaN
    else {
      (sum_coproduct * sum_coproduct) / (sum_sq_x * sum_sq_y);
    }
  }

  def rsquared(x: Array[Double], y: Array[Double], missingValue: Double): Double = {
    if (x.size != y.size) {
      throw new IllegalArgumentException("Vectors must have same length");
    }
    // if (x.size == 0) {
    //   throw new IllegalArgumentException("Vectors must contain data");
    // }

    val N = x.size;
    var sum_sq_x = 0.0;
    var sum_sq_y = 0.0;
    var sum_coproduct = 0.0;
    var mean_x = -1d;
    var mean_y = -1d;
    var meanset = false
    var i = 1
    var j = 2
    while (i <= N) {
      val X = x.apply(i - 1)
      val Y = y.apply(i - 1)
      if (X != missingValue && Y != missingValue && !java.lang.Double.isNaN(X) && !java.lang.Double.isNaN(Y)) {
        if (!meanset) {
          mean_x = X
          mean_y = Y
          meanset = true
        } else {
          val sweep = (j - 1.0) / j;
          val delta_x = X - mean_x;
          val delta_y = Y - mean_y;
          sum_sq_x += (delta_x * delta_x * sweep);
          sum_sq_y += (delta_y * delta_y * sweep);
          sum_coproduct += (delta_x * delta_y * sweep);
          mean_x += (delta_x / j);
          mean_y += (delta_y / j);
          j += 1
        }
      }
      i += 1
    }
    if (!meanset) Double.NaN
    else {
      (sum_coproduct * sum_coproduct) / (sum_sq_x * sum_sq_y);
    }
  }

}
