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

/**
 * Filon's quadrature for oscillatory integrands of the from f(x)*sin(t*x)
 *
 * Chase, S. M. & Fosdick, L. D. Commun. ACM 12, 453–457 (1969).
 */
object FilonQuadrature {

  def iterative(
    f: Double => Double,
    a: Double,
    b: Double,
    t: Double,
    rel: Double,
    abs: Double,
    startIter: Int,
    maxIterations: Int
  ) = {
    var i = startIter
    var n = 1 << startIter
    var i0 = integrate(f, a, b, t, n)
    n *= 2
    var i1 = integrate(f, a, b, t, n)

    @inline
    def fabs(x: Double, y: Double) = math.abs(x - y)

    @inline
    def frel(x: Double, y: Double) = math.abs((x - y) / x)

    while (i <= maxIterations && fabs(i0, i1) > abs && frel(i0, i1) > rel) {
      i0 = i1
      n *= 2
      i1 = integrate(f, a, b, t, n)
      i += 1
    }

    if (i == maxIterations) throw new RuntimeException("Failed to converge.")

    i1
  }

  def integrate(
    f: Double => Double,
    a: Double,
    b: Double,
    t: Double,
    n: Int
  ): Double = {
    if (a == b) 0.0
    else {
      if (n <= 1 || n % 2 != 0) throw new RuntimeException("n must be even and >= 2")

      val h = (b - a) / n
      val theta = t * h
      val sint = math.sin(theta)
      val p = n / 2
      val cost = math.cos(theta)

      val (alpha, beta, gamma) = if (6.0 * math.abs(theta) <= 1.0) {

        val alpha =
          2.0 * theta * theta * theta / 45.0 - 2.0 * math.pow(theta, 5) / 315.0 + 2.0 * math.pow(theta, 7) / 4725.0

        val beta = 2.0 / 3.0 + 2.0 * theta * theta / 15.0 - 4.0 * math.pow(theta, 4) / 105.0 + 2.0 * math.pow(theta, 6) / 567.0 - 4.0 * math.pow(theta, 8) / 22275.0

        val gamma = 4.0 / 3.0 - 2.0 * theta * theta / 15.0 + math.pow(theta, 4) / 210.0 - math.pow(theta, 6) / 11340.0

        (alpha, beta, gamma)
      } else {

        val alpha = (theta * theta + theta * sint * cost - 2.0 * sint * sint) / (theta * theta * theta)

        val beta = (2.0 * theta + 2.0 * theta * cost * cost - 4.0 * sint * cost) / (theta * theta * theta)

        val gamma = 4.0 * (sint - theta * cost) / (theta * theta * theta);

        (alpha, beta, gamma)
      }

      var s2p = 0.0
      var i = 0
      var x = a
      while (i <= p) {
        s2p += f(x) * math.sin(t * x)
        x += 2 * h
        i += 1
      }
      s2p -= 0.5 * (f(a) * math.sin(t * a) + f(b) * math.sin(t * b))

      var s2pm1 = 0.0
      var j = 1
      x = a + h
      while (j <= p) {
        s2pm1 += f(x) * math.sin(t * x)
        x += 2 * h
        j += 1
      }

      h * (
        alpha * (f(a) * math.cos(t * a) - f(b) * math.cos(t * b)) +
        beta * s2p +
        gamma * s2pm1
      )
    }
  }

}