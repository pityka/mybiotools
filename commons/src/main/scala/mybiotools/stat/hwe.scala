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

object HWE {
  def chisq(aa: Int, aA: Int, AA: Int): ChiSqTestResult = {
    val n = aa + aA + AA
    val p = (2 * AA + aA) / (2 * (AA + aA + aa).toDouble)
    val q = 1 - p
    val expAA = p * p * n
    val expAa = 2 * p * q * n
    val expaa = q * q * n

    val chisq =
      (aa - expaa) * (aa - expaa) / expaa +
        (aA - expAa) * (aA - expAa) / expAa +
        (AA - expAA) * (AA - expAA) / expAA

    val pvalue = synchronized { jdistlib.ChiSquare.cumulative(chisq, 1.0, false, false) }
    ChiSqTestResult(chisq, pvalue, 1.0)

  }
  def exact(aa: Int, aA: Int, AA: Int): Double = if (aa + aA + AA > 10000) chisq(aa, aA, AA).pValue else
    htsjdk.tribble.util.popgen.HardyWeinbergCalculation.hwCalculate(aa, aA, AA)

}