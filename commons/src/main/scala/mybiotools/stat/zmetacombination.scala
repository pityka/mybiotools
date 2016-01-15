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
import cern.jet.stat.Probability

object ZMetaCombination {

  /**
   * Convert P-value to Z-score
   *
   *  @param pval p-value
   *  @param beta effect size, only the sign is taken into account
   */
  private def convertpvalueToZ(pval: Double, beta: Double) =
    if (beta < 0) {
      Probability.normalInverse(pval / 2)
    } else {
      -1 * Probability.normalInverse(pval / 2)
    }

  /** Converts (count,beta,p) triplets into (count,z) pairs */
  def countBetaPsToZ(countsWithBetaP: Iterable[(Int, Double, Double)]) = countsWithBetaP.map {
    case (count, beta, p) =>
      (count, convertpvalueToZ(p, beta))
  }

  /** Converts (count,beta,se) triplets into (count,beta/se) pairs */
  def countBetaSEsToZ(countsWithBetaSE: Iterable[(Int, Double, Double)]) = countsWithBetaSE.map {
    case (count, beta, se) =>
      (count, beta / se)
  }

  /**
   * Converts (count,z) pairs into a count weighted sum Z variable (Stouffer's method) pairs
   *
   * From Paul's files, Stouffer's method with n_i/sum(n_i) weights.
   */
  def countsWeightedSumZ(countsWithZ: Iterable[(Int, Double)]): Double = {

    val nsum = countsWithZ.map(_._1).sum

    countsWithZ.map(x => x._2 * math.sqrt(x._1 / nsum.toDouble)).sum
  }

  /**
   * Stouffer's meta analysis from plink assoc files
   *
   * @param countsWithBetaP (count,beta,p) triplets
   */
  def plinkZMeta(countsWithBetaP: Iterable[(Int, Double, Double)]): Double = {
    zTest2Sided(countsWeightedSumZ(countBetaPsToZ(countsWithBetaP)))
  }

  /**
   * Converts (beta,se) pairs into inverse-variance weighted sums.
   *
   * http://www.nist.gov/itl/sed/training/upload/combine-1.pdf page 27
   */
  def inverseVarianceWeightedSumZ(betaSE: Iterable[(Double, Double)]): Double = {
    val weightsum = betaSE.map { case (beta, se) => 1.0 / (se * se) }.sum

    //  A/B/sqrt(B) = A/sqrt(B)
    (betaSE.map {
      case (beta, se) =>
        beta / (se * se)
    }.sum) * math.sqrt(1.0 / weightsum)
  }

  /**
   * Converts (beta,se) pairs into inverse-variance weighted sums
   *
   * http://www.nist.gov/itl/sed/training/upload/combine-1.pdf page 27
   */
  def inverseVarianceWeightedSum(betaSE: Iterable[(Double, Double)]): Double = inverseVarianceWeightedSumWithSD(betaSE)._1

  /**
   * Converts (beta,se) pairs into inverse-variance weighted sums and its SD.
   *
   * http://www.nist.gov/itl/sed/training/upload/combine-1.pdf page 27
   */
  def inverseVarianceWeightedSumWithSD(betaSE: Iterable[(Double, Double)]): (Double, Double) = {
    val weightsum = betaSE.map { case (beta, se) => 1.0 / (se * se) }.sum

    val weightedMean = (betaSE.map {
      case (beta, se) =>
        beta / (se * se)
    }.sum) * (1.0 / weightsum)
    val sdOfWeightedMean = math.sqrt((1.0 / weightsum))
    (weightedMean, sdOfWeightedMean)
  }

  /** 2 sided z test. PHI(-1*|z|)*2 */
  def zTest2Sided(z: Double): Double =
    Probability.normal(-1 * (math.abs(z))) * 2;

  /** 1 sided z test, upper tail. 1-PHI(z) */
  def zTest1sided_upper(z: Double): Double =
    1.0 - Probability.normal(z)

}