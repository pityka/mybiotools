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

import mybiotools._
import org.apache.commons.math3.stat.correlation.Covariance
import org.apache.commons.math3.distribution.MultivariateNormalDistribution
import org.apache.commons.math3.distribution.ChiSquaredDistribution
import org.ejml.data.DenseMatrix64F
import org.ejml.simple.SimpleMatrix

object MultiVariateNormal {

  def covariance(data: Seq[Traversable[Double]]) = new Covariance(data.map(_.toArray).toArray, true).getCovarianceMatrix

  def significantMahalanobisDistance(confidence: Double, dimension: Int): Double = {
    val chisq = new ChiSquaredDistribution(dimension)
    math.sqrt(chisq.inverseCumulativeProbability(confidence))
  }

  def mahalanobisDistance(mean: Array[Double], covariance: SimpleMatrix, x: Array[Double]): Double = {
    val meanSM = SimpleMatrix.wrap(DenseMatrix64F.wrap(1, mean.size, mean))
    val xSM = SimpleMatrix.wrap(DenseMatrix64F.wrap(1, x.size, x))
    val diff = xSM.minus(meanSM)
    val diffT = diff.transpose

    val r = diff.mult(covariance.invert).mult(diffT)
    assert(r.numRows == r.numCols && r.numCols == 1)
    math.sqrt(r.get(0, 0))
  }

  def extremitiesOfConfidenceEllipsoid(covariance: SimpleMatrix, confidence: Double): Seq[SimpleMatrix] = {
    val mahalanobisThreshold = significantMahalanobisDistance(confidence, covariance.numRows)
    val evd = covariance.eig
    (0 until evd.getNumberOfEigenvalues) flatMap { idx =>
      val ev = evd.getEigenVector(idx)
      val evalue = evd.getEigenvalue(idx).getReal
      val evlength = math.sqrt(ev.transpose.mult(ev).get(0, 0))
      ev.scale(mahalanobisThreshold * math.sqrt(evalue) / evlength) :: ev.scale(-1 * mahalanobisThreshold * math.sqrt(evalue) / evlength) :: Nil
    }
  }

  def extremitiesOfConfidenceEllipsoid(data: Seq[Traversable[Double]], confidence: Double): Seq[SimpleMatrix] = {
    val cov = new SimpleMatrix(covariance(data).getData)
    val means = new SimpleMatrix(data.transpose.map(d => Array(SummaryStat(d).mean)).toArray)

    extremitiesOfConfidenceEllipsoid(cov, confidence).map(x => x.plus(means))

  }

}