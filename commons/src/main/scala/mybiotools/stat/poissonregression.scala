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

import org.ejml.simple.SimpleMatrix
import org.ejml.ops.MatrixFeatures
import org.ejml.data.DenseMatrix64F
import org.ejml.ops.CommonOps
import org.ejml.alg.dense.mult.MatrixVectorMult
import mybiotools.stat.LinearRegression._
import org.saddle._

import scala.util.{ Try, Success, Failure }

case class PoissonRegressionResult(
    covariates: Map[String, (Effect, ZTestResult)],
    intercept: (Effect, ZTestResult),
    numberOfSamples: Int,
    df: Double,
    logLikelihood: LogLikelihood,
    residuals: Vec[Double]
) extends RegressionResult {
  def predict(v: Series[String, Double]): Double = math.exp(covariates.map(x => v.first(x._1).get * x._2._1.slope).sum + intercept._1.slope * v.first("intercept").get)

  def covariate(s: String) = covariates.get(s)
  def lambda = 0.0
}

trait FailedPoissonRegression extends FailedRegression

object PoissonRegression {

  // generate dummy variables upstream
  def poissonRegression[I](
    data: Frame[I, String, Double],
    covariateNames: Seq[String],
    outcomes: Series[I, Double],
    missingMode: MissingMode,
    maxIter: Int,
    relativeTolerance: Double
  )(implicit ev: org.saddle.ST[I], ord: Ordering[I]): RegressionResultOrFailure = {

    val dataCols = data.colIx.toSeq.toSet

    val covariateNamesWithIntercept = "intercept" +: (covariateNames.filter(x => dataCols.contains(x)))

    val includedCovariates: Frame[I, String, Double] =
      LogisticRegression.prepareLeakyDataTable(covariateNamesWithIntercept, data, missingMode)

    val sharedInds = (includedCovariates.rowIx.toSeq.toSet & outcomes.index.toSeq.toSet).toSeq

    val filteredOutcomes: Series[I, Double] = outcomes(sharedInds: _*)
    val filteredIncludedCovariates = includedCovariates.row(sharedInds: _*)

    poissonRegression(filteredIncludedCovariates.toMat, covariateNamesWithIntercept, filteredOutcomes.toVec, maxIter, relativeTolerance)

  }

  def poissonRegression[I](
    data: Frame[I, String, Double],
    covariateNames: Seq[String],
    outcomes: Series[I, Double],
    missingMode: MissingMode
  )(implicit ev: org.saddle.ST[I], ord: Ordering[I]): RegressionResultOrFailure = poissonRegression(data, covariateNames, outcomes, missingMode, 50, 1E-10)

  def poissonRegression(
    covariateMatrix: Mat[Double],
    covariateNames: Seq[String],
    outcome: Vec[Double],
    maxIter: Int,
    relativeTolerance: Double
  ): RegressionResultOrFailure = {

    def xt = covariateMatrix.transpose

    sealed trait IterationProto
    case class IterationFailureNonInvertable(cause: Throwable) extends IterationProto
    case class Converged(point: Vec[Double], logL: Double) extends IterationProto
    case class Iteration(point: Vec[Double], logL: Double) extends IterationProto

    def vecmult(v1: Vec[Double], v2: Vec[Double]): Vec[Double] = {
      val n = v1.length
      val ar = Array.ofDim[Double](n)
      var i = 0
      while (i < n) {
        ar(i) = v1.raw(i) * v2.raw(i)
        i += 1
      }
      Vec(ar: _*)
    }

    def vecscale(v1: Vec[Double], a: Double): Vec[Double] = {
      val n = v1.length
      val ar = Array.ofDim[Double](n)
      var i = 0
      while (i < n) {
        ar(i) = v1.raw(i) * a
        i += 1
      }
      Vec(ar: _*)
    }

    def makeXtw(xt: Mat[Double], w: Vec[Double]) = {
      // scale each column of xt with w
      // Mat(covariateMatrix.transpose.cols zip predicted.toSeq map (x => vecscale(x._1, x._2)): _*)
      val array: Array[Double] = {
        val orig = xt.contents
        val dst = Array.ofDim[Double](orig.size)
        System.arraycopy(orig, 0, dst, 0, orig.size)
        dst
      }
      val numRows = xt.numRows
      val numCols = xt.numCols
      var i = 0
      while (i < numRows) {
        var j = 0
        while (j < numCols) {
          array(i * numCols + j) = array(i * numCols + j) * w.raw(j)
          j += 1
        }
        i += 1
      }
      Mat(numRows, numCols, array)

    }

    def logL(estimates: Vec[Double]) = {
      val logPredicted: Vec[Double] = (covariateMatrix dot estimates).col(0)
      val predicted: Vec[Double] = logPredicted.map(math.exp)
      (vecmult(outcome, logPredicted) - predicted).sum
    }

    def relativeError(next: Double, old: Double) = math.abs((next - old) / old)

    def invert(m: Mat[Double]) = {
      val inverted = SimpleMatrix.wrap(DenseMatrix64F.wrap(m.numRows, m.numCols, m.contents)).invert
      Mat(inverted.numRows, inverted.numCols, inverted.getMatrix.getData)
    }

    def nextGuess(currentEstimates: Vec[Double]): IterationProto = {

      val logPredicted: Vec[Double] = (covariateMatrix dot currentEstimates).col(0)
      val predicted: Vec[Double] = logPredicted.map(math.exp)
      val currentLogL = (outcome * logPredicted - predicted).sum
      // val w: Mat[Double] = mat.diag(predicted)
      val z: Vec[Double] = logPredicted + (outcome - predicted) / predicted
      val xtw = makeXtw(xt, predicted)
      val xtwxinverseTry = Try {
        val xtwx = xtw mult covariateMatrix
        invert(xtwx)
      }
      xtwxinverseTry.map(xtwxinverse => (xtwxinverse mult xtw dot z).col(0)) match {
        case Failure(cause) => IterationFailureNonInvertable(cause)
        case Success(x) if relativeError(logL(x), currentLogL) <= relativeTolerance => Converged(x, logL(x))
        case Success(x) => Iteration(x, logL(x))
      }

    }

    def loop(start: IterationProto): Stream[IterationProto] = start match {
      case IterationFailureNonInvertable(_) | Converged(_, _) => start #:: Stream.Empty
      case x: Iteration => start #:: loop(nextGuess(x.point))
    }

    def wrapup(estimates: Vec[Double], logL: Double) = {
      val logPredicted: Vec[Double] = (covariateMatrix dot estimates).col(0)
      val predicted: Vec[Double] = logPredicted.map(math.exp)
      // val w: Mat[Double] = mat.diag(predicted)

      val parameterErrors = {
        val xtw = Mat(covariateMatrix.transpose.cols zip predicted.toSeq map (x => x._1 * x._2): _*)
        val xtwx = invert(xtw mult covariateMatrix)
        val covarianceMatrix = xtwx
        Vec(0 until estimates.length map (i => math.sqrt(covarianceMatrix.raw(i, i))): _*)
      }

      val zscores = estimates / parameterErrors

      val df = covariateMatrix.numCols

      val residuals = outcome - predicted

      PoissonRegressionResult(
        covariates = covariateNames.drop(1).zipWithIndex.map {
        case (name, i) =>
          (name, (Effect(estimates.at(i + 1), parameterErrors.at(i + 1)), (ZTestResult(zscores.at(i + 1), 2 * (cern.jet.stat.Probability.normal(-1 * math.abs(zscores.at(i + 1))))))))
      }.toMap,
        intercept = (Effect(estimates.at(0), parameterErrors.at(0)), ZTestResult(zscores.at(0), 2 * (cern.jet.stat.Probability.normal(-1 * math.abs(zscores.at(0)))))),

        numberOfSamples = covariateMatrix.numRows,
        df = df,
        logLikelihood = LogLikelihood(L = logL, df = df, numberOfSamples = covariateMatrix.numRows),
        residuals = residuals
      )
    }

    val initialEstimate: Vec[Double] = {
      // this will not work for 0 counts, in that case take a vector of 1s.
      if (outcome.findOne(_ == 0) > -1) vec.ones(covariateMatrix.numCols)
      else {
        val dr = LinearRegression.DataForRegression(y = outcome.map(math.log), design = covariateMatrix, covariateNames = covariateNames)
        val ols = LinearRegression.linearRegression(dr, 0.0)
        ols.map(x => Vec(x.intercept +: x.slopes: _*)).getOrElse(vec.ones(covariateMatrix.numCols))
      }
    }

    val iteration =
      loop(nextGuess(initialEstimate))
        .take(maxIter)

    iteration.last match {
      case IterationFailureNonInvertable(cause) => NonInvertable(cause)
      case x: Iteration => (IterationDidNotConverge(maxIter, (x.point.toSeq.toVector, x.logL)))
      case Converged(estimates, l) => wrapup(estimates, l)
    }

  }
}