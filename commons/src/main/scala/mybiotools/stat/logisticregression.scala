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

import org.ejml.factory.LinearSolverFactory
import org.ejml.simple.SimpleMatrix
import org.ejml.ops.MatrixFeatures
import org.ejml.data.DenseMatrix64F
import org.ejml.ops.CommonOps
import org.ejml.alg.dense.mult.MatrixVectorMult

import mybiotools.gwascommons.Individual
import mybiotools.gwascommons.indorder
import mybiotools._
import mybiotools.eq._

import org.saddle._
import scala.util.{ Try, Success, Failure }

case class LogLikelihood(L: Double, df: Double, numberOfSamples: Int)

case object BIC {
  def apply(l: LogLikelihood) = -2.0 * l.L + l.df * math.log(l.numberOfSamples)
}

sealed trait RegressionResultOrFailure

trait ScoreTestResult extends RegressionResult with TestResult {
  def pValue: Double
  def statistic: Double
  def numberOfSamples: Int
  def df: Int

  def covariate(s: String) = None
  def covariates: Map[String, (mybiotools.stat.Effect, mybiotools.stat.TestResult)] = Map()
  def intercept: (mybiotools.stat.Effect, mybiotools.stat.TestResult) = (Effect(Double.NaN, Double.NaN), ZTestResult(Double.NaN, Double.NaN))
  def lambda: Double = Double.NaN
  def logLikelihood: mybiotools.stat.LogLikelihood = LogLikelihood(Double.NaN, Double.NaN, numberOfSamples)

}

case class ScoreTestResultImpl(statistic: Double, pValue: Double, df: Int, numberOfSamples: Int) extends ScoreTestResult {
  def predict(v: Series[String, Double]): Double = throw new UnsupportedOperationException("score test does not estimate")

}

trait Prediction {
  def predict(v: Series[String, Double]): Double
  def predict[I](m: Frame[I, String, Double])(implicit o: Ordering[I], st: ST[I]): Series[I, Double] = Series(m.toRowSeq.map(s => s._1 -> predict(s._2)): _*)
  def residuals[I](m: Frame[I, String, Double], y: Series[I, Double])(implicit o: Ordering[I], st: ST[I]): Series[I, Double] = y - predict(m)
}

trait RegressionResult extends RegressionResultOrFailure with Prediction {
  def covariate(s: String): Option[(Effect, TestResult)]
  def covariates: Map[String, (Effect, TestResult)]
  def intercept: (Effect, TestResult)
  def numberOfSamples: Int
  def predict(v: Series[String, Double]): Double

  def toLine = s"""${covariates.map(x => x._1 + ":" + x._2._2.statistic + "/" + x._2._2.pValue).mkString(" ")} $numberOfSamples"""

  private def tableLine(d: (String, (Effect, TestResult))) =
    s"${d._1.padTo(12, " ").mkString}\t${d._2._1.slope.sFormat.padTo(12, " ").mkString}\t${d._2._1.sd.sFormat.padTo(12, " ").mkString}\t${d._2._2.statistic.sFormat.padTo(12, " ").mkString}\t${d._2._2.pValue.sFormat.padTo(12, " ").mkString}"

  def table(phenoname: String = "y") = {
    // val f = new java.math.DecimalFormat("0.##E0");
    s"""
  |Call: $phenoname ~ 1 + ${covariates.map(_._1).mkString(" + ")}
  |Coefficients:
  |\t\tEstimate    \tStd.Error   \tStatistic   \tp-value
  |(Intercept)\t${intercept._1.slope.sFormat.padTo(12, " ").mkString}\t${intercept._1.sd.sFormat.padTo(12, " ").mkString}\t${intercept._2.statistic.sFormat.padTo(12, " ").mkString}\t${intercept._2.pValue.sFormat.padTo(12, " ").mkString}
  |${covariates.toSeq.map(x => tableLine(x)).mkString("\n")}
  |N=$numberOfSamples""".stripMargin
  }

  def lambda: Double

  def logLikelihood: LogLikelihood

  def header(names: List[String] = Nil): Vector[String] =
    Vector("NSamples") ++
      covariates.toSeq.sortBy(_._1).filter(x => if (names.isEmpty) true else names.contains(x._1)).map {
        case (name, (Effect(slope, sd), TestResult(stat, pvalue))) =>
          Vector(name + "Effect", name + "SD", name + "Stat", name + "Pvalue")
      }.flatten

  def toLine(names: List[String] = Nil): Vector[String] =
    Vector(numberOfSamples.toString) ++
      covariates.toSeq.sortBy(_._1).filter(x => if (names.isEmpty) true else names.contains(x._1)).map {
        case (name, (Effect(slope, sd), TestResult(stat, pvalue))) =>
          Vector(slope.toString, sd.toString, stat.toString, pvalue.toString)
      }.flatten

}

sealed trait IterationProto
case object IterationFailureNonInvertable extends IterationProto
case class Iteration(point: Array[Double], lprimesum: Double) extends IterationProto {
  def _1 = point
  def _2 = lprimesum
}

case class LogisticRegressionEstimates(
  coefficients: IndexedSeq[Double], // log of OR
  standardErrors: IndexedSeq[Double],
  zStats: IndexedSeq[Double],
  pValues: IndexedSeq[Double],
  logLikelihood: LogLikelihood
)

case class ZTestResult(statistic: Double, pValue: Double) extends TestResult

trait AbstractRegressionResult extends RegressionResult {

  protected def data: Vec[Double]
  protected def names: Seq[String]
  def numberOfSamples: Int
  protected def _logLikelihood: Double
  protected def df: Double

  def testFactory(a: Double, b: Double): TestResult

  private def decode(idx: Int) = {
    val effect = data.raw(idx * 4)
    val sd = data.raw(idx * 4 + 1)
    val stat = data.raw(idx * 4 + 2)
    val p = data.raw(idx * 4 + 3)
    Effect(effect, sd) -> testFactory(stat, p)
  }

  def lambda = 0.0

  def covariate(i: Int) = Some(decode(i))

  def covariate(s: String) = {
    val idx = names.indexOf(s)
    if (idx == -1) None
    else {
      Some(decode(idx + 1))
    }
  }

  def covariates = (names.zipWithIndex.map {
    case (name, idx) =>
      name -> decode(idx + 1)
  }).toMap

  def logLikelihood = LogLikelihood(_logLikelihood, df, numberOfSamples)

  def intercept = decode(0)

}

case class LogisticRegressionResult(
    protected val data: Vec[Double],
    protected val names: Seq[String],
    val numberOfSamples: Int,
    protected val _logLikelihood: Double,
    protected val df: Double
) extends AbstractRegressionResult {
  def predict(v: Series[String, Double]): Double = logistic(covariates.map(x => v.first(x._1).get * x._2._1.slope).sum + intercept._1.slope * v.first("intercept").get)

  def testFactory(a: Double, b: Double) = ZTestResult.apply(a, b)
}

private[stat] object LogisticRegressionResult {
  def apply(
    covariates: Map[String, (Effect, ZTestResult)],
    intercept: (Effect, ZTestResult),
    numberOfSamples: Int,
    logLikelihood: LogLikelihood
  ): LogisticRegressionResult = {
    val names = covariates.keys.toVector.sorted
    val data: Array[Double] = Array(intercept._1.slope, intercept._1.sd, intercept._2.statistic, intercept._2.pValue) ++ names.toArray.flatMap(x => Array(covariates(x)._1.slope, covariates(x)._1.sd, covariates(x)._2.statistic, covariates(x)._2.pValue))
    LogisticRegressionResult(Vec(data), names, numberOfSamples, logLikelihood.L, logLikelihood.df)
  }
}

trait FailedRegression extends RegressionResultOrFailure
case object DesignMatrixIsEmpty extends FailedRegression
case class NonInvertable(cause: Throwable) extends FailedRegression with FailedLogisticRegression with FailedPoissonRegression

sealed trait FailedLogisticRegression extends FailedRegression
case class HessianIsNotNegativeDefinite(cause: Throwable) extends FailedLogisticRegression
case class IterationDidNotConverge(iterations: Int, lastIteration: (IndexedSeq[Double], Double)) extends FailedLogisticRegression with FailedPoissonRegression

case class DataForScoreTest(nullFit: Vec[Double], pThreshold: Double, df: Int)

/**
 * Logistic regression
 *
 * Implementation of logistic regression based on Scott A. Czeipel's article (http://czep.net/).
 * See the article for explanations for the notation and concepts.
 *
 * TODO: check for the definiteness of the second derivative (did we reached a local maximum, or minimum?). check whether we are going upwards or downwards on the likelihood surface.
 * Also see http://publib.boulder.ibm.com/infocenter/spssstat/v20r0m0/index.jsp?topic=%2Fcom.ibm.spss.statistics.help%2Falg_cslogistic_cov.htm
 *
 * K the number of factors. N the number of aggregated bins (populations).
 *
 * @param designMatrix  is a matrix with N rows and K+1 columns, in rowmajor order. K
 * @param outComeCounts  is length N. Number of positive outcomes for every of the N populations.
 * @param aggregatedBinLengths  is length N. Number of samples in each populations (positive and negative outcomes).
 */
class LogisticRegression(
    designMatrix: Mat[Double],
    outComeCounts: Vec[Int],
    aggregatedBinLengths: Vec[Int]
) {

  def this(
    designMatrix: Array[Array[Double]],
    outComeCounts: Array[Int],
    aggregatedBinLengths: Array[Int]
  ) = this(Mat(designMatrix).T, Vec(outComeCounts), Vec(aggregatedBinLengths))

  import LogisticRegression._

  assert(aggregatedBinLengths.length == outComeCounts.length, "outComeCounts.size != aggregatedBinLengths.size")
  assert(aggregatedBinLengths.length == designMatrix.numRows)
  assert(designMatrix.numRows > 0 && designMatrix.numCols > 0, "designmatrix is empty")

  private val X = new SimpleMatrix(designMatrix.numRows, designMatrix.numCols, true, designMatrix.contents: _*)

  // private val XT = X.transpose

  private val currentOdds = Array.ofDim[Double](aggregatedBinLengths.length)

  private val xtwxshared = new DenseMatrix64F(X.numCols, X.numCols)
  private val wxshared = new DenseMatrix64F(X.numRows, X.numCols)

  /**
   * Starts iteration until the first derivative of the log likelihood (l') is < convergeTo.
   * Starting estimates (betas) are 0.5 for each factor.
   * @return
   */
  def estimateWithFailure(maxIter: Int, epsilon: Double): Either[FailedLogisticRegression, LogisticRegressionEstimates] = {
    val (notconverged, converged) =
      from(Array.fill(X.numCols)(0.0))
        .take(maxIter).span(_ match {
          case IterationFailureNonInvertable => true
          case x: Iteration => x._2 >= epsilon
        })

    converged.headOption match {
      case None => notconverged.last match {
        case IterationFailureNonInvertable => Left(NonInvertable(new RuntimeException("IterationFailureNonInvertable")))
        case x: Iteration => Left(IterationDidNotConverge(maxIter, (x._1.toVector, x._2)))
      }
      case Some(IterationFailureNonInvertable) => Left(NonInvertable(new RuntimeException("IterationFailureNonInvertable")))
      case Some(Iteration(estimates, _)) =>

        {

          val x: Try[LogisticRegressionEstimates] = {

            // -1*Hessian, equal to Fisher's information
            val minusSecondDerivative = {
              odds(estimates, currentOdds)
              val estimatedOdds = currentOdds

              val diagW = createW(aggregatedBinLengths, estimatedOdds)
              // (for (i <- 0 to aggregatedBinLengths.length - 1) yield (
              // aggregatedBinLengths.raw(i) * estimatedOdds.(i) * (1.0 - estimatedOdds(i)))): _*)

              calculateXTwX5(X, diagW, xtwxshared, wxshared)
            }

            val negativeDefinite = MatrixFeatures.isPositiveDefinite(minusSecondDerivative.getMatrix.copy)

            if (!negativeDefinite) Failure(new RuntimeException("Hessian not negative definite"))
            else {
              val minusSecondDerivativeInvertedOption = LinearRegression.invertPositiveDefinite(minusSecondDerivative)
              minusSecondDerivativeInvertedOption.map { minusSecondDerivativeInverted =>

                val coeffs = estimates.toVector //logestimates map (x => math.exp(x))

                val stderrs = LogisticRegression.extractVector(minusSecondDerivativeInverted.extractDiag.transpose.getMatrix) map (x => math.sqrt(x)) toVector

                val zScores = coeffs zip stderrs map (p => p._1 / p._2) toVector

                val pValues = zScores map (s => 2 * (cern.jet.stat.Probability.normal(-1 * math.abs(s)))) toVector

                val logLikelihood = {
                  var i = 0
                  var l = 0.0
                  while (i < aggregatedBinLengths.length) {
                    val lc = linCombWithDesign(estimates, i)
                    l += (outComeCounts.raw(i) * lc) - (aggregatedBinLengths.raw(i) * math.log(1 + math.exp(lc)))
                    i += 1
                  }

                  LogLikelihood(L = l, df = estimates.length, aggregatedBinLengths.sum)
                }

                LogisticRegressionEstimates(coeffs, stderrs, zScores, pValues, logLikelihood)
              }
            }
          }

          x match {
            case Failure(cause) => Left(HessianIsNotNegativeDefinite(cause))
            case Success(x) => Right(x)
          }
        }
    }

  }

  def estimate(maxIter: Int, epsilon: Double): Option[LogisticRegressionEstimates] = estimateWithFailure(maxIter, epsilon).right.toOption

  def estimate: Option[LogisticRegressionEstimates] = estimate(20, 1E-7)

  /**
   * Starts iteration from the supplied starting position.
   * @return a lazy stream of all iterations with estimates and the l'.
   */
  def from(start: Array[Double]): Stream[IterationProto] = {

    assert(start.length == designMatrix.numCols, "initial estimate's dimension is wrong, " + start.length + " " + designMatrix.numCols)

    def loop(start: IterationProto): Stream[IterationProto] = start match {
      case IterationFailureNonInvertable => start #:: Stream.Empty
      case x: Iteration => start #:: loop(nextGuess(x._1))
    }

    val f1 = nextGuess(start)
    loop(f1)
  }

  private def linCombWithDesign(currentGuess: Array[Double], i: Int): Double = {
    // currentGuess dot designMatrix.row(i)
    var s = 0.0
    var k = 0
    val n = currentGuess.length
    while (k < n) {
      s += designMatrix.at(i, k) * currentGuess(k)
      k += 1
    }
    s
  }

  private def odds(currentGuess: Array[Double], store: Array[Double]): Unit = {
    var i = 0
    val n = aggregatedBinLengths.length
    while (i < n) {
      val s = linCombWithDesign(currentGuess, i)
      val e = math.exp(s)
      store(i) = (e / (1.0 + e))
      i += 1
    }
  }

  // (next guess, first derivative of loglikelihood, second derivative of loglikehood)
  // second derivative is also the covariance matrix of estimates
  // @param currentGuess are the current estimate for the betas.
  private def nextGuess(
    currentGuess: Array[Double]
  ): IterationProto = {
    // In the paper this is called PI. Odds at the current estimate of betas.
    odds(currentGuess, currentOdds)

    // n_i * PI_i , the expected number of positive outcomes at the current guess
    // in the paper this is mu
    // aggregatedBinLengths * currentOdds
    val expected: Array[Double] = {
      val v = Array.ofDim[Double](aggregatedBinLengths.length)
      var i = 0
      while (i < aggregatedBinLengths.length) {
        v(i) = aggregatedBinLengths.raw(i) * currentOdds(i)
        i += 1
      }
      v
    }

    // y - expected
    // outComeCounts - expected
    val diff: Array[Double] = {
      val v = Array.ofDim[Double](expected.length)
      var i = 0
      while (i < expected.length) {
        v(i) = outComeCounts.raw(i) - expected(i)
        i += 1
      }
      v
    }

    // This w is the diagonal of a diagonalmatrix
    val w: Array[Double] = createW(aggregatedBinLengths, currentOdds)
    // aggregatedBinLengths * currentOdds * (currentOdds * (-1.0) + 1)
    // (for (i <- 0 to aggregatedBinLengths.length - 1) yield (
    // aggregatedBinLengths(i) * currentOdds(i) * (1.0 - currentOdds(i))))

    // first derivative of the log likelihood function, evaluated at the current guess
    val lprime: DenseMatrix64F = {
      // XT * diff

      val vec = new DenseMatrix64F(diff.length, 1, false, diff: _*)
      val out = new DenseMatrix64F(X.numCols, 1)
      CommonOps.multTransA(X.getMatrix, vec, out)
      out
    }

    // second derivative of the log likelihood at the current guess.
    // this is almost the covariance matrix of the estimates.
    //  XT * w * X
    val minusldoubleprime = calculateXTwX5(X, w, xtwxshared, wxshared)

    // this solver stores the cholesky of the matrix
    val solver = LinearSolverFactory.chol(minusldoubleprime.numRows)
    val decomposed = solver.setA(minusldoubleprime.getMatrix)

    if (!decomposed) IterationFailureNonInvertable
    else {
      val nextGuess = {
        // currentGuess + minusldoublerpime * lprime
        val update = new DenseMatrix64F(currentGuess.length, 1)
        solver.solve(lprime, update)

        val currentGuessDM = DenseMatrix64F.wrap(currentGuess.length, 1, currentGuess)
        CommonOps.addEquals(currentGuessDM, update)
        // MatrixVectorMult.multAdd(minusldoubleprimeinverted.getMatrix, lprime, currentGuessDM)
        currentGuessDM.data
      }
      val lprimesum = {
        // LogisticRegression.extractVector(lprime)
        // .map(x => math.abs(x)).sum
        var i = 0
        var s = 0.0
        while (i < X.numCols) {
          s += math.abs(lprime.get(i, 0))
          i += 1
        }
        s
      }

      Iteration(nextGuess.toArray, lprimesum)
    }

  }

  private def diagMult(diag: Vec[Double], x: DenseMatrix64F): SimpleMatrix = {
    val rows = CommonOps.rowsToVector(x, null)
    var i = 0
    val r = new DenseMatrix64F(x.getNumRows, x.getNumCols)
    val k = rows.size
    while (i < k) {

      CommonOps.scale(diag.raw(i), rows(i))
      CommonOps.transpose(rows(i))
      CommonOps.insert(rows(i), r, i, 0)
      i += 1
    }

    SimpleMatrix.wrap(r)
  }

}

object LogisticRegression {

  def createW(aggregatedBinLengths: Vec[Int], currentOdds: Array[Double]): Array[Double] = {
    val v = Array.ofDim[Double](aggregatedBinLengths.length)
    var i = 0
    while (i < aggregatedBinLengths.length) {
      v(i) = aggregatedBinLengths.raw(i) * currentOdds(i) * (1.0 - currentOdds(i))
      i += 1
    }
    v
  }

  private[stat] def calculateXTwX(XT: SimpleMatrix, diagW: IndexedSeq[Double], X: SimpleMatrix) = {

    val wXT = new DenseMatrix64F(X.numCols, diagW.size)
    var i = 0
    var j = 0
    val k = diagW.size
    while (i < k) {
      val f = diagW(i)
      while (j < X.numCols) {
        wXT.set(j, i, f * X.get(i, j))
        j += 1
      }
      j = 0
      i += 1
    }
    val XTwX = new DenseMatrix64F(XT.numRows, wXT.numRows)
    mybiotools.stat.LinearRegression.BLAS match {
      case Some(blas) => blas.dgemm("T", "N", XT.numRows, wXT.numRows, XT.numCols, 1.0, XT.getMatrix.data, XT.numCols, wXT.getData, wXT.numCols, 0.0, XTwX.data, XTwX.getNumRows)
      case None => org.ejml.alg.dense.mult.MatrixMatrixMult.multTransB(XT.getMatrix, wXT, XTwX)
    }
    SimpleMatrix.wrap(XTwX)

  }

  private[stat] def calculateXTwX2(X: SimpleMatrix, w: Array[Double], out: DenseMatrix64F) = {

    val A = X.getMatrix.getData
    val C = out.getData
    val K = X.numRows
    val M = X.numCols

    var k = 0
    while (k < K) {
      val kM = k * M
      val w1 = w(k)
      var m = 0
      while (m < M) {
        val f = A(kM + m) * w1
        val mM = m * M
        var n = 0
        while (n < M) {
          C(mM + n) += f * A(kM + n)
          n += 1
        }
        m += 1
      }
      k += 1
    }

    SimpleMatrix.wrap(out)

  }

  private[stat] def calculateXTwX3(X: SimpleMatrix, w: Array[Double], out: DenseMatrix64F) = {

    val A = X.getMatrix.getData
    val C = out.getData
    val K = X.numRows
    val M = X.numCols

    var m = 0
    while (m < M) {
      val mM = m * M
      var k = 0
      while (k < K) {
        val kM = k * M
        val w1 = w(k)
        val f = A(kM + m) * w1
        var n = 0
        while (n < M) {
          C(mM + n) += f * A(kM + n)
          n += 1
        }
        k += 1
      }
      m += 1
    }

    SimpleMatrix.wrap(out)

  }

  private[stat] def calculateXTwX4(X: SimpleMatrix, w: Array[Double], out: DenseMatrix64F) = {

    val A = X.getMatrix.getData
    val C = out.getData
    val K = X.numRows
    val M = X.numCols
    val tmp = Array.ofDim[Double](M)
    val tmp2 = Array.ofDim[Double](M)

    var m = 0
    while (m < M) {
      val mM = m * M
      var k = 0
      while (k < K) {
        val kM = k * M
        val w1 = w(k)
        val f = A(kM + m) * w1

        mybiotools.stat.LinearRegression.BLAS match {
          case Some(blas) => {
            System.arraycopy(A, kM, tmp, 0, M)
            System.arraycopy(C, mM, tmp2, 0, M)
            blas.daxpy(M, f, tmp, 1, tmp2, 1)
            System.arraycopy(tmp2, 0, C, mM, M)
          }
          case None => {
            var n = 0
            while (n < M) {
              C(mM + n) += f * A(kM + n)
              n += 1
            }
          }
        }

        k += 1
      }
      m += 1
    }

    SimpleMatrix.wrap(out)

  }

  private[stat] def calculateXTwX5(X: SimpleMatrix, diagW: Array[Double], out: DenseMatrix64F, wX: DenseMatrix64F) = {

    val wXa = wX.getData
    val Xa = X.getMatrix.getData
    var i = 0
    var j = 0
    val K = diagW.size
    val M = X.numCols
    while (i < K) {
      val f = diagW(i)
      while (j < M) {
        wXa(i * M + j) = f * Xa(i * M + j)
        j += 1
      }
      j = 0
      i += 1
    }
    mybiotools.stat.LinearRegression.BLAS match {
      case Some(blas) => blas.dgemm("N", "T", X.numCols, wX.numCols, X.numRows, 1.0, X.getMatrix.data, X.numCols, wX.getData, wX.numCols, 0.0, out.data, out.getNumRows)
      case None => org.ejml.ops.CommonOps.multTransA(X.getMatrix, wX, out)
    }
    SimpleMatrix.wrap(out)

  }

  /** Euclidean distance. */
  private[stat] def norm2(v: IndexedSeq[Double]): Double = math.sqrt((v map (x => x * x)).sum)

  private[stat] def elementWiseMinus(v1: IndexedSeq[Double], v2: IndexedSeq[Double]) =
    (for (i <- 0 to v1.length - 1) yield (v1(i) - v2(i))).toArray

  /** ||v1 - v2|| / ||v1|| */
  private[stat] def relativeError(v1: IndexedSeq[Double], v2: IndexedSeq[Double]) = norm2(elementWiseMinus(v1, v2)) / norm2(v1)

  /** ||v1 - v2||  */
  private[stat] def absoluteError(v1: IndexedSeq[Double], v2: IndexedSeq[Double]) = norm2(elementWiseMinus(v1, v2))

  /** ||v1 - 0|| == ||v1||  */
  private[stat] def absoluteErrorZero(v1: IndexedSeq[Double]) = norm2(v1)

  /** Converts from ejml's SimpleMatrix to IndexedSeq[Double] */
  private[stat] def extractVector(v1: DenseMatrix64F): IndexedSeq[Double] = {

    if (v1.getNumRows > 1 && v1.getNumCols > 1) throw new IllegalArgumentException("1xN or Nx1 ")
    else v1.getData

  }

  // generate dummy variables upstream
  def logisticRegression(
    data: Map[Individual, (Boolean, Map[String, Option[Double]])],
    covariateNames: Seq[String],
    missingMode: MissingMode,
    maxIter: Int = 20,
    epsilon: Double = 1E-10
  ): Either[FailedRegression, RegressionResult] = {
    val d1 = data.mapValues(_._2)
    val d2 = data.mapValues(_._1)

    val frame: Frame[Individual, String, Double] = Frame(d1.mapValues(x => Series[String, Double](x.mapValues(_.getOrElse(Double.NaN)).toSeq: _*)).toSeq: _*).T

    logisticRegression(frame, Series(d2.toSeq: _*), covariateNames, missingMode, maxIter, epsilon)

  }

  // generate dummy variables upstream
  def logisticRegression[I](
    data: Frame[I, String, Double],
    outcomes: Series[I, Boolean],
    covariateNames: Seq[String],
    missingMode: MissingMode,
    maxIter: Int,
    epsilon: Double
  )(implicit ev: org.saddle.ST[I], ord: Ordering[I]): Either[FailedRegression, RegressionResult] = {

    val covNamesInData = {
      val s = data.colIx.toSeq.toSet
      covariateNames.filter(x => s.contains(x))
    }

    val covariateNamesWithIntercept = "intercept" +: covNamesInData

    val includedCovariates: Frame[I, String, Double] =
      prepareLeakyDataTable(covariateNamesWithIntercept, data, missingMode)

    val sharedInds = (includedCovariates.rowIx.toSeq.toSet & outcomes.index.toSeq.toSet).toSeq

    val filteredOutcomes: Series[I, Boolean] = outcomes(sharedInds: _*)
    val filteredIncludedCovariates = includedCovariates.row(sharedInds: _*)

    logisticRegressionFullData(filteredIncludedCovariates.toMat, filteredOutcomes.toVec, covariateNamesWithIntercept, maxIter, epsilon)

  }

  def prepareGroupings[I](includedCovariatesMat: Mat[Double]) = {

    // This saves the hashcode and prevents boxing of doubles.
    case class MemoizedHashCode(v: Vec[Double]) {
      override val hashCode = {
        var s = 1
        var i = 0
        val n = v.length
        while (i < n) {
          s = 31 * s + com.google.common.primitives.Doubles.hashCode(v.raw(i))
          i += 1
        }
        s
      }
    }

    import scala.collection.mutable.ListBuffer

    def groupFrameByRow(in: Mat[Double]): Seq[collection.mutable.BitSet] = {
      val buff = collection.mutable.ListBuffer[collection.mutable.BitSet]()
      var i = 0
      val n = in.numRows
      val set = new collection.mutable.BitSet(n)

      while (i < n) {
        if (!set.contains(i)) {
          val lb = new collection.mutable.BitSet(n)
          lb += i
          val ir = in.row(i)
          var j = i + 1
          while (j < n) {
            if (!set.contains(j) && ir == in.row(j)) {
              lb += (j)
              set += j
            }
            j += 1
          }
          buff.append(lb)

        }
        i += 1
      }

      buff
    }

    def groupFrameByRow2(in: Mat[Double]): Seq[collection.mutable.BitSet] = {
      val mmap = collection.mutable.AnyRefMap[Vec[Double], collection.mutable.BitSet]()
      val n = in.numRows
      (0 until in.numRows).foreach { idx =>
        val row = in.row(idx)
        mmap.get(row) match {
          case None => {
            val bs = new collection.mutable.BitSet(n)
            bs += idx
            mmap.update(row, bs)
          }
          case Some(x) => {
            x += idx
          }
        }
      }
      mmap.map(x => x._2).toSeq
    }

    def groupFrameByRow3(in: Mat[Double]): Seq[collection.mutable.BitSet] = {
      val mmap = collection.mutable.AnyRefMap[MemoizedHashCode, collection.mutable.BitSet]()
      val n = in.numRows
      (0 until in.numRows).foreach { idx =>
        val row = MemoizedHashCode(in.row(idx)) //.contents.toList
        mmap.get(row) match {
          case None => {
            val bs = new collection.mutable.BitSet(n)
            bs += idx
            mmap.update(row, bs)
          }
          case Some(x) => {
            x += idx
          }
        }
      }
      mmap.map(x => x._2).toSeq
    }

    // println(" ")
    // val t1 = System.nanoTime
    val groups = groupFrameByRow3(includedCovariatesMat)
    // println("y" + (System.nanoTime - t1))

    // val t2 = System.nanoTime
    // val groups = groupFrameByRow2(includedCovariatesMat)
    // println("x" + (System.nanoTime - t2))
    // println(" ")

    groups.map(_.toIndexedSeq)
  }

  def logisticRegressionFullData(
    includedCovariatesMat: Mat[Double],
    filteredOutcomes: Vec[Boolean],
    covariateNamesWithIntercept: Seq[String],
    maxIter: Int,
    epsilon: Double,
    doScoreTest: Option[DataForScoreTest] = None
  ): Either[FailedRegression, RegressionResult] = {

    val groups = prepareGroupings(includedCovariatesMat)

    logisticRegressionFullDataWithGrouping(includedCovariatesMat, filteredOutcomes, covariateNamesWithIntercept, maxIter, epsilon, groups, doScoreTest)

  }

  def logisticRegressionFullDataWithGrouping[I](
    includedCovariatesMat: Mat[Double],
    filteredOutcomes: Vec[Boolean],
    covariateNamesWithIntercept: Seq[String],
    maxIter: Int,
    epsilon: Double,
    groups: Seq[Seq[Int]],
    doScoreTest: Option[DataForScoreTest] = None
  ): Either[FailedRegression, RegressionResult] = {

    val covariateNames = covariateNamesWithIntercept filterNot (_ == "intercept")

    val designMatrix: Mat[Double] =
      includedCovariatesMat.row(groups.map(_.head): _*)

    val aggregatedBinLengths: Vec[Int] = Vec(groups.map(x => x.size): _*)

    val outComeCounts: Vec[Int] = Vec(groups.map(group => filteredOutcomes(group: _*).filter(_ === true).length): _*)

    if (designMatrix.numCols == 0 && designMatrix.numRows == 0)
      Left(DesignMatrixIsEmpty)
    else {

      val scoreTestResult: Option[ScoreTestResult] = doScoreTest.flatMap(d => scoreTest(designMatrix, outComeCounts, aggregatedBinLengths, d.nullFit, d.df).right.toOption)

      if (scoreTestResult.isDefined && scoreTestResult.get.pValue >= doScoreTest.get.pThreshold) Right(scoreTestResult.get)
      else {

        val logreg = new LogisticRegression(designMatrix, outComeCounts, aggregatedBinLengths)

        logreg.estimateWithFailure(maxIter, epsilon) match {

          case Left(x) => Left(x)
          case Right(raw) => {

            val data = Vec(0 to covariateNames.size flatMap (i => Array(raw.coefficients(i), raw.standardErrors(i), raw.zStats(i), raw.pValues(i))): _*)

            val count = aggregatedBinLengths.sum

            Right(LogisticRegressionResult(
              data,
              covariateNames,
              count,
              raw.logLikelihood.L,
              raw.logLikelihood.df
            ))
          }
        }
      }
    }
  }

  def scoreTest(
    designMatrix: Mat[Double],
    outComeCounts: Vec[Int],
    aggregatedBinLengths: Vec[Int],
    nullFit: Vec[Double],
    df: Int
  ): Either[FailedLogisticRegression, ScoreTestResult] = {

    val X = new SimpleMatrix(designMatrix.numRows, designMatrix.numCols, true, designMatrix.contents: _*)

    val xtwxshared = new DenseMatrix64F(X.numCols, X.numCols)
    val wxshared = new DenseMatrix64F(X.numRows, X.numCols)

    val currentGuess = nullFit

    val currentOdds = Array.ofDim[Double](aggregatedBinLengths.length)

    def linCombWithDesign(currentGuess: Array[Double], i: Int): Double = {
      // currentGuess dot designMatrix.row(i)
      var s = 0.0
      var k = 0
      val n = currentGuess.length
      while (k < n) {
        s += designMatrix.at(i, k) * currentGuess(k)
        k += 1
      }
      s
    }

    def odds(currentGuess: Array[Double], store: Array[Double]): Unit = {
      var i = 0
      val n = aggregatedBinLengths.length
      while (i < n) {
        val s = linCombWithDesign(currentGuess, i)
        val e = math.exp(s)
        store(i) = (e / (1.0 + e))
        i += 1
      }
    }

    odds(currentGuess, currentOdds)

    // n_i * PI_i , the expected number of positive outcomes at the current guess
    // in the paper this is mu
    // aggregatedBinLengths * currentOdds
    val expected: Array[Double] = {
      val v = Array.ofDim[Double](aggregatedBinLengths.length)
      var i = 0
      while (i < aggregatedBinLengths.length) {
        v(i) = aggregatedBinLengths.raw(i) * currentOdds(i)
        i += 1
      }
      v
    }

    // y - expected
    // outComeCounts - expected
    val diff: Array[Double] = {
      val v = Array.ofDim[Double](expected.length)
      var i = 0
      while (i < expected.length) {
        v(i) = outComeCounts.raw(i) - expected(i)
        i += 1
      }
      v
    }

    // This w is the diagonal of a diagonalmatrix
    val w: Array[Double] = createW(aggregatedBinLengths, currentOdds)
    // aggregatedBinLengths * currentOdds * (currentOdds * (-1.0) + 1)
    // (for (i <- 0 to aggregatedBinLengths.length - 1) yield (
    // aggregatedBinLengths(i) * currentOdds(i) * (1.0 - currentOdds(i))))

    // first derivative of the log likelihood function, evaluated at the current guess
    val lprime: SimpleMatrix = {
      // XT * diff

      val vec = new DenseMatrix64F(diff.length, 1, false, diff: _*)
      val out = new DenseMatrix64F(X.numCols, 1)
      CommonOps.multTransA(X.getMatrix, vec, out)
      SimpleMatrix.wrap(out)
    }

    // second derivative of the log likelihood at the current guess.
    // this is almost the covariance matrix of the estimates.
    //  XT * w * X
    LinearRegression.invertPositiveDefinite(calculateXTwX5(X, w, xtwxshared, wxshared)) match {
      case Failure(cause) => Left(HessianIsNotNegativeDefinite(cause))
      case Success(fisherI) => {
        val scoreStat = (lprime.transpose mult fisherI mult lprime).get(0, 0)

        val pValue = jdistlib.ChiSquare.cumulative(scoreStat, df, false, false)

        Right(ScoreTestResultImpl(scoreStat, pValue, df, aggregatedBinLengths.sum))
      }
    }

  }
  /**
   * Creates a completely filled in data matrix with intercept column.
   */
  def prepareLeakyDataTable[I](
    covariateNames: Seq[String],
    data: Frame[I, String, Double],
    missingMode: MissingMode
  )(implicit ev: org.saddle.ST[I], ord: Ordering[I]): Frame[I, String, Double] = {

    val withIntercept: Frame[I, String, Double] =
      mybiotools.SaddleFrameExtend.joinSPreserveColIx(data, Series[I, Double](vec.ones(data.rowIx.length), data.rowIx), "intercept")

    val filtered: Frame[I, String, Double] =
      withIntercept.col(covariateNames: _*)

    missingMode match {
      case MeanImpute => filtered.mapVec(column => LinearRegression.meanImpute(column))
      case DropSample => filtered.rfilter(row => !(row.hasNA || row.toVec.exists(_.isInfinite)))
    }

  }

}