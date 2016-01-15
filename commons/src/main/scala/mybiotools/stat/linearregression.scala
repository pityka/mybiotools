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

import org.apache.commons.math3.stat.regression.OLSMultipleLinearRegression
import mybiotools._
import mybiotools.eq._
import mybiotools.gwascommons.Individual
import mybiotools.gwascommons.indorder
import mybiotools.config.Config.configInstance

import org.ejml.simple.SimpleMatrix
import org.ejml.ops.MatrixFeatures
import org.ejml.data.DenseMatrix64F
import org.ejml.ops.CommonOps
import org.ejml.factory.LinearSolverFactory
import org.ejml.alg.dense.linsol.LinearSolverSafe
import org.ejml.alg.dense.mult.MatrixVectorMult
import LogisticRegression.extractVector

import org.saddle._
import scala.util.{ Try, Success, Failure }

sealed trait MissingMode
case object DropSample extends MissingMode
case object MeanImpute extends MissingMode

case class Effect(slope: Double, sd: Double) {
  def slopeOverSD = slope / sd
}

trait TestResult {
  def statistic: Double
  def pValue: Double
}

object TestResult {
  def unapply(t: TestResult) = Some((t.statistic, t.pValue))
}

object LinearRegression {

  lazy val BLAS: Option[com.github.fommil.netlib.BLAS] = synchronized {
    val useBlas = scala.util.Try(configInstance.getBoolean("mybiotools.stat.useBLAS")).toOption.getOrElse(true)
    if (useBlas) try {
      val resourceLogConfigFile = TempFile.getExecutableFromJar("/java.util.logging.config.file").getPath
      System.setProperty("java.util.logging.config.file", resourceLogConfigFile);
      val instance = com.github.fommil.netlib.BLAS.getInstance
      if (instance.isInstanceOf[com.github.fommil.netlib.NativeSystemBLAS]) Some(instance) else None
    } catch {
      case _: Throwable => None
    }
    else None
  }

  lazy val LAPACK: Option[com.github.fommil.netlib.LAPACK] = synchronized {
    val useBlas = scala.util.Try(configInstance.getBoolean("mybiotools.stat.useBLAS")).toOption.getOrElse(true)
    if (useBlas) try {
      val instance = com.github.fommil.netlib.LAPACK.getInstance
      if (instance.isInstanceOf[com.github.fommil.netlib.NativeSystemLAPACK]) Some(instance) else None
    } catch {
      case _: Throwable => None
    }
    else None
  }

  def readPlinkCovarFile(source: scala.io.Source, missingValue: String): Frame[Individual, String, Double] = {

    import mybiotools.tabular._
    import TabSerialization._
    val protocol = new PlinkProtocol(missingValue)
    import protocol._

    val (table, fields) = fromTab[Tuple2[Seq[Tuple3[String, String, Seq[Option[Double]]]], Seq[String]]](SimpleWhiteSpaceTableParser.parse(source, true))

    val lines: Seq[(Individual, Series[String, Double])] = table.map(line =>
      Individual(line._1, line._2) -> Series[String, Double](fields.drop(2).zipWithIndex.map {
        case (field, idx) =>
          field -> (line._3(idx) match {
            case Some(x) => x
            case None => Double.NaN
          })
      }: _*))

    val duplicatedLines: Seq[Individual] = lines.map(_._1).groupBy(x => x).filter(_._2.size > 1).map(_._1).toSeq
    assert(duplicatedLines.size == 0, """duplicated individuals in plink covariate file. """ + duplicatedLines.map(_.toLine).mkString("\n", "\n", "\n"))

    Frame(
      lines: _*
    ).T

  }

  case class LinearRegressionResult(
      slopes: IndexedSeq[Double],
      intercept: Double,
      interceptSD: Double,
      r2: Double,
      sigma: Double,
      residuals: IndexedSeq[Double],
      slopeSDs: IndexedSeq[Double],
      private val dff: Double,
      lambda: Double,
      adjR2: Double
  ) extends PenalizedRegressionFit {
    val df = if (lambda == 0.0) slopeSDs.size + 1 else dff
    lazy val coefficients = Vec(intercept +: slopes: _*)
    def rSquared = r2
  }

  case class LinearRegressionResultMapsLight(
      protected val data: Vec[Double],
      protected val names: Seq[String],
      val numberOfSamples: Int,
      protected val _logLikelihood: Double,
      protected val df: Double,
      override val lambda: Double
  ) extends AbstractRegressionResult {
    def testFactory(a: Double, b: Double) = SuccessfulStudentTest(a, b)
    def predict(v: Series[String, Double]): Double = covariates.map(x => v.first(x._1).get * x._2._1.slope).sum + intercept._1.slope * v.first("intercept").get
  }

  case class LinearRegressionResultMaps[I](
      covariates: Map[String, (Effect, StudentTestResult)],
      intercept: (Effect, StudentTestResult),
      r2: Double,
      sigma: Double,
      numberOfSamples: Int,
      df: Double,
      lambda: Double,
      adjR2: Double,
      residualsWithSampleNames: Seq[(I, Double)]
  ) extends RegressionResult {

    def predict(v: Series[String, Double]): Double = covariates.map(x => v.first(x._1).get * x._2._1.slope).sum + intercept._1.slope * v.first("intercept").get

    def covariate(s: String) = covariates.get(s)

    private def residuals = residualsWithSampleNames.map(_._2)

    def predict(data: Map[String, Double]) = (data.map {
      case (name, v) =>
        covariates(name)._1.slope * v
    }).sum + intercept._1.slope

    lazy val gcv = {
      val numSamples = residuals.size

      val denom = 1.0 - (df / numSamples)
      var sum = 0.0
      var i = 0
      while (i < numSamples) {
        val err = residuals(i)
        val x = err / denom
        sum += (x * x)
        i += 1
      }
      sum * (1.0 / numSamples)
    }

    // from R source: src/library/stats/R/logLik.R
    // .5* (sum(log(w)) - N * (log(2 * pi) + 1 - log(N) +
    // log(sum(w*res^2))))
    override val logLikelihood = {
      import scala.math.log
      import scala.math.Pi

      // This is because tr(Hat) might have numerical errors which result in df<1 in LRT
      val dfAccurate = if (lambda == 0.0) covariates.size + 1 else df

      val residualSqSum = {
        var i = 0
        var sum = 0.0
        val k = residuals.size
        while (i < k) {
          val x = residuals(i)
          sum += x * x
          i += 1
        }
        sum
      }
      val l = -0.5 * numberOfSamples * (log(2 * Pi) + 1 - log(numberOfSamples) + log(residualSqSum))
      LogLikelihood(L = l, df = dfAccurate, numberOfSamples = numberOfSamples)
    }

  }

  sealed trait StudentTestResult extends TestResult {
    def statistic: Double
    def pValue: Double
  }
  case object FailedStudentTest extends StudentTestResult {
    val statistic = Double.NaN
    val pValue = Double.NaN
  }
  case class SuccessfulStudentTest(statistic: Double, pValue: Double) extends StudentTestResult

  def studentTestLazy(estimate: Double, sd: Double, df: Double, location: Double = 0.0): StudentTestResult = {
    if (df <= 0 || sd.isNaN || estimate.isNaN || sd.isInfinite || sd == 0.0 || sd > 1E7) FailedStudentTest
    else {

      new StudentTestResult {
        val statistic = (estimate - location) / sd
        lazy val pValue = if (math.abs(statistic) > 12) 2 * (jdistlib.T.cumulative(math.abs(statistic), df.toDouble, false, false)) else 2 * (1.0 - cern.jet.stat.Probability.studentT(df.toDouble, math.abs(statistic)))
      }
    }
  }

  def studentTest(estimate: Double, sd: Double, df: Double, location: Double = 0.0): StudentTestResult = {
    val st = studentTestLazy(estimate, sd, df, location)
    st match {
      case FailedStudentTest => FailedStudentTest
      case x => SuccessfulStudentTest(x.statistic, x.pValue)
    }
  }

  def meanImpute(data: IndexedSeq[Option[Double]]): IndexedSeq[Double] = {
    val mean = mybiotools.SummaryStat(data.filter(_.isDefined).map(_.get)).mean
    data.map(_.getOrElse(mean))
  }

  def meanImpute(data: Vec[Double]): Vec[Double] = {
    val mean = data.mean
    data.fillNA(_ => mean)
  }

  def linearRegression[T](data: Map[Individual, Map[String, T]], covariates: Seq[String], yKey: String, missingMode: MissingMode, lambda: Double)(implicit num: Numeric[T]): RegressionResultOrFailure =
    linearRegression(
      data.mapValues(_.mapValues(x => Some(num.toDouble(x)))),
      covariates,
      yKey,
      missingMode,
      lambda
    )

  def linearRegression[T](data: Map[Individual, Map[String, T]], covariates: Seq[String], yKey: String, missingMode: MissingMode)(implicit num: Numeric[T]): RegressionResultOrFailure =
    linearRegression(
      data.mapValues(_.mapValues(x => Some(num.toDouble(x)))),
      covariates,
      yKey,
      missingMode,
      0.0
    )

  def linearRegression(
    data: Map[Individual, Map[String, Option[Double]]],
    covariates: Seq[String],
    yKey: String,
    missingMode: MissingMode,
    lambda: Double
  ): RegressionResultOrFailure = {

    val frame: Frame[Individual, String, Double] = Frame(data.mapValues(x => Series[String, Double](x.mapValues(_.getOrElse(Double.NaN)).toSeq: _*)).toSeq: _*).T

    linearRegression(frame, covariates, yKey, missingMode, lambda)

  }

  def linearRegression[I](
    data: Frame[I, String, Double],
    covariates: Seq[String],
    yKey: String,
    missingMode: MissingMode,
    lambda: Double
  )(implicit ev: org.saddle.ST[I], ord: Ordering[I]): RegressionResultOrFailure = {

    val data2 = createDataMatrixForLinearRegression(data, covariates, yKey, missingMode)

    val raw = linearRegression(data2, lambda)

    interpretRawResults(raw, covariates, lambda, Some(data2.sampleNames))

  }

  def linearRegressionPruneMulticollinear[I](
    data: Frame[I, String, Double],
    covariates: Seq[String],
    yKey: String,
    missingMode: MissingMode,
    lambda: Double
  )(implicit ev: org.saddle.ST[I], ord: Ordering[I]): RegressionResultOrFailure = {

    val r = linearRegression(data, covariates, yKey, missingMode, lambda)

    def loop(rr: RegressionResultOrFailure, s: Seq[String]): RegressionResultOrFailure = {
      rr match {
        case NonInvertable(DPotrfException(i)) => loop(linearRegression(
          data,
          s.zipWithIndex.filterNot(_._2 === (i - 2)).map(_._1),
          yKey,
          missingMode,
          lambda
        ), s.zipWithIndex.filterNot(_._2 === (i - 2)).map(_._1))
        case x => x
      }
    }

    loop(r, covariates)

  }

  def linearRegression[I](outcome: Series[I, Double], covariates: Series[I, Double]*)(implicit ev: org.saddle.ST[I], ord: Ordering[I]): RegressionResultOrFailure = {
    val f = Frame((("y" -> outcome) :: covariates.zipWithIndex.map(x => x._2.toString -> x._1).toList): _*)
    linearRegression(f, covariates.zipWithIndex.map(_._2.toString), "y", DropSample, 0.0)
  }

  /**
   * Minimal data for regression.
   *
   * @param y The outcome vector
   * @param design The design matrix. The first column is the intercept column.
   * @param covariateNames lists the names of the columns of the design matrix except the first intercept column
   */
  object DataForRegression {
    def apply(y: Vec[Double], design: Mat[Double], covariateNames: Seq[String]): DataForRegression[String] = DataForRegression[String](y, design, covariateNames, 0 until y.length map (i => "sample_" + i))
  }
  case class DataForRegression[I](y: Vec[Double], design: Mat[Double], covariateNames: Seq[String], sampleNames: Seq[I]) {
    def subsetCovariates(subset: Seq[String]) = {
      val indices = 0 +: subset.map(s => covariateNames.indexOf(s)).filter(_ >= 0).map(_ + 1)
      this.copy(design = design.col(indices: _*), covariateNames = covariateNames.filter(c => subset.contains(c)))
    }
    def permuteCovariates(covariates: Seq[String]) = {
      import org.ejml.data.DenseMatrix64F
      import org.ejml.ops.CommonOps

      val indicesToPermute = covariates.map(s => covariateNames.indexOf(s)).filter(_ >= 0).map(_ + 1).toArray

      val permutedRowIndices: Array[Int] = scala.util.Random.shuffle((0 until design.numRows).toList).toArray

      val densematrix = {
        val array = Array.ofDim[Double](design.contents.size)
        System.arraycopy(design.contents, 0, array, 0, design.contents.size)
        DenseMatrix64F.wrap(design.numRows, design.numCols, array)
      }
      val workcol = new DenseMatrix64F(design.numRows, 1)

      var k = 0
      while (k < indicesToPermute.size) {
        val i = indicesToPermute(k)
        CommonOps.extract(densematrix, 0, densematrix.getNumRows, i, i + 1, workcol, 0, 0)
        var j = 0
        while (j < permutedRowIndices.size) {
          densematrix.unsafe_set(j, i, workcol.get(permutedRowIndices(j), 0))
          j += 1
        }

        k += 1
      }
      this.copy(design = Mat(densematrix.getNumRows, densematrix.getNumCols, densematrix.getData))

    }
    def permuteCovariatesINPLACE(covariates: Seq[String], original: Mat[Double]): Unit = {
      import org.ejml.data.DenseMatrix64F
      import org.ejml.ops.CommonOps

      val indicesToPermute = covariates.map(s => covariateNames.indexOf(s)).filter(_ >= 0).map(_ + 1).toArray

      val permutedRowIndices: Array[Int] = scala.util.Random.shuffle((0 until design.numRows).toList).toArray

      val densematrix = {
        DenseMatrix64F.wrap(design.numRows, design.numCols, design.contents)
      }

      var j = 0
      while (j < permutedRowIndices.size) {
        var k = 0
        val p = permutedRowIndices(j)
        while (k < indicesToPermute.size) {
          val i = indicesToPermute(k)
          densematrix.unsafe_set(j, i, original.raw(p, i))
          k += 1
        }
        j += 1
      }

    }
  }

  /** covariateNames lists the names of the columns of the design matrix except the first intercept column */
  def interpretRawResults[I](raw: Try[LinearRegressionResult], covariates: Seq[String], lambda: Double, samples: Option[Seq[I]] = None): RegressionResultOrFailure = raw match {
    case Success(raw) =>
      {

        val covarsmap = Map(covariates.zipWithIndex.map {
          case (name, index) =>
            (name, (
              Effect(raw.slopes(index), raw.slopeSDs(index)),
              studentTest(raw.slopes(index), raw.slopeSDs(index), raw.residuals.length - 2)
            ))
        }: _*)

        val zippedresiduals = samples.map(_ zip raw.residuals).getOrElse(raw.residuals.zipWithIndex.map(x => "sample_" + x._2.toString -> x._1))

        LinearRegressionResultMaps(
          covariates = covarsmap,
          intercept = (Effect(raw.intercept, raw.interceptSD), studentTestLazy(raw.intercept, raw.interceptSD, raw.residuals.length - 2)),
          r2 = raw.r2,
          sigma = raw.sigma,
          numberOfSamples = raw.residuals.length,
          df = raw.df,
          lambda = lambda,
          adjR2 = raw.adjR2,
          residualsWithSampleNames = zippedresiduals
        )
      }
    case Failure(cause) => NonInvertable(cause)
  }

  /** covariateNames lists the names of the columns of the design matrix except the first intercept column */
  def interpretRawResultsLight(raw: Try[LinearRegressionResult], covariates: Seq[String], lambda: Double): RegressionResultOrFailure = raw match {
    case Success(raw) =>
      {

        val data = Vec(
          {
            val test = studentTest(raw.intercept, raw.interceptSD, raw.residuals.length - 2)
            Array(raw.intercept, raw.interceptSD, test.statistic, test.pValue)
          } ++
            (0 until covariates.size flatMap { i =>
              val test = studentTest(raw.slopes(i), raw.slopeSDs(i), raw.residuals.length - 2)
              Array(raw.slopes(i), raw.slopeSDs(i), test.statistic, test.pValue)
            }): _*
        )

        val numberOfSamples = raw.residuals.length

        val ll = {
          import scala.math.log
          import scala.math.Pi

          // This is because tr(Hat) might have numerical errors which result in df<1 in LRT
          val dfAccurate = if (lambda == 0.0) covariates.size + 1 else raw.df

          val residualSqSum = {
            var i = 0
            var sum = 0.0
            val k = raw.residuals.size
            while (i < k) {
              val x = raw.residuals(i)
              sum += x * x
              i += 1
            }
            sum
          }
          val l = -0.5 * numberOfSamples * (log(2 * Pi) + 1 - log(numberOfSamples) + log(residualSqSum))
          LogLikelihood(L = l, df = dfAccurate, numberOfSamples = numberOfSamples)
        }

        LinearRegressionResultMapsLight(
          data = data,
          names = covariates,
          numberOfSamples = numberOfSamples,
          lambda = lambda,
          _logLikelihood = ll.L,
          df = ll.df
        )
      }
    case Failure(cause) => NonInvertable(cause)
  }

  def createDataMatrixForLinearRegression[I](
    data: Frame[I, String, Double],
    covariates: Seq[String],
    yKey: String,
    missingMode: MissingMode
  )(implicit ev: org.saddle.ST[I], ord: Ordering[I]): DataForRegression[I] = {
    val covNamesInData = {
      val s = data.colIx.toSeq.toSet
      covariates.filter(x => s.contains(x))
    }
    val covarWithIntercept = "intercept" +: covNamesInData
    val covarKeysWithY: Seq[String] = covarWithIntercept :+ yKey
    // val empty = Map[String, Option[Double]](covarKeysWithY.map(x => x -> None).toSeq: _*) 

    val (yy, covarMatrix, samplenames) = {

      val afterMissingTreatment: Frame[I, String, Double] =
        LogisticRegression.prepareLeakyDataTable(covarKeysWithY, data, missingMode)

      (
        afterMissingTreatment.firstCol(yKey).toVec,
        afterMissingTreatment.col(covarWithIntercept: _*).toMat,
        afterMissingTreatment.rowIx.toSeq
      )
    }

    DataForRegression(yy, covarMatrix, covNamesInData, samplenames)

  }

  def linearRegression[I](d: DataForRegression[I], lambda: Double): Try[LinearRegressionResult] = linearRegression(d.design, d.y, lambda, Vec(0.0 +: vec.ones(d.design.numCols - 1).toSeq: _*))

  def linearRegression(
    covariateMatrix: Array[Array[Double]],
    outcome: Array[Double],
    shrinkage: Double
  ): Try[LinearRegressionResult] =
    linearRegression(Mat(covariateMatrix).T, Vec(outcome), shrinkage, Vec(0.0 +: vec.ones(covariateMatrix.head.size - 1).toSeq: _*))

  def linearRegression(
    covariateMatrix: Array[Array[Double]],
    outcome: Array[Double]
  ): Try[LinearRegressionResult] =
    linearRegression(Mat(covariateMatrix).T, Vec(outcome), 0.0, vec.ones(covariateMatrix.head.size))

  // private[mybiotools] def invert(m: SimpleMatrix): SimpleMatrix = {
  //   LAPACK match {
  //     case None => m.invert
  //     case Some(lapack) => {

  //       val marray = m.getMatrix.getData
  //       val array = Array.ofDim[Double](marray.size)
  //       System.arraycopy(marray, 0, array, 0, marray.size)

  //       val ipiv = Array.ofDim[Int](math.max(1, math.min(m.numCols, m.numRows)))

  //       lapack.dgetrf(m.numCols, m.numRows, array, m.numCols, ipiv, new org.netlib.util.intW(0))

  //       val lworkQuery = Array.ofDim[Double](1)

  //       lapack.dgetri(m.numCols, array, m.numCols, ipiv, lworkQuery, -1, new org.netlib.util.intW(0))

  //       val work = Array.ofDim[Double](lworkQuery(0).toInt + 1)
  //       lapack.dgetri(m.numCols, array, m.numCols, ipiv, work, lworkQuery(0).toInt + 1, new org.netlib.util.intW(0))

  //       SimpleMatrix.wrap(DenseMatrix64F.wrap(m.numCols, m.numCols, array))

  //     }
  //   }
  // }

  private val lapackInfoMethod = java.lang.Class.forName("org.netlib.util.intW").getField("val")

  case class DPotrfException(i: Int) extends java.lang.Exception(s"""|dpotrf error, info=$i
                |*  INFO    (output) INTEGER
|*          = 0:  successful exit
|*          < 0:  if INFO = -i, the i-th argument had an illegal value
|*          > 0:  if INFO = i, the leading minor of order i is not
|*                positive definite, and the factorization could not be
|*                completed.""".stripMargin)

  private[mybiotools] def invertPositiveDefinite(m: SimpleMatrix): Try[SimpleMatrix] = {
    LAPACK match {
      case None => {
        val out = new SimpleMatrix(m.numRows, m.numCols)
        val solver = new LinearSolverSafe(LinearSolverFactory.chol(m.numRows));
        if (!solver.setA(m.getMatrix))
          Failure(new RuntimeException("Can't invert " + m.toString.take(100)))
        else {
          solver.invert(out.getMatrix)
          Success(out)

        }
      }
      case Some(lapack) => {

        val marray = m.getMatrix.getData
        val array = Array.ofDim[Double](marray.size)
        val info = new org.netlib.util.intW(0)
        val info2 = new org.netlib.util.intW(0)
        lapack.dlacpy("L", m.numCols, m.numRows, marray, m.numCols, array, m.numCols)
        lapack.dpotrf("L", m.numCols, array, m.numCols, info)
        lapack.dpotri("L", m.numCols, array, m.numCols, info2)

        if (lapackInfoMethod.get(info) == 0 && lapackInfoMethod.get(info2) == 0) {

          val outmatrix = SimpleMatrix.wrap(DenseMatrix64F.wrap(m.numCols, m.numCols, array))

          var i = 0
          var j = 0
          while (i < m.numCols) {
            while (j < i) {
              outmatrix.set(i, j, outmatrix.get(j, i))
              j += 1
            }
            j = 0
            i += 1
          }

          Success(outmatrix)

        } else {
          if (lapackInfoMethod.get(info) != 0) {
            Failure(new DPotrfException(lapackInfoMethod.get(info).asInstanceOf[Int]))
          } else {
            Failure(new RuntimeException("ERROR in dpotri info=" + lapackInfoMethod.get(info2) + """
                |lapack says:
                |      INFO    (output) INTEGER
              |= 0:  successful exit
              |< 0:  if INFO = -i, the i-th argument had an illegal
              |value
              |> 0:  if INFO = i, the (i,i) element of the factor U
              |or L is zero, and the inverse could not be computed.""".stripMargin + ", matrix: " + m.toString))
          }

        }
      }
    }
  }

  private[mybiotools] def invertBlockWise(a: SimpleMatrix, b: SimpleMatrix, c: SimpleMatrix, d: SimpleMatrix, ainv: SimpleMatrix): SimpleMatrix = {
    val cainv = c.mult(ainv)

    val schurAInv = {
      (d.minus(cainv.mult(b))).invert
    }

    val ainvb = ainv.mult(b)

    val newA = ainv.plus(ainvb.mult(schurAInv.mult(cainv)))
    val newB = (ainvb.mult(schurAInv)).scale(-1)
    val newC = (schurAInv.mult(cainv)).scale(-1)
    val newD = schurAInv

    val newM = new DenseMatrix64F(a.numRows + c.numRows, a.numCols + b.numCols)

    matrixInsert(newA.getMatrix, newM, 0, 0)
    matrixInsert(newB.getMatrix, newM, 0, a.numCols)
    matrixInsert(newC.getMatrix, newM, a.numRows, 0)
    matrixInsert(newD.getMatrix, newM, a.numRows, a.numCols)
    SimpleMatrix.wrap(newM)

  }

  case class FixPart(fixT: Mat[Double], fixXtX: DenseMatrix64F, fixXtXInv: Try[SimpleMatrix], xTy: SimpleMatrix, templateX: DenseMatrix64F) {
    def copy: FixPart = FixPart(fixT, fixXtX, fixXtXInv, new SimpleMatrix(xTy.getMatrix), new DenseMatrix64F(templateX))
  }
  object FixPart {
    def apply(fix: Mat[Double], y: Array[Double], varColumns: Int): FixPart = {
      val X = DenseMatrix64F.wrap(fix.numRows, fix.numCols, fix.contents)
      val yy = SimpleMatrix.wrap(DenseMatrix64F.wrap(y.size, 1, y))
      val XtX = {
        val xtx = new DenseMatrix64F(fix.numCols, fix.numCols)
        org.ejml.alg.dense.mult.MatrixMultProduct.inner_reorder(X, xtx)

        xtx
      }
      val inv = invertPositiveDefinite(SimpleMatrix.wrap(XtX))
      val xty = {
        val mult = SimpleMatrix.wrap(X).transpose.mult(yy)
        val extended = new DenseMatrix64F(mult.numRows + varColumns, 1)
        matrixInsert(mult.getMatrix, extended, 0, 0)
        SimpleMatrix.wrap(extended)
      }
      val templateX = new DenseMatrix64F(X.getNumRows, X.getNumCols + varColumns)
      matrixInsert(X, templateX, 0, 0)
      FixPart(fix.T, XtX, inv, xty, templateX)
    }
  }

  private def matrixInsert(src: DenseMatrix64F, dst: DenseMatrix64F, x: Int, y: Int): Unit = {
    org.ejml.alg.dense.misc.ImplCommonOps_DenseMatrix64F.extract(src, 0, 0, dst, x, y, src.numRows, src.numCols)
  }

  def linearRegressionFixPart(
    fixCov: FixPart,
    varDenseT: SimpleMatrix,
    outcome: Array[Double],
    shrinkage: Double
  ): Try[LinearRegressionResult] = {

    assert(fixCov.fixT.numCols == varDenseT.numCols, s"fix numRows (${fixCov.fixT.numCols}) != var numRows ${varDenseT.numCols}")
    val numSamples = fixCov.fixT.numCols

    val numParameters = fixCov.fixT.numRows + varDenseT.numRows

    val fixDenseT = SimpleMatrix.wrap(DenseMatrix64F.wrap(fixCov.fixT.numRows, fixCov.fixT.numCols, fixCov.fixT.contents))

    val X = {
      val X = fixCov.templateX

      var i = 0
      var j = 0
      val n = varDenseT.numRows
      val m = varDenseT.numCols
      val offset = fixDenseT.numRows
      while (j < n) {
        val col = offset + j
        while (i < m) {
          X.set(i, col, varDenseT.get(j, i))
          i += 1
        }
        i = 0
        j += 1
      }
      // println(X)
      // matrixInsert(varDenseT.transpose.getMatrix, X, 0, fixDenseT.numRows)
      // println(X)

      SimpleMatrix.wrap(X)
    }

    val y = SimpleMatrix.wrap(DenseMatrix64F.wrap(outcome.length, 1, outcome))

    val fixXtV = {
      val result = new DenseMatrix64F(fixDenseT.numRows, varDenseT.numRows)

      // I measured these and ejml was faster

      // BLAS match {
      // case Some(blas) => {
      // blas.dgemm("T", "N", fixDenseT.numRows, varDenseT.numCols, fixDenseT.numCols, 1.0, fixDenseT.getMatrix.getData, fixDenseT.numCols, varDenseT.getMatrix.getData, varDenseT.numCols, 0.0, result.getData, result.numRows)
      // SimpleMatrix.wrap(result)
      // }
      // case _ => {
      val ret = new DenseMatrix64F(fixDenseT.numRows, varDenseT.numRows)
      org.ejml.alg.dense.mult.MatrixMatrixMult.multTransB(fixDenseT.getMatrix, varDenseT.getMatrix, ret)
      SimpleMatrix.wrap(ret)
      // }
      // }
      // fixDenseT.mult(varDense)
    }

    val vTfixX = fixXtV.transpose

    val vtv = {
      val result = new DenseMatrix64F(varDenseT.numRows, varDenseT.numRows)
      org.ejml.alg.dense.mult.MatrixMultProduct.outer(varDenseT.getMatrix, result)
      SimpleMatrix.wrap(result)
    }

    val XtX = if (shrinkage == 0.0) None else Some {
      val xtx = new DenseMatrix64F(X.numCols, X.numCols)

      val XtX = {

        matrixInsert(fixCov.fixXtX, xtx, 0, 0)
        matrixInsert(fixXtV.getMatrix, xtx, 0, fixCov.fixXtX.numCols)
        matrixInsert(vTfixX.getMatrix, xtx, fixCov.fixXtX.numCols, 0)
        matrixInsert(vtv.getMatrix, xtx, fixCov.fixXtX.numCols, fixCov.fixXtX.numCols)
        SimpleMatrix.wrap(xtx)
      }
      XtX
    }

    val XtXplusLambdaIInverseOption = if (shrinkage == 0.0) {
      fixCov.fixXtXInv.map { inv =>
        invertBlockWise(
          a = SimpleMatrix.wrap(fixCov.fixXtX),
          b = fixXtV,
          c = vTfixX,
          d = vtv,
          ainv = inv
        )
      }
    } else {
      val idExceptFirst = SimpleMatrix.identity(numParameters)
      idExceptFirst.set(0, 0, 0.0)

      invertPositiveDefinite(XtX.get.plus(shrinkage, idExceptFirst))
    }

    XtXplusLambdaIInverseOption.map { XtXplusLambdaIInverse =>
      val XTmultY = {

        val vty = {
          val ret = new DenseMatrix64F(varDenseT.numRows, 1)
          org.ejml.alg.dense.mult.MatrixMatrixMult.mult_small(varDenseT.getMatrix, y.getMatrix, ret)
          SimpleMatrix.wrap(ret)
          // varDense.transpose.mult(y)
        }

        System.arraycopy(vty.getMatrix.getData, 0, fixCov.xTy.getMatrix.getData, fixCov.xTy.getMatrix.getData.length - vty.numRows, vty.numRows)
        fixCov.xTy
      }

      linearRegressionSecondPart(X, XtX, XTmultY, XtXplusLambdaIInverse, y, numSamples, numParameters, shrinkage)

    }
  }

  def linearRegression(
    covariateMatrix: Mat[Double],
    outcome: Vec[Double]
  ): Try[LinearRegressionResult] = linearRegression(covariateMatrix, outcome, 0.0, vec.ones(covariateMatrix.numCols))

  // Array(ind1,ind2,etc)
  def linearRegression(
    covariateMatrix: Mat[Double],
    outcome: Vec[Double],
    shrinkage: Double,
    penalizationWeights: Vec[Double]
  ): Try[LinearRegressionResult] = {

    val withIntercept = covariateMatrix

    val numSamples = withIntercept.numRows

    val numParameters = withIntercept.numCols

    val X = SimpleMatrix.wrap(DenseMatrix64F.wrap(withIntercept.numRows, withIntercept.numCols, withIntercept.contents))

    val y = SimpleMatrix.wrap(DenseMatrix64F.wrap(outcome.length, 1, outcome))

    val XtX = {
      val xtx = new DenseMatrix64F(X.numCols, X.numCols)
      BLAS match {
        case Some(blas) => blas.dgemm("N", "T", withIntercept.numCols, withIntercept.numCols, withIntercept.numRows, 1.0, X.getMatrix.data, withIntercept.numCols, X.getMatrix.data, withIntercept.numCols, 0.0, xtx.data, xtx.getNumCols)
        case None => org.ejml.alg.dense.mult.MatrixMultProduct.inner_reorder(X.getMatrix, xtx)
      }

      SimpleMatrix.wrap(xtx)
    }

    val XtXplusLambdaIInverseOption = if (shrinkage == 0.0) invertPositiveDefinite(XtX) else {
      val weightedIdentity = SimpleMatrix.diag(penalizationWeights.toSeq: _*)

      invertPositiveDefinite(XtX.plus(shrinkage, weightedIdentity))
    }
    XtXplusLambdaIInverseOption.map { XtXplusLambdaIInverse =>
      val XTmultY = new DenseMatrix64F(X.numCols, 1)
      org.ejml.alg.dense.mult.MatrixVectorMult.multTransA_reorder(X.getMatrix, y.getMatrix, XTmultY)
      linearRegressionSecondPart(X, Some(XtX), SimpleMatrix.wrap(XTmultY), XtXplusLambdaIInverse, y, numSamples, numParameters, shrinkage)
    }
  }

  private def linearRegressionSecondPart(X: SimpleMatrix, XtX: Option[SimpleMatrix], XTmultY: SimpleMatrix, XtXplusLambdaIInverse: SimpleMatrix, y: SimpleMatrix, numSamples: Int, numParameters: Int, shrinkage: Double) = {

    val estimator = XtXplusLambdaIInverse.mult(XTmultY)

    val predicted = X.mult(estimator)

    val error = y minus predicted

    val RSS = error.dot(error)

    val sigma2 = (1.0 / (numSamples - numParameters)) * RSS

    val variances = if (shrinkage == 0.0) XtXplusLambdaIInverse.scale(sigma2) else {
      XtXplusLambdaIInverse.mult(XtX.get).mult(XtXplusLambdaIInverse).scale(sigma2)
    }

    val df = if (shrinkage == 0.0) numParameters + 1.0 else {
      val hat = {
        val t1 = X.mult(XtXplusLambdaIInverse)
        val out = new DenseMatrix64F(X.numRows, X.numRows)
        org.ejml.alg.dense.mult.MatrixMatrixMult.multTransB(t1.getMatrix, X.getMatrix, out)
        SimpleMatrix.wrap(out)
      }
      hat.trace
    }

    val totalSS = {

      val outcomemean = {
        val ar = y.getMatrix.getData
        var i = 0
        var sum = 0.0
        while (i < ar.size) {
          sum += ar(i)
          i += 1
        }
        sum / ar.size
      }

      var ret = 0.0
      val yar = y.getMatrix.getData
      var i = 0
      val n = y.numRows
      while (i < n) {
        val x = yar(i) - outcomemean
        ret += x * x
        i += 1
      }

      ret

    }
    val rSquared = 1.0 - (RSS / totalSS)
    val betas = extractVector(estimator.getMatrix)
    val betasDropped1 = {
      val ar = Array.ofDim[Double](betas.size - 1)
      var i = 0
      val n = ar.size
      while (i < n) {
        ar(i) = betas(i + 1)
        i += 1
      }
      ar
    }
    val intercept = betas(0)

    val (paramsds, interceptsd) = {
      val ar1 = extractVector(variances.extractDiag.getMatrix)
      var a2 = Array.ofDim[Double](ar1.size - 1)
      val n = a2.size
      var i = 0
      while (i < n) {
        a2(i) = math.sqrt(ar1(i + 1))
        i += 1
      }
      (a2, math.sqrt(ar1(0)))
    }

    val residuals = extractVector(error.getMatrix)

    val adjr2 = {
      val n = residuals.size.toDouble
      val p = (X.numCols - 1).toDouble
      1.0 - (1.0 - rSquared) * ((n - 1) / (n - p - 1))
    }

    LinearRegressionResult(betasDropped1, intercept, interceptsd, rSquared, math.sqrt(sigma2), residuals, paramsds, df, shrinkage, adjr2)

  }

}