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

import org.saddle._
import mybiotools._
import mybiotools.stat.LinearRegression._
import mybiotools.eq._
import org.apache.commons.math3.random.{ RandomGenerator, RandomDataGenerator, Well44497b }
import org.ejml.simple.SimpleMatrix
import org.ejml.data.DenseMatrix64F

object BLASHelpers {
  def dot(m: Mat[Double], v: Vec[Double]): Vec[Double] = {
    LinearRegression.BLAS match {
      case Some(blas) => {
        val out = new DenseMatrix64F(m.numRows, 1)
        blas.dgemv("T", m.numCols, m.numRows, 1.0, m.contents, m.numCols, v.contents, 1, 0.0, out.data, 1)
        Vec(out.data: _*)

      }
      case None => (m dot v).col(0)
    }

  }
}

sealed trait SearchMode
case class BrentGrid(starts: Int, gridsize: Int) extends SearchMode
case class GridSearch(gridsize: Int) extends SearchMode
case class CMAES(populationSize: Int) extends SearchMode
case class BrentGridAndCMAES(cmaes: CMAES, brentgrid: BrentGrid) extends SearchMode

sealed trait BoundsOnLambda
case object FindBounds extends BoundsOnLambda
case class FixedBoundsOnLogScale(min: Double, max: Double) extends BoundsOnLambda

case class CrossValidationResult[P <: PenalizedRegressionFit](fit: P, cvRSquareds: Double)

case class NestedCrossValidation[P <: PenalizedRegressionFit](
  coefficients: List[(P, Double, Seq[(Double, Double)])],
  finalFit: Option[P],
  overallR2: Double,
  finalFitSingleCVR2: Option[Double],
  correctedOverallR2: Double
)

case class PenalizedLeastSquaresResult(coefficients: Vec[Double], lambda1: Double, lambda2: Double, objectiveFunction: Double, iterations: Int, rSquared: Double, penalty: PenaltyType) extends PenalizedRegressionFit {

  override def toString = s"PenalizedLeastSquaresResult(coeff=${coefficients.toSeq},lambda1=$lambda1,lambda2=$lambda2,obj=$objectiveFunction,it=$iterations,rsquared=$rSquared)"
}

sealed trait CrossValidationMode {
  def generate[I](d: DataForRegression[I]): Iterator[(DataForRegression[String], DataForRegression[String])]
  def withSeed(s: Int): CrossValidationMode
}
case object LeaveOneOut extends CrossValidationMode {
  def generate[I](d: DataForRegression[I]) = PenalizedRegressionWithCrossValidation.leaveOneOut(d)
  def withSeed(i: Int) = this
}
case class Bootstrap(replica: Int, seed: Int) extends CrossValidationMode {
  def generate[I](d: DataForRegression[I]) = {
    val rng = new Well44497b(seed)
    PenalizedRegressionWithCrossValidation.bootstrap(d, replica, rng)
  }
  def withSeed(i: Int) = Bootstrap(replica, i)
}
case class KFold(folds: Int, seed: Int, replica: Int) extends CrossValidationMode {
  def generate[I](d: DataForRegression[I]) = {
    val rng = new Well44497b(seed)
    (1 to replica iterator) flatMap { r =>
      PenalizedRegressionWithCrossValidation.kFold(d, folds, rng)
    }
  }
  def withSeed(i: Int) = KFold(folds, i, replica)
}
case class KFoldStratified(folds: Int, seed: Int, tries: Int) extends CrossValidationMode {
  def generate[I](d: DataForRegression[I]) = {
    val rng = new Well44497b(seed)
    val splits = (1 to tries iterator) map { r =>
      val foldswithmean = (PenalizedRegressionWithCrossValidation.kFold(d, folds, rng).map {
        case (training, validation) =>
          (training, validation, training.y.mean)
      }).toSeq
      val means = foldswithmean.map(_._3)
      val folds1 = foldswithmean.map(x => (x._1, x._2))
      SummaryStat(means).stddev -> folds1
    }

    splits.minBy(_._1)._2.iterator

  }
  def withSeed(i: Int) = KFoldStratified(folds, i, tries)
}

sealed trait UseLambda
case object MinimumPredictionError extends UseLambda
case class MaxSelectedVariables(n: Int) extends UseLambda
case object LessComplexModel extends UseLambda

sealed trait PenaltyType
case object L1 extends PenaltyType
case object L2 extends PenaltyType
case object SCAD extends PenaltyType
case object ElasticNet extends PenaltyType

trait PenalizedRegressionFit {
  def coefficients: Vec[Double]
  def predict(v: Vec[Double]) = v dot coefficients
  def rSquared: Double
}

trait PenalizedRegression[+T <: PenalizedRegressionFit, L] extends Serializable {
  def fit(
    design: Mat[Double],
    y: Vec[Double],
    lambda: L,
    penalizationWeights: Vec[Double],
    first: Option[Vec[Double]]
  ): Option[T]

  def fit[I](
    data: Frame[I, String, Double],
    covariates: Seq[String],
    yKey: String,
    missingMode: MissingMode,
    unpenalized: Seq[String],
    standardize: Boolean,
    lambda: L
  )(implicit st: ST[I], ord: Ordering[I]): Option[(T, Series[String, Double])] = {

    val data2 = createDataMatrixForLinearRegression(data, covariates, yKey, missingMode)

    val penalizationWeights = Vec(("intercept" +: data2.covariateNames).map(x =>
      if (unpenalized.contains(x) || x == "intercept") 0.0
      else 1.0): _*)

    val standardized = if (standardize) PenalizedRegressionWithCrossValidation.standardize(data2.design) else data2.design

    fit(data2.design, data2.y, lambda, penalizationWeights, None).map { fit =>

      val selectedVariables = (("intercept" +: data2.covariateNames).zip(fit.coefficients.toSeq)) filter (x => math.abs(x._2) > 0.0) map (_._1) filterNot (_ === "intercept") toIndexedSeq

      val coefficients = Series((("intercept" +: data2.covariateNames).zip(fit.coefficients.toSeq)): _*)

      val backscaledCoefficients = if (standardize) {
        val means = Vec(data2.design.cols.map(_.mean): _*)
        val sds = Vec(data2.design.cols.map(x => math.sqrt(x.variance)): _*)
        val s1 = (fit.coefficients / sds).tail(sds.length - 1)
        val scaledintercept = fit.coefficients.raw(0) - (s1 dot means.tail(means.length - 1))
        Series(Vec(scaledintercept +: s1.toSeq: _*), Index("intercept" +: data2.covariateNames: _*))

      } else coefficients

      (fit, backscaledCoefficients)

    }
  }
}

object Ridge extends PenalizedRegression[PenalizedLeastSquaresResult, Double] {
  def fit(
    design: Mat[Double],
    y: Vec[Double],
    lambda: Double,
    penalizationWeights: Vec[Double],
    first: Option[Vec[Double]] = None
  ) = LASSOImpl.penalizedLeastSquares(design, y, Double.NaN, lambda, penalizationWeights, L2, first = first)
}

object RidgeClosedForm extends PenalizedRegression[LinearRegressionResult, Double] {
  def fit(
    design: Mat[Double],
    y: Vec[Double],
    lambda: Double,
    penalizationWeights: Vec[Double],
    first: Option[Vec[Double]] = None
  ) = linearRegression(design, y, lambda, penalizationWeights).toOption
}

object LASSO extends PenalizedRegression[PenalizedLeastSquaresResult, Double] {
  def fit(
    design: Mat[Double],
    y: Vec[Double],
    lambda: Double,
    penalizationWeights: Vec[Double],
    first: Option[Vec[Double]] = None
  ) =
    LASSOImpl.penalizedLeastSquares(design, y, lambda, Double.NaN, penalizationWeights, L1, first = first)

}

object PenalizedWithElasticNet extends PenalizedRegression[PenalizedLeastSquaresResult, (Double, Double)] {
  def fit(
    design: Mat[Double],
    y: Vec[Double],
    lambda: (Double, Double),
    penalizationWeights: Vec[Double],
    first: Option[Vec[Double]] = None
  ) = LASSOImpl.penalizedLeastSquares(design, y, lambda._1, lambda._2, penalizationWeights, ElasticNet, first = first, maxIter = 500)
}

object PenalizedWithSCAD extends PenalizedRegression[PenalizedLeastSquaresResult, Double] {
  def fit(
    design: Mat[Double],
    y: Vec[Double],
    lambda: Double,
    penalizationWeights: Vec[Double],
    first: Option[Vec[Double]] = None
  ) = LASSOImpl.penalizedLeastSquares(design, y, lambda, Double.NaN, penalizationWeights, SCAD, first = first)
}

// case class BOLASSO(bootstrap: Bootstrap, implementation: PenalizedRegression[PenalizedLeastSquaresResult], threads: Int) extends PenalizedRegression[PenalizedLeastSquaresResult] {
//   def fit(
//     design: Mat[Double],
//     y: Vec[Double],
//     lambda: Double,
//     penalizationWeights: Vec[Double],
//     first: Option[Vec[Double]]) =
//     PenalizedRegressionWithCrossValidation.bolasso(
//       design,
//       y,
//       lambda,
//       penalizationWeights,
//       bootstrap,
//       warmStart = true,
//       threads,
//       implementation,
//       first)
// }

object PenalizedRegressionWithCrossValidation {

  def rSquared(predicted: Vec[Double], y: Vec[Double]) = {
    {
      val residualSS = {
        val r = y - predicted
        r dot r
      }
      val totalSS = {
        val r = y - y.mean
        r dot r
      }
      1.0 - residualSS / totalSS
    }
  }

  def leaveOneOut[I](d: DataForRegression[I]): Iterator[(DataForRegression[String], DataForRegression[String])] = (0 until d.y.length iterator) map { i =>
    val designOut = d.design.takeRows(i)
    val designIn = d.design.withoutRows(i)
    val yOut = d.y(i)
    val yIn = d.y.without(Array(i))
    DataForRegression(yIn, designIn, d.covariateNames) -> DataForRegression(yOut, designOut, d.covariateNames)
  }

  def kFold(d: DataForRegression[_], fold: Int, rng: RandomGenerator): Iterator[(DataForRegression[String], DataForRegression[String])] = {
    val indices = (0 until d.y.length).toIndexedSeq

    val shuffled = RandomShuffler.shuffle(indices, rng)
    shuffled.grouped(shuffled.size / fold + 1).map { ii =>
      val i = ii.toArray
      val designOut = d.design.takeRows(i)
      val designIn = d.design.withoutRows(i)
      val yOut = d.y(i)
      val yIn = d.y.without(i)
      DataForRegression(yIn, designIn, d.covariateNames) -> DataForRegression(yOut, designOut, d.covariateNames)
    }
  }

  def bootstrap(d: DataForRegression[_], replica: Int, rng: RandomGenerator): Iterator[(DataForRegression[String], DataForRegression[String])] = {
    val indices = (0 until d.y.length).toIndexedSeq

    (Resampling.bootstrap(replica, indices, rng) {
      case (ii, _) =>
        val i = ii.toArray
        val designIn = d.design.takeRows(i)
        val designOut = d.design.withoutRows(i)
        val yIn = d.y(i)
        val yOut = d.y.without(i)
        DataForRegression(yIn, designIn, d.covariateNames) -> DataForRegression(yOut, designOut, d.covariateNames)
    }).iterator
  }

  trait Optimizer[L] {
    def search[P <: PenalizedRegressionFit](
      design: Mat[Double],
      y: Vec[Double],
      penalizationWeights: Vec[Double],
      implementation: PenalizedRegression[P, L],
      runLambda: L => Option[(L, SummaryStat)]
    ): L
  }

  case class Search2DCMAES(
      populationSize: Int,
      lambda1Min: Double,
      lambda1Max: Double,
      lambda2Min: Double,
      lambda2Max: Double
  ) extends Optimizer[(Double, Double)] {
    def search[P <: PenalizedRegressionFit](
      design: Mat[Double],
      y: Vec[Double],
      penalizationWeights: Vec[Double],
      implementation: PenalizedRegression[P, (Double, Double)],
      runLambda: ((Double, Double)) => Option[((Double, Double), SummaryStat)]
    ): (Double, Double) = {
      val (y1, y2) = mybiotools.search.cmaes2D(
        lambda1Min,
        lambda1Max,
        lambda2Min,
        lambda2Max,
        (lambda1Min + (lambda1Max - lambda1Min) / 2, lambda2Min + (lambda2Max - lambda2Min) / 2),
        100,
        0,
        1E-4,
        1E-4,
        new Well44497b(1),
        populationSize
      )((x: (Double, Double)) => runLambda(math.exp(x._1) -> math.exp(x._2)).map(_._2.mean).getOrElse(Double.NaN))

      (math.exp(y1), math.exp(y2))
    }
  }

  case class Search1D(
      boundsOnLambda: BoundsOnLambda,
      useLambda: UseLambda,
      searchMode: SearchMode
  ) extends Optimizer[Double] {

    def search[P <: PenalizedRegressionFit](
      design: Mat[Double],
      y: Vec[Double],
      penalizationWeights: Vec[Double],
      implementation: PenalizedRegression[P, Double],
      runLambda: Double => Option[(Double, SummaryStat)]
    ): Double = {

      // search for the largest penalization which does not shrinks all coefficient to 0
      val maxLambda = boundsOnLambda match {
        case FixedBoundsOnLogScale(_, max) => max
        case FindBounds => {
          def f(l: Double) = {
            val rnull = {
              val nonpenalizedvariables = design.col(penalizationWeights.toSeq.zipWithIndex.filter(_._1 == 0.0).map(_._2): _*)
              if (nonpenalizedvariables.numCols > 0) linearRegression(nonpenalizedvariables, y).map(_.rSquared).toOption
              else Some(rSquared(vec.zeros(design.numRows), y))
            }
            val explainsLessThanNullModel = rnull.flatMap(rnull => implementation.fit(design, y, math.exp(l), penalizationWeights, None).map(_.rSquared < (rnull + math.abs(rnull * 0.000000001)))).getOrElse(false)
            if (explainsLessThanNullModel) 1.0 else -1.0
          }
          if (f(-10.0) == 1.0) -10.0
          else {
            mybiotools.search.bisect(-10d, 30d, 9.5, 20, 1E-3, 1E-3)(f)
          }
        }
      }

      // if MaxSelectedVariables then set this minimum to that
      val minLambda = boundsOnLambda match {
        case FixedBoundsOnLogScale(min, _) => min
        case FindBounds => {
          val min1 = {
            val rsquarediwithzerolambda = implementation.fit(design, y, 0.0, penalizationWeights, None).map(_.rSquared).getOrElse(0.99999)
            def f(l: Double) = {
              val overfit = implementation.fit(design, y, math.exp(l), penalizationWeights, None).map(_.rSquared >= rsquarediwithzerolambda * 0.99999999).getOrElse(true)
              if (overfit) -1.0 else 1.0
            }

            if (f(-80) == 1) -80
            else mybiotools.search.bisect(-80, maxLambda, (-80 + maxLambda) / 2, 30, 1E-3, 1E-3)(f)
          }

          useLambda match {
            case MaxSelectedVariables(maxVariables) => {
              def f(l: Double) = {
                val numberofvariables = implementation.fit(design, y, math.exp(l), penalizationWeights, None).map(_.coefficients.countif(_ != 0.0)).getOrElse(0)
                if (numberofvariables > maxVariables) -1.0 else 1.0
              }
              if (f(min1) == 1.0) min1
              else mybiotools.search.bisect(min1, maxLambda, 0.0, 30, 1E-3, 1E-3)(f)
            }
            case _ => min1
          }

        }
      }

      // println("MINLAMBDA " + minLambda)
      // println("MAXLAMBDA " + maxLambda)

      // find lambda with minimum median prediction error
      // a series of brent searches over a grid
      def doSearch(mode: SearchMode) = mode match {
        case GridSearch(gridsize) => (minLambda to maxLambda by (maxLambda - minLambda) / gridsize map (x => runLambda(math.exp(x))) filter (_.isDefined) minBy (_.get._2.mean)).get._1
        case BrentGrid(starts, gridsize) => {

          def search(min: Double, max: Double): (Double, Double) =
            (mybiotools.search.brent(min, max, 1E-3, 1E-3, 100, starts, 123) { logLambda =>

              val r = runLambda(math.exp(logLambda)).map(_._2.mean).getOrElse(Double.NaN)
              r
            })

          math.exp(minLambda until maxLambda by (maxLambda - minLambda) / gridsize map { min =>

            val max = min + (maxLambda - minLambda) / gridsize
            search(min, max)
          } minBy (_._2) _1)
        }
        case CMAES(pop) => math.exp(mybiotools.search.cmaes(minLambda, maxLambda, minLambda + (maxLambda - minLambda) / 2, 100, 0, 1E-4, 1E-4, new Well44497b(1), pop)(x => runLambda(math.exp(x)).map(_._2.mean).getOrElse(Double.NaN)))
        case _ => throw new RuntimeException("should not happen")
      }

      val search = searchMode match {
        case BrentGridAndCMAES(cmaes, brent) => {
          val b = doSearch(brent)
          val c = doSearch(cmaes)
          val bv = runLambda(b).map(_._2.mean)
          val cv = runLambda(c).map(_._2.mean)
          if (bv.isDefined && cv.isDefined) {
            if (bv.get < cv.get) b else c
          } else if (bv.isDefined) b else c
        }
        case x => doSearch(x)
      }

      useLambda match {
        case MinimumPredictionError => search
        case MaxSelectedVariables(_) if boundsOnLambda == FindBounds => search
        case MaxSelectedVariables(maxVariables) => {
          def f(l: Double) = {
            val numberofvariables = implementation.fit(design, y, math.exp(l), penalizationWeights, None).map(_.coefficients.countif(_ != 0.0)).getOrElse(0)
            if (numberofvariables > maxVariables) -1.0 else 1.0
          }
          if (f(minLambda) == 1.0) minLambda
          else mybiotools.search.bisect(minLambda, maxLambda, 0.0, 30, 1E-3, 1E-3)(f)
        }
        case LessComplexModel => {
          val summaryAtSearchMinimum = runLambda(search).get._2

          // find largest lambda median prediction error close to the 97.5 quantile of the minium lambda's prediction errors
          val searchLarger =
            (mybiotools.search.brent(search, math.exp(maxLambda), 1E-3, 1E-1, 50, 1, 123) { lambda =>
              val l = runLambda(lambda)
              if (l.isDefined) {
                math.pow(l.get._2.mean - summaryAtSearchMinimum.mean - summaryAtSearchMinimum.stddev, 2)
              } else Double.NaN

            })._1

          searchLarger
        }
      }

    }
  }

  def standardize(m: Mat[Double]) = Mat(m.cols.zipWithIndex.map {
    case ((vec), idx) =>
      if (idx == 0) vec
      else ((vec - vec.mean) / math.sqrt(vec.variance))
  }: _*)

  def crossValidation[I, P <: PenalizedRegressionFit, L](
    data: Frame[I, String, Double],
    covariates: Seq[String],
    yKey: String,
    missingMode: MissingMode,
    crossValidationMode: CrossValidationMode,
    optimizer: Optimizer[L],
    warmStart: Boolean = true,
    generateCVSamplesOnThFly: Boolean = false,
    threads: Int = 1,
    unpenalized: Seq[String] = Nil,
    standardize: Boolean = true,
    implementation: PenalizedRegression[P, L]
  )(implicit st: ST[I], o: Ordering[I]): Option[(CrossValidationResult[P], Seq[String], Series[String, Double])] = {

    val data2 = createDataMatrixForLinearRegression(data, covariates, yKey, missingMode)

    val penalizationWeights = Vec(("intercept" +: data2.covariateNames).map(x =>
      if (unpenalized.contains(x) || x == "intercept") 0.0
      else 1.0): _*)

    val standardized = if (standardize) PenalizedRegressionWithCrossValidation.standardize(data2.design) else data2.design

    crossValidation(standardized, data2.y, penalizationWeights, crossValidationMode, optimizer, warmStart, generateCVSamplesOnThFly, threads, implementation).map { finalLasso =>

      val selectedVariables = (("intercept" +: data2.covariateNames).zip(finalLasso.fit.coefficients.toSeq)) filter (x => math.abs(x._2) > 0.0) map (_._1) filterNot (_ === "intercept") toIndexedSeq

      val coefficients = Series((("intercept" +: data2.covariateNames).zip(finalLasso.fit.coefficients.toSeq)): _*)

      val backscaledCoefficients = if (standardize) {
        val means = Vec(data2.design.cols.map(_.mean): _*)
        val sds = Vec(data2.design.cols.map(x => math.sqrt(x.variance)): _*)
        val s1 = (finalLasso.fit.coefficients / sds).tail(sds.length - 1)
        val scaledintercept = finalLasso.fit.coefficients.raw(0) - (s1 dot means.tail(means.length - 1))
        Series(Vec(scaledintercept +: s1.toSeq: _*), Index("intercept" +: data2.covariateNames: _*))

      } else coefficients

      (finalLasso, selectedVariables, backscaledCoefficients)

    }

  }

  def crossValidation[P <: PenalizedRegressionFit, L](
    design: Mat[Double],
    y: Vec[Double],
    penalizationWeights: Vec[Double],
    crossValidationMode: CrossValidationMode,
    optimizer: Optimizer[L],
    warmStart: Boolean,
    generateCVSamplesOnThFly: Boolean,
    threads: Int,
    implementation: PenalizedRegression[P, L]
  ): Option[CrossValidationResult[P]] =
    {

      val data2 = DataForRegression(y, design, Nil)

      val lastResult = scala.collection.mutable.Map[Int, Vec[Double]]()

      var cvsamples: Option[List[(DataForRegression[_], DataForRegression[_])]] = None

      def getCV =
        if (generateCVSamplesOnThFly) crossValidationMode.generate(data2)
        else {
          if (cvsamples.isDefined) cvsamples.get.iterator
          else {
            cvsamples = Some(crossValidationMode.generate(data2).toList)
            cvsamples.get.iterator
          }
        }

      def runLambda(lambda: L) = {
        val cvsamples = getCV

        val raw = (ParIterator.map(cvsamples.zipWithIndex, threads) {
          case ((training, validation), cvidx) =>

            val first = if (!warmStart) None else {
              lastResult.get(cvidx) match {
                case None => None
                case Some(x) => Some(x)
              }
            }

            implementation.fit(training.design, training.y, lambda, penalizationWeights, first).map { model =>

              // Evaluate model on training examples and compute training error

              val validationPrediction = Vec(validation.design.rows.zipWithIndex.map {
                case (validationFeatures, idx) =>
                  model.predict(validationFeatures)
              }: _*)

              val squaredError = (validationPrediction - validation.y) dot (validationPrediction - validation.y)

              val rSquared = PenalizedRegressionWithCrossValidation.rSquared(validationPrediction, validation.y)

              // val squaredError = validation.design.rows.zipWithIndex.map {
              //   case (validationFeatures, idx) =>
              //     val prediction: Double = model.predict(validationFeatures)

              //     math.pow((validation.y.raw(idx) - prediction), 2)
              // }.sum

              // val rSquared = 

              synchronized {
                lastResult.update(cvidx, model.coefficients)
              }

              (squaredError / validation.y.length, model.coefficients, rSquared, validation.y, validationPrediction)
            }

        }).toList

        if (raw.exists(_.isEmpty)) None
        else {

          val predictionErrors = raw.map(_.get._1)
          val rsquareds = raw.map(_.get._3)

          val innerOverallRSquared = {
            val outcomes = Vec(raw.map(_.get._4.toSeq).flatten: _*)
            val pp = Vec(raw.map(_.get._5.toSeq).flatten: _*)
            PenalizedRegressionWithCrossValidation.rSquared(pp, outcomes)
          }

          // val (predictionErrors, coefficients, rsquareds) = raw.map(_.get).unzip

          val summary = SummaryStat(predictionErrors.toList)

          // println((lambda, summary.mean))

          Some((lambda, summary, rsquareds, innerOverallRSquared))
        }

      }

      val usedLambda: L = optimizer.search(design, y, penalizationWeights, implementation, (x: L) => runLambda(x).map(x => (x._1, x._2)))

      // println("USELAMBDA " + usedLambda)
      implementation.fit(data2.design, data2.y, usedLambda, penalizationWeights, None).map { f =>
        CrossValidationResult(
          f,
          runLambda(usedLambda).map(_._4).get
        )
      }

    }

  // def bolasso[P <: PenalizedRegressionFit](
  //   design: Mat[Double],
  //   y: Vec[Double],
  //   lambda: Double,
  //   penalizationWeights: Vec[Double],
  //   bootstrap: Bootstrap,
  //   warmStart: Boolean,
  //   threads: Int,
  //   implementation: PenalizedRegression[P],
  //   firstGuess: Option[Vec[Double]]): Option[P] = {

  //   val data2 = DataForRegression(y, design, Nil)

  //   var firstGuess1: Option[Vec[Double]] = firstGuess

  //   val list = (ParIterator.map(bootstrap.generate(data2), threads) {
  //     case (training, _) =>

  //       val fit = implementation.fit(training.design, training.y, lambda, penalizationWeights, firstGuess1)

  //       if (warmStart && fit.isDefined) {
  //         synchronized {
  //           if (firstGuess1.isEmpty) {
  //             firstGuess1 = fit.map(x => (x.coefficients))
  //           }
  //         }
  //       }

  //       fit
  //   }).toList

  //   scala.util.Try {

  //     val minimumCoefficients = Vec(Mat(list.map(_.map(_.coefficients)).flatten: _*)
  //       .rows
  //       .map(x => x.map(math.abs).min.get): _*)

  //     val penalizationWeights2: Vec[Double] = {
  //       penalizationWeights.zipMap(minimumCoefficients)((x1, x2) => if (x1 == 0.0) 0.0 else 1.0 / (x2 * x2))
  //     }

  //     implementation.fit(design, y, lambda, penalizationWeights2, None)
  //   }.toOption.getOrElse(None)

  // }

  def nestedCrossValidation[I, P <: PenalizedRegressionFit, L](
    data: Frame[I, String, Double],
    covariates: Seq[String],
    yKey: String,
    missingMode: MissingMode,
    crossValidationModeInner: CrossValidationMode,
    crossValidationModeOuter: CrossValidationMode,
    optimizer: Optimizer[L],
    warmStart: Boolean = true,
    generateCVSamplesOnThFly: Boolean = false,
    threads: Int = 1,
    unpenalized: Seq[String] = Nil,
    standardize: Boolean = true,
    implementation: PenalizedRegression[P, L]
  )(implicit st: ST[I], o: Ordering[I]): (NestedCrossValidation[P], Option[Series[String, Double]]) = {

    val data2 = createDataMatrixForLinearRegression(data, covariates, yKey, missingMode)

    val penalizationWeights = Vec(("intercept" +: data2.covariateNames).map(x =>
      if (unpenalized.contains(x) || x == "intercept") 0.0
      else 1.0): _*)

    val standardized = if (standardize) PenalizedRegressionWithCrossValidation.standardize(data2.design) else data2.design

    val result = nestedCrossValidation(
      standardized,
      data2.y,
      penalizationWeights,
      crossValidationModeInner,
      crossValidationModeOuter,
      optimizer,
      warmStart,
      generateCVSamplesOnThFly,
      None,
      threads,
      implementation
    )

    val finalFitCoefficients = result.finalFit.map { fit =>

      val selectedVariables = (("intercept" +: data2.covariateNames).zip(fit.coefficients.toSeq)) filter (x => math.abs(x._2) > 0.0) map (_._1) filterNot (_ === "intercept") toIndexedSeq

      val coefficients = Series((("intercept" +: data2.covariateNames).zip(fit.coefficients.toSeq)): _*)

      if (standardize) {
        val means = Vec(data2.design.cols.map(_.mean): _*)
        val sds = Vec(data2.design.cols.map(x => math.sqrt(x.variance)): _*)
        val s1 = (fit.coefficients / sds).tail(sds.length - 1)
        val scaledintercept = fit.coefficients.raw(0) - (s1 dot means.tail(means.length - 1))
        Series(Vec(scaledintercept +: s1.toSeq: _*), Index("intercept" +: data2.covariateNames: _*))

      } else coefficients
    }

    result -> finalFitCoefficients

  }

  def nestedCrossValidation[P <: PenalizedRegressionFit, L](
    design: Mat[Double],
    y: Vec[Double],
    penalizationWeights: Vec[Double],
    crossValidationModeInner: CrossValidationMode,
    crossValidationModeOuter: CrossValidationMode,
    optimizer: Optimizer[L],
    warmStart: Boolean,
    generateCVSamplesOnThFly: Boolean,
    adaptive: Option[Optimizer[Double]],
    threads: Int,
    implementation: PenalizedRegression[P, L]
  ): NestedCrossValidation[P] = {

    val data2 = DataForRegression(y, design, Nil)

    val cvsamples = crossValidationModeOuter.generate(data2)

    val list = (ParIterator.map(cvsamples, threads) {
      case (training, validation) =>

        val cv = if (adaptive.isDefined) {

          val training1 = DataForRegression(
            training.y(0 until training.y.length / 2: _*),
            training.design.takeRows(0 until training.y.length / 2: _*),
            Nil
          )

          val training2 = DataForRegression(
            training.y(training.y.length / 2 until training.y.length: _*),
            training.design.takeRows(training.y.length / 2 until training.y.length: _*),
            Nil
          )

          crossValidation(training1.design, training1.y, penalizationWeights, crossValidationModeInner, adaptive.get, warmStart, generateCVSamplesOnThFly, threads = 1, Ridge).flatMap {
            case CrossValidationResult(firstStep, _) =>

              val penalizationWeights2 =
                penalizationWeights.zipMap(firstStep.coefficients) {
                  case (pw, coef) =>
                    if (pw == 0.0) 0.0 else 1.0 / (coef * coef)
                }

              crossValidation(training2.design, training2.y, penalizationWeights2, crossValidationModeInner, optimizer, warmStart, generateCVSamplesOnThFly, 1, implementation)
          }
        } else crossValidation(training.design, training.y, penalizationWeights, crossValidationModeInner, optimizer, warmStart, generateCVSamplesOnThFly, 1, implementation)

        cv.map {
          case CrossValidationResult(model, _) =>

            val predictions = validation.design.rows.zipWithIndex.map {
              case (validationFeatures, idx) =>
                val prediction: Double = model.predict(validationFeatures)

                (validation.y.raw(idx), prediction)
            }

            val innerRSquared = {
              val (obs, pred) = predictions.unzip
              rSquared(Vec(pred: _*), Vec(obs: _*))
            }

            // println("INNER RESULT " + innerRSquared + "\n " + model.coefficients.toSeq)

            (model, innerRSquared, predictions, validation)
        }

    }).toList.filter(_.isDefined).map(_.get)

    val predictions = list.flatMap(_._3)

    val rsq = {
      rSquared(Vec(predictions.map(_._2): _*), Vec(predictions.map(_._1): _*))
    }

    val removedCoefficients = Vec(Mat(list.map(_._1.coefficients): _*)
      .rows
      .map(x => (x.min.isDefined && x.map(math.abs).min.get == 0.0) || (x.max.isDefined && x.min.isDefined && x.max.get > 0 && x.min.get < 0)): _*)

    val penalizationWeights3: Vec[Double] = {
      penalizationWeights.zipMap(removedCoefficients)((x1, x2) => if (x1 == 0.0) 0.0 else if (x2) Double.PositiveInfinity else 1.0)
    }

    val intersectCV = crossValidation(design, y, penalizationWeights3, crossValidationModeInner, optimizer, warmStart, generateCVSamplesOnThFly, threads, implementation)

    val adjustedR2 = {

      val indices = removedCoefficients.toSeq.zipWithIndex.filter(_._1 != true).map(_._2).toArray //.sortBy(x => math.abs(x._1)).reverse.take(math.sqrt(y.length).toInt).map(_._2).toArray

      val predictions = list.flatMap {
        case (model, _, _, validationdata) =>

          validationdata.design.rows.zipWithIndex.map {
            case (validationFeatures, idx) =>
              val prediction: Double = model.coefficients(indices) dot validationFeatures(indices)

              (validationdata.y.raw(idx), prediction)
          }

      }
      rSquared(Vec(predictions.map(_._2): _*), Vec(predictions.map(_._1): _*))
    }

    NestedCrossValidation[P](list.map(x => (x._1, x._2, x._3)), intersectCV.map(_.fit), rsq, intersectCV.map(_.cvRSquareds), adjustedR2)
  }

  def nestedCrossValidationAfterPreselection[P <: PenalizedRegressionFit, L](
    design: Mat[Double],
    y: Vec[Double],
    penalizationWeights: Vec[Double],
    crossValidationModeInner: CrossValidationMode,
    crossValidationModeOuter: CrossValidationMode,
    optimizer: Optimizer[L],
    optimizerForPreselectionLasso: Optimizer[Double],
    warmStart: Boolean,
    generateCVSamplesOnThFly: Boolean,
    adaptive: Option[Optimizer[Double]],
    threads: Int,
    implementation: PenalizedRegression[P, L]
  ) = {

    crossValidation(
      design,
      y,
      penalizationWeights,
      crossValidationModeInner.withSeed(987),
      optimizerForPreselectionLasso,
      warmStart,
      generateCVSamplesOnThFly,
      threads,
      LASSO
    ).flatMap { preselect =>

        val indices = preselect.fit.coefficients.toSeq.zipWithIndex.filter(x => math.abs(x._1) > 0.0).map(_._2)

        if (indices.size > 0) {

          Some((nestedCrossValidation(
            design.col(indices: _*),
            y,
            penalizationWeights(indices: _*),
            crossValidationModeInner,
            crossValidationModeOuter,
            optimizer,
            warmStart,
            generateCVSamplesOnThFly,
            adaptive,
            threads = threads,
            implementation
          ), indices))
        } else None
      }

  }

  def estimateR2WithTwoNestedSteps[P <: PenalizedRegressionFit, L](
    design: Mat[Double],
    y: Vec[Double],
    penalizationWeights: Vec[Double],
    crossValidationModeInner: CrossValidationMode,
    crossValidationModeOuter: CrossValidationMode,
    optimizer: Optimizer[L],
    warmStart: Boolean,
    generateCVSamplesOnThFly: Boolean,
    adaptive: Option[Optimizer[Double]],
    threads: Int,
    implementation: PenalizedRegression[P, L]
  ) = {

    nestedCrossValidation(
      design,
      y,
      penalizationWeights,
      crossValidationModeInner,
      crossValidationModeOuter,
      optimizer,
      warmStart,
      generateCVSamplesOnThFly,
      adaptive,
      threads = threads,
      implementation
    ).finalFit.flatMap { fit =>

      val indices = fit.coefficients.toSeq.zipWithIndex.filter(x => math.abs(x._1) > 0.0).map(_._2)

      if (indices.size > 0) {

        Some((nestedCrossValidation(
          design.col(indices: _*),
          y,
          penalizationWeights(indices: _*),
          crossValidationModeInner.withSeed(9213),
          crossValidationModeOuter.withSeed(943),
          optimizer,
          warmStart,
          generateCVSamplesOnThFly,
          adaptive,
          threads = threads,
          implementation
        ), indices))

      } else None
    }

  }

  def estimateR2With3Loops[P <: PenalizedRegressionFit, L](
    design: Mat[Double],
    y: Vec[Double],
    penalizationWeights: Vec[Double],
    crossValidationModeInner: CrossValidationMode,
    crossValidationModeOuter: CrossValidationMode,
    crossValidationModeOuter2: CrossValidationMode,
    optimizer: Optimizer[L],
    warmStart: Boolean,
    generateCVSamplesOnThFly: Boolean,
    adaptive: Option[Optimizer[Double]],
    threads: Int,
    implementation: PenalizedRegression[P, L]
  ) = {

    val data2 = DataForRegression(y, design, Nil)

    val list = (ParIterator.map(crossValidationModeOuter2.generate(data2), threads) {
      case (training, validation) =>

        estimateR2WithTwoNestedSteps(
          training.design,
          training.y,
          penalizationWeights,
          crossValidationModeInner,
          crossValidationModeOuter,
          optimizer,
          warmStart,
          generateCVSamplesOnThFly,
          adaptive,
          threads = 1,
          implementation
        ).flatMap(x => x._1.finalFit.map { model =>

          validation.design.rows.zipWithIndex.map {
            case (validationFeatures, idx) =>
              val prediction: Double = model.predict(validationFeatures(x._2: _*))

              (validation.y.raw(idx), prediction)
          }

        })

    } toList).filter(_.isDefined).map(_.get)

    val rsq = {
      rSquared(Vec(list.flatMap(_.map(_._2)): _*), Vec(list.flatMap(_.map(_._1)): _*))
    }

    rsq

  }

}

object LASSOImpl {

  def lasso(
    design: Mat[Double],
    y: Vec[Double],
    lambda: Double,
    penalizationWeights: Vec[Double],
    relativeThreshold: Double = 1E-4,
    maxIter: Int
  ) = penalizedLeastSquares(design, y, lambda, Double.NaN, penalizationWeights, L1, relativeThreshold, maxIter)

  /**
   * accelerated proximal gradient descent (FISTA)
   *
   * ref: https://web.stanford.edu/~boyd/papers/pdf/prox_algs.pdf page 152
   * ref: https://blogs.princeton.edu/imabandit/2013/04/11/orf523-ista-and-fista/
   * ref: http://mechroom.technion.ac.il/~becka/papers/71654.pdf eq 1.4
   * ref for SCAD: http://sites.stat.psu.edu/~rli/research/penlike.pdf
   * ref for SCAD: https://hal.archives-ouvertes.fr/hal-01102810/document !!NOTE! that the scad proximity operator, first case on top of page 4 is wrong there is a typo, absolute value is missing in the divisor. correct is sign(z)*max(0,(|x|-lambda)) OR x*max(0,(1-lambda/|x|))
   */
  def penalizedLeastSquares(
    design: Mat[Double],
    y: Vec[Double],
    lambda1: Double,
    lambda2: Double,
    penalizationWeights: Vec[Double],
    penalty: PenaltyType,
    relativeThreshold: Double = 1E-6,
    absoluteThreshold: Double = 1E-10,
    maxIter: Int = 1500,
    first: Option[Vec[Double]] = None
  ) = {

    val designT = design.T

    def shrinkElasticNet(w: Vec[Double], alpha1: Double, alpha2: Double) = w.zipMap(penalizationWeights)((old, pw) =>
      if (pw.isPosInfinity) 0.0
      else math.signum(old) * math.max(0.0, (math.abs(old) - alpha1 * pw) / (1.0 + pw * alpha2)))

    def shrinkL1(w: Vec[Double], alpha: Double) =
      w.zipMap(penalizationWeights)((old, pw) =>
        math.signum(old) * math.max(0.0, math.abs(old) - alpha * pw))

    def shrinkL2(w: Vec[Double], alpha: Double) = w.zipMap(penalizationWeights)((old, pw) =>
      old / (1.0 + pw * alpha))

    def shrinkSCAD(w: Vec[Double], alpha: Double) = w.zipMap(penalizationWeights) { (old, pw) =>
      val pwa = pw * alpha
      val a = 3.7
      if (math.abs(old) <= 2 * pwa) math.signum(old) * math.max(0.0, (math.abs(old) - pwa)) //old * math.max(0.0, 1.0 - (pwa / math.abs(old)))
      else if (2 * pwa < math.abs(old) && math.abs(old) <= a * pwa) (old / (a - 2.0)) * (a - 1 - ((a * pwa) / math.abs(old)))
      else old
    }

    def shrink(w: Vec[Double], alpha1: Double, alpha2: Double) = penalty match {
      case L1 => shrinkL1(w, alpha1)
      case L2 => shrinkL2(w, alpha2)
      case SCAD => shrinkSCAD(w, alpha1)
      case ElasticNet => shrinkElasticNet(w, alpha1, alpha2)
    }

    def penaltyFunction(w: Vec[Double]) = penalty match {
      case L1 => lambda1 * (w.zipMap(penalizationWeights)((w, pw) => math.abs(w) * pw).sum)
      case L2 => lambda2 * (w.zipMap(penalizationWeights)((w, pw) => w * w * pw).sum)
      case ElasticNet => (w.zipMap(penalizationWeights)((w, pw) => lambda2 * w * w * pw + lambda1 * math.abs(w) * pw).sum)
      case SCAD => (w.zipMap(penalizationWeights) { (w1, pw) =>
        val w = math.abs(w1)
        val a = 3.7
        val pwa = pw * lambda1
        if (w <= pwa) pwa * w
        else if (pwa < w && w <= a * pwa) (a * pwa * w - w * w * 0.5) / (a - 1)
        else a * pwa
      }).sum
    }

    def gradientAt(w: Vec[Double], residualw: Vec[Double]): Vec[Double] = {
      BLASHelpers.dot(designT, residualw)
    }

    def step(stepSize: Double, w: Vec[Double], gradientw: Vec[Double]) = {
      shrink(
        w - (gradientw * stepSize),
        stepSize * lambda1,
        stepSize * lambda2
      )
    }

    def residual(w: Vec[Double]): Vec[Double] = (predict(w) - y)

    def predict(w: Vec[Double]): Vec[Double] = BLASHelpers.dot(design, w)

    def objective(w: Vec[Double]): Double =
      objectiveUnpenalized(w)._1 + penaltyFunction(w)

    def objectiveUnpenalized(w: Vec[Double]): (Double, Vec[Double]) = {
      val r: Vec[Double] = residual(w)
      ((r dot r) * 0.5, r)
    }

    def upperBound(x: Vec[Double], y: Vec[Double], a: Double, objectiveUnpenalizedy: Double, gradienty: Vec[Double]) = {
      objectiveUnpenalizedy + (gradienty dot (x - y)) + (1.0 / (2.0 * a)) * ((x - y) dot (x - y))
    }

    def stop(n1: Double, n2: Double, i: Int, w: Vec[Double]): Boolean = {
      ((n1 > n2 && (n1 - n2) / n2 < relativeThreshold) || (math.abs(n1 - n2) < absoluteThreshold) || i >= maxIter || w.contents.forall(_ === 0d))
    }

    def lineSearch(stepSize: Double, w: Vec[Double]): (Double, Vec[Double]) = {
      var l = stepSize

      val (objectiveUnpenalizedw, residualw) = objectiveUnpenalized(w)
      val penaltyw = penaltyFunction(w)
      val objectivePenalizedW = objectiveUnpenalizedw + penaltyw
      val gradientw = gradientAt(w, residualw)

      var z = step(l, w, gradientw)

      def lineSearchStop(z: Vec[Double]) = penalty match {
        case L1 => (objectiveUnpenalized(z)._1 > upperBound(z, w, l, objectiveUnpenalizedw, gradientw))
        case SCAD | L2 | ElasticNet => objective(z) > objectivePenalizedW
      }

      while (lineSearchStop(z)) {
        l = l * 0.5
        z = step(l, w, gradientw)
      }

      (l, z)
    }

    var coeff = first.getOrElse(vec.zeros(design.numCols))
    var yCoeff = coeff
    var t = 1.0
    var lastObj = objective(coeff)
    val firstObj = lastObj
    var stopV = false
    var i = 0
    var stepSize = 1.0

    var stream: List[Double] = lastObj :: Nil

    while (!stopV) {

      val (nstepSize, ncoeff) = lineSearch(stepSize, yCoeff)

      stepSize = nstepSize
      val tplus1 = (1 + math.sqrt(1 + 4 * t * t)) * 0.5
      yCoeff = ncoeff + (ncoeff - coeff) * ((t - 1.0) / tplus1)
      coeff = ncoeff
      yCoeff = ncoeff
      t = tplus1

      val newobj = objective(yCoeff)
      i += 1
      stopV = stop(lastObj, newobj, i, yCoeff)
      lastObj = newobj
      stream = lastObj :: stream
    }

    if (i == maxIter) {
      // println(s"WARNING FISTA REACHED MAX ITER $maxIter lambda1=" + lambda1 + " lambda2=" + lambda2 + " step=" + stepSize + " samples=" + design.numRows + " features=" + design.numCols + " lastObj=" + lastObj + " firstObj=" + firstObj + " iterations=" + (stream.take(10) ++ stream.takeRight(10)))
    }

    // println(i)

    val rsquared = {
      val predicted = predict(yCoeff)
      PenalizedRegressionWithCrossValidation.rSquared(predicted = predicted, y = y)
    }

    Some(PenalizedLeastSquaresResult(yCoeff, lambda1, lambda2, lastObj, i, rsquared, penalty))

  }

}