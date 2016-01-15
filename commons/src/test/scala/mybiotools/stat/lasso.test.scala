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

package mybiotools
package stat
import LinearRegression._

import org.scalatest.FunSpec
import org.scalatest.Matchers
import mybiotools.gwascommons.Individual
import org.saddle._
import org.saddle.io._
import org.apache.commons.math3.random.{ RandomGenerator, RandomDataGenerator, Well44497b }

class LassoSpec extends FunSpec with Matchers {

  // library(glmnet)
  // data(QuickStartExample)
  // cvfit = cv.glmnet(x,y,nfolds=100,standardize=F)
  // coef(cvfit,s="lambda.min")

  describe("lasso alone") {

    val frame = CsvParser.parse(CsvFile(getClass.getResource("/").getPath + "/glmnetexample.csv")).withColIndex(0).withRowIndex(0).mapValues(_.toDouble)
    val data2 = LinearRegression.createDataMatrixForLinearRegression(
      frame,
      2 to 21 map (i => "V" + i),
      "V22",
      DropSample
    )

    it("lambda=0") {

      val lassoresult = LASSO.fit(data2.design, data2.y, 0, vec.ones(21)).get

      val ridgeresult = Ridge.fit(data2.design, data2.y, 0, vec.ones(21)).get

      val ln = LinearRegression.linearRegression(data2, 0)

      val glmnet = Vector(
        0.110947733,
        1.378783189,
        0.023275142,
        0.763496730,
        0.060802214,
        -0.901598952,
        0.614425728,
        0.118277706,
        0.397670502,
        -0.031395521,
        0.128445909,
        0.247276619,
        -0.064775290,
        -0.045980087,
        -1.159522599,
        -0.138015549,
        -0.045678862,
        -0.048419881,
        0.052383392,
        -0.002729424,
        -1.144092052
      )

      ((ln.get.intercept +: ln.get.slopes) zip lassoresult.coefficients.toSeq).foreach {
        case (x, y) =>
          (math.abs(x - y) < 0.01) should equal(true)
      }

      (glmnet zip ridgeresult.coefficients.toSeq).foreach {
        case (x, y) =>
          (math.abs(x - y) < 0.01) should equal(true)
      }

      (glmnet zip lassoresult.coefficients.toSeq).foreach {
        case (x, y) =>
          (math.abs(x - y) < 0.01) should equal(true)
      }

    }

    it("lambda=0.5") {

      val lassoresult = LASSO.fit(data2.design, data2.y, .5, vec.ones(21)).get

      // this is glmnet lambda=0.5/200 because they use different weighting inside the objective function
      val glmnet = Vector(
        0.110418116,
        1.379439342,
        0.023714461,
        0.764764752,
        0.062580731,
        -0.903005714,
        0.615777699,
        0.120207005,
        0.398766538,
        -0.033049035,
        0.130978034,
        0.248744614,
        -0.066345896,
        -0.047107247,
        -1.160905703,
        -0.140931254,
        -0.047584249,
        -0.050790214,
        0.053901783,
        -0.003888988,
        -1.145513852
      )

      val penalized = Vector(
        0.111620533,
        1.377918731,
        0.022577490,
        0.761963299,
        0.058647387,
        -0.899923091,
        0.613034669,
        0.116095232,
        0.396443319,
        -0.029603638,
        0.125616865,
        0.245748790,
        -0.062984463,
        -0.044669540,
        -1.157973887,
        -0.134747133,
        -0.043526123,
        -0.045685510,
        0.050687485,
        -0.001390277,
        -1.142443919
      )

      (penalized zip lassoresult.coefficients.toSeq).foreach {
        case (x, y) =>
          if (!(math.abs(x - y) < 0.01)) {
            println("penalized: " + x + " vs my: " + y)
          }
          (math.abs(x - y) < 0.01) should equal(true)
      }

      (glmnet zip lassoresult.coefficients.toSeq).foreach {
        case (x, y) =>
          if (!(math.abs(x - y) < 0.01)) {
            println("glmnet: " + x + " vs my: " + y)
          }
          (math.abs(x - y) < 0.01) should equal(true)
      }

    }

    it("lambda=50 against penalized") {

      val lassoresult = LASSO.fit(data2.design, data2.y, 50.0, Vec(
        0d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d
      )).get

      val penalized = Vector(
        0.25867058,
        1.03666762,
        0.00000000,
        0.26086084,
        0.00000000,
        -0.38564371,
        0.26790191,
        0.00000000,
        0.01054177,
        0.00000000,
        0.00000000,
        0.00000000,
        0.00000000,
        0.00000000,
        -0.86703516,
        0.00000000,
        0.00000000,
        0.00000000,
        0.00000000,
        0.00000000,
        -0.46707087
      )

      (penalized zip lassoresult.coefficients.toSeq).foreach {
        case (x, y) =>
          if (!(math.abs(x - y) < 0.01)) {
            println("penalized: " + x + " vs my: " + y)
          }
          (math.abs(x - y) < 0.02) should equal(true)
      }

      // (glmnet zip lassoresult.coefficients.toSeq).foreach {
      //   case (x, y) =>
      //     if (!(math.abs(x - y) < 0.01)) {
      //       println("glmnet: " + x + " vs my: " + y)
      //     }
      //     (math.abs(x - y) < 0.01) should equal(true)
      // }

    }

    // it("lambda=50 against penalized L2") {

    //   val result = Ridge.fit(data2.design, data2.y, 50.0, Vec(
    //     0d,
    //     1d,
    //     1d,
    //     1d,
    //     1d,
    //     1d,
    //     1d,
    //     1d,
    //     1d,
    //     1d,
    //     1d,
    //     1d,
    //     1d,
    //     1d,
    //     1d,
    //     1d,
    //     1d,
    //     1d,
    //     1d,
    //     1d,
    //     1d)).get

    //   val penalized = IndexedSeq(0.305767252,
    //     0.973779894,
    //     0.068934404,
    //     0.497185503,
    //     -0.043501453,
    //     -0.603505652,
    //     0.517660241,
    //     0.112092970,
    //     0.282919550,
    //     -0.058310925,
    //     0.010746792,
    //     0.173727036,
    //     -0.057283446,
    //     -0.006831348,
    //     -0.826476739,
    //     -0.030049603,
    //     0.013115346,
    //     0.017033835,
    //     0.061459813,
    //     0.014791350,
    //     -0.686263503)

    //   (penalized zip result.coefficients.toSeq).foreach {
    //     case (x, y) =>
    //       if (!(math.abs(x - y) < 0.02)) {
    //         println("penalized: " + x + " vs my: " + y)
    //       }
    //       (math.abs(x - y) < 0.02) should equal(true)
    //   }

    // }

    it("scad lambda=50 against ncvreg") {

      val lassoresult = PenalizedWithSCAD.fit(data2.design, data2.y, 10, Vec(
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d
      )).get

      val ncvreg = Seq(
        0.1308801426,
        1.3796539851,
        0.0000000000,
        0.7659849860,
        0.0072198822,
        -0.9020176288,
        0.6060924633,
        0.0447846110,
        0.3972901012,
        0.0000000000,
        0.0472768243,
        0.2569556151,
        0.0000000000,
        -0.0092788275,
        -1.1364997738,
        -0.0331382513,
        -0.0005243524,
        0.0000000000,
        0.0003476129,
        0.0000000000,
        -1.1584229366
      )

      // println(lassoresult.coefficients)

      (ncvreg zip lassoresult.coefficients.toSeq).foreach {
        case (x, y) =>
          if (!(math.abs(x - y) < 0.2)) {
            println("penalized: " + x + " vs my: " + y)
          }
          (math.abs(x - y) < 0.2) should equal(true)
      }

      // (glmnet zip lassoresult.coefficients.toSeq).foreach {
      //   case (x, y) =>
      //     if (!(math.abs(x - y) < 0.01)) {
      //       println("glmnet: " + x + " vs my: " + y)
      //     }
      //     (math.abs(x - y) < 0.01) should equal(true)
      // }

    }

    it("elastic net  against penalized") {

      val lassoresult = PenalizedWithElasticNet.fit(data2.design, data2.y, 50d -> 25d, Vec(
        0d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d,
        1d
      )).get

      val penalized = Seq(
        0.329945491,
        0.849655715,
        0.000000000,
        0.206803964,
        0.000000000,
        -0.322154692,
        0.261500178,
        0.000000000,
        0.008798273,
        0.000000000,
        0.000000000,
        0.000000000,
        0.000000000,
        0.000000000,
        -0.723725582,
        0.000000000,
        0.000000000,
        0.000000000,
        0.000000000,
        0.000000000,
        -0.363951587
      )

      // println(lassoresult.coefficients)

      (penalized zip lassoresult.coefficients.toSeq).foreach {
        case (x, y) =>
          if (!(math.abs(x - y) < 0.1)) {
            println("penalized: " + x + " vs my: " + y)
          }
          (math.abs(x - y) < 0.1) should equal(true)
      }

      // (glmnet zip lassoresult.coefficients.toSeq).foreach {
      //   case (x, y) =>
      //     if (!(math.abs(x - y) < 0.01)) {
      //       println("glmnet: " + x + " vs my: " + y)
      //     }
      //     (math.abs(x - y) < 0.01) should equal(true)
      // }

    }

    ignore("large (runtime measurement)") {
      val numSamples = 500
      val numFeatures = 10000
      val trueFeatures = 400
      val rnd = new Well44497b(1)
      val rndd = new RandomDataGenerator(rnd)
      def simulateRandomColumn(sd: Double): Vec[Double] = Vec(1 to numSamples map (i => rndd.nextGaussian(0.0, sd)): _*)
      val design = Mat(1 to numFeatures map (i => simulateRandomColumn(1.0)): _*)
      val trueCoeff = Vec((0 until (numFeatures - (trueFeatures).toInt) map { i =>
        0.0
      }) ++ (0 until (trueFeatures).toInt map (i => rnd.nextGaussian)): _*)

      val outcome: Vec[Double] = (design dot trueCoeff).col(0) + simulateRandomColumn(2.0) * 3

      val t1 = System.nanoTime
      1 to 200 foreach { i =>

        val l = Ridge.fit(
          design = design,
          y = outcome,
          lambda = 50.0,
          penalizationWeights = vec.ones(numFeatures)
        )

      }
      println((System.nanoTime - t1) / 1E9)
      val t2 = System.nanoTime
      (1 to 200 par) foreach { i =>

        val l = Ridge.fit(
          design = design,
          y = outcome,
          lambda = 50.0,
          penalizationWeights = vec.ones(numFeatures)
        )

      }
      println((System.nanoTime - t2) / 1E9)

    }
  }
  describe("cross validation vs glmnet") {
    // ignore("leave one out") {
    //   val frame = CsvParser.parse(CsvFile(getClass.getResource("/").getPath + "/glmnetexample.csv")).withColIndex(0).withRowIndex(0).mapValues(_.toDouble)
    //   val (lasso, _) = PenalizedRegressionWithCrossValidation.crossValidation(frame,
    //     2 to 21 map (i => "V" + i),
    //     "V22",
    //     DropSample,
    //     LeaveOneOut,
    //     LessComplexModel,
    //     threads = 8,
    //     implementation = LASSO,
    //     maxLambda = 500.0).get

    //   val glmnet = IndexedSeq(0.1477086,
    //     1.3413552,
    //     0.0,
    //     0.7003682,
    //     0.0,
    //     -0.8315680,
    //     0.5507735,
    //     0.0294112,
    //     0.3446125,
    //     0.0,
    //     0.0,
    //     0.1733043,
    //     0.0,
    //     0.0,
    //     -1.0840644,
    //     0.0,
    //     0.0,
    //     0.0,
    //     0.0,
    //     0.0,
    //     -1.0441256)

    //   (glmnet zip lasso.coefficients.toSeq).drop(1).foreach {
    //     case (x, y) =>
    //       // if (!(math.abs(x - y) < 0.1)) {
    //       println("glmnet: " + x + " vs my: " + y)
    //       // }
    //       (math.abs(x - y) < 0.1) should equal(true)
    //   }
    // }

    ignore("4 fold") {

      val optimizer = PenalizedRegressionWithCrossValidation.Search1D(
        FindBounds,
        MinimumPredictionError,
        BrentGridAndCMAES(CMAES(50), BrentGrid(1, 20))
      )

      val frame = CsvParser.parse(CsvFile(getClass.getResource("/").getPath + "/glmnetexample.csv")).withColIndex(0).withRowIndex(0).mapValues(_.toDouble)
      val cv = PenalizedRegressionWithCrossValidation.crossValidation(
        frame,
        2 to 21 map (i => "V" + i),
        "V22",
        DropSample,
        LeaveOneOut,
        optimizer,
        threads = 8,
        implementation = LASSO
      )

      val lasso = cv.get._1.fit

      val glmnet = IndexedSeq(
        0.1482955,
        1.3381070,
        0.0,
        0.6936938,
        0.0,
        -0.8249778,
        0.5448439,
        0.0230429,
        0.3398575,
        0.0,
        0.0,
        0.1648445,
        0.0,
        0.0,
        -1.0801745,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        -1.0338882
      )

      // println((glmnet zip lasso.coefficients.toSeq))

      (glmnet zip lasso.coefficients.toSeq).drop(1).foreach {
        case (x, y) =>
          if (!(math.abs(x - y) < 0.1)) {
            println("glmnet: " + x + " vs my: " + y)
          }
          (math.abs(x - y) < 0.1) should equal(true)
      }
    }
  }

  describe("simulated binary data") {
    val numSamples = 500
    val numFeatures = 100
    val trueFeatures = 20
    val rnd = new Well44497b(1)
    val rndd = new RandomDataGenerator(rnd)
    def simulateRandomColumn(sd: Double): Vec[Double] = Vec(1 to numSamples map (i => rndd.nextGaussian(0.0, sd)): _*)
    def simulateRandomBinaryColumn: Vec[Double] = Vec(1 to numSamples map (i => rndd.nextBinomial(1, 0.5).toDouble): _*)
    val design = PenalizedRegressionWithCrossValidation.standardize(Mat(1 to numFeatures map (i => simulateRandomBinaryColumn): _*))
    val design2 = Mat(1 to numFeatures map (i => simulateRandomBinaryColumn): _*)
    val trueCoeff = Vec((0 until (numFeatures - (trueFeatures).toInt) map { i =>
      0.0
    }) ++ (0 until (trueFeatures).toInt map (i => rnd.nextGaussian * 10d)): _*)

    val outcome: Vec[Double] = (design dot trueCoeff).col(0) + simulateRandomColumn(20.0)
    val outcome2: Vec[Double] = (design2 dot trueCoeff).col(0) + simulateRandomColumn(20.0)

    val trueR2 = {
      val p = (design mult trueCoeff).col(0)
      val sse = (outcome - p) dot (outcome - p)
      val sst = (outcome - outcome.mean) dot (outcome - outcome.mean)
      1.0 - sse / sst
    }

    // {
    //   import org.saddle.io.CsvImplicits._
    //   val f1 = TempFile.createTempFile(".design.txt")
    //   Frame(design).writeCsvFile(f1.getAbsolutePath)
    //   val f2 = TempFile.createTempFile(".design2.txt")
    //   Frame(design2).writeCsvFile(f2.getAbsolutePath)
    //   val f3 = TempFile.createTempFile(".trueCoeff.txt")
    //   Series(trueCoeff).writeCsvFile(f3.getAbsolutePath)
    //   val f4 = TempFile.createTempFile(".outcome.txt")
    //   Series(outcome).writeCsvFile(f4.getAbsolutePath)
    //   val f5 = TempFile.createTempFile(".outcome2.txt")
    //   Series(outcome2).writeCsvFile(f5.getAbsolutePath)
    //   println(f1.getAbsolutePath)
    //   println(f2.getAbsolutePath)
    //   println(f3.getAbsolutePath)
    //   println(f4.getAbsolutePath)
    //   println(f5.getAbsolutePath)
    // }

    // println(trueCoeff.toSeq)
    println("TRUER2: " + trueR2)

    ignore("lasso alone") {
      // val r = PenalizedWithSCAD.fit(design, outcome, 50, vec.ones(numFeatures), None)

      val optimizer = PenalizedRegressionWithCrossValidation.Search1D(
        FindBounds,
        MinimumPredictionError,
        BrentGridAndCMAES(CMAES(50), BrentGrid(1, 20))
      )

      val optimizer2d = PenalizedRegressionWithCrossValidation.Search2DCMAES(150, -20, 20, -20, 20)

      val r = PenalizedRegressionWithCrossValidation.crossValidation(
        design = (design),
        y = outcome,
        penalizationWeights = vec.ones(numFeatures),
        crossValidationMode = KFold(3, 123, 1),
        // crossValidationModeOuter2 = KFoldStratified(5, 1233421, 100),
        // crossValidationModeInner = KFoldStratified(4, 12344543, 100),
        // searchMode = BrentGrid(1, 20), //CMAES,
        optimizer = optimizer,
        warmStart = false,
        generateCVSamplesOnThFly = false,
        threads = 4,
        implementation = LASSO
      )
      println(r)
      val coeff = r.get.fit.coefficients
      println("test r2 true " + {
        val adjustedCoeff: Vec[Double] = Vec[Double]((0 until (numFeatures - (trueFeatures).toInt) map { i =>
          0.0
        }): _*) concat coeff.splitAt(numFeatures - trueFeatures)._2

        val p = (design2 mult adjustedCoeff).col(0)
        val sse = (outcome2 - p) dot (outcome2 - p)
        val sst = (outcome2 - outcome2.mean) dot (outcome2 - outcome2.mean)
        1.0 - sse / sst
      })

      println("test r2 real " + {
        val p = (design2 mult coeff).col(0)
        val sse = (outcome2 - p) dot (outcome2 - p)
        val sst = (outcome2 - outcome2.mean) dot (outcome2 - outcome2.mean)
        1.0 - sse / sst
      })

      // println(Mat(r.get.fit.coefficients, trueCoeff).stringify(trueCoeff.length, 2))
      // println(r.get.rSquared)
    }
  }

  describe("simulated data") {
    val numSamples = 60
    val numFeatures = 200
    val trueFeatures = 20
    val rnd = new Well44497b(1)
    val rndd = new RandomDataGenerator(rnd)
    def simulateRandomColumn(sd: Double): Vec[Double] = Vec(1 to numSamples map (i => rndd.nextGaussian(0.0, sd)): _*)
    val design = Mat(1 to numFeatures map (i => simulateRandomColumn(1.0)): _*)
    val design2 = Mat(1 to numFeatures map (i => simulateRandomColumn(1.0)): _*)
    val trueCoeff = Vec((0 until (numFeatures - (trueFeatures).toInt) map { i =>
      0.0
    }) ++ (0 until (trueFeatures).toInt map (i => rnd.nextGaussian * 10d)): _*)

    val outcome: Vec[Double] = (design dot trueCoeff).col(0) + simulateRandomColumn(80.0)
    val outcome2: Vec[Double] = (design2 dot trueCoeff).col(0) + simulateRandomColumn(80.0)

    val trueR2 = {
      val p = (design mult trueCoeff).col(0)
      val sse = (outcome - p) dot (outcome - p)
      val sst = (outcome - outcome.mean) dot (outcome - outcome.mean)
      1.0 - sse / sst
    }

    {
      import org.saddle.io.CsvImplicits._
      val f1 = TempFile.createTempFile(".design.txt")
      Frame(design).writeCsvFile(f1.getAbsolutePath)
      val f2 = TempFile.createTempFile(".design2.txt")
      Frame(design2).writeCsvFile(f2.getAbsolutePath)
      val f3 = TempFile.createTempFile(".trueCoeff.txt")
      Series(trueCoeff).writeCsvFile(f3.getAbsolutePath)
      val f4 = TempFile.createTempFile(".outcome.txt")
      Series(outcome).writeCsvFile(f4.getAbsolutePath)
      val f5 = TempFile.createTempFile(".outcome2.txt")
      Series(outcome2).writeCsvFile(f5.getAbsolutePath)
      println(f1.getAbsolutePath)
      println(f2.getAbsolutePath)
      println(f3.getAbsolutePath)
      println(f4.getAbsolutePath)
      println(f5.getAbsolutePath)
    }

    // println(trueCoeff.toSeq)
    println("TRUER2: " + trueR2)

    it("lasso alone") {
      println(LASSO.fit(design, outcome, 0.14, vec.ones(numFeatures), None))

      // val optimizer = PenalizedRegressionWithCrossValidation.Search1D(FindBounds,
      //   MinimumPredictionError,
      //   BrentGridAndCMAES(CMAES(150), BrentGrid(1, 20))
      // )

      // val optimizer2d = PenalizedRegressionWithCrossValidation.Search2DCMAES(150, -20, 20, -20, 20)

      // val r = PenalizedRegressionWithCrossValidation.crossValidation(
      //   design = design,
      //   y = outcome,
      //   penalizationWeights = vec.ones(numFeatures),
      //   crossValidationMode = KFold(3, 1253, 1),
      //   // crossValidationModeOuter2 = KFoldStratified(5, 1233421, 100),
      //   // crossValidationModeInner = KFoldStratified(4, 12344543, 100),
      //   // searchMode = BrentGrid(1, 20), //CMAES,
      //   optimizer = optimizer,
      //   warmStart = false,
      //   generateCVSamplesOnThFly = false,
      //   threads = 1,
      //   implementation = LASSO)
      // println(r)
      // val coeff = r.get.fit.coefficients
      // println("test r2 true " + {
      //   val adjustedCoeff: Vec[Double] = Vec[Double]((0 until (numFeatures - (trueFeatures).toInt) map { i =>
      //     0.0
      //   }): _*) concat coeff.splitAt(numFeatures - trueFeatures)._2

      //   val p = (design2 mult adjustedCoeff).col(0)
      //   val sse = (outcome2 - p) dot (outcome2 - p)
      //   val sst = (outcome2 - outcome2.mean) dot (outcome2 - outcome2.mean)
      //   1.0 - sse / sst
      // })

      // println("test r2 real " + {
      //   val p = (design2 mult coeff).col(0)
      //   val sse = (outcome2 - p) dot (outcome2 - p)
      //   val sst = (outcome2 - outcome2.mean) dot (outcome2 - outcome2.mean)
      //   1.0 - sse / sst
      // })

      // println(Mat(r.get.fit.coefficients, trueCoeff).stringify(trueCoeff.length, 2))
      // println(r.get.rSquared)
    }

    ignore("L2") {
      val optimizer = PenalizedRegressionWithCrossValidation.Search1D(
        FindBounds,
        MinimumPredictionError,
        BrentGridAndCMAES(CMAES(50), BrentGrid(1, 20))
      )

      val optimizer2d = PenalizedRegressionWithCrossValidation.Search2DCMAES(150, -20, 20, -20, 20)

      val estimates = PenalizedRegressionWithCrossValidation.nestedCrossValidation(
        design = design,
        y = outcome,
        penalizationWeights = vec.ones(numFeatures),
        crossValidationModeOuter = KFold(5, 123, 1),
        // crossValidationModeOuter2 = KFold(6, 1233421, 1),
        crossValidationModeInner = KFold(5, 12344543, 1),
        optimizer = optimizer,
        warmStart = true,
        generateCVSamplesOnThFly = false,
        adaptive = None,
        threads = 1,
        implementation = LASSO
      )
      // implementation = BOLASSO(Bootstrap(5, 123), LASSO, 1))

      // println(estimates.finalFit)
      // println(estimates)

      // println("L2 inner test r2 mean/sd: " + SummaryStat(estimates.map(_._2)).mean + " / " + SummaryStat(estimates.map(_._2)).stddev)
      // println("L2 inner test r2 list: " + estimates.coefficients.map(_._2))
      // println("L2 outer test r2: " + outerR2)
      // println("L2 final penalized r2: " + outerLassoResult.get.rSquared)

      println(Mat(trueCoeff +: estimates.finalFit.get.coefficients +: estimates.coefficients.map(_._1.coefficients): _*).stringify(trueCoeff.length, estimates.coefficients.length + 2))
      println("unbiased " + estimates.overallR2)
      println("intersect training " + estimates.finalFit.get.rSquared)
      println("intersect validation " + estimates.finalFitSingleCVR2)
      println("intersect unbiased " + estimates.correctedOverallR2)

      val coeff = estimates.finalFit.get.coefficients
      println("test r2 true " + {
        val adjustedCoeff: Vec[Double] = Vec[Double]((0 until (numFeatures - (trueFeatures).toInt) map { i =>
          0.0
        }): _*) concat coeff.splitAt(numFeatures - trueFeatures)._2

        val p = (design2 mult adjustedCoeff).col(0)
        val sse = (outcome2 - p) dot (outcome2 - p)
        val sst = (outcome2 - outcome2.mean) dot (outcome2 - outcome2.mean)
        1.0 - sse / sst
      })

      println("test r2 real " + {
        val p = (design2 mult coeff).col(0)
        val sse = (outcome2 - p) dot (outcome2 - p)
        val sst = (outcome2 - outcome2.mean) dot (outcome2 - outcome2.mean)
        1.0 - sse / sst
      })

      // println(Mat(outerFeatures, trueCoeff).stringify(trueCoeff.length, 2))

      // println("L2 final lasso r2: " + outerLassoResult.get.rSquared)

      // println("lm r2: " + LinearRegression.linearRegression(design, outcome, 0.0, vec.ones(design.numCols)).get.r2)

      // (SummaryStat(outerFolds.map(_._2)).mean + 2 * SummaryStat(outerFolds.map(_._2)).stddev) should be > trueR2
      // (SummaryStat(outerFolds.map(_._2)).mean - 2 * SummaryStat(outerFolds.map(_._2)).stddev) should be < trueR2

    }

    // it("L1") {
    //   val NestedCrossValidation(outerFolds, outerR2, outerFeatures, outerLassoResult) = PenalizedRegressionWithCrossValidation.nestedCrossValidation(
    //     design = design,
    //     y = outcome,
    //     penalizationWeights = vec.ones(numFeatures),
    //     crossValidationModeOuter = KFoldStratified(10, 123, 100),
    //     crossValidationModeOuter2 = KFoldStratified(10, 3243, 100),
    //     crossValidationModeInner = KFoldStratified(5, 12344543, 100),
    //     searchMode = BrentGrid(1, 3),
    //     boundsOnLambda = FindBounds,
    //     useLambda = MinimumPredictionError,
    //     warmStart = true,
    //     generateCVSamplesOnThFly = false,
    //     threads = 4,
    //     implementation = LASSO)
    //   println("L1 inner test r2 mean/sd: " + SummaryStat(outerFolds.map(_._2)).mean + " / " + SummaryStat(outerFolds.map(_._2)).stddev)
    //   println("L1 inner test r2 list: " + outerFolds.map(_._2))
    //   println("L1 outer test r2: " + outerR2)
    //   // println("L1 final penalized r2: " + outerLassoResult.get.rSquared)

    //   println(Mat(outerLassoResult.get.coefficients, trueCoeff).stringify(trueCoeff.length, 2))
    //   println("L1 final lasso r2: " + outerLassoResult.get.rSquared)

    //   // println("lm r2: " + LinearRegression.linearRegression(design, outcome, 0.0, vec.ones(design.numCols)).get.r2)

    //   SummaryStat(outerFolds.map(_._2)).mean + 2 * SummaryStat(outerFolds.map(_._2)).stddev should be > trueR2
    //   SummaryStat(outerFolds.map(_._2)).mean - 2 * SummaryStat(outerFolds.map(_._2)).stddev should be < trueR2

    // }

  }

  describe("BLAS helper") {
    it("dot") {
      val m = Mat(Vec(1d, 2d, 3d, 4d, 5d), Vec(6d, 7d, 8d, 9d, 11d))
      val v = Vec(12d, 13d)

      BLASHelpers.dot(m, v) should equal((m dot v).col(0))
    }
    ignore("measure") {
      val numSamples = 500
      val numFeatures = 20000

      val rnd = new Well44497b(1)
      val rndd = new RandomDataGenerator(rnd)
      def simulateRandomColumn(l: Int, sd: Double): Vec[Double] = Vec(1 to l map (i => rndd.nextGaussian(0.0, sd)): _*)
      val design = Mat(1 to numFeatures map (i => simulateRandomColumn(numSamples, 1.0)): _*)
      val v = simulateRandomColumn(numFeatures, 1.0)

      val blas = 1 to 1000 map { i =>
        val t1 = System.nanoTime
        BLASHelpers.dot(design, v)
        System.nanoTime - t1
      }
      val ejml = 1 to 1000 map { i =>
        val t1 = System.nanoTime
        design dot v
        System.nanoTime - t1
      }

      println(SummaryStat(blas.drop(700)) + " " + SummaryStat(ejml.drop(700)))

    }
  }

}