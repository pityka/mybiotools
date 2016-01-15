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

class LinearRegressionSpec extends FunSpec with Matchers {

  describe("Blockwise inversion") {
    import org.ejml.simple.SimpleMatrix

    it("simple 4x4") {
      val a = new SimpleMatrix(Array(Array(2.0, 3.0), Array(1.0, 0.0)))
      val b = new SimpleMatrix(Array(Array(1.0, 5.0), Array(3.0, 1.0)))
      val c = new SimpleMatrix(Array(Array(0.0, 2.0), Array(0.0, 2.0)))
      val d = new SimpleMatrix(Array(Array(-3.0, 2.0), Array(3.0, 1.0)))

      val ainv = a.invert

      val full = new SimpleMatrix(Array(
        Array(2.0, 3.0, 1.0, 5.0),
        Array(1.0, 0.0, 3.0, 1.0),
        Array(0.0, 2.0, -3.0, 2.0),
        Array(0.0, 2.0, 3.0, 1.0)
      ))

      invertBlockWise(a, b, c, d, ainv).getMatrix.getData.toVector.map(math.round) should equal(full.invert.getMatrix.getData.toVector.map(math.round))
    }
  }

  describe("R example") {
    val ctrl = List(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
    val trt = List(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)

    val data = ctrl.zipWithIndex.map(x => Individual(x._2.toString) -> Map("v" -> x._1, "g" -> 0.0)).toMap ++
      trt.zipWithIndex.map(x => Individual((x._2 + 10).toString) -> Map("v" -> x._1, "g" -> 1.0)).toMap

    val result = linearRegression(data, List("g"), "v", MeanImpute).asInstanceOf[LinearRegressionResultMaps[Individual]]

    it("should have the right estimates") {
      math.abs(result.covariates("g")._1.slope - (-0.3710)) should be < 0.01
      math.abs(result.covariates("g")._1.sd - (0.3114)) should be < 0.01
      math.abs(result.intercept._1.slope - 5.032) should be < 0.01
      math.abs(result.intercept._1.sd - (0.2202)) should be < 0.1
    }
    it("should have the right r2") {
      math.abs(result.r2 - 0.07308) should be < 0.01
    }
    it("should have the right loglikelihood") {
      math.abs(result.logLikelihood.L - (-20.08824)) should be < 0.001
      math.abs(result.logLikelihood.df - (2)) should be < 0.001 // R tells 3: 2 for the betas and 1 for the sigma
    }
    it("df") {
      math.abs(result.df - (2)) should be < 0.001
    }
    it("should have the right residual") {
      result.residualsWithSampleNames.sortBy(_._1.FID.value.toInt).map(x => (x._2 * 1000.0).round / 1000.0) should equal(Vector(-0.862, 0.548, 0.148, 1.078, -0.532, -0.422, 0.138, -0.502, 0.298, 0.108, 0.149, -0.491, -0.251, -1.071, 1.209, -0.831, 1.369, 0.229, -0.341, 0.029))
    }
    // >  ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
    // >      trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
    // >      group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
    // >      weight <- c(ctl, trt)
    // >      lm.D9 <- lm(weight ~ group)
    // >      

    //    Call:
    // lm(formula = weight ~ group)

    // Residuals:
    //     Min      1Q  Median      3Q     Max 
    // -1.0710 -0.4938  0.0685  0.2462  1.3690 

    // Coefficients:
    //             Estimate Std. Error t value Pr(>|t|)    
    // (Intercept)   5.0320     0.2202  22.850 9.55e-15 ***
    // groupTrt     -0.3710     0.3114  -1.191    0.249    
    // ---
    // Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

    // Residual standard error: 0.6964 on 18 degrees of freedom
    // Multiple R-squared: 0.07308,  Adjusted R-squared: 0.02158 
    // F-statistic: 1.419 on 1 and 18 DF,  p-value: 0.249  
    //     > logLik(lm.D9)
    // 'log Lik.' -20.08824 (df=3)
  }

  describe("R example shrinkage") {
    val ctrl = List(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
    val trt = List(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)

    val cov2 = (1 to 20).toList.map(_.toDouble)

    val data = ctrl.zipWithIndex.map(x => Individual(x._2.toString) -> Map("v" -> x._1, "g" -> 0.0, "h" -> cov2(x._2))).toMap ++
      trt.zipWithIndex.map(x => Individual((x._2 + 10).toString) -> Map("v" -> x._1, "g" -> 1.0, "h" -> cov2(x._2 + 10))).toMap

    val result = linearRegression(data, List("h", "g"), "v", MeanImpute, 10).asInstanceOf[LinearRegressionResultMaps[String]]

    it("should have the right estimates") {
      math.abs(result.covariates("g")._1.slope - (-0.0710)) should be < 0.01
      math.abs(result.covariates("h")._1.slope - (-0.0158)) should be < 0.01
      math.abs(result.covariates("g")._1.sd - (0.06829)) should be < 0.01
      math.abs(result.covariates("h")._1.sd - (0.02680)) should be < 0.01
      math.abs(result.intercept._1.slope - 5.0479) should be < 0.01
      // math.abs(result.intercept._1.sd - (0.2202)) should be < 0.1
    }
    // it("should have the right r2") {
    //   math.abs(result.r2 - 0.07308) should be < 0.01
    // }
    // it("should have the right loglikelihood") {
    //   math.abs(result.logLikelihood.L - (-20.08824)) should be < 0.001
    //   math.abs(result.logLikelihood.df - (2)) should be < 0.001 // R tells 3: 2 for the betas and 1 for the sigma
    // }
    // it("df") {
    //   math.abs(result.df - (2)) should be < 0.001
    // }
    // >  ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
    // >      trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
    // >      group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
    // >      weight <- c(ctl, trt)
    // >      lm.D9 <- lm(weight ~ group)
    // >      

    //    Call:
    // lm(formula = weight ~ group)

    // Residuals:
    //     Min      1Q  Median      3Q     Max 
    // -1.0710 -0.4938  0.0685  0.2462  1.3690 

    // Coefficients:
    //             Estimate Std. Error t value Pr(>|t|)    
    // (Intercept)   5.0320     0.2202  22.850 9.55e-15 ***
    // groupTrt     -0.3710     0.3114  -1.191    0.249    
    // ---
    // Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

    // Residual standard error: 0.6964 on 18 degrees of freedom
    // Multiple R-squared: 0.07308,  Adjusted R-squared: 0.02158 
    // F-statistic: 1.419 on 1 and 18 DF,  p-value: 0.249  
    //     > logLik(lm.D9)
    // 'log Lik.' -20.08824 (df=3)
  }

  describe("Linear Regression basics") {

    val covariates = Array(
      Array[Double](1, 1, 23),
      Array[Double](1, 2, 65),
      Array[Double](1, 3, 12),
      Array[Double](1, 4, -12)
    )

    val y = Array[Double](1, 2, 3, 4)

    val result = linearRegression(covariates, y).get

    it(" dimension of returned arrays") {
      result.slopes.size should equal(covariates.head.size - 1)
      result.slopeSDs.size should equal(covariates.head.size - 1)
      // result.residuals.size should equal(covariates.size)
    }
    it(" should have similar results as R (within 1E-4)") {
      math.abs(result.intercept - 0.0) should be < 1E-4
      math.abs(result.interceptSD - 0.0) should be < 1E-4
      math.abs(result.slopes(0) - 1.0) should be < 1E-4
      math.abs(result.slopes(1) - 0.0) should be < 1E-4
      math.abs(result.slopeSDs(0) - 0.0) should be < 1E-4
      math.abs(result.slopeSDs(1) - 0.0) should be < 1E-4
      math.abs(result.r2 - 1.0) should be < 1E-4

      // math.abs(result.residuals(0) - 0.0) should be < 1E-4
      // math.abs(result.residuals(1) - 0.0) should be < 1E-4
      // math.abs(result.residuals(2) - 0.0) should be < 1E-4
      // math.abs(result.residuals(3) - 0.0) should be < 1E-4

      math.abs(result.sigma - 0.0) should be < 1E-4
    }
  }

  describe("Linear Regression basics from block") {

    val covariates1 = Array(
      Array[Double](1, 1),
      Array[Double](1, 2),
      Array[Double](1, 3),
      Array[Double](1, 4)
    )

    val y = Array[Double](1, 2, 3, 4)

    val fixcov = FixPart(Mat(covariates1).T, y, 1)

    val covariates2 = Array(
      Array[Double](23),
      Array[Double](65),
      Array[Double](12),
      Array[Double](-12)
    )

    val result = linearRegressionFixPart(fixcov, new org.ejml.simple.SimpleMatrix(covariates2).transpose, y, 0.0).get

    it(" dimension of returned arrays") {
      result.slopes.size should equal(2)
      result.slopeSDs.size should equal(2)
      // result.residuals.size should equal(covariates.size)
    }
    it(" should have similar results as R (within 1E-4)") {
      math.abs(result.intercept - 0.0) should be < 1E-4
      math.abs(result.interceptSD - 0.0) should be < 1E-4
      math.abs(result.slopes(0) - 1.0) should be < 1E-4
      math.abs(result.slopes(1) - 0.0) should be < 1E-4
      math.abs(result.slopeSDs(0) - 0.0) should be < 1E-4
      math.abs(result.slopeSDs(1) - 0.0) should be < 1E-4
      math.abs(result.r2 - 1.0) should be < 1E-4

      // math.abs(result.residuals(0) - 0.0) should be < 1E-4
      // math.abs(result.residuals(1) - 0.0) should be < 1E-4
      // math.abs(result.residuals(2) - 0.0) should be < 1E-4
      // math.abs(result.residuals(3) - 0.0) should be < 1E-4

      math.abs(result.sigma - 0.0) should be < 1E-4
    }
  }

  describe("Linear Regression basics from maps") {

    val data = Map(
      Individual("ind1") -> Map("y" -> 1, "x1" -> 1, "x3" -> -1, "x2" -> 23),
      Individual("ind2") -> Map("y" -> 2, "x1" -> 2, "x3" -> -1, "x2" -> 65),
      Individual("ind3") -> Map("y" -> 3, "x1" -> 3, "x3" -> -1, "x2" -> 12),
      Individual("ind4") -> Map("y" -> 4, "x1" -> 4, "x3" -> -1, "x2" -> -12)
    )

    val covariates = Array(
      Array[Double](1, 23),
      Array[Double](2, 65),
      Array[Double](3, 12),
      Array[Double](4, -12)
    )

    val y = Array[Double](1, 2, 3, 4)

    val result = linearRegression(data, List("x2", "x1"), "y", MeanImpute).asInstanceOf[LinearRegressionResultMaps[String]]

    it(" dimension of returned arrays") {
      result.covariates.size should equal(covariates.head.size)
      // result.residuals.size should equal(covariates.size)
    }
    it(" should have similar results as R (within 1E-4)") {
      math.abs(result.intercept._1.slope - 0.0) should be < 1E-4
      math.abs(result.intercept._1.sd - 0.0) should be < 1E-4
      math.abs(result.covariates("x1")._1.slope - 1.0) should be < 1E-4
      math.abs(result.covariates("x2")._1.slope - 0.0) should be < 1E-4
      math.abs(result.covariates("x1")._1.sd - 0.0) should be < 1E-4
      math.abs(result.covariates("x2")._1.sd - 0.0) should be < 1E-4
      math.abs(result.r2 - 1.0) should be < 1E-4

      // math.abs(result.residuals(0) - 0.0) should be < 1E-4
      // math.abs(result.residuals(1) - 0.0) should be < 1E-4
      // math.abs(result.residuals(2) - 0.0) should be < 1E-4
      // math.abs(result.residuals(3) - 0.0) should be < 1E-4

      math.abs(result.sigma - 0.0) should be < 1E-4
    }
  }

  describe("Linear Regression basics from maps only intercept") {

    val data = Map(
      Individual("ind1") -> Map("y" -> 1),
      Individual("ind2") -> Map("y" -> 2),
      Individual("ind3") -> Map("y" -> 3),
      Individual("ind4") -> Map("y" -> 4)
    )

    val result = linearRegression(data, List(), "y", MeanImpute).asInstanceOf[LinearRegressionResultMaps[String]]

    it(" dimension of returned arrays") {
      result.covariates.size should equal(0)
      // result.residuals.size should equal(covariates.size)
    }
    it(" should have similar results as R (within 1E-4)") {
      math.abs(result.intercept._1.slope - 2.5) should be < 1E-4
      math.abs(result.intercept._1.sd - 0.6454) should be < 1E-4
      math.abs(result.r2 - 0.0) should be < 1E-4

      // math.abs(result.residuals(0) - 0.0) should be < 1E-4
      // math.abs(result.residuals(1) - 0.0) should be < 1E-4
      // math.abs(result.residuals(2) - 0.0) should be < 1E-4
      // math.abs(result.residuals(3) - 0.0) should be < 1E-4

      math.abs(result.sigma - 1.2909) should be < 1E-4
    }
  }

  describe("Linear Regression basics from maps - no associations") {

    val data = Map(
      Individual("ind1") -> Map("y" -> 1, "x2" -> 23),
      Individual("ind2") -> Map("y" -> 2, "x2" -> 65),
      Individual("ind3") -> Map("y" -> 3, "x2" -> 12),
      Individual("ind4") -> Map("y" -> 4, "x2" -> -12)
    )

    val covariates = Array(
      Array[Double](23),
      Array[Double](65),
      Array[Double](12),
      Array[Double](-12)
    )

    val y = Array[Double](1, 2, 3, 4)

    val result = linearRegression(data, List("x2"), "y", MeanImpute).asInstanceOf[LinearRegressionResultMaps[String]]

    it(" dimension of returned arrays") {
      result.covariates.size should equal(covariates.head.size)
      // result.residuals.size should equal(covariates.size)
    }
    it(" should have similar results as R (within 1E-4)") {
      math.abs(result.intercept._1.slope - 3.05956) should be < 1E-4
      math.abs(result.intercept._1.sd - 0.77900) should be < 1E-4
      math.abs(result.covariates("x2")._1.slope + 0.02543) should be < 1E-4
      math.abs(result.covariates("x2")._1.sd - 0.02194) should be < 1E-4
      math.abs(result.r2 - 0.4019) should be < 1E-4

      // math.abs(result.residuals(0) + 1.4745654) should be < 1E-4
      // math.abs(result.residuals(1) - 0.5936896) should be < 1E-4
      // math.abs(result.residuals(2) - 0.2456536) should be < 1E-4
      // math.abs(result.residuals(3) - 0.6352222) should be < 1E-4

      math.abs(result.sigma - 1.223) should be < 5E-4
    }
    it("should produce a p-value similar to R") {
      math.abs(result.covariates("x2")._2.pValue - 0.3661) should be < 1E-4
      math.abs(result.intercept._2.pValue - 0.0591) should be < 1E-4
    }
  }

  describe("Linear Regression basics from maps meanimpute") {

    val data = Map(
      Individual("ind1") -> Map("y" -> 1, "x1" -> 1, "x2" -> 23, "x3" -> 123),
      Individual("ind2") -> Map("y" -> 2, "x1" -> 2, "x2" -> 65),
      Individual("ind3") -> Map("y" -> 3, "x1" -> 3, "x2" -> 12),
      Individual("ind4") -> Map("y" -> 4, "x1" -> 4, "x3" -> -1, "x2" -> -12)
    )

    val covariates = Array(
      Array[Double](1, 23),
      Array[Double](2, 65),
      Array[Double](3, 12),
      Array[Double](4, -12)
    )

    val y = Array[Double](1, 2, 3, 4)

    val result = linearRegression(data, List("x1", "x2", "x3"), "y", MeanImpute).asInstanceOf[LinearRegressionResultMaps[String]]

    it(" dimension of returned arrays") {
      result.covariates.size should equal(3)
      // result.residuals.size should equal(covariates.size)
    }
    it(" should have similar results as R (within 1E-4)") {
      math.abs(result.intercept._1.slope - 0.0) should be < 1E-4
      math.abs(result.intercept._1.sd) should equal(Double.PositiveInfinity)
      math.abs(result.covariates("x1")._1.slope - 1.0) should be < 1E-4
      math.abs(result.covariates("x2")._1.slope - 0.0) should be < 1E-4
      math.abs(result.covariates("x3")._1.slope - 0.0) should be < 1E-4
      math.abs(result.covariates("x1")._1.sd) should equal(Double.PositiveInfinity)
      math.abs(result.covariates("x2")._1.sd) should equal(Double.PositiveInfinity)
      math.abs(result.covariates("x3")._1.sd) should equal(Double.PositiveInfinity)
      math.abs(result.r2 - 1.0) should be < 1E-4

      // math.abs(result.residuals(0) - 0.0) should be < 1E-4
      // math.abs(result.residuals(1) - 0.0) should be < 1E-4
      // math.abs(result.residuals(2) - 0.0) should be < 1E-4
      // math.abs(result.residuals(3) - 0.0) should be < 1E-4

      math.abs(result.sigma) should equal(Double.PositiveInfinity)
    }
  }

  describe("Linear Regression basics from maps dropsample") {

    val data = Map(
      Individual("ind1") -> Map("y" -> 1, "x1" -> 1, "x2" -> 23),
      Individual("ind2") -> Map("y" -> 2, "x1" -> 2),
      Individual("ind3") -> Map("y" -> 3, "x1" -> 3, "x2" -> 12),
      Individual("ind4") -> Map("y" -> 4, "x1" -> 4, "x3" -> -12)
    )

    val covariates = Array(
      Array[Double](1, 23),
      Array[Double](2, 65),
      Array[Double](3, 12),
      Array[Double](4, -12)
    )

    val y = Array[Double](1, 2, 3, 4)

    ignore(" dimension of returned arrays") {
      evaluating {
        linearRegression(data, List("x1", "x2"), "y", DropSample)
      } should produce[org.apache.commons.math3.exception.MathIllegalArgumentException]
    }
  }

  describe("mean impute") {
    it("should leave complete data as it is") {
      val x = Vector(Some(1.0), Some(2.0), Some(3.0))
      meanImpute(x) should equal(x.map(_.get))
    }
    it("empty list") {
      val x = Vector()
      meanImpute(x) should equal(Vector())
    }
    it("list of Nones produces list of NaNs") {
      val x = Vector(None, None, None)
      meanImpute(x).toString should equal(Vector(Double.NaN, Double.NaN, Double.NaN).toString)
    }
    it("1 missing") {
      val x = Vector(Some(1.0), None, Some(3.0))
      meanImpute(x) should equal(Vector(1.0, 2.0, 3.0))
    }
    it("2 missing") {
      val x = Vector(Some(1.0), None, None)
      meanImpute(x) should equal(Vector(1.0, 1.0, 1.0))
    }
    it("1 missing b") {
      val x = Vector(Some(1.0), None, Some(5.0))
      meanImpute(x) should equal(Vector(1.0, 3.0, 5.0))
    }
  }

  // describe("compare autosomes with plink") {
  //   import mybiotools.gwascommons.genotypedata._
  //   import mybiotools.gwascommons.associationresults._
  //   import mybiotools.gwascommons._

  //   val pedfile = getClass.getResource("/genotypedata/medium.ped").getPath
  //   val mapfile = getClass.getResource("/genotypedata/medium.map").getPath
  //   val phenofile = getClass.getResource("/genotypedata/medium.linear.pheno").getPath
  //   val plinkresult = getClass.getResource("/genotypedata/mediumlinear.assoc.linear").getPath

  //   val genotypes = DiscreteGenotypeData.fromPedMap(new java.io.File(pedfile), new java.io.File(mapfile))

  //   val phenotypes: Vector[(Individual, Double)] = scala.io.Source.fromFile(phenofile).getLines.map { line =>
  //     Individual(line.split(" ")(0), line.split(" ")(1)) -> line.split(" ")(2).toDouble
  //   }.toVector

  //   def close(d1: Double, d2: Double): Boolean = math.abs(d1 - d2) < 0.01

  //   it("should have agree with plink (24k SNPs, 388 individuals, random phenotype)") {

  //     val plinkresultAssocs = scala.io.Source.fromFile(plinkresult).getLines.drop(1).grouped(2).foreach { lines =>

  //       val addline = lines.head
  //       val genderline = lines(1)

  //       val splittedadd = mybiotools.fastSplitSetSeparator(addline, Set(' ', '\t'))
  //       val splittedgender = mybiotools.fastSplitSetSeparator(genderline, Set(' ', '\t'))

  //       if (splittedadd(11) != "NA" && splittedadd(9) != "inf" && splittedadd(0).toInt < 23) {

  //         val snpname = mybiotools.stringstore.StringStore(splittedadd(1))
  //         val nmiss = splittedadd(5).toInt
  //         val orADD = splittedadd(6).toDouble
  //         val seADD = splittedadd(7).toDouble

  //         val orSEX = splittedgender(6).toDouble
  //         val seSEX = splittedgender(7).toDouble

  //         val data: Map[Individual, Map[String, Option[Double]]] = phenotypes.map {
  //           case (ind, pheno) =>
  //             val dose = genotypes.getStateWithRespectToMinor(ind.FID, snpname).map(x => if (x != DiscreteGenotypes.Missing) Some(x.variantAlleleDosage) else None).flatten
  //             val gender = genotypes.getPatient(ind.FID).get.gender.map {
  //               _ match {
  //                 case Male => 1.0
  //                 case Female => 0.0
  //               }
  //             }

  //             ind -> (Map("SEX" -> gender, "ADD" -> dose, "PHENO" -> Some(pheno)))
  //         }.toMap

  //         val myresult = LinearRegression.linearRegression(data, List("ADD", "SEX"), "PHENO", DropSample, 0.0).asInstanceOf[LinearRegressionResultMaps[String]]

  //         val myORADD = myresult.covariates("ADD")._1.slope
  //         val mySDADD = myresult.covariates("ADD")._1.sd

  //         val myORSEX = myresult.covariates("SEX")._1.slope
  //         val mySDSEX = myresult.covariates("SEX")._1.sd

  //         myresult.numberOfSamples should equal(nmiss)

  //         if (!((close(myORADD, orADD) && close(myORSEX, orSEX) && close(mySDADD, seADD) && close(mySDSEX, seSEX)))) {
  //           println(close(myORADD, orADD))
  //           println(myORADD)
  //           println(orADD)
  //           println(close(myORSEX, orSEX))
  //           println(close(mySDADD, seADD))
  //           println(close(mySDSEX, seSEX))
  //           println(myresult)
  //           println(lines)
  //           true should equal(false)
  //         }

  //       }
  //     }
  //   }
  // }

  // describe("compare autosomes with plink recessive") {
  //   import mybiotools.gwascommons.genotypedata._
  //   import mybiotools.gwascommons.associationresults._
  //   import mybiotools.gwascommons._

  //   val pedfile = getClass.getResource("/genotypedata/medium.ped").getPath
  //   val mapfile = getClass.getResource("/genotypedata/medium.map").getPath
  //   val phenofile = getClass.getResource("/genotypedata/medium.linear.pheno").getPath
  //   val plinkresult = getClass.getResource("/genotypedata/mediumlinearrecessive.assoc.linear").getPath

  //   val genotypes = DiscreteGenotypeData.fromPedMap(new java.io.File(pedfile), new java.io.File(mapfile))

  //   val phenotypes: Vector[(Individual, Double)] = scala.io.Source.fromFile(phenofile).getLines.map { line =>
  //     Individual(line.split(" ")(0), line.split(" ")(1)) -> line.split(" ")(2).toDouble
  //   }.toVector

  //   def close(d1: Double, d2: Double): Boolean = math.abs(d1 - d2) < 0.01

  //   it("should have agree with plink (24k SNPs, 388 individuals, random phenotype)") {

  //     val plinkresultAssocs = scala.io.Source.fromFile(plinkresult).getLines.drop(1).grouped(2).foreach { lines =>

  //       val addline = lines.head
  //       val genderline = lines(1)

  //       val splittedadd = mybiotools.fastSplitSetSeparator(addline, Set(' ', '\t'))
  //       val splittedgender = mybiotools.fastSplitSetSeparator(genderline, Set(' ', '\t'))

  //       if (splittedadd(11) != "NA" && splittedadd(9) != "inf" && splittedadd(0).toInt < 23) {

  //         val snpname = mybiotools.stringstore.StringStore(splittedadd(1))
  //         val nmiss = splittedadd(5).toInt
  //         val orADD = splittedadd(6).toDouble
  //         val seADD = splittedadd(7).toDouble

  //         val orSEX = splittedgender(6).toDouble
  //         val seSEX = splittedgender(7).toDouble

  //         val data: Map[Individual, Map[String, Option[Double]]] = phenotypes.map {
  //           case (ind, pheno) =>
  //             val dose = genotypes.getStateWithRespectToMinor(ind.FID, snpname).map(x => if (x != DiscreteGenotypes.Missing) Some(x.variantAlleleDosage) else None).flatten.map { minor =>
  //               mybiotools.gwascommons.gwas.GWAS.recodeGenotype(minor, mybiotools.gwascommons.gwas.Recessive)
  //             }
  //             val gender = genotypes.getPatient(ind.FID).get.gender.map {
  //               _ match {
  //                 case Male => 1.0
  //                 case Female => 0.0
  //               }
  //             }

  //             ind -> (Map("SEX" -> gender, "REC" -> dose, "PHENO" -> Some(pheno)))
  //         }.toMap

  //         val myresult = LinearRegression.linearRegression(data, List("REC", "SEX"), "PHENO", DropSample, 0.0).asInstanceOf[LinearRegressionResultMaps[String]]

  //         val myORADD = myresult.covariates("REC")._1.slope
  //         val mySDADD = myresult.covariates("REC")._1.sd

  //         val myORSEX = myresult.covariates("SEX")._1.slope
  //         val mySDSEX = myresult.covariates("SEX")._1.sd

  //         myresult.numberOfSamples should equal(nmiss)

  //         if (!((close(myORADD, orADD) && close(myORSEX, orSEX) && close(mySDADD, seADD) && close(mySDSEX, seSEX)))) {
  //           println(close(myORADD, orADD))
  //           println(myORADD)
  //           println(orADD)
  //           println(close(myORSEX, orSEX))
  //           println(close(mySDADD, seADD))
  //           println(close(mySDSEX, seSEX))
  //           println(myresult)
  //           println(lines)
  //           true should equal(false)
  //         }

  //       }
  //     }
  //   }
  // }

}
