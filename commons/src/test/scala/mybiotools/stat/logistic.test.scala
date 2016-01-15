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
import LogisticRegression._
import mybiotools.gwascommons._

import org.scalatest.FunSpec
import org.scalatest.Matchers

import org.ejml.simple.SimpleMatrix
import org.ejml.data.DenseMatrix64F

class LogisticRegressionSpec extends FunSpec with Matchers {

  def close(d1: Double, d2: Double): Boolean = math.abs(d1 - d2) < 0.01

  describe("xtwx") {
    it("simple") {
      val x = new SimpleMatrix(5, 2, true, 0d, 1d, 2d, 3d, 4d, 5d, 6d, 7d, 8d, 9d, 10d)
      val wx = new SimpleMatrix(5, 2, true, 0d, 1d, 2d, 3d, 4d, 5d, 6d, 7d, 8d, 9d, 10d).getMatrix
      val w = Array(1d, 2d, 3d, 4d, 5d)
      val out = new DenseMatrix64F(2, 2)
      LogisticRegression.calculateXTwX2(x, w, out).toString should equal(x.transpose.mult(SimpleMatrix.diag(w: _*)).mult(x).toString)
      val out2 = new DenseMatrix64F(2, 2)
      LogisticRegression.calculateXTwX3(x, w, out2).toString should equal(x.transpose.mult(SimpleMatrix.diag(w: _*)).mult(x).toString)
      val out3 = new DenseMatrix64F(2, 2)
      LogisticRegression.calculateXTwX4(x, w, out3).toString should equal(x.transpose.mult(SimpleMatrix.diag(w: _*)).mult(x).toString)
      val out4 = new DenseMatrix64F(2, 2)
      LogisticRegression.calculateXTwX5(x, w, out4, wx).toString should equal(x.transpose.mult(SimpleMatrix.diag(w: _*)).mult(x).toString)
    }
    ignore("benchmark") {
      val samples = 1000
      val vars = 20
      val x = new SimpleMatrix(samples, vars, true, Array.fill[Double](samples * vars)(scala.util.Random.nextDouble): _*)
      val wx = new SimpleMatrix(samples, vars, true, Array.fill[Double](samples * vars)(scala.util.Random.nextDouble): _*).getMatrix
      val xt = x.transpose
      val w = Array.fill[Double](samples)(scala.util.Random.nextDouble)
      val out = new DenseMatrix64F(vars, vars)

      val r1 = LogisticRegression.calculateXTwX2(x, w, out)
      val r2 = LogisticRegression.calculateXTwX(xt, w, x)
      r1.toString should equal(r2.toString)

      0 to 20000 foreach { i =>
        val r1 = LogisticRegression.calculateXTwX2(x, w, out)
      }
      0 to 20000 foreach { i =>
        val r1 = LogisticRegression.calculateXTwX(xt, w, x)
      }
      0 to 20000 foreach { i =>
        val r1 = LogisticRegression.calculateXTwX3(x, w, out)
      }
      0 to 20000 foreach { i =>
        val r1 = LogisticRegression.calculateXTwX4(x, w, out)
      }
      0 to 20000 foreach { i =>
        val r1 = LogisticRegression.calculateXTwX5(x, w, out, wx)
      }

      var T1 = 0.0
      var T2 = 0.0
      var T3 = 0.0
      var T4 = 0.0
      var T5 = 0.0

      0 to 10000 foreach { i =>
        val t1 = System.nanoTime
        val r1 = LogisticRegression.calculateXTwX2(x, w, out)
        T1 += (System.nanoTime - t1) / 1E9 / 10000
      }
      0 to 10000 foreach { i =>
        val t1 = System.nanoTime
        val r1 = LogisticRegression.calculateXTwX(xt, w, x)
        T2 += (System.nanoTime - t1) / 1E9 / 10000
      }
      0 to 10000 foreach { i =>
        val t1 = System.nanoTime
        val r1 = LogisticRegression.calculateXTwX3(x, w, out)
        T3 += (System.nanoTime - t1) / 1E9 / 10000
      }
      0 to 10000 foreach { i =>
        val t1 = System.nanoTime
        val r1 = LogisticRegression.calculateXTwX4(x, w, out)
        T4 += (System.nanoTime - t1) / 1E9 / 10000
      }
      0 to 10000 foreach { i =>
        val t1 = System.nanoTime
        val r1 = LogisticRegression.calculateXTwX5(x, w, out, wx)
        T5 += (System.nanoTime - t1) / 1E9 / 10000
      }
      println(T1)
      println(T2)
      println(T3)
      println(T4)
      println(T5)
    }
  }

  describe("R example") {
    val case1 = List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    val spontaneous = List(2, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 2, 1, 0, 1, 1, 1, 0, 0, 1, 1)
    val induced = List(1, 1, 2, 2, 1, 2, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 2, 0, 0)

    val data = ((case1 zip spontaneous) zip induced map (x => Map("case" -> x._1._1, "spont" -> x._1._2, "induced" -> x._2))).zipWithIndex.map(x => (Individual(x._2.toString), (x._1("case") match {
      case 0 => false
      case 1 => true
    }, x._1.mapValues(x => Some(x.toDouble))))).toMap

    val result = LogisticRegression.logisticRegression(data, List("spont", "induced"), DropSample).right.toOption.get

    it("should have the right estimates") {
      math.abs(result.covariates("spont")._1.slope - (-0.3917)) should be < 0.01
      math.abs(result.covariates("spont")._1.sd - (0.7244)) should be < 0.01
      math.abs(result.covariates("induced")._1.slope - (0.8925)) should be < 0.01
      math.abs(result.covariates("induced")._1.sd - (0.6390)) should be < 0.01
      math.abs(result.intercept._1.slope - (-0.3914)) should be < 0.01
      math.abs(result.intercept._1.sd - (0.7630)) should be < 0.1
    }
    it("should have the right loglikelihood") {
      math.abs(result.logLikelihood.L - (-13.12042)) should be < 0.001
      result.logLikelihood.df should equal(3)
      result.logLikelihood.numberOfSamples should equal(21)
    }
  }

  describe("LogisticRegression with baseline success rate of 20/100, and two mutually exclusive features: factor1 80/100, factor2 50/100.") {

    val x = new LogisticRegression(
      designMatrix = Array(
        Array(1.0, 0.0, 0.0),
        Array(1.0, 1.0, 0.0),
        Array(1.0, 0.0, 1.0)
      ),
      outComeCounts = Array(20, 80, 50),
      aggregatedBinLengths = Array(100, 100, 100)
    )

    val stream = x.from(Array(0.5, 0.5, 0.5)).take(50).takeWhile(x => x.asInstanceOf[Iteration]._2 >= 5E-3)

    val result = stream.last.asInstanceOf[Iteration]._1.map(x => math.exp(x)).toList

    ignore("should converge in 3 iterations") {
      stream.size should equal(3)
    }
    it("estimated baseline odd ratio should be 1/4") {
      result(0) - 0.25 should be < 0.001
    }
    it("estimated factor1 odd ratio over baseline should be 16") {
      result(1) - 16.0 should be < 0.001
    }
    it("estimated factor2 odd ratio over baseline should be 4") {
      result(2) - 4.0 should be < 0.001
    }

  }

  describe("Logistic regression with continuous independent variable with states 1.0 (success rate 0/100), 1.5 (success: 25/100), 2.0 (success: 50/100). Test numbers match R's glm.") {
    val x = new LogisticRegression(
      designMatrix = Array(
        Array(1, 1.0),
        Array(1, 1.5),
        Array(1, 2.0)
      ),
      outComeCounts = Array(0, 25, 50),
      aggregatedBinLengths = Array(100, 100, 100)
    )
    val stream = x.from(Array(0.5, 0.5)).take(50).takeWhile(x => x.asInstanceOf[Iteration]._2 >= 5E-10).toList

    val resultODDs = stream.last.asInstanceOf[Iteration]._1.map(x => math.exp(x)).toList
    val resultLODs = stream.last.asInstanceOf[Iteration]._1

    it("should converge in 5 iterations")(stream.size should equal(5))

    it("intercept LOD should be -6.46")(math.abs(resultLODs(0) - (-6.6461)) should be < 0.001)

    it("slope LOD should be 3.3980 ")(math.abs(resultLODs(1) - (3.3980)) should be < 0.001)

    it("method estimate should give the same standard Errors and z scores as R") {
      math.abs(x.estimate.get.coefficients(0) - (-6.6461)) should be < 0.001
      math.abs(x.estimate.get.coefficients(1) - (3.3980)) should be < 0.001

      math.abs(x.estimate.get.standardErrors(0) - (0.8396)) should be < 0.001
      math.abs(x.estimate.get.standardErrors(1) - (0.4724)) should be < 0.001

      math.abs(x.estimate.get.zStats(0) - (-7.916)) should be < 0.001
      math.abs(x.estimate.get.zStats(1) - (7.193)) should be < 0.001
    }

  }

  describe("same but with maps") {
    val data = (1 to 300).map { (i: Int) =>
      val ind = mybiotools.gwascommons.Individual(i.toString)
      val cov = i match {
        case x if x <= 100 => Some(1.0)
        case x if x <= 200 => Some(1.5)
        case x if x <= 300 => Some(2.0)
      }
      val outcome = i match {
        case x if x <= 100 => false
        case x if x <= 125 => true
        case x if x <= 200 => false
        case x if x <= 250 => true
        case _ => false
      }
      ind -> (outcome, Map("cov" -> cov))
    }.toMap
    val estimate = LogisticRegression.logisticRegression(data, List("cov"), MeanImpute)
    it("should give the same results as before") {
      estimate should equal(Right(LogisticRegressionResult(Map("cov" -> (Effect(3.397982273969736, 0.4723757242859011), ZTestResult(7.193388862449541, 6.320260192164651E-13))), (Effect(-6.646107472920015, 0.8395796363468516), ZTestResult(-7.915994129917581, 2.452851903416847E-15)), 300, LogLikelihood(-131.39747651295107, 2.0, 300))))
    }
  }

  describe("Ill snp with R") {

    val x = new LogisticRegression(
      designMatrix = Array(
        Array(1.0, 2.0, 1.0),
        Array(1.0, 2.0, 0.0),
        Array(1.0, 0.0, 1.0),
        Array(1.0, 1.0, 0.0),
        Array(1.0, 1.0, 1.0),
        Array(1.0, 0.0, 0.0)
      ),
      outComeCounts = Array(43, 7, 35, 18, 90, 9),
      aggregatedBinLengths = Array(78, 16, 78, 34, 166, 16)
    )
    val stream = x.from(Array(0, 0, 0)).take(50).takeWhile(x => x.asInstanceOf[Iteration]._2 >= 5E-10).toList

    val resultODDs = stream.last.asInstanceOf[Iteration]._1.map(x => math.exp(x)).toList
    val resultLODs = stream.last.asInstanceOf[Iteration]._1

    it("should converge in 3 iterations")(stream.size should equal(3))

    it("intercept LOD ")(math.abs(resultLODs(0) - (-0.06731)) should be < 0.001)

    it("slope 1 LOD ")(math.abs(resultLODs(1) - (0.12805)) should be < 0.001)

    it("slope 2 LOD  ")(math.abs(resultLODs(2) - (0.02644)) should be < 0.001)

    it("method estimate should give the same standard Errors and z scores as R") {
      math.abs(x.estimate.get.coefficients(0) - (-0.06731)) should be < 0.001
      math.abs(x.estimate.get.coefficients(1) - (0.12805)) should be < 0.001
      math.abs(x.estimate.get.coefficients(2) - (0.02644)) should be < 0.001

      math.abs(x.estimate.get.standardErrors(0) - (0.28654)) should be < 0.001
      math.abs(x.estimate.get.standardErrors(1) - (0.14629)) should be < 0.001
      math.abs(x.estimate.get.standardErrors(2) - (0.27065)) should be < 0.001

      math.abs(x.estimate.get.zStats(0) - (-0.235)) should be < 0.001
      math.abs(x.estimate.get.zStats(1) - (0.875)) should be < 0.001
      math.abs(x.estimate.get.zStats(2) - (0.098)) should be < 0.001
    }

  }

  describe("score test") {
    import org.saddle._
    val designMatrix = Mat(Array(
      Array(1.0, 2.0, 1.0),
      Array(1.0, 2.0, 0.0),
      Array(1.0, 0.0, 1.0),
      Array(1.0, 1.0, 0.0),
      Array(1.0, 1.0, 1.0),
      Array(1.0, 0.0, 0.0)
    )).T

    val outComeCounts = Array(43, 7, 35, 18, 90, 9)
    val aggregatedBinLengths = Array(78, 16, 78, 34, 166, 16)

    val nullfit = new LogisticRegression(Mat(Array(
      Array(1.0),
      Array(1.0),
      Array(1.0),
      Array(1.0),
      Array(1.0),
      Array(1.0)
    )).T, outComeCounts, aggregatedBinLengths).estimateWithFailure(50, 1E-6).right.toOption.get.coefficients.head

    val fullfit = new LogisticRegression(designMatrix, outComeCounts, aggregatedBinLengths).estimateWithFailure(50, 1E-6).right.toOption.get

    it("run") {

      val sc = scoreTest(
        designMatrix,
        outComeCounts,
        aggregatedBinLengths,
        Vec(nullfit, 0.0, 0.0),
        2
      )

      sc.right.get.pValue should equal(0.6781456881023809)

    }
  }

  describe("compare autosomes with plink") {
    import mybiotools.gwascommons.genotypedata._
    import mybiotools.gwascommons.associationresults._
    import mybiotools.gwascommons._

    val pedfile = getClass.getResource("/genotypedata/medium.ped").getPath
    val mapfile = getClass.getResource("/genotypedata/medium.map").getPath
    val phenofile = getClass.getResource("/genotypedata/medium.pheno").getPath
    val plinkresult = getClass.getResource("/genotypedata/mediumlogistic.assoc.logistic").getPath

    val genotypes = DiscreteGenotypeData.fromPedMap(new java.io.File(pedfile), new java.io.File(mapfile))

    val phenotypes: Vector[(Individual, Double)] = io.Source.fromFile(phenofile).getLines.map { line =>
      Individual(line.split(" ")(0), line.split(" ")(1)) -> line.split(" ")(2).toDouble
    }.toVector

    it("should have agree with plink (24k SNPs, 388 individuals, random phenotype)") {

      val plinkresultAssocs = io.Source.fromFile(plinkresult).getLines.drop(1).grouped(2).foreach { lines =>

        val addline = lines.head
        val genderline = lines(1)

        val splittedadd = mybiotools.fastSplitSetSeparator(addline, Set(' ', '\t'))
        val splittedgender = mybiotools.fastSplitSetSeparator(genderline, Set(' ', '\t'))

        if (splittedadd(11) != "NA" && splittedadd(9) != "inf" && splittedadd(0).toInt < 23) {

          val snpname = mybiotools.stringstore.StringStore(splittedadd(1))
          val nmiss = splittedadd(5).toInt
          val orADD = splittedadd(6).toDouble
          val seADD = splittedadd(7).toDouble

          val orSEX = splittedgender(6).toDouble
          val seSEX = splittedgender(7).toDouble

          val data: Map[Individual, (Boolean, Map[String, Option[Double]])] = phenotypes.map {
            case (ind, pheno) =>
              val dose = genotypes.getStateWithRespectToMinor(ind.FID, snpname).map(x => if (x != DiscreteGenotypes.Missing) Some(x.variantAlleleDosage) else None).flatten
              val gender = genotypes.getPatient(ind.FID).get.gender.map {
                _ match {
                  case Male => 1.0
                  case Female => 0.0
                }
              }
              val bpheno = if (pheno == 1.0) false else true

              ind -> (bpheno, Map("SEX" -> gender, "ADD" -> dose))
          }.toMap

          val myresult = LogisticRegression.logisticRegression(data, List("ADD", "SEX"), DropSample)

          val myORADD = math.exp(myresult.right.get.covariates("ADD")._1.slope)
          val mySDADD = myresult.right.get.covariates("ADD")._1.sd

          val myORSEX = math.exp(myresult.right.get.covariates("SEX")._1.slope)
          val mySDSEX = myresult.right.get.covariates("SEX")._1.sd

          myresult.right.get.numberOfSamples should equal(nmiss)

          if (!((close(myORADD, orADD) && close(myORSEX, orSEX) && close(mySDADD, seADD) && close(mySDSEX, seSEX)))) {
            println(myresult)
            println(lines)
            true should equal(false)
          }

        }
      }
    }
  }
}