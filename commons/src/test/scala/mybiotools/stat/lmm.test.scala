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
import java.io.File
import org.apache.commons.math3.random.{ RandomGenerator, RandomDataGenerator, Well44497b }

class LMMSpec extends FunSpec with Matchers {

  describe("simulated data 2") {
    val numSamples = 200
    val numFeatures = 5
    val trueFeatures = 5
    val rnd = new Well44497b(1)
    val rndd = new RandomDataGenerator(rnd)
    def simulateRandomColumn(sd: Double): Vec[Double] = Vec(1 to numSamples map (i => rndd.nextGaussian(0.0, sd)): _*)
    val design = Mat(1 to numFeatures map (i => simulateRandomColumn(1.0)): _*)
    val trueCoeff = Vec((0 until (numFeatures - (trueFeatures).toInt) map { i =>
      0.0
    }) ++ (0 until (trueFeatures).toInt map (i => rnd.nextGaussian * 10d)): _*)

    val outcome: Vec[Double] = (design dot trueCoeff).col(0) + simulateRandomColumn(100.0)

    val trueR2 = {
      val p = (design mult trueCoeff).col(0)
      val sse = (outcome - p) dot (outcome - p)
      val sst = (outcome - outcome.mean) dot (outcome - outcome.mean)
      1.0 - sse / sst
    }

    println("TRUE R2: " + trueR2)

    val lm = LinearRegression.linearRegression(design, outcome)
    println("LM" + lm.map(_.r2))

    val grm = (design mult design.T) / (design.numCols)

    val grm_withoutFirstCol = (design.withoutCols(1) mult design.withoutCols(1).T) / (design.numCols - 1)

    {

      import org.saddle.io.CsvImplicits._
      val f1 = TempFile.createTempFile(".design.txt")
      Frame(design).writeCsvFile(f1.getAbsolutePath)
      val f3 = TempFile.createTempFile(".trueCoeff.txt")
      Series(trueCoeff).writeCsvFile(f3.getAbsolutePath)
      val f4 = TempFile.createTempFile(".outcome.txt")
      Series(outcome).writeCsvFile(f4.getAbsolutePath)
      println(f1.getAbsolutePath)

      println(f3.getAbsolutePath)
      println(f4.getAbsolutePath)

      val f = Frame(grm, Index(0 until design.numRows: _*), Index(0 until design.numRows: _*)).mapColIndex(x => Individual(x.toString, x.toString)).mapRowIndex(x => Individual(x.toString, x.toString))
      val tmp1 = TempFile.createTempFile("")
      val tmpgrmgz = new File(tmp1.getAbsolutePath + ".grm.gz")
      val tmpgrmid = new File(tmp1.getAbsolutePath + ".grm.id")
      val gctaphenofile = new File(tmp1.getAbsolutePath + ".gcta.pheno.txt")
      openFileWriter(tmpgrmgz) { gzwriter =>
        openFileWriter(tmpgrmid) { idwriter =>
          mybiotools.gwascommons.genotypedata.GRM.write(f, gzwriter, idwriter)
        }
      }
      writeToFile(gctaphenofile, outcome.toSeq.zipWithIndex.map(x => x._2 + " " + x._2 + " " + x._1).mkString("\n"))
      println(tmpgrmgz)
      println(tmpgrmid)
      println(gctaphenofile)

    }

    it("vs truth, fixed and random effects") {

      val estimated = VarianceComponentModel.reml_nr(
        outcome,
        Mat(vec.ones(outcome.length), design.col(0)),
        Vector(mat.ident(outcome.length), grm_withoutFirstCol),
        1E-6,
        1E-8,
        50
      ).get

      // println(estimated)
      println("LMM" + estimated.varianceComponentsRelativeToTotal)

      val residualR2 = {
        val p = (design.withoutCols(0) mult trueCoeff.without(Array(0))).col(0)
        val residual = outcome - (design.col(0) mult trueCoeff(0)).col(0)
        val sse = (residual - p) dot (residual - p)
        val sst = (residual - outcome.mean) dot (residual - outcome.mean)
        1.0 - sse / sst
      }
      println(residualR2)

      math.abs(
        estimated.varianceComponentsRelativeToTotal.slope -
          residualR2
      ) should be < 0.07

    }

  }

  describe("simulated data") {
    val numSamples = 100
    val numFeatures = 5
    val trueFeatures = 5
    val rnd = new Well44497b(1)
    val rndd = new RandomDataGenerator(rnd)
    def simulateRandomColumn(sd: Double): Vec[Double] = Vec(1 to numSamples map (i => rndd.nextGaussian(0.0, sd)): _*)
    val design = Mat(1 to numFeatures map (i => simulateRandomColumn(1.0)): _*)
    val trueCoeff = Vec((0 until (numFeatures - (trueFeatures).toInt) map { i =>
      0.0
    }) ++ (0 until (trueFeatures).toInt map (i => rnd.nextGaussian * 10d)): _*)

    val outcome: Vec[Double] = (design dot trueCoeff).col(0) + simulateRandomColumn(10.0)

    val trueR2 = {
      val p = (design mult trueCoeff).col(0)
      val sse = (outcome - p) dot (outcome - p)
      val sst = (outcome - outcome.mean) dot (outcome - outcome.mean)
      1.0 - sse / sst
    }

    println("TRUE R2: " + trueR2)

    val lm = LinearRegression.linearRegression(design, outcome)
    println(lm.map(_.r2))

    val grm = (design mult design.T) / (design.numCols)

    val grm_withoutFirstCol = (design.withoutCols(1) mult design.withoutCols(1).T) / (design.numCols - 1)

    {

      import org.saddle.io.CsvImplicits._
      val f1 = TempFile.createTempFile(".design.txt")
      Frame(design).writeCsvFile(f1.getAbsolutePath)
      val f3 = TempFile.createTempFile(".trueCoeff.txt")
      Series(trueCoeff).writeCsvFile(f3.getAbsolutePath)
      val f4 = TempFile.createTempFile(".outcome.txt")
      Series(outcome).writeCsvFile(f4.getAbsolutePath)
      println(f1.getAbsolutePath)

      println(f3.getAbsolutePath)
      println(f4.getAbsolutePath)

      val f = Frame(grm, Index(0 until design.numRows: _*), Index(0 until design.numRows: _*)).mapColIndex(x => Individual(x.toString, x.toString)).mapRowIndex(x => Individual(x.toString, x.toString))
      val tmp1 = TempFile.createTempFile("")
      val tmpgrmgz = new File(tmp1.getAbsolutePath + ".grm.gz")
      val tmpgrmid = new File(tmp1.getAbsolutePath + ".grm.id")
      val gctaphenofile = new File(tmp1.getAbsolutePath + ".gcta.pheno.txt")
      openFileWriter(tmpgrmgz) { gzwriter =>
        openFileWriter(tmpgrmid) { idwriter =>
          mybiotools.gwascommons.genotypedata.GRM.write(f, gzwriter, idwriter)
        }
      }
      writeToFile(gctaphenofile, outcome.toSeq.zipWithIndex.map(x => x._2 + " " + x._2 + " " + x._1).mkString("\n"))
      println(tmpgrmgz)
      println(tmpgrmid)
      println(gctaphenofile)

    }

    /* GCTA output

  *******************************************************************
  * Genome-wide Complex Trait Analysis (GCTA)
  * version 1.24.2
  * (C) 2010-2013 Jian Yang, Hong Lee, Michael Goddard and Peter Visscher
  * The University of Queensland
  * MIT License
  *******************************************************************
  Analysis started: Sat Jul 18 10:00:06 2015

  Options:
  --reml
  --grm-gz bio2015_07_18_09_59_046614724594908057077
  --pheno bio2015_07_18_09_59_046614724594908057077.gcta.pheno.txt

  Note: This is a multi-thread program. You could specify the number of threads by the --thread-num option to speed up the computation if there are multiple processors in your machine.

  Reading IDs of the GRM from [bio2015_07_18_09_59_046614724594908057077.grm.id].
  100 IDs read from [bio2015_07_18_09_59_046614724594908057077.grm.id].
  Reading the GRM from [bio2015_07_18_09_59_046614724594908057077.grm.gz].
  Pairwise genetic relationships between 100 individuals are included from [bio2015_07_18_09_59_046614724594908057077.grm.gz].
  Reading phenotypes from [bio2015_07_18_09_59_046614724594908057077.gcta.pheno.txt].
  Non-missing phenotypes of 100 individuals are included from [bio2015_07_18_09_59_046614724594908057077.gcta.pheno.txt].

  100 individuals are in common in these files.

  Performing  REML analysis ... (Note: may take hours depending on sample size).
  100 observations, 1 fixed effect(s), and 2 variance component(s)(including residual variance).
  Calculating prior values of variance components by EM-REML ...
  Updated prior values: 164.499 87.6382
  logL: -288.99
  Running AI-REML algorithm ...
  Iter. logL  V(G)  V(e)  
  1 -280.65 181.54963 86.01899  
  2 -280.51 196.23558 84.96174  
  3 -280.44 234.40599 82.74302  
  4 -280.37 242.89232 82.80135  
  5 -280.36 243.18875 82.80168  
  6 -280.36 243.18854 82.80169  
  Log-likelihood ratio converged.

  Calculating the logLikelihood for the reduced model ...
  (variance component 1 is dropped from the model)
  Calculating prior values of variance components by EM-REML ...
  Updated prior values: 321.88339
  logL: -337.62496
  Running AI-REML algorithm ...
  Iter. logL  V(e)  
  1 -337.62 321.88339 
  Log-likelihood ratio converged.

  Summary result of REML analysis:
  Source  Variance  SE
  V(G)  243.188543  156.652880
  V(e)  82.801689 12.077254
  Vp  325.990233  157.069939
  V(G)/Vp 0.745999  0.125255

  Variance/Covariance Matrix of the estimates:
  24540.1 
  -7.50966  145.86  

  Summary result of REML analysis has been saved in the file [gcta.hsq].

  Analysis finished: Sat Jul 18 10:00:06 2015
  Computational time: 0:0:0

      */

    it("vs gcta64, cmaes") {

      val estimated = VarianceComponentModel.reml_direct(
        outcome,
        Mat(vec.ones(outcome.length)),
        Vector(mat.ident(outcome.length), grm),
        1E-10,
        1E-16,
        100
      )

      // println(estimated)
      // println(estimated.varianceComponentsRelativeToTotal)

      ((estimated.varianceComponents - Vec(82.801689, 243.188543)) / Vec(82.801689, 243.188543)).toSeq.foreach { x =>
        x should be < 0.1
      }
    }

    it("vs gcta64") {

      val estimated = VarianceComponentModel.reml_nr(
        outcome,
        Mat(vec.ones(outcome.length)),
        Vector(mat.ident(outcome.length), grm),
        1E-6,
        1E-8,
        100
      ).get

      // println(estimated)
      // println(estimated.varianceComponentsRelativeToTotal)

      ((estimated.varianceComponents - Vec(82.801689, 243.188543)) / Vec(82.801689, 243.188543)).toSeq.foreach { x =>
        x should be < 0.1
      }
    }

    it("vs truth, fixed and random effects") {

      val estimated = VarianceComponentModel.reml_nr(
        outcome,
        Mat(vec.ones(outcome.length), design.col(0)),
        Vector(mat.ident(outcome.length), grm_withoutFirstCol),
        1E-6,
        1E-8,
        100
      ).get

      println(trueCoeff)
      println(estimated)
      println(estimated.varianceComponentsRelativeToTotal)

      val residualR2 = {
        val p = (design.withoutCols(0) mult trueCoeff.without(Array(0))).col(0)
        val residual = outcome - (design.col(0) mult trueCoeff(0)).col(0)
        val sse = (residual - p) dot (residual - p)
        val sst = (residual - outcome.mean) dot (residual - outcome.mean)
        1.0 - sse / sst
      }
      println(residualR2)

      math.abs(
        estimated.fixed.toSeq(1) - trueCoeff.raw(0)
      ) / trueCoeff.raw(0) should be < 0.2

      math.abs(
        estimated.varianceComponentsRelativeToTotal.slope -
          residualR2
      ) should be < 0.15
    }

  }

  // describe("simulated data BIC") {
  //   val numSamples = 100
  //   val numFeatures = 10000
  //   val trueFeatures = 20
  //   val rnd = new Well44497b(1)
  //   val rndd = new RandomDataGenerator(rnd)
  //   def simulateRandomColumn(sd: Double): Vec[Double] = Vec(1 to numSamples map (i => rndd.nextGaussian(0.0, sd)): _*)
  //   val design = Mat(1 to numFeatures map (i => simulateRandomColumn(1.0)): _*)
  //   val trueCoeff = Vec((0 until (numFeatures - (trueFeatures).toInt) map { i =>
  //     0.0
  //   }) ++ (0 until (trueFeatures).toInt map (i => rnd.nextGaussian * 10d)): _*)

  //   val outcome: Vec[Double] = (design dot trueCoeff).col(0) + simulateRandomColumn(50.0)

  //   val trueR2 = {
  //     val p = (design mult trueCoeff).col(0)
  //     val sse = (outcome - p) dot (outcome - p)
  //     val sst = (outcome - outcome.mean) dot (outcome - outcome.mean)
  //     1.0 - sse / sst
  //   }

  //   println("TRUE R2: " + trueR2)

  //   val grm = (design mult design.T) / (design.numCols)

  //   val grm_withoutFirstCol = (design.withoutCols(1) mult design.withoutCols(1).T) / (design.numCols - 1)

  //   // {

  //   //   import org.saddle.io.CsvImplicits._
  //   //   val f1 = TempFile.createTempFile(".design.txt")
  //   //   Frame(design).writeCsvFile(f1.getAbsolutePath)
  //   //   val f3 = TempFile.createTempFile(".trueCoeff.txt")
  //   //   Series(trueCoeff).writeCsvFile(f3.getAbsolutePath)
  //   //   val f4 = TempFile.createTempFile(".outcome.txt")
  //   //   Series(outcome).writeCsvFile(f4.getAbsolutePath)
  //   //   println(f1.getAbsolutePath)

  //   //   println(f3.getAbsolutePath)
  //   //   println(f4.getAbsolutePath)

  //   //   val f = Frame(grm, Index(0 until design.numRows: _*), Index(0 until design.numRows: _*)).mapColIndex(x => Individual(x.toString, x.toString)).mapRowIndex(x => Individual(x.toString, x.toString))
  //   //   val tmp1 = TempFile.createTempFile("")
  //   //   val tmpgrmgz = new File(tmp1.getAbsolutePath + ".grm.gz")
  //   //   val tmpgrmid = new File(tmp1.getAbsolutePath + ".grm.id")
  //   //   val gctaphenofile = new File(tmp1.getAbsolutePath + ".gcta.pheno.txt")
  //   //   openFileWriter(tmpgrmgz) { gzwriter =>
  //   //     openFileWriter(tmpgrmid) { idwriter =>
  //   //   //       mybiotools.gwascommons.genotypedata.GRM.write(f, gzwriter, idwriter)
  //   //   //     }
  //   //   //   }
  //   //   //   writeToFile(gctaphenofile, outcome.toSeq.zipWithIndex.map(x => x._2 + " " + x._2 + " " + x._1).mkString("\n"))
  //   //   //   println(tmpgrmgz)
  //   //   //   println(tmpgrmid)
  //   //   //   println(gctaphenofile)

  //   //   // }

  //   //   ignore("bic") {

  //   //     val estimated = VarianceComponentModel.reml_direct(
  //   //       outcome,
  //   //       Mat(vec.ones(outcome.length)),
  //   //       Vector(mat.ident(outcome.length), grm),
  //   //       1E-10,
  //   //       1E-16,
  //   //       5000
  //   //     )

  //   //     println(estimated)
  //   //     println(estimated.varianceComponentsRelativeToTotal)

  //   //     // ((estimated.varianceComponents - Vec(82.801689, 243.188543)) / Vec(82.801689, 243.188543)).toSeq.foreach { x =>
  //   //     //   x should be < 0.1
  //   //     // }
  //   //   }
  //   // }

}