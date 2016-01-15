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

package settest.tests

import mybiotools.gwascommons._
import mybiotools._
import settest._
import scala.util._
import org.saddle._
import org.saddle.scalar._

import java.io.File

case class SKATResult(pValue: Double, numberOfSamples: Int) extends TestResult {
  def toLine = pValue.toString + " " + numberOfSamples
}

case class SkatTest(
    skatTestType: Skat.SkatTestType,
    skatMethod: String,
    weights: (Double, Double)
) extends Test {
  def testName = s"$skatTestType(${weights._1},${weights._2})"
  def runTest(
    data: Frame[Individual, String, Double],
    phenoName: String,
    phenoScale: PhenotypeScale,
    covariateNames: Seq[String],
    snpNames: Seq[String]
  ): Try[SKATResult] = Skat.runSkat2(data, snpNames, covariateNames, phenoName, phenoScale, weights, skatTestType, skatMethod)
}

object Skat {

  case class SkatFiles(ssd: File, ssdinfo: File)
  case class SkatPhenoFiles(pheno: File, covar: File)
  case class SkatPhenoStrings(pheno: String, covar: String)

  def prepareSkatPhenoFiles(
    individuals: Seq[Individual],
    covariates: Frame[Individual, String, Double],
    covariateNames: Seq[String],
    phenoName: String,
    phenoscale: PhenotypeScale
  ): SkatPhenoFiles = {

    val d = prepareSkatPhenoStrings(
      individuals,
      covariates,
      covariateNames,
      phenoName,
      phenoscale
    )

    val phenoString = d.pheno

    val covariateString = d.covar

    val fPheno = TempFile.createTempFile(".pheno")
    mybiotools.writeToFile(fPheno.getAbsolutePath, phenoString)

    val fCovar = TempFile.createTempFile(".covar")
    mybiotools.writeToFile(fCovar.getAbsolutePath, covariateString)

    SkatPhenoFiles(fPheno, fCovar)

  }

  // TODO unit test this function
  def prepareSkatPhenoStrings(
    individuals: Seq[Individual],
    covariates: Frame[Individual, String, Double],
    covariateNames: Seq[String],
    phenoName: String,
    phenoscale: PhenotypeScale
  ): SkatPhenoStrings = {

    val indset = individuals.toSet

    val covariatesNamesWithVariation = {
      val subsetToIndividuals = covariates.rfilterIx(ind => individuals.contains(ind))
      covariateNames.filter { name =>
        subsetToIndividuals.firstCol(name).values.toSeq.filterNot(_.isNaN).distinct.size > 1
      }
    }

    def convertValue(x: Double, phenoscale: PhenotypeScale) =
      if (phenoscale == Linear) x
      else x - 1.0

    def escape(x: Option[Double]): String = x match {
      case None => "NA"
      case Some(y) => y.toString
    }

    val phenotypeValuesInOrder: Seq[String] = individuals.map(x => escape(Scalar.scalarToOption(covariates.firstCol(phenoName).first(x)).map(x => convertValue(x, phenoscale))))

    val covarsToWrite: Seq[Seq[String]] =
      individuals.map { x => covariatesNamesWithVariation.map { n => escape(Scalar.scalarToOption(covariates.firstCol(n).first(x))) } }

    val covariateString = covarsToWrite.map(_.mkString(" ")).mkString("\n")
    val phenoString = phenotypeValuesInOrder.mkString("\n")

    SkatPhenoStrings(pheno = phenoString, covar = covariateString)

  }

  def genotypeMatrixForSkat(
    genotypes: Frame[Individual, String, Double],
    individuals: Seq[Individual]
  ): String = {

    val transposed: Frame[String, Individual, Double] = genotypes.T

    individuals.map { ind =>
      val inddata: Series[String, Double] = transposed.firstCol(ind)
      inddata.values.toSeq.mkString(" ")
    }.mkString("\n")

  }

  sealed trait SkatTestType
  case object SKATStandard extends SkatTestType {
    override def toString = "SKAT"
  }
  case object SKATC extends SkatTestType {
    override def toString = "SKATC"
  }

  def runSkat(
    genotypeFile: File,
    covarFile: File,
    phenoFile: File,
    setName: String,
    phenoscale: PhenotypeScale,
    out: File,
    alpha1: Double,
    alpha2: Double,
    skatTestType: SkatTestType,
    skatMethod: String,
    numberOfSamples: Int
  ): Try[SKATResult] = {

    val phenoscalestring = phenoscale match {
      case Linear => "C"
      case Logistic => "D"
      case _ => throw new RuntimeException("SKAT only works on Linear and Logistic phenotypes")
    }

    val executable = TempFile.getExecutableFromJar("/skat.run.R")

    val runskatC = if (skatTestType == SKATC) "1" else "0"

    val skatcmd = s"/usr/bin/env Rscript --vanilla --default-packages=base,utils,stats ${executable} ${genotypeFile.getAbsolutePath} ${phenoFile.getAbsolutePath} ${covarFile.getAbsolutePath} ${out.getAbsolutePath} ${setName} ${phenoscalestring} ${alpha1} ${alpha2} ${runskatC} $skatMethod"

    // println(skatcmd)

    scala.util.Try {

      val (output, stderr) = mybiotools.execStreamsAsList(skatcmd)

      val cleanoutput = output.dropWhile(_ != "MARK").drop(1)

      println(stderr)
      println(output)

      SKATResult(cleanoutput(3).toDouble, numberOfSamples)
    }

  }

  private[settest] case class TPedStrings(tfam: String, tped: String)

  private[settest] def writeTpedToString(
    data: Frame[Individual, String, Double],
    hardcallThreshold: Double
  ): TPedStrings = {

    def hardcall(d: Double): String = d match {
      case x if x.isNaN => "0 0"
      case x if (math.abs(x - 2.0) <= hardcallThreshold) => "A A"
      case x if (math.abs(x - 1.0) <= hardcallThreshold) => "A B"
      case x if (math.abs(x - 0.0) <= hardcallThreshold) => "B B"
      case _ => "0 0"
    }

    val hardcalled: Frame[Individual, String, String] = data.mapValues(hardcall)
    val inds: Vector[Individual] = hardcalled.rowIx.toSeq.toVector

    import mybiotools.tabular._
    import TabSerialization._
    import RProtocol._

    val famstring = inds.map(ind => (ind.FID.value, ind.IID.value, "0", "0", "0", "-9").productIterator.toList.mkString(" ")).mkString("\n")

    val snps: Vector[String] = hardcalled.colIx.toSeq.toVector

    val tpedstring = snps.map { snp =>
      (Vector("0", snp, "0", "0") ++ hardcalled.firstCol(snp).values.toSeq).mkString(" ")
    }.mkString("\n")

    TPedStrings(famstring, tpedstring)
  }

  def runSkat2(
    data: Frame[Individual, String, Double],
    snpsToTest: Seq[String],
    covariateNames: Seq[String],
    phenoName: String,
    phenoScale: PhenotypeScale,
    weightShapes: (Double, Double),
    skatTestType: SkatTestType,
    skatMethod: String
  ): Try[SKATResult] = {

    val testnames: Seq[String] = snpsToTest

    val testdata = data.col(testnames: _*)

    val indorder = testdata.rowIx.toSeq.sorted

    val genotypeFile: File =
      mybiotools.writeToTempFile(genotypeMatrixForSkat(testdata, indorder))

    val skatphenofiles =
      prepareSkatPhenoFiles(indorder, data, covariateNames, phenoName, phenoScale)

    val outfile = TempFile.createTempFile(".skatout")

    val p =
      runSkat(
        genotypeFile = genotypeFile,
        covarFile = skatphenofiles.covar,
        phenoFile = skatphenofiles.pheno,
        setName = "set",
        phenoscale = phenoScale,
        out = outfile,
        alpha1 = weightShapes._1,
        alpha2 = weightShapes._2,
        skatTestType = skatTestType,
        skatMethod = skatMethod,
        numberOfSamples = data.numRows
      )

    genotypeFile.delete
    skatphenofiles.covar.delete
    skatphenofiles.pheno.delete
    outfile.delete

    p

  }
}