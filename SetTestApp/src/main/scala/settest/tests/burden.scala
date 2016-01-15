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

import org.saddle._
import mybiotools.gwascommons.gwas._
import mybiotools.stat._
import mybiotools.stat.LinearRegression._
import mybiotools.gwascommons._
import scala.util._

case class BurdenTestResult(r: RegressionResultOrFailure) extends settest.TestResult {
  def pValue = r match {
    case x: RegressionResult => x.covariates("burden")._2.pValue
    case _ => Double.NaN
  }
  def toLine = r match {
    case x: RegressionResult => x.toLine
    case x => x.toString
  }
  def numberOfSamples = r match {
    case x: RegressionResult => x.numberOfSamples
    case _ => 0
  }
}

object BurdenTest extends settest.Test {

  val testName = "BURDEN"

  def runTest(
    data: Frame[Individual, String, Double],
    phenoName: String,
    phenoScale: PhenotypeScale,
    covariateNames: Seq[String],
    snpNames: Seq[String]
  ): Try[BurdenTestResult] = {
    if (snpNames.isEmpty) Failure(new RuntimeException("Empty test"))
    else {

      val mutationBurden: Series[Individual, Double] = {

        val flippedTestData = {
          val testData = data.col(snpNames: _*)
          val alleleFrequencies: Map[String, Double] = testData.toColSeq.map(x => x._1 -> x._2.toVec.sum / (x._2.toVec.length * 2.0)).toMap
          Frame(testData.toColSeq.map {
            case (name, series) =>
              alleleFrequencies(name) match {
                case x if x < 0.5 => (name, series)
                case _ => (name, series.mapValues(2.0 - _))
              }
          }: _*)
        }
        Series(flippedTestData.toRowSeq.map {
          case (ind, series) =>
            ind -> series.toVec.sum
        }: _*)
      }

      mybiotools.stat.Regression.regression(
        covariates = Frame(data.toColSeq :+ "burden" -> mutationBurden: _*),
        covariateNames = covariateNames :+ "burden",
        phenoscale = phenoScale,
        phenoName = phenoName
      ).map(x => BurdenTestResult(x))

    }
  }
}