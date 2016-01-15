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
import scala.util.Try
import mybiotools.stat.LinearRegression._
import org.saddle._
import mybiotools.gwascommons._

object Regression {
  def regression(
    covariates: Frame[Individual, String, Double],
    covariateNames: Seq[String],
    phenoscale: PhenotypeScale,
    phenoName: String
  ): Try[RegressionResultOrFailure] = {
    phenoscale match {
      case Count | Rank => {

        Try(PoissonRegression.poissonRegression(
          data = covariates,
          outcomes = covariates.firstCol(phenoName),
          covariateNames = covariateNames,
          maxIter = 40,
          relativeTolerance = 1E-5,
          missingMode = DropSample
        ))

      }
      case Linear => Try(linearRegression(covariates, covariateNames, phenoName, DropSample, 0.0))
      case Logistic => Try {
        val outcome2: Series[Individual, Boolean] = covariates.firstCol(phenoName).mapValues(PlinkPhenotypeCoding.convertToBooleanNonMissing)

        LogisticRegression.logisticRegression(
          data = covariates,
          outcomes = outcome2,
          covariateNames = covariateNames,
          missingMode = DropSample,
          maxIter = 40,
          epsilon = 1E-5
        ) match {
          case Left(x) => x
          case Right(x) => x
        }
      }
    }
  }
}