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

package settest

import mybiotools.stringstore._
import scala.util._
import mybiotools.gwascommons._
import org.saddle._

case class RegionName(value: String8) extends AnyVal

case class RegionSetName(value: String8) extends AnyVal {
  override def toString = value.toString
}

case class PValue(value: Double) extends AnyVal {
  override def toString = value.toString
}

trait TestResult extends java.io.Serializable {
  def pValue: Double
  def numberOfSamples: Int
  def toLine: String
}

trait Test extends java.io.Serializable {

  override def toString: String = testName

  def runTest(
    data: Frame[Individual, String, Double],
    phenoName: String,
    phenoScale: PhenotypeScale,
    covariateNames: Seq[String],
    snpNames: Seq[String]
  ): Try[TestResult]

  def testName: String
}
