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

package mybiotools.gwascommons

sealed trait PhenotypeScale
case object Linear extends PhenotypeScale
case object Logistic extends PhenotypeScale
case object Count extends PhenotypeScale
case object Rank extends PhenotypeScale

object PhenotypeScale {
  def apply(s: String): PhenotypeScale = s.toLowerCase match {
    case "linear" => Linear
    case "logistic" => Logistic
    case "count" => Count
    case "poisson" => Count
    case "rank" | "nonparametric" => Rank
  }
}

object PlinkPhenotypeCoding {
  def convertToBooleanNonMissing(d: Double): Boolean = d match {
    case x if x.isNaN => throw new RuntimeException("this should have been filtered before")
    case 1.0 => false
    case 2.0 => true
  }

  def convertToBoolean(d: Option[Double]): Option[Boolean] = d.map(_ match {
    case Double.NaN => None
    case 1.0 => Some(false)
    case 2.0 => Some(true)
  }).flatten
}
