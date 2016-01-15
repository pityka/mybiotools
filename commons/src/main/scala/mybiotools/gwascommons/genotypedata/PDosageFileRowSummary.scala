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

package mybiotools.gwascommons.genotypedata

import mybiotools._
import mybiotools.stringstore._
import mybiotools.gwascommons._

case class PDosageFileRowSummary(
    snpName: String8,
    al1: String8, //  first allele in file
    al2: String8,
    sumAl1Dosage: Float,
    sumAl2Dosage: Float,
    count: Int,
    maf: Float, // real minor (can be al1 or al2)
    missingCount: Int
) extends HasName {

  def name = snpName

  def toLine = snpName + " " + al1 + " " + al2 + " " + sumAl1Dosage + " " + sumAl2Dosage + " " + count + " " + maf

  def missingRate = missingCount.toDouble / (count + missingCount)

  def toLocusDataWithGenomicLocation(gl: Option[GenomicLocation]) = {
    val a1 = SimpleAllele.makeSingleton(al1)
    val a2 = SimpleAllele.makeSingleton(al2)
    LocusData(
      name = snpName,
      genomicLocation = gl,
      alleles = Seq(a1, a2).sorted,
      alleleFrequencies = List((a1, sumAl1Dosage / (2.0 * count)), (a2, sumAl2Dosage / (2.0 * count))).sorted
    )
  }
}

object PDosageFileRowSummary {

  def apply(s: String, missingValue: Float = -9f): PDosageFileRowSummary = {
    val spl = fastSplitSeparatorIterator(s, ' ')
    val name = StringStore(new java.lang.String(spl.next))
    val al1 = StringStore(new java.lang.String(spl.next))
    val al2 = StringStore(new java.lang.String(spl.next))
    apply(name, al1, al2, spl.map(_.toFloat).toSeq, missingValue)
  }

  def apply(name: String8, al1: String8, al2: String8, list: Seq[Float], missingValue: Float): PDosageFileRowSummary = {

    var sumal1 = 0.0
    var sumal2 = 0.0
    var count = 0
    var missing = 0
    list.foreach { elem =>
      if (elem != missingValue && !elem.isNaN) {
        sumal1 = sumal1 + elem
        sumal2 = sumal2 + (2.0 - elem)
        count += 1
      } else {
        missing += 1
      }
    }
    val af1 = ((sumal1) / (2.0 * (count))).toFloat

    PDosageFileRowSummary(name, (al1), (al2), sumal1.toFloat, sumal2.toFloat, count, (if (af1 <= 0.5) af1 else 1.0f - af1), missing)
  }

  def apply(name: String, al1: String, al2: String, list: Seq[Float], missingValue: Float): PDosageFileRowSummary =
    this(StringStore(name), StringStore(al1), StringStore(al2), list, missingValue)

}