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

package dispensability

import org.scalatest.FunSpec
import org.scalatest.Matchers
import java.io.File
import mybiotools.stringstore._
import mybiotools.gwascommons._

class InputSpec extends FunSpec with Matchers {
  val nutvarfile = new File(getClass.getResource("/").getPath + "/nutvarout.txt")
  val nutvarfile2 = new File(getClass.getResource("/").getPath + "/nutvarout2.txt")
  val vcffile = new File(getClass.getResource("/").getPath + "/tmp.vcf")
  val vcffile2 = new File(getClass.getResource("/").getPath + "/tmp2.vcf")
  val vcffile3 = new File(getClass.getResource("/").getPath + "/tmp3.vcf")
  val vcffileexac = new File(getClass.getResource("/").getPath + "/tmpexac.vcf")

  describe("nutvar input regression tests") {

    it("nutvar") {
      Input.readNutVarOutput(io.Source.fromFile(nutvarfile), Map(StringStore("POTEH") -> Gene(StringStore("POTEH")), StringStore("CCT8L2") -> Gene(StringStore("CCT8L2")))).toString should equal(Map(VariantKey(GenomicLocation(16287784, "22"), StringStore("C"), StringStore("T")) -> Map(Gene(StringStore("POTEH")) -> NutVarAnnotation(GenomicLocation(16287784, "22"), Gene(StringStore("POTEH")), Some(0.088641713), Some(0.748454930153234), Some(0.066344327112078), None, None, Some(90.4040972534894), Some(12.1262458471761), Some(44.6333687566419), None, None, 1638, 93.7728937728938, None)), VariantKey(GenomicLocation(17072347, "22"), StringStore("C"), StringStore("T")) -> Map(Gene(StringStore("CCT8L2")) -> NutVarAnnotation(GenomicLocation(17072347, "22"), Gene(StringStore("CCT8L2")), None, Some(0.465784205310653), None, Some(0.314179756), Some(0.196605108256458), Some(28.6976587122918), None, None, Some(35.7178217821782), Some(43.7924316665683), 1674.0, 34.647550776583, None))).toString)
    }

    it("vcf 1 readgenotypes") {

      val events = Input.readGenotypeCountsFromVCF(io.Source.fromFile(vcffile))(Input.extractGenotypeCountFromGeneralVCF).toList
      events should equal(List(VariantWithoutAnnotationImpl(GenomicLocation(16287649, "22"), s8"G", s8"A", 1012, 79, 1), VariantWithoutAnnotationImpl(GenomicLocation(16287784, "22"), s8"C", s8"T", 1079, 13, 0)))

    }
    it("vcf 3 readgenotypes") {

      val events = Input.readGenotypeCountsFromVCF(io.Source.fromFile(vcffile3))(Input.extractGenotypeCountFromGeneralVCF).toList
      events should equal(List(
        VariantWithoutAnnotationImpl(GenomicLocation(16287649, "22"), s8"G", s8"A", 1, 2, 0),
        VariantWithoutAnnotationImpl(GenomicLocation(16287649, "22"), s8"G", s8"T", 1, 2, 0)
      ))

    }

  }

}