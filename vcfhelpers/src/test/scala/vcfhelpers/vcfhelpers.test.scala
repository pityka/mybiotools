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
package vcfhelpers

import mybiotools.gwascommons._
import java.io.File
import org.scalatest.FunSpec
import org.scalatest.Matchers
import collection.JavaConversions._

class VCFHelpersSpec extends FunSpec with Matchers {

  case class A(genomicLocation: GenomicLocation) extends HasGenomicLocation

  describe("vcf") {
    it("open unindexed") {
      val p = new File(getClass.getResource("/").getPath + "haplotypecaller.from.chr22:50316926.vcf")
      val reader = VCFHelpers.openVCF(p, None)
      reader.getFileHeader.getGenotypeSamples.size should be(1)
    }
    it("read individuals") {
      val p = new File(getClass.getResource("/").getPath + "haplotypecaller.from.chr22:50316926.vcf")
      val reader = VCFHelpers.openVCF(p, None)
      VCFHelpers.getIndividuals(reader) should be(List(Individual("002")))
    }
    it("read snps") {
      val p = new File(getClass.getResource("/").getPath + "haplotypecaller.from.chr22:50316926.vcf")
      val reader = VCFHelpers.openVCF(p, None)
      VCFHelpers.getSNPs(reader, Vector(
        A(GenomicLocation(17063359, "chr22")),
        A(GenomicLocation(17280822, "chr22"))
      )) should equal(Vector(IndexedSeq(2.0f), IndexedSeq(1.0f)))
    }
  }

}