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
package vcfhelpers.parser

import mybiotools.gwascommons._
import java.io.File
import org.scalatest.FunSpec
import org.scalatest.Matchers
import collection.JavaConversions._
import mybiotools._
import htsjdk.variant.variantcontext.VariantContext

class VCFParserSpec extends FunSpec with Matchers {

  describe("compare to variantcontext") {
    it("CEU.exon.2010_06.genotypes.vcf.gz") {
      val file = new File(getClass.getResource("/").getPath + "CEU.exon.2010_06.genotypes.vcf.gz")
      val vcontext = openSource(file)(s => vcfhelpers.VCFHelpers.readVCF(s)._2.toList)
      val my = openSource(file)(s => VCFParser.parse(s, 10).iter.toList)

      def compare(my: VCFDataWithoutGenotypes, vc: VariantContext): Unit = {
        assert(vc.getChr == my.chromosome)
        assert(vc.getStart == my.start)
        assert(vc.getReference.getBaseString == my.reference)
        assert(vc.getAlternateAlleles.map(_.getBaseString).toVector == my.alternatives)
        assert(vc.getHomRefCount == my.homRefCount)
        assert(vc.getHomVarCount == my.homVarCount)
        assert(vc.getHetCount == my.genotypeCounts(0.toByte, 1.toByte))

        assert(vc.getHetCount == my.hetCount)
        assert(vc.getHomVarCount == my.genotypeCounts(1.toByte, 1.toByte))
        assert(vc.getHomRefCount == my.genotypeCounts(0.toByte, 0.toByte))
        assert(vc.getAttribute("AN").asInstanceOf[String].toInt == my.getAttribute("AN").get.asInstanceOf[InfoSingleValue].value.toInt)
      }

      vcontext zip my foreach { x => compare(x._2, x._1) }

    }
  }

}