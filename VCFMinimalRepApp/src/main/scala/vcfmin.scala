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

import mybiotools._
import vcfhelpers._
import org.broadinstitute.variant.vcf._
import org.broadinstitute.variant.variantcontext._
import org.broadinstitute.variant.variantcontext.writer._
import scala.collection.JavaConversions._

object VCFMinimalRepApp extends App {



  val source = io.Source.stdin

   val (vcfheader, vcfiterator) = vcfhelpers.VCFHelpers.readVCF(source)
   val refdict = vcfheader.getSequenceDictionary

  val outputstream = System.out

   val vcwriter = VariantContextWriterFactory.create(outputstream, refdict, java.util.EnumSet.noneOf(classOf[org.broadinstitute.variant.variantcontext.writer.Options]))
      vcwriter.writeHeader(vcfheader)

    vcfiterator.foreach { vcontext =>
      val chr = vcontext.getChr
      val bp1 = vcontext.getStart
      val ref1 = vcontext.getReference.getBaseString
      vcontext.getAlternateAlleles.toIndexedSeq.map(_.getBaseString).foreach { alt1 =>
        val MinimalRepresentation(ref,alt,bp) = vcfhelpers.minimalRepresentation(ref1, alt1, bp1)        
        val alRef = Allele.create(ref,true)
        val alAlt = Allele.create(alt,false)

        val genotypes = vcontext.getGenotypes.iterator.toList.map{ gt =>
        	val count = gt.countAllele(Allele.create(alt1,false))
        	val alleles = count match {
        		case 0 => alRef :: alRef ::Nil
        		case 1 => alRef :: alAlt :: Nil
        		case 2 => alAlt :: alAlt:: Nil
        	}
        	new GenotypeBuilder().alleles(alleles).name(gt.getSampleName).make
        }




        val v2 = (new VariantContextBuilder())        
        .start(bp)
        .chr(chr)
        .alleles(alRef::alAlt::Nil)
        .computeEndFromAlleles(List(alRef,alAlt),bp)
        .genotypes(genotypes:_*)
        .make
        vcwriter.add(v2)
      }
   
  }
     vcwriter.close
}