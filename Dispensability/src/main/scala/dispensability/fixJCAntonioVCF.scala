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

import mybiotools.config.Config.configInstance
import mybiotools._
import java.io.File
import htsjdk.variant.variantcontext.writer.VariantContextWriterFactory
import htsjdk.samtools.SAMSequenceDictionary
import htsjdk.samtools.SAMSequenceRecord
import scala.collection.JavaConversions._
import htsjdk.variant.vcf.{ VCFHeader, VCFHeaderLine }
import htsjdk.variant.variantcontext.VariantContextBuilder
import htsjdk.variant.variantcontext.GenotypeBuilder
import htsjdk.variant.variantcontext._
import htsjdk.variant.vcf._

object FixJCAntonioVCF extends App {
  val inputs = openSource(configInstance.getString("files"))(_.getLines.map(x => new File(x)).toList)

  val ncbi36chromosomelengths = Map(
    1 -> 247249719,
    2 -> 242951149,
    3 -> 199501827,
    4 -> 191273063,
    5 -> 180857866,
    6 -> 170899992,
    7 -> 158821424,
    8 -> 146274826,
    9 -> 140273252,
    10 -> 135374737,
    11 -> 134452384,
    12 -> 132349534,
    13 -> 114142980,
    14 -> 106368585,
    15 -> 100338915,
    16 -> 88827254,
    17 -> 78774742,
    18 -> 76117153,
    19 -> 63811651,
    20 -> 62435964,
    21 -> 46944323,
    22 -> 49691432,
    23 -> 154913754,
    24 -> 57772954

  )

  inputs.map { infile =>
    println("Processing " + infile)
    openZippedFileOutputStream(new File(infile.getAbsolutePath + ".standard.vcf.gz")) { outputstream =>
      val refdict = new SAMSequenceDictionary((1 to 22 map { i =>
        new SAMSequenceRecord(i.toString, ncbi36chromosomelengths(i))
      }).toList)

      val header = new VCFHeader(Set[VCFHeaderLine](), List("JCV"))
      header.setSequenceDictionary(refdict)

      val gtformatline = new VCFFormatHeaderLine("GT", 1,
        VCFHeaderLineType.String,
        "Genotype")

      header.addMetaDataLine(gtformatline)

      val vcwriter = VariantContextWriterFactory.create(outputstream, refdict, java.util.EnumSet.noneOf(classOf[htsjdk.variant.variantcontext.writer.Options]))
      vcwriter.writeHeader(header)
      openSource(infile.getAbsolutePath)(_.getLines.drop(1).foreach { line =>
        val spl = fastSplit1WideSeparator(line, '\t')
        val chr = spl(0)
        val pos = spl(1).toLong
        val id = spl(2)
        val ref = spl(3)
        val alt = spl(4)
        val qual = spl(5).toDouble / (-10.0)
        val filter = spl(6)
        val info = spl(7)

        val genotype = if (info == "heterozygous_SNP") new GenotypeBuilder("JCV", List(Allele.create(ref, true), Allele.create(alt, false))).make else new GenotypeBuilder("JCV", List(Allele.create(alt, false), Allele.create(alt, false))).make

        val vc = new VariantContextBuilder().alleles(ref, alt).chr(chr).passFilters.start(pos).genotypes(genotype).computeEndFromAlleles(List(Allele.create(ref, true), Allele.create(alt, false)), pos.toInt).log10PError(qual).id(id).make

        vcwriter.add(vc)

      })

      vcwriter.close

    }
  }

}