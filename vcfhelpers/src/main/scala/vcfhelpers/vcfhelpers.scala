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
import htsjdk.variant.vcf.{ VCFFileReader, VCFHeader }
import htsjdk.variant.variantcontext.VariantContext
import scala.collection.JavaConversions._
import java.io.File
import mybiotools.stringstore._
import mybiotools.gwascommons._
import mybiotools.gwascommons.genotypedata._

import htsjdk.tribble.readers.LineIterator

case class MinimalRepresentation(ref: String, alt: String, position: Int)

/** Invalidates the source! */
class SourceLineIterator(source: scala.io.Source) extends LineIterator with Iterator[String] {

  def close = source.close

  private val lineIter = source.getLines

  private var nextItem: Option[String] = if (lineIter.hasNext) Some(lineIter.next) else None

  def hasNext = nextItem.isDefined

  def next = {
    val r = nextItem
    nextItem = if (lineIter.hasNext) Some(lineIter.next) else None
    r.get
  }

  def peek = nextItem.get

  def remove = throw new UnsupportedOperationException

}

object VCFHelpers {

  def processVariantContext(vcontext: VariantContext) = {

    val genotypes: Array[Float] = vcontext.getGenotypes.iterator.map { genotype =>
      if (genotype.isNoCall) MissingValue
      else if (genotype.isHet) 1.0f
      else if (genotype.isHomRef) 0.0f
      else if (genotype.isHomVar) 2.0f
      else throw new RuntimeException("not nocall het hom hom, then what?")
    }.toIndexedSeq.toArray

    val name = getID(vcontext)
    val al1 = StringStore(vcontext.getAlternateAllele(0).getDisplayString)
    val al2 = StringStore(vcontext.getReference.getDisplayString)
    val sumAl1Dosage = (vcontext.getHomVarCount * 2 + vcontext.getHetCount).toFloat
    val sumAl2Dosage = (vcontext.getHomRefCount * 2 + vcontext.getHetCount).toFloat

    val maf = (vcontext.getHomVarCount * 2 + vcontext.getHetCount).toDouble / (vcontext.getNSamples - vcontext.getNoCallCount)

    val pdfrs = PDosageFileRowSummary(
      snpName = name,
      al1 = al1,
      al2 = al2,
      sumAl1Dosage = sumAl1Dosage,
      sumAl2Dosage = sumAl2Dosage,
      count = vcontext.getNSamples - vcontext.getNoCallCount,
      maf = maf.toFloat,
      missingCount = vcontext.getNoCallCount
    )

    (pdfrs, genotypes)
  }

  def getSNPMajorQuery(reader: VCFFileReader, map: GenomicMap) = new SNPMajorQuery {
    val missingValue = MissingValue
    val individuals = getIndividuals(reader)
    val allSNPs = map.toSeq.sortBy(_._2).map(_._1).toVector
    def query(snp: String8) = {
      map.get(snp).flatMap { genomicLocation =>

        val iter = reader.query(genomicLocation.chromosome, genomicLocation.basePairPosition, genomicLocation.basePairPosition)
        val list = iter.toList

        list.headOption.map(processVariantContext)
      }

    }
  }

  def readVCF(source: scala.io.Source): Tuple2[VCFHeader, Iterator[VariantContext]] = {
    val codec = new htsjdk.variant.vcf.VCFCodec
    val lineiterator = new SourceLineIterator(source)
    val header: VCFHeader = codec.readActualHeader(lineiterator).asInstanceOf[VCFHeader]
    val iter = lineiterator.map { line =>
      codec.decode(line).asInstanceOf[VariantContext]
    }
    (header, iter)
  }

  def getSNPMajorIterators(vcf: File, subset: SNPMajorSubset, filterMarkerNames: Set[String]): SNPMajorIterators = {
    val reader = openVCF(vcf, None)
    getSNPMajorIterators(reader, subset, filterMarkerNames)
  }
  def getSNPMajorIterators(reader: VCFFileReader, subset: SNPMajorSubset, filterMarkerNames: Set[String]): SNPMajorIterators = {
    val inds = getIndividuals(reader)
    val snps = reader.iterator.zipWithIndex.filter(x => subset.contains(x._2) && (filterMarkerNames.isEmpty || filterMarkerNames.contains(getID(x._1)))).map(_._1).map(processVariantContext)

    SNPMajorIterators(
      individuals = inds,
      snpIterator = snps,
      duplicatedSNPs = Set(),
      duplicatedIndividuals = Set(),
      missingValue = MissingValue
    )
  }

  val MissingValue = -9f

  def getID(vcontext: VariantContext) = {
    val chr = vcontext.getChr
    val bp = vcontext.getStart
    StringStore(if (vcontext.hasID) vcontext.getID else chr + ":" + bp.toString)
  }

  def getMAF[T <: HasName with HasGenomicLocation](
    file: String,
    snps: Vector[T]
  ): Option[Vector[(String8, Double)]] = {
    val reader = openVCF(new File(file), None)
    val prefix = getPrefix(reader)
    Some(snps.flatMap { snp =>
      reader.query(prefix + snp.genomicLocation.chromosome, snp.genomicLocation.basePairPosition, snp.genomicLocation.basePairPosition).filter { vc =>
        vc.getStart == snp.genomicLocation.basePairPosition
      }.map(vc => getID(vc) -> (vc.getHomVarCount * 2 + vc.getHetCount).toDouble / (vc.getNSamples - vc.getNoCallCount))
    })
  }

  def openVCF(file: File, maybeIdx: Option[File]): VCFFileReader = {

    def putBesides(bam: File, bai: File): (File, File) = {
      def besides(a: File, b: File): Boolean = a.getCanonicalPath.dropRight(3) == b.getCanonicalPath.dropRight(3)

      if (!besides(bam, bai)) {
        val tmp = mybiotools.TempFile.createTempFile("vcfbesideidx")
        val bamfilepath = java.nio.file.Paths.get(bam.getCanonicalPath)
        val bamfilelinkpath = java.nio.file.Paths.get(tmp.getCanonicalPath + (".vcf"))
        java.nio.file.Files.createSymbolicLink(bamfilelinkpath, bamfilepath)

        val idxfilepath = java.nio.file.Paths.get(bai.getCanonicalPath)
        val idxfilelinkpath = java.nio.file.Paths.get(tmp.getCanonicalPath + (".vcf.idx"))
        java.nio.file.Files.createSymbolicLink(idxfilelinkpath, idxfilepath)
        (new File(tmp.getCanonicalPath + (".vcf")), new File(tmp.getCanonicalPath + (".vcf.idx")))
      } else (bam, bai)
    }

    val idx: File = maybeIdx.getOrElse {
      val idxObj = htsjdk.tribble.index.IndexFactory.createLinearIndex(file, new htsjdk.variant.vcf.VCFCodec)
      val tmp = mybiotools.TempFile.createTempFile(".idx")
      htsjdk.tribble.index.IndexFactory.writeIndex(idxObj, tmp)
      tmp
    }

    val (vcf, vcfidx) = putBesides(file, idx)

    new VCFFileReader(vcf)

  }

  def getGenomicMap(vcf: VCFFileReader): GenomicMap = {
    val iter = vcf.iterator
    val mmap = collection.mutable.AnyRefMap[String8, GenomicLocation]()
    iter.foreach { vcontext =>
      val chr = vcontext.getChr
      val bp = vcontext.getStart
      val name = getID(vcontext)
      mmap += name -> GenomicLocation(bp, chr)
    }
    iter.close
    mmap
  }

  def getPrefix(vcf: VCFFileReader): String = {
    val it = vcf.iterator
    val prefix = it.next.getChr.take(3)
    val x = if (prefix.toLowerCase == "chr") prefix else ""
    it.close
    x
  }

  def getSNPs[T <: HasGenomicLocation](vcf: VCFFileReader, snps: Vector[T]): Vector[IndexedSeq[Float]] = snps.map { snp =>

    val prefix = getPrefix(vcf)

    val iter = vcf.query(prefix + snp.genomicLocation.chromosome, snp.genomicLocation.basePairPosition, snp.genomicLocation.basePairPosition)
    val list = iter.toList

    assert(list.size == 1, list)

    val variantContext = list.head

    val ar: IndexedSeq[Float] = variantContext.getGenotypes.iterator.map { genotype =>
      if (genotype.isNoCall) MissingValue
      else if (genotype.isHet) 1.0f
      else if (genotype.isHomRef) 0.0f
      else if (genotype.isHomVar) 2.0f
      else throw new RuntimeException("not nocall het hom hom, then what?")
    }.toIndexedSeq

    iter.close

    ar

  }

  def getIndividuals(source: scala.io.Source): IndexedSeq[Individual] = readVCF(source)._1.getGenotypeSamples.map(s => Individual(s)).toIndexedSeq

  def getIndividuals(vcf: VCFFileReader): IndexedSeq[Individual] = vcf.getFileHeader.getGenotypeSamples.map(s => Individual(s)).toIndexedSeq
}