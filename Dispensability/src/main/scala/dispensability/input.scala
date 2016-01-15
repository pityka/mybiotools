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

import mybiotools.gwascommons._
import scala.collection.JavaConversions._
import mybiotools.stringstore._
import htsjdk.variant.variantcontext.writer.VariantContextWriterFactory
import htsjdk.variant.variantcontext.VariantContext
import java.io.File
import mybiotools.gwascommons.DiscreteGenotypes._
import vcfhelpers._
import mybiotools.eq._
import scala.util.Try

import mybiotools._
import mybiotools.intervaltree._
import mybiotools.gwascommons._

import vcfhelpers.parser._

sealed trait AnnotationFormat

case object NutVar extends AnnotationFormat
case object SnpEffVCF extends AnnotationFormat

case class VariantKey(location: GenomicLocation, ref: String8, alt: String8)
object VariantKey {
  def apply(location: GenomicLocation, ref: String, alt: String): VariantKey = VariantKey(location, StringStore(ref), StringStore(alt))
}

object Input {

  def readPerGeneVariable(file: scala.io.Source): Map[String8, Double] = file.getLines.map { l =>
    val spl = fastSplitSetSeparator(l, Set(' ', '\t'))
    StringStore(spl(0)) -> spl(1).toDouble
  }.toMap

  def readSamochaSupplement(file: scala.io.Source): Set[Gene] = {
    val iter = file.getLines
    val header = fastSplitSetSeparator(iter.next, Set(' ', '\t'))
    val nameidx = header.indexOf("gene")
    val probsynidx = header.indexOf("syn")
    val probstopidx = header.indexOf("non")
    val probfsidx = header.indexOf("frameshift")

    iter.map { l =>
      val spl = fastSplitSetSeparator(l, Set(' ', '\t'))
      val name = StringStore(spl(nameidx))
      val prob_stop = math.pow(10.0, spl(probstopidx).replaceFirst(",", ".").toDouble)
      val prob_fs = math.pow(10.0, spl(probfsidx).replaceFirst(",", ".").toDouble)
      val prob_syn = math.pow(10.0, spl(probsynidx).replaceFirst(",", ".").toDouble)

      Gene(
        hgnc = (name),
        probFS = prob_fs, probStop = prob_stop, probSplice = 0.0, probSynonym = prob_syn
      )

    }.toSet
  }

  def readGMT(source: scala.io.Source, allgenes: Map[String8, Gene]): Map[GeneSetKey, Set[Gene]] = source.getLines.map { line =>
    val splitted = fastSplit1WideSeparator(line, '\t')
    val name = StringStore(splitted.head.replaceAll("/", ""))
    // val desc = new String(splitted(1))
    val rest = splitted.drop(2).map(x => StringStore(x)).toSet

    GeneSetKey(name) -> rest.flatMap(name => allgenes.get(name))

  }.toMap

  type AnnotationIndex[T <: Annotation[T]] = collection.Map[VariantKey, Map[Gene, T]]

  object NutVarHeader {
    val BaseNCBI37 = "baseNCBI37"
    val Alleles = "Alleles_REF>ALT"
    val Genename = "GeneNameCCDS"
    val LongestCCDSLength = "LongestCCDSLength"
    val PercentageLongestCCDSAffected = "PercentageLongestCCDSAffected"
    val MacArthur2012 = "MaCArthur2012"
    val Nutvar_probPathogenic = "NUTvar_ProbabilityPathogenic"
    val Nutvar_probPathogenicXmacArthur2012 = "NUTvar_ProbabilityPathogenicxMaCArthur2012"
    val Rvis = "RVIS"
    val Nutvar_probPathogenicXrvis = "NUTvar_ProbabilityPathogenicxRVIS"
    val GEUVADIS_1000G_AverageGeneZscoreAll = "GEUVADIS_1000G_AverageGeneZscoreAll"

    val NutVar_probPathogenicRankPercentile = "NUTvar_ProbabilityPathogenic_RankPercentile"
    val MaCArthur2012_RankPercentile = "MaCArthur2012_RankPercentile"
    val NUTvar_ProbabilityPathogenicxMaCArthur2012_RankPercentile = "NUTvar_ProbabilityPathogenicxMaCArthur2012_RankPercentile"
    val RVIS_RankPercentile = "RVIS_RankPercentile"
    val NUTvar_ProbabilityPathogenicxRVIS_RankPercentile = "NUTvar_ProbabilityPathogenicxRVIS_RankPercentile"
  }

  /**
   *
   * @return iterator(vkey,effect,gene,aapos,line)
   */
  def extractVariantKeysFromSNPEff(source: scala.io.Source): Iterator[(VariantKey, String, String, Int, String)] = {
    val lofs = Set("STOP_GAINED", "SPLICE_SITE_DONOR", "SPLICE_SITE_ACCEPTOR", "FRAME_SHIFT")
    val snpEffRegexp = new scala.util.matching.Regex("""([^\|]*)\(([^\|]*)\|([^\|]*)\|([^\|]*)\|([^\|]*)\|([^\|]*)\|([^\|]*)\|([^\|]*)\|([^\|]*)\|([^\|]*)\|([^\|]*)\|([^\|]*)(?:\|([^\|]*))?(?:\|([^\|]*))?\)""", "Effect", "Effect_Impact", "Functional_Class", "Codon_Change", "Amino_Acid_change", "Amino_Acid_length", "Gene_Name", "Gene_BioType", "Coding", "Gene", "Exon", "Genotype_Number", "err", "warn")

    source.getLines.dropWhile(_.startsWith("#")).flatMap { line =>
      val spl = fastSplit1WideSeparatorIterator(line, '\t')
      val chrom = spl.next
      val pos = spl.next.toInt
      val id = spl.next
      val ref = spl.next
      val alts = fastSplit1WideSeparatorIterator(spl.next, ',').toIndexedSeq
      val qual = spl.next
      val filter = spl.next
      val splinfo = fastSplit1WideSeparatorIterator(spl.next, ';')
      val snpeff: Iterator[String] = splinfo.find(_.startsWith("EFF=")).iterator.flatMap(s => fastSplit1WideSeparatorIterator(s, ','))

      snpeff.map { effect =>
        effect match {
          case snpEffRegexp(effect, impact, functclass, codonchange, aachange, aalength, genename, genebiotype, coding, gene, exon, genotypenumber, warn, error) => (lofs.contains(effect), effect, alts(genotypenumber.toInt - 1), genename, aachange.drop(1).dropRight(1).toInt)
        }
      }.filter(_._1).map { x =>
        val gl = GenomicLocation(pos, chrom)
        val vkey = VariantKey(gl, StringStore(ref), StringStore(x._3))
        val gene = x._4
        val effect = x._2
        val aapos = x._5
        (vkey, effect, gene, aapos, line)
      }

      // EFF= Effect ( Effect_Impact | Functional_Class | Codon_Change | Amino_Acid_Change| Amino_Acid_Length | Gene_Name | Gene_BioType | Gene_Coding | Gene_ID | Exon_Rank  | Genotype_Number [ | ERRORS | WARNINGS ] )

    }
  }

  def readVariantKeysFromSnpEffAnnotationFile(source: scala.io.Source): Set[VariantKey] = source.getLines.map { line =>
    val spl = fastSplitSetSeparator(line, Set(' ', '\t'))
    val chr = spl(0)
    val bp1 = spl(1)
    val ref1 = spl(3)
    val alt1 = spl(4)
    val effect = spl(5)
    val gl = GenomicLocation(bp1.toInt, chr)

    VariantKey(gl, StringStore(ref1), StringStore(alt1))
  }.toSet

  def readAntonioSnpEffFile(source: scala.io.Source, genelist: Map[String8, Gene]): AnnotationIndex[SnpEffAnnotation] = source.getLines.flatMap { line =>
    val spl = fastSplitSetSeparator(line, Set(' ', '\t'))
    val chr = spl(0)
    val bp1 = spl(1)
    val ref1 = spl(3)
    val alt1 = spl(4)
    val effect = spl(5)
    val gene = spl(6)
    val gl = GenomicLocation(bp1.toInt, chr)
    val gene1 = genelist.get(StringStore(gene))
    gene1.map { gene =>
      VariantKey(gl, StringStore(ref1), StringStore(alt1)) -> Map(gene -> SnpEffAnnotation(gl, gene, effect, 1))
    }
  }.toMap

  def readNutVarOutputKeys(source: scala.io.Source): Set[VariantKey] = {

    import NutVarHeader._
    val lineiter = source.getLines
    val header = fastSplit1WideSeparator(lineiter.next, '\t')
    val idx_base = header.indexOf(BaseNCBI37)
    val idx_alleles = header.indexOf(Alleles)

    lineiter.map { line =>
      val spl = fastSplit1WideSeparator(line, '\t')
      val chr = spl(idx_base).split(":")(0)
      val bp = spl(idx_base).split(":")(1).toInt
      val ref = spl(idx_alleles).split(">")(0)
      val alt = {
        val s = spl(idx_alleles)
        val index = s.indexOf(">")
        s.substring(index + 1, s.size)
      }

      val gl = GenomicLocation(bp, chr)

      VariantKey(gl, StringStore(ref), StringStore(alt))

    }.toSet

  }

  def readSnpEffVCF(source: scala.io.Source, genes: Map[String8, Gene]): AnnotationIndex[SnpEffAnnotation] =
    extractVariantKeysFromSNPEff(source).flatMap {
      case (vkey, effect, gene, aapos, _) =>
        genes.get(StringStore(gene)).map { gene =>
          vkey -> SnpEffAnnotation(vkey.location, gene, effect, aapos)
        }
    }.toList.groupBy(_._1).map(x => x._1 -> x._2.map(y => y._2.gene -> y._2).toMap)

  def readSnpEffVCFs(nutvars: List[File], totalGeneList: Map[String8, Gene]) = nutvars.map(f => openSource(f.getAbsolutePath)(s => Input.readSnpEffVCF(s, totalGeneList))).reduce((x, y) => addMaps(x, y)((z, zz) => z ++ zz))

  def readAntonioSnpEffFiles(nutvars: List[File], totalGeneList: Map[String8, Gene]) = nutvars.map(f => openSource(f.getAbsolutePath)(s => Input.readAntonioSnpEffFile(s, totalGeneList))).reduce((x, y) => addMaps(x, y)((z, zz) => z ++ zz))

  def readNutVarOutput(source: scala.io.Source, genes: Map[String8, Gene]): AnnotationIndex[NutVarAnnotation] = {

    def escapeNaN(d: Option[Double]) = d match {
      case Some(x) if x.isNaN => None
      case x => x
    }

    import NutVarHeader._
    val lineiter = source.getLines
    val header = fastSplit1WideSeparator(lineiter.next, '\t')
    val idx_base = header.indexOf(BaseNCBI37)
    val idx_alleles = header.indexOf(Alleles)
    val idx_name = header.indexOf(Genename)
    val idx_longestccds = header.indexOf(LongestCCDSLength)
    val idx_percentageLongestCCDS = header.indexOf(PercentageLongestCCDSAffected)
    val idx_mcarthur = header.indexOf(MacArthur2012)
    val idx_nutprob = header.indexOf(Nutvar_probPathogenic)
    val idx_nutprobxmcarthur = header.indexOf(Nutvar_probPathogenicXmacArthur2012)
    val idx_rvis = header.indexOf(Rvis)
    val idx_nutvarprobxrvis = header.indexOf(Nutvar_probPathogenicXrvis)
    val idx_nutvarpercentile = header.indexOf(NutVar_probPathogenicRankPercentile)
    val idx_mcarthurpercentile = header.indexOf(MaCArthur2012_RankPercentile)
    val idx_nutvarxmcarthurpercentile = header.indexOf(NUTvar_ProbabilityPathogenicxMaCArthur2012_RankPercentile)
    val idx_rvispercentile = header.indexOf(RVIS_RankPercentile)
    val idx_nutvarxrvispercentile = header.indexOf(NUTvar_ProbabilityPathogenicxRVIS_RankPercentile)
    val idx_GEUVADIS_1000G_AverageGeneZscoreAll = header.indexOf(GEUVADIS_1000G_AverageGeneZscoreAll)

    val mmap = scala.collection.mutable.AnyRefMap[VariantKey, Map[Gene, NutVarAnnotation]]()

    lineiter.foreach { line =>
      val spl = fastSplit1WideSeparator(line, '\t')
      val chr = spl(idx_base).split(":")(0)
      val bp = spl(idx_base).split(":")(1).toInt
      val ref = spl(idx_alleles).split(">")(0)
      val alt = {
        val s = spl(idx_alleles)
        val index = s.indexOf(">")
        s.substring(index + 1, s.size)
      }
      val genename = StringStore(spl(idx_name))
      genes.get(genename).foreach { gene =>
        val gl = GenomicLocation(bp, chr)
        val nutvarannotation = NutVarAnnotation(
          location = gl,
          gene = gene,
          MaCArthur2012 = escapeNaN(Try(spl(idx_mcarthur).toDouble).toOption),
          NUTvar_ProbabilityPathogenic = escapeNaN(Try(spl(idx_nutprob).toDouble).toOption),
          NUTvar_ProbabilityPathogenicxMaCArthur2012 = escapeNaN(Try(spl(idx_nutprobxmcarthur).toDouble).toOption),
          RVIS = escapeNaN(Try(spl(idx_rvis).toDouble).toOption),
          NUTvar_ProbabilityPathogenicxRVIS = escapeNaN(Try(spl(idx_nutvarprobxrvis).toDouble).toOption),
          NUTvar_ProbabilityPathogenic_RankPercentile = escapeNaN(Try(spl(idx_nutvarpercentile).toDouble).toOption),
          MaCArthur2012_RankPercentile = escapeNaN(Try(spl(idx_mcarthurpercentile).toDouble).toOption),
          NUTvar_ProbabilityPathogenicxMaCArthur2012_RankPercentile = escapeNaN(Try(spl(idx_nutvarxmcarthurpercentile).toDouble).toOption),
          RVIS_RankPercentile = escapeNaN(Try(spl(idx_rvispercentile).toDouble).toOption),
          NUTvar_ProbabilityPathogenicxRVIS_RankPercentile = escapeNaN(Try(spl(idx_nutvarxrvispercentile).toDouble).toOption),
          PercentageLongestCCDSAffected = spl(idx_percentageLongestCCDS).toDouble,
          LongestCCDSLength = spl(idx_longestccds).toDouble,
          GEUVADIS_1000G_AverageGeneZscoreAll = escapeNaN(Try(spl(idx_GEUVADIS_1000G_AverageGeneZscoreAll).toDouble).toOption)

        )

        val vkey = VariantKey(gl, StringStore(ref), StringStore(alt))

        mmap.get(vkey) match {
          case None => mmap.update(vkey, Map(gene -> nutvarannotation))
          case Some(x) => mmap.update(vkey, x + (gene -> nutvarannotation))
        }

      }
    }
    mmap

  }

  def isGONLVCF(source: scala.io.Source): Boolean = {
    val (vcfheader, vcfiterator) = vcfhelpers.VCFHelpers.readVCF(source)
    if (vcfiterator.hasNext) {
      val vc = vcfiterator.next
      !vc.hasGenotypes && !vc.hasAttribute("GTC") && !vc.hasAttribute("AC_Adj") && vc.hasAttribute("AF") && vc.hasAttribute("AC") && vc.hasAttribute("AN")
    } else false
  }
  def isExACVCF(source: scala.io.Source): Boolean = {
    val (vcfheader, vcfiterator) = vcfhelpers.VCFHelpers.readVCF(source)
    if (vcfiterator.hasNext) {
      val vc = vcfiterator.next
      !vc.hasGenotypes && vc.hasAttribute("AN") && vc.hasAttribute("AC_Adj") && vc.hasAttribute("AC_Het")
    } else false
  }

  def isEVSVCF(source: scala.io.Source): Boolean = {
    val (vcfheader, vcfiterator) = vcfhelpers.VCFHelpers.readVCF(source)
    if (vcfiterator.hasNext) {
      val vc = vcfiterator.next
      !vc.hasGenotypes && vc.hasAttribute("GTS") && vc.hasAttribute("GTC")
    } else false
  }

  def extractGenotypeCountFromGONL(vcontext: VariantContext, ref: String, alt: String, altidx: Int) = {

    val afs = if (vcontext.getAlternateAlleles.size > 1) vcontext.getAttribute("AF", "").asInstanceOf[java.util.ArrayList[String]].toList.toIndexedSeq else Vector(vcontext.getAttribute("AF", "").asInstanceOf[String])
    val acs = if (vcontext.getAlternateAlleles.size > 1) vcontext.getAttribute("AC", "").asInstanceOf[java.util.ArrayList[String]].toList.toIndexedSeq else Vector(vcontext.getAttribute("AC", "").asInstanceOf[String])
    val an = vcontext.getAttribute("AN", "").asInstanceOf[String].toInt
    val maf = afs(altidx).toDouble

    val (homvar, het, homref) = {

      val ac = acs(altidx).toInt
      // val an = ans(altidx).toInt
      val homvar = (maf * maf * an / 2.0).toInt
      val het = math.max(0, ac - homvar * 2)
      val homref = (an - ac) / 2
      (homvar, het, homref)
    }

    Genotypes(homvar = homvar, het = het, homref = homref)

  }

  def extractGenotypeCountFromGONL(vcontext: VCFDataWithoutGenotypes, ref: String, alt: String, altidx: Int) = {

    val afs = vcontext.getAttribute("AF") match {
      case Some(InfoVectorValue(value, _, _)) => value.map(_.toDouble)
      case _ => throw new RuntimeException("missing AF")
    }

    val acs = vcontext.getAttribute("AC") match {
      case Some(InfoVectorValue(value, _, _)) => value.map(_.toInt)
      case _ => throw new RuntimeException("missing AC")
    }

    val an = vcontext.getAttribute("AN") match {
      case Some(InfoSingleValue(value, _, _)) => value.toInt
      case _ => throw new RuntimeException("missing AN")
    }
    val maf = afs(altidx).toDouble

    val (homvar, het, homref) = {

      val ac = acs(altidx).toInt
      // val an = ans(altidx).toInt
      val homvar = (maf * maf * an / 2.0).toInt
      val het = math.max(0, ac - homvar * 2)
      val homref = (an - ac) / 2
      (homvar, het, homref)
    }

    Genotypes(homvar = homvar, het = het, homref = homref)

  }

  def extractGenotypeCountFromEXAC(vcontext: VariantContext, ref: String, alt: String, altidx: Int) = {

    val an = vcontext.getAttribute("AN", "").asInstanceOf[String].toInt
    val afs = if (vcontext.getAlternateAlleles.size > 1) vcontext.getAttribute("AF", "").asInstanceOf[java.util.ArrayList[String]].toList.toIndexedSeq else Vector(vcontext.getAttribute("AF", "").asInstanceOf[String])
    val ac_adjs = if (vcontext.getAlternateAlleles.size > 1) vcontext.getAttribute("AC_Adj", "").asInstanceOf[java.util.ArrayList[String]].toList.toIndexedSeq.map(_.toInt) else Vector(vcontext.getAttribute("AC_Adj", "").asInstanceOf[String].toInt)

    val het = vcontext.getAttribute("AC_Het", "").asInstanceOf[String].toInt
    val homvar = {
      val acadj = ac_adjs(altidx)
      (acadj - het) / 2
    }
    val homref = an / 2 - het - homvar

    Genotypes(homvar = homvar, het = het, homref = homref)

  }

  def extractGenotypeCountFromEXAC(vcontext: VCFDataWithoutGenotypes, ref: String, alt: String, altidx: Int) = {

    val an = vcontext.getAttribute("AN") match {
      case Some(InfoSingleValue(value, _, _)) => value.toInt
      case _ => throw new RuntimeException("missing AN")
    }
    val afs = vcontext.getAttribute("AF") match {
      case Some(InfoVectorValue(value, _, _)) => value.map(_.toDouble)
      case _ => throw new RuntimeException("missing AF")
    }

    val ac_adj = vcontext.getAttribute("AC_Adj") match {
      case Some(InfoVectorValue(value, _, _)) => value.map(_.toInt).apply(altidx)
      case Some(InfoSingleValue(value, _, _)) => value.toInt
      case _ => throw new RuntimeException("missing AC_Adj")
    }

    val het = vcontext.getAttribute("AC_Het") match {
      case Some(InfoSingleValue(value, _, _)) => value.toInt
      case _ => throw new RuntimeException("missing AC_Het")

    }
    val homvar = math.max({
      (ac_adj - het) / 2
    }, 0)
    val homref = math.max(an / 2 - het - homvar, 0)

    Genotypes(homvar = homvar, het = het, homref = homref)

  }

  def countSubstring(str1: String, str2: String): Int = {

    def count(pos: Int, c: Int): Int = {
      val idx = str1 indexOf (str2, pos)
      if (idx == -1) c else count(idx + str2.size, c + 1)
    }
    count(0, 0)
  }

  case class Genotypes(homvar: Int, het: Int, homref: Int) {
    assert(homvar >= 0)
    assert(het >= 0)
    assert(homref >= 0)
  }

  def extractGenotypeCountFromEVS(vcontext: VariantContext, ref: String, alt: String, altidx: Int) = {

    val gts = vcontext.getAttribute("GTS", "").asInstanceOf[java.util.ArrayList[String]].toList
    val gtc = vcontext.getAttribute("GTC", "").asInstanceOf[java.util.ArrayList[String]].toList.map(_.toInt)
    val gtsindel = gts.exists(_.contains('R'))
    val HomVar = "HOMVAR"
    val Het = "HET"
    val HomRef = "HOMREF"

    val gtszipgtc = gts zip gtc

    val homvar = {
      val search = if (gtsindel) "A" + (altidx + 1) + "A" + (altidx + 1) else alt + alt
      gtszipgtc.filter(x => x._1 == search).map(_._2).headOption.getOrElse(0)
    }
    val het = {
      val search = if (gtsindel) "A" + (altidx + 1) else alt
      val list = gtszipgtc filter (x => countSubstring(x._1, search) == 1) map (_._2)
      if (list.isEmpty) 0 else list.sum
    }
    val homref = {
      val search = if (gtsindel) "RR" else ref + ref
      gtszipgtc.filter(x => x._1 == search).map(_._2).headOption.getOrElse(0)
    }

    Genotypes(homvar = homvar, het = het, homref = homref)

  }

  def extractGenotypeCountFromEVS(vcontext: VCFDataWithoutGenotypes, ref: String, alt: String, altidx: Int) = {

    val gts = vcontext.getAttribute("GTS") match {
      case Some(InfoVectorValue(list, InfoString, _)) => list
      case _ => Vector[String]()
    }
    val gtc = vcontext.getAttribute("GTC") match {
      case Some(InfoVectorValue(list, InfoString, _)) => list.map(_.toInt)
      case _ => Vector[Int]()
    }

    val gtsindel = gts.exists(_.contains('R'))
    val HomVar = "HOMVAR"
    val Het = "HET"
    val HomRef = "HOMREF"

    val gtszipgtc = gts zip gtc

    val homvar = {
      val search = if (gtsindel) "A" + (altidx + 1) + "A" + (altidx + 1) else alt + alt
      gtszipgtc.filter(x => x._1 == search).map(_._2).headOption.getOrElse(0)
    }
    val het = {
      val search = if (gtsindel) "A" + (altidx + 1) else alt
      val list = gtszipgtc filter (x => countSubstring(x._1, search) == 1) map (_._2)
      if (list.isEmpty) 0 else list.sum
    }
    val homref = {
      val search = if (gtsindel) "RR" else ref + ref
      gtszipgtc.filter(x => x._1 == search).map(_._2).headOption.getOrElse(0)
    }

    Genotypes(homvar = homvar, het = het, homref = homref)

  }

  def readNutvarOutputs(nutvars: List[File], totalGeneList: Map[String8, Gene]) = nutvars.map(f => openSource(f.getAbsolutePath)(s => Input.readNutVarOutput(s, totalGeneList))).reduce((x, y) => addMaps(x, y)((z, zz) => z ++ zz))

  def filterVCF(source: scala.io.Source, writer: java.io.OutputStream, nutvar: Set[VariantKey]): Unit = {

    val (vcfheader, vcfiterator) = vcfhelpers.VCFHelpers.readVCF(source)

    val vcwriter = VariantContextWriterFactory.create(writer, vcfheader.getSequenceDictionary, java.util.EnumSet.of(htsjdk.variant.variantcontext.writer.Options.ALLOW_MISSING_FIELDS_IN_HEADER))

    vcwriter.writeHeader(vcfheader)

    vcfiterator.foreach { vcontext =>
      val chr = vcontext.getChr
      val bp1 = vcontext.getStart
      val ref1 = vcontext.getReference.getBaseString
      val alts = vcontext.getAlternateAlleles.toIndexedSeq.map(_.getBaseString)

      val vkeys = alts.map { alt1 =>

        val MinimalRepresentation(ref, alt, bp) = minimalRepresentation(ref1, alt1, bp1)
        val gloc = GenomicLocation(bp, chr)

        VariantKey(gloc, StringStore(ref), StringStore(alt))
      }

      if (vkeys.exists(vkey => nutvar.contains(vkey))) {
        vcwriter.add(vcontext)
      }
    }
    vcwriter.close

  }

  def extractGenotypeCountFromGeneralVCF(vcontext: VariantContext, ref: String, alt: String, altidx: Int) = {
    val (homref, het, homvar) = if (vcontext.getAlternateAlleles.size == 1) {

      val homref = vcontext.getHomRefCount
      val het = vcontext.getHetCount
      val homvar = vcontext.getHomVarCount

      (homref, het, homvar)

    } else {

      val altstring = vcontext.getAlternateAlleles.apply(altidx).getBaseString

      vcontext.getGenotypes.iterator.filter(_.isCalled).foldLeft((0, 0, 0)) {
        case ((acc1, acc2, acc3), gt) =>
          val firstAllele = gt.getAllele(0).getBaseString
          val secondAllele = Try(gt.getAllele(1).getBaseString).toOption.getOrElse(firstAllele)

          val (homref, het, homvar) = (firstAllele, secondAllele) match {
            case (a1, a2) if a1 == a2 && a1 == ref => (1, 0, 0)
            case (a1, a2) if a1 == a2 && (a1 == altstring || a2 == altstring) => (0, 0, 1)
            case (a1, a2) if a1 != a2 && (a1 == altstring || a2 == altstring) => (0, 1, 0)
            case _ => (0, 0, 0)
          }
          (acc1 + homref, acc2 + het, acc3 + homvar)
      }
    }

    Genotypes(homvar = homvar, het = het, homref = homref)

  }

  def readGenotypeCountsFromVCF(source: scala.io.Source)(extractGenotypeCount: (VariantContext, String, String, Int) => Genotypes): Iterator[VariantWithoutAnnotation] = {

    val (vcfheader, vcfiterator) = vcfhelpers.VCFHelpers.readVCF(source)

    vcfiterator.filter(_.isNotFiltered).flatMap { vcontext =>
      val chr = vcontext.getChr
      val bp1 = vcontext.getStart
      val ref1 = vcontext.getReference.getBaseString

      vcontext.getAlternateAlleles.toIndexedSeq.map(_.getBaseString).zipWithIndex.map {
        case (alt, altidx) =>
          val mr = vcfhelpers.minimalRepresentation(ref1, alt, bp1)
          (alt, mr, altidx)
      }.map {
        case (alt1, minrep, alt1idx) =>

          val genotypes = extractGenotypeCount(vcontext, ref1, alt1, alt1idx)
          import genotypes._
          VariantWithoutAnnotationImpl(
            location = GenomicLocation(minrep.position, chr),
            reference = StringStore(minrep.ref),
            alternate = StringStore(minrep.alt),
            homRefCount = homref,
            homVarCount = homvar,
            hetCount = het
          )

      }
    }

  }

  def extractGenotypeCountFromGeneralVCF(vcontext: VCFDataWithoutGenotypes, ref: String, alt: String, altidx: Int) = {
    val (homref, het, homvar) = {

      val homref = vcontext.genotypeCounts(0, 0)
      val het = vcontext.genotypeCounts(0, altidx + 1)
      val homvar = vcontext.genotypeCounts(altidx + 1, altidx + 1)

      (homref, het, homvar)

    }

    Genotypes(homvar = homvar, het = het, homref = homref)

  }

  def readGenotypeCountsFromVCFMyParser(source: scala.io.Source, cpus: Int)(extractGenotypeCount: (VCFDataWithoutGenotypes, String, String, Int) => Genotypes): Iterator[VariantWithoutAnnotation] = {

    val vcfiterator = VCFParser.parse(source, cpus).iter

    vcfiterator.filter(_.pass).flatMap { vcontext =>
      val chr = vcontext.chromosome
      val bp1 = vcontext.start
      val ref1 = vcontext.reference

      vcontext.alternatives.zipWithIndex.map {
        case (alt, altidx) =>
          val mr = vcfhelpers.minimalRepresentation(ref1, alt, bp1)
          (alt, mr, altidx)
      }.map {
        case (alt1, minrep, alt1idx) =>

          val genotypes = extractGenotypeCount(vcontext, ref1, alt1, alt1idx)
          import genotypes._
          VariantWithoutAnnotationImpl(
            location = GenomicLocation(minrep.position, chr),
            reference = StringStore(minrep.ref),
            alternate = StringStore(minrep.alt),
            homRefCount = homref,
            homVarCount = homvar,
            hetCount = het
          )

      }
    }

  }

  case class AntoniosVariantInformation(key: VariantKey, ratioAffectedIsoforms_stop_gained: Double, ratioAffectedIsoforms_frameshift: Double, ratioAffectedIsoforms_coding_synonymous: Double, GeneNameCCDS: String8) {
    def synonym = ratioAffectedIsoforms_coding_synonymous > 0.0
  }

  def readAntoniosVariantInformationFile(source: scala.io.Source): Iterator[AntoniosVariantInformation] = {
    val lines = source.getLines
    val headerline = lines.next
    val headerSplit = fastSplit1WideSeparator(headerline, '\t')

    val baseidx = headerSplit.indexOf("baseNCBI37")
    val allelestringidx = headerSplit.indexOf("Alleles_REF>ALT")
    val genenameidx = headerSplit.indexOf("GeneNameCCDS")
    val ratioStopidx = headerSplit.indexOf("ratioAffectedIsoforms_stop-gained")
    val ratioFSidx = headerSplit.indexOf("ratioAffectedIsoforms_frameshift")
    val ratioSynidx = headerSplit.indexOf("ratioAffectedIsoforms_coding-synonymous")

    lines.map { line =>
      val spl = fastSplit1WideSeparator(line, '\t')

      val vkey = {
        val positionstring = spl(baseidx)
        val alleles = spl(allelestringidx)
        val ccds = StringStore(spl(genenameidx))
        val positionspl = fastSplit1WideSeparator(positionstring, ':')
        val allelesspl = fastSplit1WideSeparator(alleles, '>')
        val chr = positionspl(0)
        val bp1 = positionspl(1)
        val ref1 = allelesspl(0)
        val alt1 = {
          val s = alleles
          val index = s.indexOf(">")
          s.substring(index + 1, s.size)
        }
        val vcfhelpers.MinimalRepresentation(minRef, minAlt, minBp) = vcfhelpers.minimalRepresentation(ref1, alt1, bp1.toInt)
        val gloc = GenomicLocation(minBp, chr)
        VariantKey(gloc, StringStore(minRef), StringStore(minAlt))
      }

      AntoniosVariantInformation(
        key = vkey,
        ratioAffectedIsoforms_stop_gained = spl(ratioStopidx).toDouble,
        ratioAffectedIsoforms_frameshift = spl(ratioFSidx).toDouble,
        ratioAffectedIsoforms_coding_synonymous = spl(ratioSynidx).toDouble,
        GeneNameCCDS = StringStore(spl(genenameidx))
      )
    }
  }

  def getSynonymCounts(s: scala.io.Source, valid: Set[VariantKey]): Map[String8, Int] = {
    val mmap = scala.collection.mutable.AnyRefMap[String8, Int]()
    readAntoniosVariantInformationFile(s).filter(x => x.synonym && valid.contains(x.key)).foreach { e =>
      mmap.get(e.GeneNameCCDS) match {
        case Some(x) => mmap.update(e.GeneNameCCDS, x + 1)
        case None => mmap.update(e.GeneNameCCDS, 1)
      }
    }
    mmap.toMap
  }

  /* This reads in the file produced by JoinAntoniosFile */
  def attachAnnotation[T <: Annotation[T]](iter: Iterator[VariantWithoutAnnotation], nutvar: AnnotationIndex[T]): Iterator[VariantWithSumGenotype[T]] = iter.flatMap { variant =>
    import variant._

    nutvar.get(VariantKey(location, reference, alternate)).toList.flatMap {
      _.map {
        case (gene, annotation) =>

          VariantWithSumGenotype[T](
            annotation = annotation,
            homRefCount = homRefCount,
            hetCount = hetCount,
            homVarCount = homVarCount,
            location = location,
            reference = reference,
            alternate = alternate
          )

      }
    }
  }

  /* This reads in the file produced by JoinAntoniosFile */
  def readVariantCountsFile(source: scala.io.Source, hweThreshold: Double): Iterator[VariantWithoutAnnotation] = source.getLines.flatMap { line =>
    // s"${gloc.chromosome}:${gloc.basePairPosition}\t$ref\t$alt\t$homref\t$het\t$homvar\n"
    val spl = fastSplit1WideSeparator(line, '\t')
    val spl2 = fastSplit1WideSeparator(spl(0), ':')
    val chr = spl2(0)
    val bp = spl2(1).toInt
    val ref = StringStore(spl(1))
    val alt = StringStore(spl(2))
    val gloc = GenomicLocation(bp, chr)

    if (Try(spl(3).toInt).toOption.isDefined) {
      val homref = spl(3).toInt
      val het = spl(4).toInt
      val homvar = spl(5).toInt

      val hwepass = (hweThreshold == 0.0 || Try(stat.HWE.exact(homvar, het, homref)).toOption.getOrElse(1.0) > hweThreshold)
      if (!hwepass) None
      else {
        Some(VariantWithoutAnnotationImpl(
          homRefCount = homref,
          hetCount = het,
          homVarCount = homvar,
          location = gloc,
          reference = ref,
          alternate = alt
        ))
      }
    } else None
  }

}