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

package rnaseqalign.htseqcount

import org.scalatest.FunSpec
import org.scalatest.PrivateMethodTester
import org.scalatest.Matchers
import org.scalatest.prop.Checkers
// import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.BooleanOperators
// import org.scalacheck.Gen._
import org.scalacheck.{ Arbitrary, Gen }

import HTSeqCount._
import mybiotools.gwascommons._
import java.io.File
import rnaseqalign.Helpers
import scala.collection.immutable.ListMap
import mybiotools.stringstore._
import mybiotools.sequence.alignment._

class HTSeqCountSpec extends FunSpec with Matchers with PrivateMethodTester with Checkers {

  // implicit override val generatorDrivenConfig =
  //   PropertyCheckConfig(minSuccessful = 1000, maxDiscarded = 10000)

  // implicit def arbRegion[Region] =
  //   Arbitrary {
  //     for {
  //       chr <- Gen.choose(1, 22)
  //       from <- Gen.choose(0, 1000)
  //       to <- Gen.choose(from, 1000)
  //     } yield {
  //       Region(chr, from, to)
  //     }
  //   }

  val chr = "ukmukk"

  describe("readgtf") {
    val source = """|##description: evidence-based annotation of the human genome (GRCh37), version 19 (Ensembl 74)
|##provider: GENCODE
|##contact: gencode@sanger.ac.uk
|##format: gtf
|##date: 2013-12-05
|chr1	HAVANA	gene	11869	14412	.	+	.	gene_id "ENSG00000223972.4"; transcript_id "ENSG00000223972.4"; gene_type "pseudogene"; gene_status "KNOWN"; gene_name "DDX11L1"; transcript_type "pseudogene"; transcript_status "KNOWN"; transcript_name "DDX11L1"; level 2; havana_gene "OTTHUMG00000000961.2";
|chr1	HAVANA	transcript	11869	14409	.	+	.	gene_id "ENSG00000223972.4"; transcript_id "ENST00000456328.2"; gene_type "pseudogene"; gene_status "KNOWN"; gene_name "DDX11L1"; transcript_type "processed_transcript"; transcript_status "KNOWN"; transcript_name "DDX11L1-002"; level 2; tag "basic"; havana_gene "OTTHUMG00000000961.2"; 
|""".stripMargin
    it("1") {
      readGTF(scala.io.Source.fromString(source), s8"gene_id", s8"transcript_id") { (x: String8) => x === StringStore("gene") } should equal(List(GFFEntry(Region("chr1", 11868, 14412), Forward, s8"gene", ListMap(s8"gene_id" -> s8"ENSG00000223972.4", s8"transcript_id" -> s8"ENSG00000223972.4"), s8"ENSG00000223972.4")))
    }

  }

  describe("htseqcount") {
    val features =
      GFFEntry(Region(chr, 10, 20), Forward, s8"exon", ListMap(s8"gene_id" -> s8"g1", s8"transcript_id" -> s8"t1"), s8"g1") ::
        GFFEntry(Region(chr, 30, 40), Forward, s8"exon", ListMap(s8"gene_id" -> s8"g2", s8"transcript_id" -> s8"t2"), s8"g2") ::
        GFFEntry(Region(chr, 40, 50), Forward, s8"exon", ListMap(s8"gene_id" -> s8"g3", s8"transcript_id" -> s8"t3"), s8"g3") ::
        GFFEntry(Region(chr, 45, 55), Reverse, s8"exon", ListMap(s8"gene_id" -> s8"g4", s8"transcript_id" -> s8"t4"), s8"g4") :: Nil

    it("empty") {
      countSingleEndReadsInFeatures(
        iterator = List().iterator,
        features = features,
        strandedness = NotStranded,
        minAverageQual = 0,
        allowMultiMapInSameGene = false
      ) should equal(Map())
    }
    it("1 read") {
      countSingleEndReadsInFeatures(
        iterator = List(
        SAMRecordEssentials(alignmentStart = 5, cigar = Cigar(CigarElement(10, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = None, queryName = "XY")
      ).iterator,
        features = features,
        strandedness = NotStranded,
        minAverageQual = 0,
        allowMultiMapInSameGene = false
      ) should equal(Map("__ambiguous" -> 0, "__not_aligned" -> 0, "g1" -> 1, "__alignment_not_unique" -> 0, "__no_feature" -> 0, "__too_low_aQual" -> 0))
    }
    it("1 read low qual") {
      countSingleEndReadsInFeatures(
        iterator = List(
        SAMRecordEssentials(alignmentStart = 5, cigar = Cigar(CigarElement(10, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = None, queryName = "XY")
      ).iterator,
        features = features,
        strandedness = NotStranded,
        minAverageQual = 100,
        allowMultiMapInSameGene = false
      ) should equal(Map("__ambiguous" -> 0, "__not_aligned" -> 0, "__alignment_not_unique" -> 0, "__no_feature" -> 0, "__too_low_aQual" -> 1))
    }
    it("1 read no feature") {
      countSingleEndReadsInFeatures(
        iterator = List(
        SAMRecordEssentials(alignmentStart = 1, cigar = Cigar(CigarElement(1, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = None, queryName = "XY")
      ).iterator,
        features = features,
        strandedness = NotStranded,
        minAverageQual = 0,
        allowMultiMapInSameGene = false
      ) should equal(Map("__ambiguous" -> 0, "__not_aligned" -> 0, "__alignment_not_unique" -> 0, "__no_feature" -> 1, "__too_low_aQual" -> 0))
    }
    it("1 read ambiguous") {
      countSingleEndReadsInFeatures(
        iterator = List(
        SAMRecordEssentials(alignmentStart = 1, cigar = Cigar(CigarElement(100, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = None, queryName = "XY")
      ).iterator,
        features = features,
        strandedness = NotStranded,
        minAverageQual = 0,
        allowMultiMapInSameGene = false
      ) should equal(Map("__ambiguous" -> 1, "__not_aligned" -> 0, "__alignment_not_unique" -> 0, "__no_feature" -> 0, "__too_low_aQual" -> 0))
    }
    it("1 read not aligned") {
      countSingleEndReadsInFeatures(
        iterator = List(
        SAMRecordEssentials(alignmentStart = 1, cigar = Cigar(CigarElement(100, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = true, mapQ = 30, NH = None, queryName = "XY")
      ).iterator,
        features = features,
        strandedness = NotStranded,
        minAverageQual = 0,
        allowMultiMapInSameGene = false
      ) should equal(Map("__ambiguous" -> 0, "__not_aligned" -> 1, "__alignment_not_unique" -> 0, "__no_feature" -> 0, "__too_low_aQual" -> 0))
    }
    it("1 read nonunique") {
      countSingleEndReadsInFeatures(
        iterator = List(
        SAMRecordEssentials(alignmentStart = 1, cigar = Cigar(CigarElement(100, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = Some(2), queryName = "XY")
      ).iterator,
        features = features,
        strandedness = NotStranded,
        minAverageQual = 0,
        allowMultiMapInSameGene = false
      ) should equal(Map("__ambiguous" -> 0, "__not_aligned" -> 0, "__alignment_not_unique" -> 1, "__no_feature" -> 0, "__too_low_aQual" -> 0))
    }
    it("2 read nonunique, same gene multimap on") {
      countSingleEndReadsInFeatures(
        iterator = List(
        SAMRecordEssentials(alignmentStart = 1, cigar = Cigar(CigarElement(20, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = Some(2), queryName = "XY"),
        SAMRecordEssentials(alignmentStart = 1, cigar = Cigar(CigarElement(20, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = Some(2), queryName = "XY")
      ).iterator,
        features = features,
        strandedness = NotStranded,
        minAverageQual = 0,
        allowMultiMapInSameGene = true
      ) should equal(Map("__ambiguous" -> 0, "__not_aligned" -> 0, "__alignment_not_unique" -> 0, "__no_feature" -> 0, "__too_low_aQual" -> 0, "g1" -> 1))
    }
    it("2 read nonunique, diff genes multimap on") {
      countSingleEndReadsInFeatures(
        iterator = List(
        SAMRecordEssentials(alignmentStart = 40, cigar = Cigar(CigarElement(10, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = Some(2), queryName = "XY"),
        SAMRecordEssentials(alignmentStart = 1, cigar = Cigar(CigarElement(20, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = Some(2), queryName = "XY")
      ).iterator,
        features = features,
        strandedness = NotStranded,
        minAverageQual = 0,
        allowMultiMapInSameGene = true
      ) should equal(Map("__ambiguous" -> 0, "__not_aligned" -> 0, "__alignment_not_unique" -> 1, "__no_feature" -> 0, "__too_low_aQual" -> 0))
    }
    it("2 read nonunique, same gene multimap off") {
      countSingleEndReadsInFeatures(
        iterator = List(
        SAMRecordEssentials(alignmentStart = 1, cigar = Cigar(CigarElement(20, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = Some(2), queryName = "XY"),
        SAMRecordEssentials(alignmentStart = 1, cigar = Cigar(CigarElement(20, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = Some(2), queryName = "XY")
      ).iterator,
        features = features,
        strandedness = NotStranded,
        minAverageQual = 0,
        allowMultiMapInSameGene = false
      ) should equal(Map("__ambiguous" -> 0, "__not_aligned" -> 0, "__alignment_not_unique" -> 2, "__no_feature" -> 0, "__too_low_aQual" -> 0))
    }
    it("2 read same gene") {
      countSingleEndReadsInFeatures(
        iterator = List(
        SAMRecordEssentials(alignmentStart = 5, cigar = Cigar(CigarElement(10, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = None, queryName = "XY"),
        SAMRecordEssentials(alignmentStart = 15, cigar = Cigar(CigarElement(10, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = None, queryName = "XY")
      ).iterator,
        features = features,
        strandedness = NotStranded,
        minAverageQual = 0,
        allowMultiMapInSameGene = false
      ) should equal(Map("__ambiguous" -> 0, "__not_aligned" -> 0, "g1" -> 2, "__alignment_not_unique" -> 0, "__no_feature" -> 0, "__too_low_aQual" -> 0))
    }
    it("2 read 2 gene") {
      countSingleEndReadsInFeatures(
        iterator = List(
        SAMRecordEssentials(alignmentStart = 5, cigar = Cigar(CigarElement(10, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = None, queryName = "XY"),
        SAMRecordEssentials(alignmentStart = 25, cigar = Cigar(CigarElement(10, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = None, queryName = "XY")
      ).iterator,
        features = features,
        strandedness = NotStranded,
        minAverageQual = 0,
        allowMultiMapInSameGene = false
      ) should equal(Map("__ambiguous" -> 0, "__not_aligned" -> 0, "g1" -> 1, "g2" -> 1, "__alignment_not_unique" -> 0, "__no_feature" -> 0, "__too_low_aQual" -> 0))
    }
    it("2 read 1 gene no strand") {
      countSingleEndReadsInFeatures(
        iterator = List(
        SAMRecordEssentials(alignmentStart = 40, cigar = Cigar(CigarElement(1, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = None, queryName = "XY"),
        SAMRecordEssentials(alignmentStart = 50, cigar = Cigar(CigarElement(10, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = None, queryName = "XY")
      ).iterator,
        features = features,
        strandedness = NotStranded,
        minAverageQual = 0,
        allowMultiMapInSameGene = false
      ) should equal(Map("__ambiguous" -> 1, "__not_aligned" -> 0, "g2" -> 1, "__alignment_not_unique" -> 0, "__no_feature" -> 0, "__too_low_aQual" -> 0))
    }
    it("2 read 2 gene same strand, stranded") {
      countSingleEndReadsInFeatures(
        iterator = List(
        SAMRecordEssentials(alignmentStart = 40, cigar = Cigar(CigarElement(1, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = None, queryName = "XY"),
        SAMRecordEssentials(alignmentStart = 50, cigar = Cigar(CigarElement(10, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = None, queryName = "XY")
      ).iterator,
        features = features,
        strandedness = Stranded,
        minAverageQual = 0,
        allowMultiMapInSameGene = false
      ) should equal(Map("__ambiguous" -> 0, "__not_aligned" -> 0, "g2" -> 1, "g3" -> 1, "__alignment_not_unique" -> 0, "__no_feature" -> 0, "__too_low_aQual" -> 0))
    }
    it("2 read 2 gene 2 strands, stranded") {
      countSingleEndReadsInFeatures(
        iterator = List(
        SAMRecordEssentials(alignmentStart = 40, cigar = Cigar(CigarElement(1, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = None, queryName = "XY"),
        SAMRecordEssentials(alignmentStart = 50, cigar = Cigar(CigarElement(10, M) :: Nil), negativeStrandFlag = true, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = None, queryName = "XY")
      ).iterator,
        features = features,
        strandedness = Stranded,
        minAverageQual = 0,
        allowMultiMapInSameGene = false
      ) should equal(Map("__ambiguous" -> 0, "__not_aligned" -> 0, "g2" -> 1, "g4" -> 1, "__alignment_not_unique" -> 0, "__no_feature" -> 0, "__too_low_aQual" -> 0))
    }
    it("2 read 2 gene 2 strands, reverse") {
      countSingleEndReadsInFeatures(
        iterator = List(
        SAMRecordEssentials(alignmentStart = 40, cigar = Cigar(CigarElement(1, M) :: Nil), negativeStrandFlag = false, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = None, queryName = "XY"),
        SAMRecordEssentials(alignmentStart = 50, cigar = Cigar(CigarElement(10, M) :: Nil), negativeStrandFlag = true, chromosome = chr, pairedEnd = false, unmappedFlag = false, mapQ = 30, NH = None, queryName = "XY")
      ).iterator,
        features = features,
        strandedness = ReverseStranded,
        minAverageQual = 0,
        allowMultiMapInSameGene = false
      ) should equal(Map("__ambiguous" -> 0, "__not_aligned" -> 0, "g3" -> 1, "__alignment_not_unique" -> 0, "__no_feature" -> 1, "__too_low_aQual" -> 0))
    }
    it("compare with python HTSeq-count") {
      val sam = new File(getClass.getResource("/").getPath + "/sam1.sam")
      val gtf = new File(getClass.getResource("/").getPath + "/GencodeV19+ERCC+HIV.exons.gtf.gz")
      val pythonout = new File(getClass.getResource("/").getPath + "/htseqcount.out")

      val is = new java.io.BufferedInputStream(new java.io.FileInputStream(sam))

      val map = Helpers.htseqCountFromBam(
        in = is,
        gtfcontents = mybiotools.openSource(gtf.getAbsolutePath) { s => HTSeqCount.readGTF(s, s8"gene_id", s8"transcript_id")(x => List("exon").contains(x.value)).toList },
        strandedness = NotStranded,
        minQual = 0,
        allowMultiMapInSameGene = false
      ).filter(_._2 > 0)

      is.close

      // println(map)

      val python = mybiotools.openSource(pythonout.getAbsolutePath)(_.getLines.map { x =>
        val t = x.split("\t")
        t(0) -> t(1).toInt
      }.toMap.filter(_._2 > 0))

      // println(python.keys.toSet &~ map.keys.toSet)
      (python.keys.toSet &~ map.keys.toSet) should equal(Set[String]())
      (map.keys.toSet &~ python.keys.toSet) should equal(Set[String]())
      map.keys.toSet should equal(python.keys.toSet)

      map should equal(python)

      // python should equal(map)

    }
  }

  describe("parsecigar") {

    it("empty") {
      parseCigar(
        cigar = Cigar(Nil),
        chromosome = chr,
        start = 1,
        strand = Forward
      ) should equal(List())
    }
    it("10M is 0-10") {
      parseCigar(
        cigar = Cigar(CigarElement(10, M) :: Nil),
        chromosome = chr,
        start = 1,
        strand = Forward
      ) should equal(List(GenomicIntervalWithStrand(Region(chr, 0, 10), Forward)))
    }
    it("10D is empty") {
      parseCigar(
        cigar = Cigar(CigarElement(10, D) :: Nil),
        chromosome = chr,
        start = 1,
        strand = Forward
      ) should equal(List())
    }
    it("10I is empty") {
      parseCigar(
        cigar = Cigar(CigarElement(10, I) :: Nil),
        chromosome = chr,
        start = 1,
        strand = Forward
      ) should equal(List())
    }
    it("10N is empty") {
      parseCigar(
        cigar = Cigar(CigarElement(10, I) :: Nil),
        chromosome = chr,
        start = 1,
        strand = Forward
      ) should equal(List())
    }
    it("10M20D30I40M50N60M is (0-10)(30-70)(120-180)") {
      parseCigar(
        cigar = Cigar(
          CigarElement(10, M) ::
            CigarElement(20, D) ::
            CigarElement(30, I) ::
            CigarElement(40, M) ::
            CigarElement(50, N) ::
            CigarElement(60, M) :: Nil
        ),
        chromosome = chr,
        start = 1,
        strand = Forward
      ) should equal(List(
          GenomicIntervalWithStrand(Region(chr, 0, 10), Forward),
          GenomicIntervalWithStrand(Region(chr, 30, 70), Forward),
          GenomicIntervalWithStrand(Region(chr, 120, 180), Forward)
        ).reverse)
    }
    it("20D30I40M50N60M is (20-60)(110-170)") {
      parseCigar(
        cigar = Cigar(
          CigarElement(20, D) ::
            CigarElement(30, I) ::
            CigarElement(40, M) ::
            CigarElement(50, N) ::
            CigarElement(60, M) :: Nil
        ),
        chromosome = chr,
        start = 1,
        strand = Forward
      ) should equal(List(
          GenomicIntervalWithStrand(Region(chr, 20, 60), Forward),
          GenomicIntervalWithStrand(Region(chr, 110, 170), Forward)
        ).reverse)
    }
    it("30I40M50N60M is (0-40)(90-150)") {
      parseCigar(
        cigar = Cigar(
          CigarElement(30, I) ::
            CigarElement(40, M) ::
            CigarElement(50, N) ::
            CigarElement(60, M) :: Nil
        ),
        chromosome = chr,
        start = 1,
        strand = Forward
      ) should equal(List(
          GenomicIntervalWithStrand(Region(chr, 0, 40), Forward),
          GenomicIntervalWithStrand(Region(chr, 90, 150), Forward)
        ).reverse)
    }

  }

}