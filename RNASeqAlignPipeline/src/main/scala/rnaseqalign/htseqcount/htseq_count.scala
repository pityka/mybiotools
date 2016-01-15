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

import mybiotools._
import mybiotools.eq._
import mybiotools.gwascommons._
import scala.collection.JavaConversions._
import mybiotools.intervaltree.IntervalTree
import scala.collection.immutable.ListMap
import htsjdk.samtools.{ SAMRecord, CigarOperator => SAMCO }
import mybiotools.stringstore._
import mybiotools.sequence.alignment._

sealed trait Strandedness
case object Stranded extends Strandedness
case object NotStranded extends Strandedness
case object ReverseStranded extends Strandedness

sealed trait Strand
case object Forward extends Strand
case object Reverse extends Strand

case class GFFEntry(region: Region, strand: Strand, featureType: String8, attributes: ListMap[String8, String8], name: String8) extends RegionStub[String8, GenomicLocation, GFFEntry] {

  def chromosomeAsT = region.chromosomeAsT

  def from: Int = region.from

  def to: Int = region.to

  def chromosome = region.chromosome
}
case class GenomicIntervalWithStrand(region: Region, strand: Strand)
case class SAMRecordEssentials(alignmentStart: Int, cigar: Cigar, negativeStrandFlag: Boolean, chromosome: String, pairedEnd: Boolean, unmappedFlag: Boolean, mapQ: Int, NH: Option[Int], queryName: String)
object SAMRecordEssentials {
  def apply(samRecord: SAMRecord): Option[SAMRecordEssentials] = {
    val cigar = {
      val orig = samRecord.getCigar
      if (orig != null) {
        Some(Cigar(orig.getCigarElements.map(ce => CigarElement(length = ce.getLength, operator = ce.getOperator match {
          case x if x === SAMCO.M => M
          case x if x === SAMCO.N => N
          case x if x === SAMCO.D => D
          case x if x === SAMCO.I => I
          case x if x === SAMCO.X => X
          case x if x === SAMCO.H => H
          case x if x === SAMCO.S => S
          case x if x === SAMCO.P => P
          case x if x === SAMCO.EQ => EQ
        })).toList))
      } else None
    }
    val nh = if (samRecord.getAttribute("NH") === null) None else scala.util.Try(Some(samRecord.getAttribute("NH").asInstanceOf[Int])).toOption.flatten

    if (cigar.isDefined) Some(SAMRecordEssentials(alignmentStart = samRecord.getAlignmentStart, cigar = cigar.get, negativeStrandFlag = samRecord.getReadNegativeStrandFlag, chromosome = samRecord.getReferenceName, pairedEnd = samRecord.getReadPairedFlag, unmappedFlag = samRecord.getReadUnmappedFlag, mapQ = samRecord.getMappingQuality, NH = nh, queryName = samRecord.getReadName))
    else None
  }
}

object HTSeqCount {

  def getLengths(
    rawlist: List[GFFEntry],
    Transcript_id: String8,
    Gene_id: String8,
    Exon: String8,
    resolveLongestTranscript: Boolean
  ): (List[GFFEntry], Map[String, Int]) = {
    if (!resolveLongestTranscript) {
      rawlist -> rawlist.groupBy(_.attributes(Gene_id)).map {
        case (g_id, exons) =>
          g_id.value -> exons.map(_.region.size).sum
      }.toMap
    } else {
      longestTranscript(rawlist, Gene_id, Transcript_id, Exon)

    }

  }

  def longestTranscript(gtf: List[GFFEntry], Gene_id: String8, Transcript_id: String8, Exon: String8): (List[GFFEntry], Map[String, Int]) = {
    val zipped: List[(List[GFFEntry], (String, Int))] =
      gtf.filter(_.featureType === Exon).groupBy(_.attributes(Gene_id)).map {
        case (gene_id, exons) =>
          val longestTranscript: (String, Int, List[GFFEntry]) = exons.groupBy(_.attributes(Transcript_id)).map {
            case (t_id, exons) =>
              (exons.head.attributes(Gene_id).value, exons.map(_.region.size).sum, exons)
          }.maxBy(_._2)
          (longestTranscript._3, (longestTranscript._1, longestTranscript._2))
      }.toList

    val (gffs, lengths) = zipped.unzip
    (gffs.flatten, lengths.toMap)
  }

  def readGTF(s: scala.io.Source, Gene_id: String8, Transcript_id: String8)(featureFilter: String8 => Boolean): List[GFFEntry] = s.getLines.dropWhile(_.startsWith("#")).map { line =>
    val spl = mybiotools.fastSplitSeparator(line, '\t')
    val seqname = StringStore(new String(spl(0)))
    val feature = StringStore(new String(spl(2)))
    val start = spl(3).toInt
    val end = spl(4).toInt
    val strand = spl(6) match {
      case "+" => Forward
      case "-" => Reverse
    }
    val attributes = ListMap(mybiotools.fastSplitSeparatorIterator(spl(8), ';').map(_.trim).filter(_.size > 0).map { t =>
      val spl = mybiotools.fastSplitSeparator(t, ' ')
      StringStore(new String(spl(0))) -> StringStore(new String(spl.drop(1).mkString(" ").filterNot(_ === '"')))
    }.filter(x => x._1 === Gene_id || x._1 === Transcript_id).toList: _*)
    (feature, GFFEntry(Region(seqname, start - 1, end), strand, feature, attributes, name = attributes(Gene_id)))
  }.filter(x => featureFilter(x._1)).map(_._2).toList

  def parseCigar(
    cigar: Cigar,
    chromosome: String,
    start: Int,
    strand: Strand
  ): List[GenomicIntervalWithStrand] =
    cigar.cigarElements.foldLeft((List[GenomicIntervalWithStrand](), start)) {
      case ((acc, pos), elem) =>
        if (elem.operator === M || elem.operator === EQ || elem.operator === X)
          (GenomicIntervalWithStrand(Region(chromosome, pos - 1, pos - 1 + elem.length), strand) :: acc, pos + elem.length)
        else if (elem.operator === N || elem.operator === D) (acc, pos + elem.length)
        else (acc, pos)
    }._1

  def createIntervalTree(features: List[GFFEntry]) = features.groupBy(_.chromosome).map(x => x._1 -> IntervalTree.makeTree(x._2.sortBy(_.from))).toMap

  /** Scala translation of Eoulsan's htseqcounter */
  def countSingleEndReadsInFeatures(
    iterator: Iterator[SAMRecordEssentials],
    features: List[GFFEntry],
    strandedness: Strandedness,
    minAverageQual: Int,
    allowMultiMapInSameGene: Boolean
  ): Map[String, Long] = {

    val intervaltrees: Map[String, IntervalTree[GFFEntry]] = createIntervalTree(features)

    countSingleEndReadsInFeatures(iterator, intervaltrees, strandedness, minAverageQual, allowMultiMapInSameGene)

  }

  def countSingleEndReadsInFeatures(
    iterator: Iterator[SAMRecordEssentials],
    intervaltrees: Map[String, IntervalTree[GFFEntry]],
    strandedness: Strandedness,
    minAverageQual: Int,
    allowMultiMapInSameGene: Boolean
  ): Map[String, Long] = {

    if (!iterator.hasNext) Map() else {

      val mmap = scala.collection.mutable.Map[String, Long]()
      val nonuniques = scala.collection.mutable.Map[String, List[SAMRecordEssentials]]()

      var empty = 0L;
      var ambiguous = 0L;
      var notaligned = 0L;
      var lowqual = 0L;
      var nonunique = 0L;

      def overlaps(samRecord: SAMRecordEssentials): Set[String] = {
        val intervals: List[GenomicIntervalWithStrand] = {

          val strand =
            if (strandedness === ReverseStranded) (if (samRecord.negativeStrandFlag) Forward else Reverse)
            else (if (samRecord.negativeStrandFlag) Reverse else Forward)

          parseCigar(samRecord.cigar, samRecord.chromosome,
            samRecord.alignmentStart, strand)

        }

        intervals.flatMap {
          case GenomicIntervalWithStrand(queryRegion, strand) =>
            intervaltrees.get(queryRegion.chromosome).toList.flatMap(intervaltree => IntervalTree.lookup(queryRegion, intervaltree).filter {
              case GFFEntry(_, strandfeature, _, _, _) => {
                // println(s"query: $queryRegion , hit: $regionfeature - $name")
                (strandedness match {
                  case Stranded | ReverseStranded => strandfeature === strand
                  case NotStranded => true
                })
              }
            })
        }.map(_.name.value).toSet
      }

      def countMultiMapped(record: SAMRecordEssentials) {
        if (allowMultiMapInSameGene) {
          nonuniques.get(record.queryName) match {
            case None => nonuniques.update(record.queryName, List(record))
            case Some(x) => nonuniques.update(record.queryName, record :: x)
          }
        } else {
          nonunique += 1
        }
      }

      def countMultiMappedFinish {
        nonuniques.foreach {
          case (qname, records) =>
            val fs: Set[String] = records.flatMap(overlaps).toSet
            fs.size match {
              case 0 => { empty += 1 }
              case 1 => count(fs.head)
              case _ => { nonunique += 1 }
            }
        }
      }

      def count(i: String): Unit = mmap get i match {
        case None => mmap.update(i, 1)
        case Some(x) => mmap.update(i, x + 1)
      }

      val first = iterator.next

      val pairedEnd = first.pairedEnd

      assert(!pairedEnd, "paired end counting not implemented")

      (List(first).iterator ++ iterator).foreach { samRecord =>

        // unmapped read
        if (samRecord.unmappedFlag) {
          notaligned += 1;
        } else if ((samRecord.NH.isDefined
          && samRecord.NH.get > 1)) {
          countMultiMapped(samRecord)
        } else if (samRecord.mapQ < minAverageQual) {
          lowqual += 1;
        } else {

          val fs = overlaps(samRecord)

          fs.size match {
            case 0 => { empty += 1 }
            case 1 => count(fs.head)
            case _ => { ambiguous += 1 }
          }
        }

      }

      countMultiMappedFinish

      mmap.update("__no_feature", empty)
      mmap.update("__ambiguous", ambiguous)
      mmap.update("__too_low_aQual", lowqual)
      mmap.update("__not_aligned", notaligned)
      mmap.update("__alignment_not_unique", nonunique)

      mmap.toMap
    }

  }

}

