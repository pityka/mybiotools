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

package rnaseqalign

import picard.sam._
import htsjdk.samtools.SAMFileHeader.SortOrder;
import htsjdk.samtools.util.Iso8601Date;
import htsjdk.samtools.{ SamFileHeaderMerger, MergingSamRecordIterator }
import htsjdk.samtools.fastq.FastqReader
import java.io._
import scala.collection.JavaConversions._
import scala.util.{ Try, Success, Failure }
import picard.fastq._
import htsjdk.samtools.{ CigarOperator => SAMCO, SAMFileReader, SAMFileWriterFactory, SAMFileHeader, SAMReadGroupRecord, SAMTag }
import rnaseqalign.htseqcount._
import mybiotools._
import mybiotools.workflows._
import mybiotools.intervaltree._
import mybiotools.gwascommons._

import org.saddle._
import scala.reflect.ClassTag

object Helpers {

  def translateHGNC(ensemble: String, map: Map[String, String], reverse: Map[String, Int]): String = {
    val withoutdot = ensemble.split("\\.").head
    val candidate = map.get(withoutdot)
    candidate.map { c =>
      if (reverse(c) > 1) ensemble
      else c
    }.getOrElse(ensemble)

  }

  def readEnsembleHGNCTable: (Map[String, String], Map[String, Int]) = {
    val source = scala.io.Source.fromInputStream(getClass.getResourceAsStream("/ensembl_hgnc.txt"))
    val m = source.getLines.map { l =>
      val spl = fastSplitSetSeparator(l, Set(' ', '\t'))
      spl(0) -> spl(1)
    }.toMap
    source.close
    val reverse = m.toSeq.groupBy(_._2).map(x => x._1 -> x._2.size).toMap
    (m, reverse)
  }

  def openIteratorOnBam(bam: File, bai: File, region: Region) = {
    val reader = new SAMFileReader(bam, bai)
    reader.setValidationStringency(htsjdk.samtools.ValidationStringency.SILENT)
    reader.query(region.chromosome, region.from + 1, region.to, false).map(x => SAMRecordEssentials(x).map(y => y -> x.getReadString)).filter(_.isDefined).map(_.get)
  }

  def openIteratorOnBam(bam: File, assertCoordinateSort: Boolean) = {
    import SAMFileHeader.SortOrder
    val reader = new SAMFileReader(bam)
    reader.setValidationStringency(htsjdk.samtools.ValidationStringency.SILENT)
    if (assertCoordinateSort) {
      assert(reader.getFileHeader.getSortOrder == SortOrder.coordinate, s"bam file $bam is not coordinate sorted ")
    }
    reader.iterator.map(x => SAMRecordEssentials(x).map(y => y -> x.getReadString)).filter(_.isDefined).map(_.get)
  }

  def readCountPlotFromBamFile(bam: File, bai: File, strandedness: Strandedness, region: Region) = {
    val iterator = {
      val reader = new SAMFileReader(bam, bai)
      reader.setValidationStringency(htsjdk.samtools.ValidationStringency.SILENT)
      reader.query(region.chromosome, region.from + 1, region.to, false).map(SAMRecordEssentials.apply).filter(_.isDefined).map(_.get)
    }

    ReadCountPlot.plot(iterator, strandedness)

  }

  def readCountPlotFromMultipleBamFiles(
    bams: Seq[(File, File, Double)],
    strandedness: Strandedness,
    region: Region
  ) = {

    val iterators = bams.map {
      case (bam, bai, sizeFactor) =>
        val reader = new SAMFileReader(bam, bai)
        reader.setValidationStringency(htsjdk.samtools.ValidationStringency.SILENT)
        (bam.getName, reader.query(region.chromosome, region.from + 1, region.to, false).map(SAMRecordEssentials.apply).filter(_.isDefined).map(_.get), sizeFactor)
    }

    ReadCountPlot.plotMultiple(iterators, strandedness)

  }

  def htseqCountFromBam(in: InputStream, gtfcontents: List[GFFEntry], strandedness: Strandedness, minQual: Int, allowMultiMapInSameGene: Boolean): Map[String, Long] =
    htseqCountFromBam(in, HTSeqCount.createIntervalTree(gtfcontents), strandedness, minQual, allowMultiMapInSameGene)

  def htseqCountFromBam(in: InputStream, intervaltrees: Map[String, IntervalTree[GFFEntry]], strandedness: Strandedness, minQual: Int, allowMultiMapInSameGene: Boolean): Map[String, Long] = {
    val iterator = {
      val reader = new SAMFileReader(in)
      reader.setValidationStringency(htsjdk.samtools.ValidationStringency.SILENT)
      reader.iterator.map(SAMRecordEssentials.apply).filter(_.isDefined).map(_.get)
    }

    HTSeqCount.countSingleEndReadsInFeatures(
      iterator = iterator,
      intervaltrees = intervaltrees,
      strandedness = strandedness,
      minAverageQual = minQual,
      allowMultiMapInSameGene = allowMultiMapInSameGene
    )
  }

  def mergeUnsortedBams(input: List[File], outFile: File): Unit = {

    val (readers, headers) = input.map { inFile =>
      val reader = new SAMFileReader(inFile)
      val header = reader.getFileHeader
      (reader, header)
    }.toSeq.unzip

    val headerMergerSortOrder = SAMFileHeader.SortOrder.unsorted

    val headerMerger = new SamFileHeaderMerger(headerMergerSortOrder, headers, false);
    val iterator = new MergingSamRecordIterator(headerMerger, readers, true);
    val header = headerMerger.getMergedHeader();

    header.setSortOrder(headerMergerSortOrder);

    val out = new SAMFileWriterFactory().makeBAMWriter(header, true, outFile);

    while (iterator.hasNext()) {
      out.addAlignment(iterator.next)
    }

    out.close
  }

  def getFlowCellInfosFromFastQHeader(br: BufferedReader): Set[FlowCellInfo] = {

    val regexp1 = "[^:]+:[^:]+:([^:]+):([0-9]+):[0-9]+:[0-9]+:[0-9]+ [0-2]+:[YN]:[0-9]+:([ATGCNU]+)".r

    val regexp2 = "[^:]+:([0-9]+):[^:]+:[^:]+:[^:]+#([^:]+).*".r

    val reader = new FastqReader(br)
    var set = Set[FlowCellInfo]()
    val indexCounts = collection.mutable.Map[String, Int]()
    reader.iterator.foreach { fqr =>
      val header = fqr.getReadHeader

      val fc = header match {
        case regexp1(fc, lane, index) => FlowCellInfo(fc, lane, index)
        case _ => {
          header match {
            case regexp2(lane, index) => FlowCellInfo("", lane, index)
            case _ => throw new RuntimeException("can't extract flowcell,lane,index from fastq header: " + header)
          }
        }
      }
      indexCounts.get(fc.indexSequence) match {
        case None => indexCounts.update(fc.indexSequence, 1)
        case Some(x) => indexCounts.update(fc.indexSequence, 1 + x)
      }
      set = set + fc
    }

    reader.close

    val mostAbundantIndex = indexCounts.toSeq.maxBy(_._2)._1

    set.toSet.map((x: FlowCellInfo) => x.copy(indexSequence = mostAbundantIndex))
  }

}