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

package genotyper
import genotyper.tasks._
import mybiotools.gwascommons._
import java.io.File
import mybiotools._
import htsjdk.samtools.{ SAMFileReader, SAMFileWriterFactory, SAMSequenceRecord, SAMSequenceDictionary, SAMFileHeader, SAMRecord, SAMTextHeaderCodec }
import scala.collection.JavaConversions._
import scala.util.Try
import mybiotools.stringstore._
import java.nio.{ ByteBuffer, ByteOrder }
import htsjdk.samtools.cram.ref.ReferenceSource
import htsjdk.samtools._
import mybiotools.eq._

object Helpers {

  def plotDensity(d1: Seq[(Region, Double)], d2: Seq[(Region, Double)]) = {
    import mybiotools.plots._
    import ScatterPlot._
    implicit val order = regionOrderingByStart[String8, GenomicLocation, Region]

    val d = mybiotools.addMaps(d1.toMap.filter(_._2 > 0), d2.toMap.filter(_._2 > 0))((x, y) => (x.toDouble - y) / y.toDouble).toSeq

    val dd = d.sortBy(_._1).zipWithIndex.map(x => x._2.toDouble -> x._1._2.toDouble).toIndexedSeq
    val p = ScatterPlot.createScatterPlotFromMultiple(List(ScatterPlot.Label("", java.awt.Color.BLACK, new java.awt.BasicStroke(1.0f)) -> dd), ylim = Some((-2.0, 2.0)), main = "x-y/y")
    p.setBounds(0, 0, 1000, 1000)
    p
  }

  def slidingVariantDensity(
    reader: htsjdk.tribble.AbstractFeatureReader[vcfhelpers.QuickTribbleFeature, _],
    regions: Set[Region],
    windowSize: Int,
    minAF: Double,
    maxAF: Double
  ): Seq[(Region, Double)] = {

    val header = reader.getHeader

    implicit val order = regionOrderingByStart[String8, GenomicLocation, Region]
    val windows: Seq[Region] = regions.toList.sorted.flatMap { region =>
      region.from until region.to by windowSize map { windowstart =>
        Region(region.chromosome, windowstart, math.min(windowstart + windowSize, region.to))
      }
    }

    windows.map { iv =>
      iv -> iterableAsScalaIterable(reader.query(iv.chromosome, iv.from + 1, iv.to)).filter { line =>
        val spl = line.line.fastSplit('\t')
        val filter = spl(6)
        val af = {
          spl(7).fastSplitIterator(';').find(_.startsWith("AF=")).map(_.drop(3).fastSplitIterator(',').next.toDouble).getOrElse(0.0)
        }
        (filter === "PASS" || filter === ".") && (af >= minAF && af <= maxAF)
      }.size.toDouble
    }

  }

  def mergeCram2Bam(input: List[File], ref: File, out: File): Unit = {

    val readers = input.map { inFile =>
      val reader = new htsjdk.samtools.SamReader.PrimitiveSamReaderToSamReaderAdapter(
        new CRAMFileReader(inFile, new ReferenceSource(ref)), SamInputResource.of(inFile)
      )
      reader

    }.toSeq
    val headerMerger = new SamFileHeaderMerger(readers, htsjdk.samtools.SAMFileHeader.SortOrder.coordinate, false);

    val iterator = new MergingSamRecordIterator(headerMerger, readers, true);
    val header = headerMerger.getMergedHeader();

    val writer = new SAMFileWriterFactory().makeBAMWriter(header, true, out);

    while (iterator.hasNext()) {
      writer.addAlignment(iterator.next)
    }

    writer.close

  }

  def subsetSortAddChrDedup(bam: File, bai: File, intervals: Iterable[Region], deduplicate: Boolean, dedupjar: File, maxmem: Int): (File, File) = {

    def replace(oldname: String) = {
      if (oldname startsWith "chr") oldname
      else {
        if (oldname.toLowerCase startsWith "chr") "chr" + oldname.drop(3)
        else {
          if (AddChrToBamHelper.isChr(oldname)) "chr" + oldname
          else oldname
        }
      }
    }

    val tmpout = TempFile.createTempFile(".bam")
    val reader = new SAMFileReader(bam, bai)
    reader.setValidationStringency(htsjdk.samtools.ValidationStringency.SILENT)
    val oldheader = reader.getFileHeader
    val sqs = oldheader.getSequenceDictionary.getSequences.map { sq =>
      new SAMSequenceRecord(replace(sq.getSequenceName), sq.getSequenceLength)
    }
    val newheader = oldheader.clone
    newheader.setSequenceDictionary(new SAMSequenceDictionary(sqs))
    // newheader.setSortOrder(SAMFileHeader.SortOrder.unsorted)

    val writer = {
      val fac = new SAMFileWriterFactory
      fac.setUseAsyncIo(false)
      fac.makeBAMWriter(newheader, false, tmpout)
    }

    val prefix = {
      val it = reader.iterator
      val x = it.next.getReferenceName
      val y = if (AddChrToBamHelper.isChr(x)) "" else x.take(3)
      it.close
      y
    }

    val alreadyWritten = scala.collection.mutable.Set[(String, String8, Int, String, Int, Boolean)]()

    Region.collapse[String8, GenomicLocation, Region](intervals, 0).foreach { interval =>
      val iter = reader.queryOverlapping(prefix + interval.chromosome, interval.from + 1, interval.to)
      iter.foreach { record =>
        if (!alreadyWritten.contains((record.getReadName, StringStore(record.getReferenceName), record.getAlignmentStart, record.getCigar.toString, record.getReadLength, record.getReadNegativeStrandFlag))) {

          writer.addAlignment(record)
          alreadyWritten += {
            (record.getReadName, StringStore(record.getReferenceName), record.getAlignmentStart, record.getCigar.toString, record.getReadLength, record.getReadNegativeStrandFlag)
          }
        }
      }
      iter.close
    }
    writer.close

    reader.close

    val deduplicated = if (!deduplicate) tmpout else {
      val tmpdedup = TempFile.createTempFile(".bam")
      val cmd = s"java -Xmx${maxmem}m -jar ${dedupjar.getAbsolutePath} INPUT=${tmpout.getAbsolutePath} OUTPUT=${tmpdedup.getAbsolutePath} METRICS_FILE=${tmpdedup.getAbsolutePath + ".metrics.txt"}"

      val (stdout, stderr, succ) = execGetStreamsAndCode(cmd, unsuccessfulOnErrorStream = false)

      if (!succ) throw new RuntimeException("error in picard during dedup" + stdout.mkString("\n") + stderr.mkString("\n"))

      // delete previous stage
      tmpout.delete

      tmpdedup
    }

    val index = {
      val cmd = s"samtools index ${deduplicated.getAbsolutePath}"

      val (stdout, stderr, succ) = execGetStreamsAndCode(cmd)

      if (!succ) throw new RuntimeException("error in samtools during index" + stdout.mkString("\n") + stderr.mkString("\n"))

      new File(deduplicated.getAbsolutePath + ".bai")
    }
    (deduplicated, index)
  }

  def putBesidesBam(bam: File, bai: File): (File, File) = {
    def besides(a: File, b: File): Boolean = a.getCanonicalPath.dropRight(3) == b.getCanonicalPath.dropRight(3)

    if (!besides(bam, bai)) {
      val tmp = mybiotools.TempFile.createTempFile("bambesideidx")
      val bamfilepath = java.nio.file.Paths.get(bam.getCanonicalPath)
      val bamfilelinkpath = java.nio.file.Paths.get(tmp.getCanonicalPath + (".bam"))
      java.nio.file.Files.createSymbolicLink(bamfilelinkpath, bamfilepath)

      val idxfilepath = java.nio.file.Paths.get(bai.getCanonicalPath)
      val idxfilelinkpath = java.nio.file.Paths.get(tmp.getCanonicalPath + (".bai"))
      java.nio.file.Files.createSymbolicLink(idxfilelinkpath, idxfilepath)
      (new File(tmp.getCanonicalPath + (".bam")), new File(tmp.getCanonicalPath + (".bai")))
    } else (bam, bai)
  }

  def putBesidesFasta(fasta: File, fai: File, dict: File): (File, File, File) = {

    val tmp = mybiotools.TempFile.createTempFile("fastafaidict")
    val fastafilepath = java.nio.file.Paths.get(fasta.getCanonicalPath)
    val fastafilelinkpath = java.nio.file.Paths.get(tmp.getCanonicalPath + (".fasta"))
    java.nio.file.Files.createSymbolicLink(fastafilelinkpath, fastafilepath)

    val faifilepath = java.nio.file.Paths.get(fai.getCanonicalPath)
    val faifilelinkpath = java.nio.file.Paths.get(tmp.getCanonicalPath + (".fasta.fai"))
    java.nio.file.Files.createSymbolicLink(faifilelinkpath, faifilepath)

    val dictfilepath = java.nio.file.Paths.get(dict.getCanonicalPath)
    val dictfilelinkpath = java.nio.file.Paths.get(tmp.getCanonicalPath + (".dict"))
    java.nio.file.Files.createSymbolicLink(dictfilelinkpath, dictfilepath)

    (new File(tmp.getCanonicalPath + (".fasta")), new File(tmp.getCanonicalPath + (".fasta.fai")), new File(tmp.getCanonicalPath + (".dict")))
  }
}
