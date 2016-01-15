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

import rnaseqalign.tasks._

import org.scalatest._

import mybiotools.tasks._
import scala.concurrent.ExecutionContext.Implicits.global

import org.scalatest.FunSpec
import org.scalatest.Matchers
import mybiotools.gwascommons._
import mybiotools.saddlehelpers._
import mybiotools.workflows._
import mybiotools.stringstore._
import mybiotools.sequence.alignment._

object TestsAlign {

  val tmpfolder = mybiotools.TempFile.createTempFile("")
  tmpfolder.delete
  tmpfolder.mkdir

  val ts = defaultTaskSystem(s"""
    tasks.cacheEnabled=false
    tasks.logFile="" 
    tasks.fileServiceBaseFolder = "${tmpfolder.getAbsolutePath}"
    """)
  import ts._

  val fastq = new java.io.File(getClass.getResource("/").getPath + "/small.fastq.txt")

  val referenceFolder =
    new java.io.File(getClass.getResource("/").getPath + "/chrLength.txt") ::
      new java.io.File(getClass.getResource("/").getPath + "/chrName.txt") ::
      new java.io.File(getClass.getResource("/").getPath + "/chrNameLength.txt") ::
      new java.io.File(getClass.getResource("/").getPath + "/chrStart.txt") ::
      new java.io.File(getClass.getResource("/").getPath + "/Genome") ::
      new java.io.File(getClass.getResource("/").getPath + "/genomeParameters.txt") ::
      new java.io.File(getClass.getResource("/").getPath + "/SA") ::
      new java.io.File(getClass.getResource("/").getPath + "/SAindex") :: Nil

  val gtf = new java.io.File(getClass.getResource("/").getPath + "/example.gtf")

  val adapter = ""
  val readGroup = ReadGroupContents(cn = "cn", ds = "ds", dt = "2014-01-01", lb = "lb", pl = "pl", pu = "pu", sm = "sm")
  val extraparams = ""

  val input = AlignRNASeqInput(
    fastq = fastq, refIndex = referenceFolder, adapter = adapter, readgroup = readGroup, starparameters = extraparams
  )

  val aligntask = fastq2bam(input, cpu = 1, memory = 100)

  val mergetask = mergeUnsortedBams(
    in = MergeBamInput(1, outname = "merge"),
    memory = 100
  )

  val htseqtask = htseqcountTask(
    in = HTSeqInput(gtf = gtf, gtfparam = GTFFileParameters(s8"transcript_id", s8"gene_id", s8"gene", false), strandedness = htseqcount.NotStranded, minQual = 0,
      allowMultiMapInSameGene = false, columnName = "whatever"),
    memory = 100
  )

  val sorttask = sortbam(SortBamInput.empty, memory = 100)
  val indextask = indexbam(IndexBamInput.empty, memory = 100)
  val bambaiaggregatortask = bambaiaggregator(BamBaiInput.empty)
  val readcountplottask = readcountplot(ReadCountInput(
    List(("dfsd", Region("chr1", 0, 30))),
    htseqcount.NotStranded
  ))

  aligntask ~> mergetask ~> htseqtask
  mergetask ~> sorttask ~> indextask
  indextask ~> bambaiaggregatortask
  sorttask ~> bambaiaggregatortask
  bambaiaggregatortask ~> readcountplottask

  val aligned = aligntask.?![AlignOut]
  val merged = mergetask.?![BamFile]
  val counts = htseqtask.?![HTSeqCounts]
  val plots = readcountplottask.?![ReadCountPlots].plots.map(_._2.file)

  val bamfile = aligned.bam.file
  val bamname = aligned.bam.name

  val starlogfile = aligned.starlog.file
  val starlogname = aligned.starlog.name

  val starsplicejunctionsfile = aligned.starsplicejunctions.file
  val starsplicejunctionsname = aligned.starsplicejunctions.name

  val mergedfile = merged.file.file
  val mergedname = merged.file.name

  val countsfile = counts.countsInFile.file
  val countsfilename = counts.countsInFile.name
  val countsframe = envelopeToFrame(counts.counts)

  ts.shutdown

}

class TestsAlignSuite extends FunSuite with Matchers {

  test("align -> sam -> bam") {
    TestsAlign.bamfile.canRead should be(true)
    TestsAlign.starlogfile.canRead should be(true)
    TestsAlign.starsplicejunctionsfile.canRead should be(true)
    TestsAlign.mergedfile.canRead should be(true)
    TestsAlign.countsfile.canRead should be(true)
    TestsAlign.bamname should be("small.fastq.txt.bam")
    TestsAlign.starlogname should be("small.fastq.txt.star.log")
    TestsAlign.starsplicejunctionsname should be("small.fastq.txt.star.SJ.out.tab")
    TestsAlign.mergedname should be("merge.merged.bam")
    TestsAlign.countsfilename should be("whatever.tsv")

    TestsAlign.countsframe(Array("ENSG00000223972.4"), Array("whatever")).raw(0, 0) should equal(1)
    TestsAlign.plots.head.canRead should be(true)

  }

}