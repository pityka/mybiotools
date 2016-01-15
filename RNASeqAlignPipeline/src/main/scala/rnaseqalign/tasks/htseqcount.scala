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

package rnaseqalign.tasks

import mybiotools.tasks._
import java.io.File
import mybiotools._

import scala.sys.process._
import scala.concurrent._
import scala.concurrent.duration._
import java.util.concurrent.Executors
import rnaseqalign.Helpers
import scala.util.{ Try, Success, Failure }
import htsjdk.samtools.SAMFileReader
import htsjdk.samtools.{ CigarOperator => SAMCO }
import scala.collection.JavaConversions._
import mybiotools.saddlehelpers._
import mybiotools.workflows._

import rnaseqalign._
import rnaseqalign.htseqcount._
import org.saddle._
import mybiotools.intervaltree._
import mybiotools.stringstore._
import mybiotools.eq._

case class GTFFileParameters(
  Transcript_id: String8,
  Gene_id: String8,
  Exon: String8,
  resolveLongestTranscript: Boolean
) extends Result

case class HTSeqCounts(
  counts: FrameInSerializableEnvelope[String, String, Long],
  countsInFile: SharedFile,
  lengths: Map[String, Int],
  lengthsInFile: SharedFile
) extends ResultWithSharedFiles(countsInFile, lengthsInFile)

case class HTSeqInput(
  bam: Option[SharedFile],
  gtf: Option[SharedFile],
  gtfFileParameters: Option[GTFFileParameters],
  strandedness: Option[Strandedness],
  minQual: Option[Int],
  allowMultiMapInSameGene: Option[Boolean],
  columnName: Option[String]
) extends SimplePrerequisitive[HTSeqInput]

object HTSeqInput {

  def apply(gtf: File, gtfparam: GTFFileParameters, strandedness: Strandedness, minQual: Int, allowMultiMapInSameGene: Boolean, columnName: String)(implicit components: TaskSystemComponents): HTSeqInput = {

    HTSeqInput(
      gtf = Some(SharedFile(gtf)),
      gtfFileParameters = Some(gtfparam),
      bam = None,
      strandedness = Some(strandedness),
      minQual = Some(minQual),
      allowMultiMapInSameGene = Some(allowMultiMapInSameGene),
      columnName = Some(columnName)
    )

  }

  def updateMergeBamInput: UpdatePrerequisitive[HTSeqInput] = {
    case (self, i: AlignOut) => self.copy(bam = Some(i.bam))
    case (self, i: BamFile) => self.copy(bam = Some(i.file))
  }

}

object htseqcountTask {
  def apply(
    in: HTSeqInput,
    memory: Int,
    update: UpdatePrerequisitive[HTSeqInput] = HTSeqInput.updateMergeBamInput orElse identity[HTSeqInput]
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = memory)
    ) {

      case (
        HTSeqInput(
          Some(bam),
          Some(gtf),
          Some(GTFFileParameters(transcript_id, gene_id, exon, resolveLongestTranscript)),
          Some(strandedness),
          Some(minQual),
          Some(allowMultiMapInSameGene),
          Some(columnName)), ce) =>
        import ce._

        val (gtfcontents,
          transcriptLenghts) =
          NodeLocalCache.getItemBlocking(s"gtfcontents:${gtf.name}") {

            val rawlist = openSource(gtf.file.getAbsolutePath) { s => HTSeqCount.readGTF(s, gene_id, transcript_id)(_ === exon) }

            val (list, lengths) = HTSeqCount.getLengths(rawlist, transcript_id, gene_id, exon, resolveLongestTranscript)

            val intervaltrees: Map[String, IntervalTree[GFFEntry]] = list.groupBy(_.chromosome).map(x => x._1 -> IntervalTree.makeTree(x._2.sortBy(_.from))).toMap

            (intervaltrees, lengths)

          }

        val counts = openFileInputStream(bam.file) { is =>
          Helpers.htseqCountFromBam(
            in = is,
            intervaltrees = gtfcontents,
            strandedness = strandedness,
            minQual = minQual,
            allowMultiMapInSameGene = allowMultiMapInSameGene
          )
        }

        val frame = Frame((columnName, Series(counts.toSeq: _*)))

        val envelope = frameToEnvelope(frame)

        val tmpfile = TempFile.createTempFile("counts")
        openFileWriter(tmpfile) { writer =>
          counts.foreach {
            case (gene, count) =>
              writer.write(gene + "\t" + count + "\n")
          }
        }

        val tmpfileLengths = TempFile.createTempFile("genelengths")
        openFileWriter(tmpfileLengths) { writer =>
          transcriptLenghts.foreach {
            case (gene, count) =>
              writer.write(gene + "\t" + count + "\n")
          }
        }

        HTSeqCounts(counts = envelope, countsInFile = SharedFile(tmpfile, name = columnName + ".tsv", canMoveAway = true), lengths = transcriptLenghts, lengthsInFile = SharedFile(tmpfileLengths, name = bam.name + ".genelengths.tsv", canMoveAway = true))

    }

}

