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
import rnaseqalign.{ FlowCellInfo }
import mybiotools.workflows._

case class AlignOut(
  bam: SharedFile,
  starlog: SharedFile,
  starsplicejunctions: SharedFile
) extends ResultWithSharedFiles(bam, starlog, starsplicejunctions)

case class AlignRNASeqInput(
  fastq: Option[SharedFile],
  referenceIndexFolder: Option[List[SharedFile]],
  adapterSequence: Option[String],
  readgroup: Option[ReadGroupContents],
  starparameters: Option[String]
) extends SimplePrerequisitive[AlignRNASeqInput]

object AlignRNASeqInput {

  def apply(fastq: File, refIndex: List[File], adapter: String, readgroup: ReadGroupContents, starparameters: String)(implicit components: TaskSystemComponents): AlignRNASeqInput = {

    AlignRNASeqInput(
      referenceIndexFolder = Some(refIndex.map(f => SharedFile(f))),
      adapterSequence = Some(adapter),
      readgroup = Some(readgroup),
      fastq = Some(SharedFile(fastq)),
      starparameters = Some(starparameters)
    )

  }

}

object fastq2bam {
  def apply(
    in: AlignRNASeqInput,
    memory: Int,
    cpu: Int,
    update: UpdatePrerequisitive[AlignRNASeqInput] = identity[AlignRNASeqInput]
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = cpu, memory = memory)
    ) {

      case (
        AlignRNASeqInput(
          Some(fastq),
          Some(referenceIndexFolder),
          Some(adapter),
          Some(readgroup),
          Some(starparameters)), ce) =>
        import ce._

        {

          val (readGroupFromFastq, indexSequence) = {
            val flowcellinfos = openFileReader(fastq.file) { br =>
              Helpers.getFlowCellInfosFromFastQHeader(br)
            }
            assert(flowcellinfos.map(x => (x.flowcell, x.lane)).toSet.size == 1, "Different flowcells or lanes in 1 fastq. " + flowcellinfos)
            val fc = flowcellinfos.head.flowcell
            val lane = flowcellinfos.head.lane
            val indices = flowcellinfos.map(_.indexSequence).mkString("-")
            val pu = fc + "-" + indices + "." + lane
            (readgroup.copy(pu = pu), flowcellinfos.head.indexSequence)
          }

          val cpu = resourceAllocated.cpu

          val adapterForStar = if (adapter.size > 0) adapter.replace("INDEX", indexSequence) else "-"

          val prefix = TempFile.createTempFile(fastq.name).getAbsolutePath

          val genomefolder = new File(prefix + ".genomedir")
          genomefolder.mkdir

          referenceIndexFolder.foreach { sf =>
            val link = new File(genomefolder, sf.file.getName)
            if (!link.exists) {
              createSymbolicLink(link, sf.file)
            }

          }

          val readFilesCommand = if (isGZipFile(fastq.file.getAbsolutePath)) "zcat" else "-"

          val starcmd = s"STAR $starparameters --outFileNamePrefix $prefix --outStd SAM --runThreadN $cpu --genomeDir ${genomefolder.getAbsolutePath} --readFilesIn ${fastq.file} --readFilesCommand $readFilesCommand --clip3pAdapterSeq $adapterForStar --outSAMattributes All --outSAMunmapped Within"

          println(starcmd)

          val starlog = new File(prefix + "Log.final.out")
          val starsplicejunctions = new File(prefix + "SJ.out.tab")

          val tmpbam = TempFile.createTempFile(".bam")

          val executorService = Executors.newSingleThreadExecutor
          implicit val ec = ExecutionContext.fromExecutorService(executorService)

          val process = Process(starcmd).run(new ProcessIO(
            in = _.close,
            out = { is =>
            AddReadGroup.addReadGroup(is, tmpbam, readGroupFromFastq)
            is.close
          },
            err = { is =>
            io.Source.fromInputStream(is).getLines.map { x => println(x + "XXX"); log.error(x) }
            is.close
          }
          ))
          val f = Future { process.exitValue }
          try {
            Await.result(f, atMost = 168 hours)
          } catch {
            case t: TimeoutException => { throw t }
          } finally {
            scala.util.Try {
              process.destroy
            }
            scala.util.Try {
              executorService.shutdownNow
            }
          }
          val exitValue = f.value.map(_.toOption).flatten

          if (exitValue != Some(0)) throw new RuntimeException(s"starAligner exit code: $exitValue")

          AlignOut(bam = SharedFile(tmpbam, name = fastq.name + ".bam"), starlog = SharedFile(starlog, name = fastq.name + ".star.log"), starsplicejunctions = SharedFile(starsplicejunctions, name = fastq.name + ".star.SJ.out.tab"))

        }

    }

}

