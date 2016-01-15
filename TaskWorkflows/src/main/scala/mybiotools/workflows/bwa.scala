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

package mybiotools.workflows

import mybiotools.tasks._
import java.io.File
import mybiotools._
import mybiotools.gwascommons._

import mybiotools.workflows._

import scala.collection.JavaConversions._
import htsjdk.samtools.util.Iso8601Date
import htsjdk.samtools._
import scala.sys.process._
import scala.concurrent._
import scala.concurrent.duration._
import java.util.concurrent.Executors
import htsjdk.samtools.cram.ref.ReferenceSource

object AddReadGroup {
  def getReadGroup(is: java.io.InputStream): Seq[ReadGroupContents] = {
    val in = new SAMFileReader(is);
    in.getFileHeader().getReadGroups.map { samrg =>
      ReadGroupContents(
        cn = samrg.getSequencingCenter,
        ds = samrg.getDescription,
        dt = samrg.getRunDate.toString,
        lb = samrg.getLibrary,
        pl = samrg.getPlatform,
        pu = samrg.getPlatformUnit,
        sm = samrg.getSample
      )
    }
  }
  def getReadGroupCRAM(f: File, ref: File): Seq[ReadGroupContents] = {
    val in = new CRAMFileReader(f, new ReferenceSource(ref));
    in.getFileHeader().getReadGroups.map { samrg =>
      ReadGroupContents(
        cn = samrg.getSequencingCenter,
        ds = samrg.getDescription,
        dt = samrg.getRunDate.toString,
        lb = samrg.getLibrary,
        pl = samrg.getPlatform,
        pu = samrg.getPlatformUnit,
        sm = samrg.getSample
      )
    }
  }
  def addReadGroup(is: java.io.InputStream, out: File, readgroup: ReadGroupContents): Unit = {

    val in = new SAMFileReader(is);

    val RGID = readgroup.toString

    // create the read group we'll be using
    val rg = new SAMReadGroupRecord(RGID);
    if (readgroup.lb != "") {
      rg.setLibrary(readgroup.lb);
    }
    if (readgroup.pl != "") {
      rg.setPlatform(readgroup.pl);
    }
    if (readgroup.sm != "") {
      rg.setSample(readgroup.sm);
    }
    if (readgroup.pu != "") {
      rg.setPlatformUnit(readgroup.pu);
    }
    if (readgroup.cn != "") {
      rg.setSequencingCenter(readgroup.cn);
    }
    if (readgroup.ds != "") {
      rg.setDescription(readgroup.ds);
    }
    if (readgroup.dt != "") {
      rg.setRunDate(new Iso8601Date(readgroup.dt))
    }

    // create the new header and output file
    val inHeader = in.getFileHeader();
    val outHeader = inHeader.clone();
    outHeader.setReadGroups(List(rg));

    val outWriter = new SAMFileWriterFactory()
      .makeBAMWriter(
        outHeader,
        true,
        out
      )

    in.iterator.foreach { read =>
      read.setAttribute(SAMTag.RG.name(), RGID);
      outWriter.addAlignment(read);
    }
    outWriter.close

  }

}

case class BWAIndex(indexfiles: List[(String, SharedFile)]) extends ResultWithSharedFiles(indexfiles.map(_._2).toSeq: _*)

case class BWAIndexGenerationInput(
  fasta: Option[SharedFile],
  extraArgs: Option[String]
) extends SimplePrerequisitive[BWAIndexGenerationInput]

object BWAIndexGenerationInput {

  def apply(referenceFasta: File, extraArgs: String)(implicit components: TaskSystemComponents): BWAIndexGenerationInput = {

    BWAIndexGenerationInput(
      fasta = Some(SharedFile(referenceFasta)),
      extraArgs = Some(extraArgs)
    )

  }

  def updateBWAIndexGenerationInput: UpdatePrerequisitive[BWAIndexGenerationInput] = {
    case (self, i: FastaWithIndex) => self.copy(fasta = Some(i.fasta))
  }

}

case class BWAAlignInput(
  index: Option[BWAIndex],
  reads: Option[List[SharedFile]],
  extraArgs: Option[String],
  name: Option[String],
  readgroup: Option[ReadGroupContents]
) extends SimplePrerequisitive[BWAAlignInput]

object BWAAlignInput {

  def apply(files: List[File], extraArgs: String, name: String, readgroup: ReadGroupContents)(implicit components: TaskSystemComponents): BWAAlignInput = {

    BWAAlignInput(
      extraArgs = Some(extraArgs),
      reads = Some(files.map(f => SharedFile(f))),
      index = None,
      name = Some(name),
      readgroup = Some(readgroup)
    )

  }

  def updateBWAAlignInput: UpdatePrerequisitive[BWAAlignInput] = {
    case (self, i: BWAIndex) => self.copy(index = Some(i))
  }

}

object bwaAlign {
  def apply(
    in: BWAAlignInput,
    memory: Int,
    cpu: Int,
    update: UpdatePrerequisitive[BWAAlignInput] = BWAAlignInput.updateBWAAlignInput orElse identity[BWAAlignInput]
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = cpu, memory = memory)
    ) {
      case (
        BWAAlignInput(
          Some(index),
          Some(reads),
          Some(extraArgs),
          Some(name),
          Some(readgroup)), ce) =>
        import ce._

        val indexFolder = TempFile.createTempFile("indexfolder")
        indexFolder.delete
        indexFolder.mkdir
        index.indexfiles.foreach {
          case (name, sf) =>
            java.nio.file.Files.createSymbolicLink(java.nio.file.Paths.get(new File(indexFolder, name).getAbsolutePath), java.nio.file.Paths.get(sf.localFile.getAbsolutePath))
        }

        val indexprefix = indexFolder.getAbsolutePath + "/" + index.indexfiles.head._1.fastSplit('.').dropRight(1).mkString(".")

        val out = TempFile.createTempFile("aligned.bam")

        val cpu = resourceAllocated.cpu

        val cmd = s"bwa mem -t $cpu $extraArgs $indexprefix ${reads.map(_.localFile.getAbsolutePath).mkString(" ")}"

        val tmpbam = TempFile.createTempFile(".bam")

        val executorService = Executors.newSingleThreadExecutor
        implicit val ec = ExecutionContext.fromExecutorService(executorService)

        val process = Process(cmd).run(new ProcessIO(
          in = _.close,
          out = { is =>
          AddReadGroup.addReadGroup(is, tmpbam, readgroup)
          is.close
        },
          err = { is =>
          io.Source.fromInputStream(is).getLines.foreach { x => log.debug(x) }
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

        if (exitValue != Some(0)) throw new RuntimeException(s"bwa exit code: $exitValue")

        BamFile(SharedFile(tmpbam, name = name + ".bam", canMoveAway = true))

    }

}

object bwaIndexGeneration {
  def apply(
    in: BWAIndexGenerationInput,
    memory: Int,
    cpu: Int,
    update: UpdatePrerequisitive[BWAIndexGenerationInput] = BWAIndexGenerationInput.updateBWAIndexGenerationInput orElse identity[BWAIndexGenerationInput]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = cpu, memory = memory)
    ) {
      case (
        BWAIndexGenerationInput(
          Some(fasta),
          Some(extraArgs)), components) =>
        import components._

        val prefix = TempFile.createTempFile("bwaindexfolder")
        prefix.delete

        val cpu = resourceAllocated.cpu

        val cmd = s"bwa index -p $prefix $extraArgs ${fasta.file.getAbsolutePath} "

        val (stdout, stderr, succ) = execGetStreamsAndCodeWithLog(cmd, unsuccessfulOnErrorStream = false)

        if (!succ) throw new RuntimeException("error in bwa index" + stdout.mkString("\n") + stderr.mkString("\n"))

        val files = List(
          fasta.name + ".sa" -> new File(prefix + ".sa"),
          fasta.name + ".pac" -> new File(prefix + ".pac"),
          fasta.name + ".ann" -> new File(prefix + ".ann"),
          fasta.name + ".bwt" -> new File(prefix + ".bwt"),
          fasta.name + ".amb" -> new File(prefix + ".amb")
        ).map(f => f._1 -> SharedFile(f._2, name = f._1, canMoveAway = true))

        BWAIndex(files)

    }

}

