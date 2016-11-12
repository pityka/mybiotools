// /*
// * The MIT License
// *
// * Copyright (c) 2015 ECOLE POLYTECHNIQUE FEDERALE DE LAUSANNE, Switzerland,
// * Group Fellay
// *
// * Permission is hereby granted, free of charge, to any person obtaining
// * a copy of this software and associated documentation files (the "Software"),
// * to deal in the Software without restriction, including without limitation
// * the rights to use, copy, modify, merge, publish, distribute, sublicense,
// * and/or sell copies of the Software, and to permit persons to whom the Software
// * is furnished to do so, subject to the following conditions:
// *
// * The above copyright notice and this permission notice shall be included in all
// * copies or substantial portions of the Software.
// *
// * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// * SOFTWARE.
// */
//
// package mybiotools.workflows
//
// import mybiotools.tasks._
// import java.io.File
// import mybiotools._
// import mybiotools.gwascommons._
//
// import mybiotools.workflows._
//
// import scala.collection.JavaConversions._
// import htsjdk.samtools.util.Iso8601Date
// import htsjdk.samtools._
//
// case class ReadGroupContents(cn: String, ds: String, dt: String, lb: String, pl: String, pu: String, sm: String) {
//   override def toString = s"""RG(CN$cn,DS$ds,DT$dt,LB$lb,PL$pl,PU$pu,SM$sm)"""
// }
//
// object Reheader {
//
//   def reheader(bam: File, header: SAMFileHeader): (File, File) = {
//
//     val tmp = TempFile.createTempFile(".bam")
//     htsjdk.samtools.BamFileIoUtils.reheaderBamFile(header, bam, tmp, false, true)
//
//     (tmp, new File(tmp.getAbsolutePath.dropRight(3) + "bai"))
//
//   }
// }
//
// case class FastaWithIndex(fasta: SharedFile, fai: SharedFile, dict: SharedFile) extends ResultWithSharedFiles(fasta, fai, dict)
//
// case class SnapIndex(snapindexfiles: List[(String, SharedFile)]) extends ResultWithSharedFiles(snapindexfiles.map(_._2).toSeq: _*)
//
// case class SnapOutput(bam: SharedFile, bai: SharedFile, log: SharedFile) extends ResultWithSharedFiles(bam, bai, log) with BamWithBai
//
// case class SnapIndexGenerationInput(
//     fasta: Option[SharedFile],
//     seed: Option[Int],
//     extraArgs: Option[String]
// ) extends Prerequisitive[SnapIndexGenerationInput] {
//   def ready = {
//     fasta.isDefined &&
//       seed.isDefined &&
//       extraArgs.isDefined
//   }
// }
//
// object SnapIndexGenerationInput {
//
//   def apply(referenceFasta: File, seed: Int, extraArgs: String)(implicit components: TaskSystemComponents): SnapIndexGenerationInput = {
//
//     SnapIndexGenerationInput(
//       fasta = Some(SharedFile(referenceFasta)),
//       seed = Some(seed),
//       extraArgs = Some(extraArgs)
//     )
//
//   }
//
//   def updateSnapIndexGenerationInput: UpdatePrerequisitive[SnapIndexGenerationInput] = {
//     case (self, i: FastaWithIndex) => self.copy(fasta = Some(i.fasta))
//   }
//
// }
//
// case class SnapAlignInput(
//     index: Option[SnapIndex],
//     reads: Option[List[SharedFile]],
//     extraArgs: Option[String],
//     name: Option[String],
//     readgroup: Option[Option[ReadGroupContents]]
// ) extends Prerequisitive[SnapAlignInput] {
//   def ready = {
//     index.isDefined &&
//       reads.isDefined &&
//       extraArgs.isDefined &&
//       name.isDefined &&
//       readgroup.isDefined
//   }
// }
//
// object SnapAlignInput {
//
//   def apply(files: List[File], extraArgs: String, name: String, readgroup: Option[ReadGroupContents])(implicit components: TaskSystemComponents): SnapAlignInput = {
//
//     SnapAlignInput(
//       extraArgs = Some(extraArgs),
//       reads = Some(files.map(f => SharedFile(f))),
//       index = None,
//       name = Some(name),
//       readgroup = Some(readgroup)
//     )
//
//   }
//
//   def updateSnapAlignInput: UpdatePrerequisitive[SnapAlignInput] = {
//     case (self, i: SnapIndex) => self.copy(index = Some(i))
//   }
//
// }
//
// object snapAlign {
//   def apply(
//     in: SnapAlignInput,
//     memory: Int,
//     cpu: Int,
//     update: UpdatePrerequisitive[SnapAlignInput] = SnapAlignInput.updateSnapAlignInput orElse identity[SnapAlignInput]
//   )(implicit components: TaskSystemComponents) =
//     newTask(
//       in, update, CPUMemoryRequest(cpu = cpu, memory = memory)
//     ) {
//       case (
//         SnapAlignInput(
//           Some(index),
//           Some(reads),
//           Some(extraArgs),
//           Some(name),
//           Some(readgroup)), ce) =>
//         import ce._
//
//         val indexFolder = TempFile.createTempFile("indexfolder")
//         indexFolder.delete
//         indexFolder.mkdir
//         index.snapindexfiles.foreach {
//           case (name, sf) =>
//             java.nio.file.Files.createSymbolicLink(java.nio.file.Paths.get(new File(indexFolder, name).getAbsolutePath), java.nio.file.Paths.get(sf.file.getAbsolutePath))
//         }
//
//         val out = TempFile.createTempFile("aligned.bam")
//
//         val cpu = resourceAllocated.cpu
//
//         val rgstring = if (readgroup.isDefined) s"-rg ${readgroup.get}" else ""
//
//         val cmd = s"snap paired ${indexFolder.getAbsolutePath} ${reads.map(_.localFile.getAbsolutePath).mkString(" ")} -so -o ${out.getAbsolutePath} -t $cpu -sa $extraArgs $rgstring"
//
//         val (stdout, stderr, succ) = execGetStreamsAndCodeWithLog(cmd, unsuccessfulOnErrorStream = false)
//
//         if (!succ) throw new RuntimeException("error in snap during align" + stdout.mkString("\n") + stderr.mkString("\n"))
//
//         val logf = TempFile.createTempFile(".log")
//
//         mybiotools.writeToFile(logf, stdout + "\n" + stderr)
//
//         def replace(oldname: String) = {
//           oldname.split("_").head
//         }
//
//         import htsjdk.samtools.{ SAMFileReader, SAMFileWriterFactory, SAMSequenceRecord, SAMSequenceDictionary }
//         val reader = new SAMFileReader(out)
//         reader.setValidationStringency(htsjdk.samtools.ValidationStringency.SILENT)
//         val oldheader = reader.getFileHeader
//         val sqs = oldheader.getSequenceDictionary.getSequences.map { sq =>
//           new SAMSequenceRecord(replace(sq.getSequenceName), sq.getSequenceLength)
//         }
//         val newheader = oldheader.clone
//         newheader.setSequenceDictionary(new SAMSequenceDictionary(sqs))
//
//         readgroup.foreach { readgroup =>
//           val RGID = readgroup.toString
//
//           // create the read group we'll be using
//           val rg = new htsjdk.samtools.SAMReadGroupRecord(RGID);
//           if (readgroup.lb != "") {
//             rg.setLibrary(readgroup.lb);
//           }
//           if (readgroup.pl != "") {
//             rg.setPlatform(readgroup.pl);
//           }
//           if (readgroup.sm != "") {
//             rg.setSample(readgroup.sm);
//           }
//           if (readgroup.pu != "") {
//             rg.setPlatformUnit(readgroup.pu);
//           }
//           if (readgroup.cn != "") {
//             rg.setSequencingCenter(readgroup.cn);
//           }
//           if (readgroup.ds != "") {
//             rg.setDescription(readgroup.ds);
//           }
//           if (readgroup.dt != "") {
//             rg.setRunDate(new Iso8601Date(readgroup.dt))
//           }
//
//           newheader.setReadGroups(List(rg));
//         }
//
//         reader.close
//
//         val (reheaderbam, bai) = Reheader.reheader(out, newheader)
//
//         out.delete
//
//         SnapOutput(
//           SharedFile(reheaderbam, name = name + ".bam", canMoveAway = true),
//           SharedFile(bai, name = name + ".bam.bai", canMoveAway = true),
//           SharedFile(logf, name = name + ".snaplog", canMoveAway = true)
//         )
//     }
//
// }
//
// object snapIndexGeneration {
//   def apply(
//     in: SnapIndexGenerationInput,
//     memory: Int,
//     cpu: Int,
//     update: UpdatePrerequisitive[SnapIndexGenerationInput] = SnapIndexGenerationInput.updateSnapIndexGenerationInput orElse identity[SnapIndexGenerationInput]
//   )(implicit components: TaskSystemComponents) =
//     newTask(
//       in, update, CPUMemoryRequest(cpu = cpu, memory = memory)
//     ) {
//       case (
//         SnapIndexGenerationInput(
//           Some(fasta),
//           Some(seed),
//           Some(extraArgs)), ce) =>
//         import ce._
//
//         val out = TempFile.createTempFile("snapfolder")
//         out.delete
//
//         println(out.getAbsolutePath)
//
//         val cpu = resourceAllocated.cpu
//
//         val cmd = s"snap index ${fasta.file.getAbsolutePath} ${out.getAbsolutePath} -s $seed -O100 $extraArgs -t$cpu "
//
//         val (stdout, stderr, succ) = execGetStreamsAndCodeWithLog(cmd, unsuccessfulOnErrorStream = false)
//
//         if (!succ) throw new RuntimeException("error in snap during SnapIndexGeneration" + stdout.mkString("\n") + stderr.mkString("\n"))
//
//         val files = List(
//           "Genome" -> new File(out.getAbsolutePath, "Genome"),
//           "GenomeIndex" -> new File(out.getAbsolutePath, "GenomeIndex"),
//           "GenomeIndexHash" -> new File(out.getAbsolutePath, "GenomeIndexHash"),
//           "OverflowTable" -> new File(out.getAbsolutePath, "OverflowTable")
//         ).map(f => f._1 -> SharedFile(f._2, name = fasta.name + ".snap." + ((f._2).getName), canMoveAway = true))
//
//         SnapIndex(files)
//
//     }
//
// }
//
