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

object SortHelper {
  def isSorted(file: File): Boolean = {
    import htsjdk.samtools.{ SAMFileReader, SAMFileWriterFactory, SAMSequenceRecord, SAMSequenceDictionary, SAMFileHeader }
    import scala.collection.JavaConversions._
    import SAMFileHeader.SortOrder

    val reader = new SAMFileReader(file)
    reader.setValidationStringency(htsjdk.samtools.ValidationStringency.SILENT)
    val b = reader.getFileHeader.getSortOrder == SortOrder.coordinate

    reader.close
    b

  }
}

case class SortBamInput(
  bam: Option[SharedFile]
) extends SimplePrerequisitive[SortBamInput]

object SortBamInput {
  def empty = SortBamInput(None)

  def apply(file: File)(implicit components: TaskSystemComponents): SortBamInput = {

    SortBamInput(
      bam = Some(SharedFile(file))
    )

  }

  def updateSortBamInput: UpdatePrerequisitive[SortBamInput] = {
    case (self, i: BamFile) => self.copy(bam = Some(i.file))
  }

}

object sortbam {
  def apply(
    in: SortBamInput,
    memory: Int,
    update: UpdatePrerequisitive[SortBamInput] = SortBamInput.updateSortBamInput orElse identity[SortBamInput]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = memory)
    ) {

      case (
        SortBamInput(
          Some(bamfile)), ce) =>
        import ce._

        val tmp = TempFile.createTempFile("sortedbam")
        // val bamfilepath = java.nio.file.Paths.get(bamfile.file.getCanonicalPath)
        // val bamfilelinkpath = java.nio.file.Paths.get(tmp.getCanonicalPath + ".x")
        // java.nio.file.Files.createSymbolicLink(bamfilelinkpath, bamfilepath)
        // val linked = new File(tmp.getCanonicalPath + ".x")

        // samtools sort [-no] [-m maxMem] <in.bam> <out.prefix>
        val cmd = s"samtools sort -m ${(resourceAllocated.memory * 0.8).toLong * 1024L * 1024L} ${bamfile.file} ${tmp.getAbsolutePath}"

        val (stdout, stderr, succ) = execGetStreamsAndCodeWithLog(cmd, unsuccessfulOnErrorStream = false)

        if (!succ) throw new RuntimeException("error in samtools during sort" + stdout.mkString("\n") + stderr.mkString("\n"))

        BamFile(SharedFile(new File(tmp.getAbsolutePath + ".bam"), name = bamfile.name + ".sorted.bam"))
    }

}

