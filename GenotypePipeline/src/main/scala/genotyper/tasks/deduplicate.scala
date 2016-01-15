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

package genotyper.tasks

import mybiotools.tasks._
import java.io.File
import mybiotools._
import mybiotools.workflows._

case class DeduplicateOutput(
  bam: SharedFile,
  metrics: SharedFile
) extends ResultWithSharedFiles(bam, metrics)

case class DeduplicateInput(
  bam: Option[SharedFile],
  jar: Option[SharedFile]
) extends SimplePrerequisitive[DeduplicateInput]

object DeduplicateInput {
  def empty = DeduplicateInput(None, None)

  def apply(file: File, jar: File)(implicit components: TaskSystemComponents): DeduplicateInput = {

    DeduplicateInput(
      bam = Some(SharedFile(file)),
      jar = Some(SharedFile(jar))
    )

  }

  def apply(jar: File)(implicit components: TaskSystemComponents): DeduplicateInput = {

    DeduplicateInput(
      jar = Some(SharedFile(jar)),
      bam = None

    )

  }

  def updateDeduplicateInput: UpdatePrerequisitive[DeduplicateInput] = {
    case (self, i: BamFile) => self.copy(bam = Some(i.file))
  }

}

object deduplicate {
  def apply(
    in: DeduplicateInput,
    memory: Int,
    update: UpdatePrerequisitive[DeduplicateInput] = DeduplicateInput.updateDeduplicateInput orElse identity[DeduplicateInput]
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = memory)
    ) {

      case (
        DeduplicateInput(
          Some(bamfile),
          Some(jar)), ce) =>
        import ce._

        val tmp = TempFile.createTempFile("dedup.bam")

        //        java -jar MarkDuplicates.jar \ 
        // INPUT=sorted_reads.bam \ 
        // OUTPUT=dedup_reads.bam \
        //         METRICS=metrics.txt
        val cmd = s"java -Xmx${resourceAllocated.memory}m -jar ${jar.file.getAbsolutePath} INPUT=${bamfile.file.getAbsolutePath} OUTPUT=${tmp.getAbsolutePath} METRICS_FILE=${tmp.getAbsolutePath + ".metrics.txt"}"

        val (stdout, stderr, succ) = execGetStreamsAndCodeWithLog(cmd, unsuccessfulOnErrorStream = false)

        if (!succ) throw new RuntimeException("error in picard during dedup" + stdout.mkString("\n") + stderr.mkString("\n"))

        DeduplicateOutput(
          bam = SharedFile(tmp, name = bamfile.name + ".dedup.bam", canMoveAway = true),
          metrics = SharedFile(new File(tmp.getAbsolutePath + ".metrics.txt"), name = bamfile.name + ".dedup.bam.metrics")
        )
    }

}

