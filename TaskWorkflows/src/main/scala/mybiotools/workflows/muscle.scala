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

import mybiotools._
import mybiotools.tasks._
import java.io.File
import akka.actor.{ ActorRefFactory }
import scala.util.Try
import mybiotools.stat.ZMetaCombination._

case class FastaFile(file: SharedFile) extends ResultWithSharedFiles(file)

case class MuscleInput(
  fasta: Option[SharedFile]
) extends SimplePrerequisitive[MuscleInput]

object MuscleInput {
  def updateMuscleInput: UpdatePrerequisitive[MuscleInput] = {
    case (self, i: FastaFile) => self.copy(fasta = Some(i.file))
  }
}

object muscle {

  def apply(
    in: MuscleInput,
    update: UpdatePrerequisitive[MuscleInput] = MuscleInput.updateMuscleInput orElse identity[MuscleInput]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = 1000)
    ) {
      case (MuscleInput(Some(fasta)), ce) =>
        import ce._

        val tmp = TempFile.createTempFile("fasta")

        val (stdout, stderr, success) = execGetStreamsAndCode("muscle -quiet -in " + fasta.file.getAbsolutePath + " -out " + tmp, false)

        if (!success) throw new RuntimeException("error in muscle" + stdout.mkString + stderr.mkString)

        val unaligned = openSource(fasta.file)(readFasta)

        val patched = openSource(tmp)(readFasta).map(x => x._1 -> mybiotools.sequence.patchAlignedSequence(aligned = x._2, unaligned = unaligned(x._1)))

        val tmp2 = TempFile.createTempFile(".fasta")
        mybiotools.writeFasta(tmp2.getAbsolutePath, patched, 80)

        FastaFile(SharedFile(tmp2, fasta.name + ".aligned.fasta", canMoveAway = true))

    }
}
