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

case class NewickTreeFile(file: SharedFile) extends ResultWithSharedFiles(file)

case class RAxMLInput(
  fasta: Option[SharedFile]
) extends SimplePrerequisitive[RAxMLInput]

object RAxMLInput {
  def updateRAxMLInput: UpdatePrerequisitive[RAxMLInput] = {
    case (self, i: FastaFile) => self.copy(fasta = Some(i.file))
  }
}

object raxml {

  def apply(
    in: RAxMLInput,
    cpu: Int,
    update: UpdatePrerequisitive[RAxMLInput] = RAxMLInput.updateRAxMLInput orElse identity[RAxMLInput]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = (cpu, 2000), memory = 1000)
    ) {
      case (RAxMLInput(Some(fasta)), ce) =>
        import ce._

        val tmp = TempFile.createTempFile("fasta")

        val commandline =
          if (ce.availableHostsForMPI.isEmpty) s"raxml -w ${tmp.getParentFile.getAbsolutePath} -s ${fasta.file.getAbsolutePath} -m GTRCAT -f a -N 30 -D -k -n ${tmp.getName} -T ${resourceAllocated.cpu} -x 1234 -p 1234"
          else {
            val totalcpu = ce.availableHostsForMPI.map(_.slots).sum
            val corespernode = totalcpu / ce.availableHostsForMPI.size
            val numberofnodes = ce.availableHostsForMPI.size
            s"mpirun -np $numberofnodes -hostfile ${ce.mpiHostFile.getAbsolutePath} -bynode raxml -w ${tmp.getParentFile.getAbsolutePath} -s ${fasta.file.getAbsolutePath} -m GTRCAT -f a -N 30 -D -k -n ${tmp.getName} -T $corespernode -x 1234 -p 1234"
          }

        val (stdout, stderr, success) = execGetStreamsAndCodeWithLog(
          commandline, false
        )

        val besttree = new File(tmp.getParent, "RAxML_bestTree." + tmp.getName)

        if (!success) throw new RuntimeException("error in RAxML" + stdout.mkString + stderr.mkString)

        NewickTreeFile(SharedFile(besttree, fasta.name + ".newick.tree", canMoveAway = true))

    }
}
