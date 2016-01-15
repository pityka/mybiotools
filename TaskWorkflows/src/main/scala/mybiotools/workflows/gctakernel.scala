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
import com.typesafe.config.{ Config, ConfigFactory }
import collection.JavaConversions._
import mybiotools.gwascommons.gcta._

import hdfdosage.{ DosageFileFormat, HDFDosage, PDose, PGenotypeProbabilities, BIGSNPMajor, TPed }

case class GCTAKernelInput(
  bim: Option[BimFile],
  bed: Option[BedFile],
  fam: Option[FamFile],
  extractList: Option[Option[SharedFile]]
) extends SimplePrerequisitive[GCTAKernelInput]

object GCTAKernelInput {
  def apply(base: File, extractList: Option[File])(implicit components: TaskSystemComponents): GCTAKernelInput = {

    GCTAKernelInput(
      bim = Some(BimFile(SharedFile(new File(base.getAbsolutePath + ".bim")))),
      bed = Some(BedFile(SharedFile(new File(base.getAbsolutePath + ".bed")))),
      fam = Some(FamFile(SharedFile(new File(base.getAbsolutePath + ".fam")))),
      extractList = extractList.map(f => Some(SharedFile(f)))
    )
  }

  def updateGCTAKernelInputFromPlinkFiles: UpdatePrerequisitive[GCTAKernelInput] = {
    case (self, i: MakeBedOut) => self.copy(bim = Some(i.bim), bed = Some(i.bed), fam = Some(i.fam))
    case (self, i: SmartPCAInput) => self.copy(bim = i.bim, bed = i.bed, fam = i.fam)
    case (self, i: BimFile) => self.copy(bim = Some(i))
    case (self, i: FamFile) => self.copy(fam = Some(i))
    case (self, i: BedFile) => self.copy(bed = Some(i))
    case (self, i: DosageToolIncludeSNPList) => self.copy(extractList = Some(i.file))
  }
}

object gctakerneltask {
  def apply(
    in: GCTAKernelInput,
    cpu: Int = 1,
    memory: Int = 2000,
    update: UpdatePrerequisitive[GCTAKernelInput] = GCTAKernelInput.updateGCTAKernelInputFromPlinkFiles orElse identity[GCTAKernelInput]
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = cpu, memory = memory)
    ) {

      case (
        GCTAKernelInput(
          Some(BimFile(bim)),
          Some(BedFile(bed)),
          Some(FamFile(fam)),
          Some(extractList)
          ), ce) =>
        import ce._

        val tmp = {

          val trunk = putBesides(bim.file -> ".bim", bed.file -> ".bed", fam.file -> ".fam")
          trunk.getAbsolutePath
        }
        val threads = resourceAllocated.cpu

        val out = TempFile.createTempFile("grm")

        val extractString = extractList.map { e =>
          s" --extract ${e.file.getAbsolutePath} "
        }.getOrElse("")

        val cmd = s"gcta64 --make-grm-gz --bfile $tmp --thread-num $threads --out ${out.getAbsolutePath} $extractString "

        val (stdout, stderr, success) = mybiotools.execGetStreamsAndCode(cmd, unsuccessfulOnErrorStream = true)

        if (!success) throw new RuntimeException("Error in gcta " + stdout.mkString("\n") + stderr.mkString("\n"))

        GRMGZPair(
          grmgz = SharedFile(new File(out.getAbsolutePath + ".grm.gz"), name = bed.name + ".grm.gz", canMoveAway = true),
          grmid = SharedFile(new File(out.getAbsolutePath + ".grm.id"), name = bed.name + ".grm.id", canMoveAway = true),
          name = bed.name + ".grm.gz"
        )
    }

}

