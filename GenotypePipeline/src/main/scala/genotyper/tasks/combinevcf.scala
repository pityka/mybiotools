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
import mybiotools.gwascommons._
import genotyper.Helpers
import mybiotools.workflows._

case class CombineVCFInput(
    vcfs: Option[Set[SharedFile]],
    expectedVcfs: Option[Int],
    reference: Option[FastaWithIndex],
    gatkjar: Option[SharedFile],
    extraArgs: Option[String],
    outname: Option[String]
) extends Prerequisitive[CombineVCFInput] {
  override def persistent = this.copy(gatkjar = None)

  def ready = {
    reference.isDefined &&
      gatkjar.isDefined &&
      vcfs.isDefined &&
      outname.isDefined &&
      extraArgs.isDefined &&
      expectedVcfs.isDefined &&
      expectedVcfs.get == vcfs.get.size
  }

}

case class CombineVCFOutput(
  vcf: SharedFile,
  log: SharedFile
) extends ResultWithSharedFiles(vcf, log)

object CombineVCFInput {
  def empty = CombineVCFInput(None, None, None, None, None, None)

  def apply(jar: File, referenceFasta: File, referenceFai: File, referenceDict: File, extraArgs: String, expected: Int, outname: String)(implicit components: TaskSystemComponents): CombineVCFInput = {

    CombineVCFInput(
      gatkjar = Some(SharedFile(jar)),
      reference = Some(FastaWithIndex(
        fasta = SharedFile(referenceFasta),
        fai = SharedFile(referenceFai),
        dict = SharedFile(referenceDict)
      )),
      extraArgs = Some(extraArgs),
      vcfs = None,
      expectedVcfs = Some(expected),
      outname = Some(outname)
    )

  }

  private def updateVCFList(input: CombineVCFInput, vcf: SharedFile): CombineVCFInput = {
    val updated = input.vcfs match {
      case None => Some(Set(vcf))
      case Some(xs) => Some(xs + vcf)
    }
    input.copy(vcfs = updated)

  }

  def updateCombineVCFInput: UpdatePrerequisitive[CombineVCFInput] = {
    case (self, i: VCFFile) => updateVCFList(self, i.file)
    case (self, i: HaplotypeCallerOutput) => updateVCFList(self, i.vcf)
    case (self, i: GenotypeGVCFOutput) => updateVCFList(self, i.vcf)
    case (self, i: GenotypeWithPlatypusOutput) => updateVCFList(self, i.vcf)
    case (self, i: VQSROutput) => updateVCFList(self, i.vcf)
    case (self, i: VariantFiltrationOutput) => updateVCFList(self, i.vcf)
    case (self, i: DbsnpIDAnnotationOutput) => updateVCFList(self, i.vcf)
  }

}

object combinevcf {
  def apply(
    in: CombineVCFInput,
    memory: Int,
    cpu: Int,
    update: UpdatePrerequisitive[CombineVCFInput] = CombineVCFInput.updateCombineVCFInput orElse identity[CombineVCFInput]
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = cpu, memory = memory)
    ) {

      case (
        CombineVCFInput(
          Some(vcfs),
          _,
          Some(FastaWithIndex(referenceFasta, referenceFai, referenceDict)),
          Some(jarFile),
          Some(extraArgs),
          Some(outname)), ce) =>
        import ce._

        val (fasta, fai, dict) = Helpers.putBesidesFasta(referenceFasta.file, referenceFai.file, referenceDict.file)

        //        java -Xmx2g -jar GenomeAnalysisTK.jar \
        // -R ref.fasta \
        // -T CombineVariants \
        // --variant input1.vcf \
        // --variant input2.vcf \
        // -o output.vcf \
        // -genotypeMergeOptions REQUIRE_UNIQUE

        val out = TempFile.createTempFile(".vcf")

        val cmd1 = s"java -Xmx${resourceAllocated.memory}m -jar ${jarFile.file.getCanonicalPath} -T CombineVariants -R ${fasta.getAbsolutePath} --out ${out.getCanonicalPath} ${vcfs.map(x => "-V " + x.file.getCanonicalPath).mkString(" ")} --log_to_file ${out.getCanonicalPath + ".log"} -nt ${resourceAllocated.cpu} "

        val (stdout1, stderr1, succ1) = execGetStreamsAndCodeWithLog(cmd1, unsuccessfulOnErrorStream = false)

        if (!succ1) throw new RuntimeException("error in GATK during variant combine" + stdout1.mkString("\n") + stderr1.mkString("\n"))

        CombineVCFOutput(
          vcf = SharedFile(out, name = outname + ".combined.vcf", canMoveAway = true),
          log = SharedFile(new File(out.getCanonicalPath + ".log"), name = outname + ".combined.vcf.log", canMoveAway = true)
        )

    }

}

