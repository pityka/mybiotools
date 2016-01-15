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

case class VariantFiltrationInput(
    vcf: Option[SharedFile],
    reference: Option[FastaWithIndex],
    gatkjar: Option[SharedFile],
    extraArgs: Option[String],
    interval: Option[Set[Region]]
) extends SimplePrerequisitive[VariantFiltrationInput] {
  override def persistent = this.copy(gatkjar = None)
}

case class VariantFiltrationOutput(
  vcf: SharedFile,
  log: SharedFile
) extends ResultWithSharedFiles(vcf, log)

object VariantFiltrationInput {

  def apply(jar: File, referenceFasta: File, referenceFai: File, referenceDict: File, extraArgs: String, interval: Iterable[Region])(implicit components: TaskSystemComponents): VariantFiltrationInput = {

    VariantFiltrationInput(
      gatkjar = Some(SharedFile(jar)),
      reference = Some(FastaWithIndex(
        fasta = SharedFile(referenceFasta),
        fai = SharedFile(referenceFai),
        dict = SharedFile(referenceDict)
      )),
      extraArgs = Some(extraArgs),
      vcf = None,
      interval = Some(interval.toSet)
    )

  }

  def updateVariantFiltrationInput: UpdatePrerequisitive[VariantFiltrationInput] = {
    case (self, i: VCFFile) => self.copy(vcf = Some(i.file))
    case (self, i: HaplotypeCallerOutput) => self.copy(vcf = Some(i.vcf))
    case (self, i: VQSROutput) => self.copy(vcf = Some(i.vcf))
    case (self, i: CombineVCFOutput) => self.copy(vcf = Some(i.vcf))
    case (self, i: DbsnpIDAnnotationOutput) => self.copy(vcf = Some(i.vcf))
    case (self, i: VariantFiltrationOutput) => self.copy(vcf = Some(i.vcf))
  }

}

object variantFiltration {
  def apply(
    in: VariantFiltrationInput,
    memory: Int,
    update: UpdatePrerequisitive[VariantFiltrationInput] = VariantFiltrationInput.updateVariantFiltrationInput orElse identity[VariantFiltrationInput]
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = memory)
    ) {

      case (
        VariantFiltrationInput(
          Some(vcf),
          Some(FastaWithIndex(referenceFasta, referenceFai, referenceDict)),
          Some(jarFile),
          Some(extraArgs),
          Some(interval)), ce) =>
        import ce._

        val (fasta, fai, dict) = Helpers.putBesidesFasta(referenceFasta.file, referenceFai.file, referenceDict.file)

        val intervalString = if (interval.isEmpty) "" else {
          val tmp = TempFile.createTempFile(".bed")
          writeToFile(tmp.getAbsolutePath, interval.map(_.toLine).mkString("\n"))
          s" -L ${tmp.getCanonicalPath} "
        }

        val out = TempFile.createTempFile(".vcf")

        val cmd1 = s"java -Xmx${resourceAllocated.memory}m -jar ${jarFile.file.getCanonicalPath} -T VariantFiltration -R ${fasta.getAbsolutePath} --log_to_file ${out.getCanonicalPath + ".log"} $intervalString -o ${out.getCanonicalPath} -V ${vcf.file.getCanonicalPath} $extraArgs"

        val (stdout1, stderr1, succ1) = execGetStreamsAndCodeWithLog(cmd1, unsuccessfulOnErrorStream = false)

        if (!succ1) throw new RuntimeException("error in GATK during VariantFiltration" + stdout1.mkString("\n") + stderr1.mkString("\n"))

        VariantFiltrationOutput(
          vcf = SharedFile(out, name = vcf.name + ".variantFiltration.vcf"),
          log = SharedFile(new File(out.getCanonicalPath + ".log"), name = vcf.name + ".variantFiltration.vcf.log")
        )

    }

}

