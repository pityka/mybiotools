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
import mybiotools.workflows.{ MakeBedOut, BimFile, BedFile, FamFile }
import scala.collection.JavaConversions._
import mybiotools.workflows._

case class VCF2PlinkInput(
    vcf: Option[SharedFile],
    gatkjar: Option[SharedFile],
    reference: Option[FastaWithIndex]
) extends SimplePrerequisitive[VCF2PlinkInput] {
  override def persistent = this.copy(gatkjar = None)
}

object VCF2PlinkInput {

  def apply(jar: File, referenceFasta: File, referenceFai: File, referenceDict: File)(implicit components: TaskSystemComponents): VCF2PlinkInput = {

    VCF2PlinkInput(
      gatkjar = Some(SharedFile(jar)),
      vcf = None,
      reference = Some(FastaWithIndex(
        fasta = SharedFile(referenceFasta),
        fai = SharedFile(referenceFai),
        dict = SharedFile(referenceDict)
      ))
    )

  }

  def updateVCF2PlinkInput: UpdatePrerequisitive[VCF2PlinkInput] = {
    case (self, i: VCFFile) => self.copy(vcf = Some(i.file))
    case (self, i: HaplotypeCallerOutput) => self.copy(vcf = Some(i.vcf))
    case (self, i: VQSROutput) => self.copy(vcf = Some(i.vcf))
    case (self, i: CombineVCFOutput) => self.copy(vcf = Some(i.vcf))
    case (self, i: DbsnpIDAnnotationOutput) => self.copy(vcf = Some(i.vcf))
    case (self, i: VariantFiltrationOutput) => self.copy(vcf = Some(i.vcf))
  }

}

object vcf2plink {
  def apply(
    in: VCF2PlinkInput,
    update: UpdatePrerequisitive[VCF2PlinkInput] = VCF2PlinkInput.updateVCF2PlinkInput orElse identity[VCF2PlinkInput]
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = 2000)
    ) {

      case (
        VCF2PlinkInput(
          Some(vcf),
          Some(jarFile),
          Some(FastaWithIndex(referenceFasta, referenceFai, referenceDict))), ce) =>
        import ce._

        val out = TempFile.createTempFile(".bed")

        val (fasta, fai, dict) = Helpers.putBesidesFasta(referenceFasta.file, referenceFai.file, referenceDict.file)

        val fam = {
          val r = new htsjdk.variant.vcf.VCFFileReader(vcf.file)
          val samples = r.getFileHeader.getGenotypeSamples
          r.close
          val str = samples.map { s => s"$s 1 0 0 0 -9" }.mkString("\n")
          val f = TempFile.createTempFile(".fam")
          mybiotools.writeToFile(f.getCanonicalPath, str)
          f
        }

        val cmd1 = s"java -Xmx${resourceAllocated.memory}m -jar ${jarFile.file.getCanonicalPath} -T VariantsToBinaryPed  -R ${fasta.getAbsolutePath} --log_to_file ${out.getCanonicalPath + ".log"} -V ${vcf.file.getCanonicalPath} --bed ${out.getCanonicalPath + ".bed"} --bim ${out.getCanonicalPath + ".bim"} --fam ${out.getCanonicalPath + ".fam"} --outputMode SNP_MAJOR --minGenotypeQuality 0  --metaData ${fam.getCanonicalPath} "

        val (stdout1, stderr1, succ1) = execGetStreamsAndCodeWithLog(cmd1, unsuccessfulOnErrorStream = false)

        if (!succ1) throw new RuntimeException("error in GATK during VCF2Plink" + stdout1.mkString("\n") + stderr1.mkString("\n"))

        MakeBedOut(
          bed = BedFile(SharedFile(new File(out.getCanonicalPath + ".bed"), name = vcf.name + ".bed")),
          bim = BimFile(SharedFile(new File(out.getCanonicalPath + ".bim"), name = vcf.name + ".bim")),
          fam = FamFile(SharedFile(new File(out.getCanonicalPath + ".fam"), name = vcf.name + ".fam")),
          log = SharedFile(new File(out.getCanonicalPath + ".log"), name = vcf.name + ".vcf2plink.log")
        )

    }

}

