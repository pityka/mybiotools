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

case class VariantEvalInput(
    vcf: Option[SharedFile],
    reference: Option[FastaWithIndex],
    gatkjar: Option[SharedFile],
    extraArgs: Option[String],
    dbsnpvcf: Option[SharedFile]
) extends Prerequisitive[VariantEvalInput] {
  override def persistent = this.copy(gatkjar = None)

  def ready = {
    reference.isDefined &&
      gatkjar.isDefined &&
      vcf.isDefined &&
      extraArgs.isDefined &&
      dbsnpvcf.isDefined
  }

}

case class VariantEvalOutput(
  report: SharedFile,
  log: SharedFile
) extends ResultWithSharedFiles(report, log)

object VariantEvalInput {
  def empty = VariantEvalInput(None, None, None, None, None)

  def apply(jar: File, referenceFasta: File, referenceFai: File, referenceDict: File, extraArgs: String, dbsnpvcf: File)(implicit components: TaskSystemComponents): VariantEvalInput = {

    VariantEvalInput(
      gatkjar = Some(SharedFile(jar)),
      reference = Some(FastaWithIndex(
        fasta = SharedFile(referenceFasta),
        fai = SharedFile(referenceFai),
        dict = SharedFile(referenceDict)
      )),
      extraArgs = Some(extraArgs),
      vcf = None,
      dbsnpvcf = Some(SharedFile(dbsnpvcf))
    )

  }

  def updateVariantEvalInput: UpdatePrerequisitive[VariantEvalInput] = {
    case (self, i: VCFFile) => self.copy(vcf = Some(i.file))
    case (self, i: HaplotypeCallerOutput) => self.copy(vcf = Some(i.vcf))
    case (self, i: VQSROutput) => self.copy(vcf = Some(i.vcf))
    case (self, i: VariantFiltrationOutput) => self.copy(vcf = Some(i.vcf))
    case (self, i: CombineVCFOutput) => self.copy(vcf = Some(i.vcf))
    case (self, i: DbsnpIDAnnotationOutput) => self.copy(vcf = Some(i.vcf))

  }

}

object varianteval {
  def apply(
    in: VariantEvalInput,
    memory: Int,
    cpu: Int,
    update: UpdatePrerequisitive[VariantEvalInput] = VariantEvalInput.updateVariantEvalInput orElse identity[VariantEvalInput]
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = cpu, memory = memory)
    ) {

      case (
        VariantEvalInput(
          Some(vcf),
          Some(FastaWithIndex(referenceFasta, referenceFai, referenceDict)),
          Some(jarFile),
          Some(extraArgs),
          Some(dbsnpvcf)), ce) =>
        import ce._

        //           java -Xmx60g -jar $GATK \
        // -R $GATK_BUNDLE/hg19/hg19.fasta \
        // -T VariantEval \
        // -L /archive/epfl/fellay/Capture-kits/Agilent-v4/BED/S03723424_Regions.bed \
        // -noST \
        // -noEV \
        // -EV CountVariants \
        // -EV TiTvVariantEvaluator \
        // -EV MendelianViolationEvaluator \
        // -ST CompRod \
        // -ST EvalRod \
        // -ST Sample \
        // -ST Novelty \
        // -ST Filter \
        // -D $GATK_BUNDLE/hg19/dbsnp_137.hg19_mod.vcf \
        // --eval:PRI-48.recalibrated.filtered.annot.dbsnp.snpEff-RBphased.vcf,VCF /home/sasgari/archive/my_projects/respiratory_infection_exome/variants_exomseq_results/PRI/PRI_vcf_haplocaller/PRI-48.recalibrated.filtered.annot.dbsnp.snpEff-RBphased.vcf \
        // --out PRI-48.recalibrated.filtered.annot.dbsnp.snpEff-RBphased.eval

        val (fasta, fai, dict) = Helpers.putBesidesFasta(referenceFasta.file, referenceFai.file, referenceDict.file)

        val out = TempFile.createTempFile(".gatkreport")

        val cmd1 = s"java -Xmx${resourceAllocated.memory}m -jar ${jarFile.file.getCanonicalPath} -T VariantEval -R ${fasta.getAbsolutePath} -eval ${vcf.file.getCanonicalPath} --out ${out.getCanonicalPath} -D ${dbsnpvcf.file.getCanonicalPath} -noST -noEV -EV CountVariants -EV TiTvVariantEvaluator -EV MendelianViolationEvaluator -ST CompRod -ST EvalRod -ST Sample -ST Novelty -ST Filter -EV CompOverlap --log_to_file ${out.getCanonicalPath + ".log"} -nt ${resourceAllocated.cpu} $extraArgs"

        val (stdout1, stderr1, succ1) = execGetStreamsAndCodeWithLog(cmd1, unsuccessfulOnErrorStream = false)

        if (!succ1) throw new RuntimeException("error in GATK during varianteval" + stdout1.mkString("\n") + stderr1.mkString("\n"))

        VariantEvalOutput(
          report = SharedFile(out, name = vcf.name + ".varianteval.report"),
          log = SharedFile(new File(out.getCanonicalPath + ".log"), name = vcf.name + ".varianteval.report.log")
        )

    }

}

