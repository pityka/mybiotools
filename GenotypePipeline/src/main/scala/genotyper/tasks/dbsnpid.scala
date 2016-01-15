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

case class DbsnpIDAnnotationInput(
    vcf: Option[SharedFile],
    reference: Option[FastaWithIndex],
    gatkjar: Option[SharedFile],
    dbsnpvcf: Option[SharedFile]
) extends SimplePrerequisitive[DbsnpIDAnnotationInput] {
  override def persistent = this.copy(gatkjar = None)

}

case class DbsnpIDAnnotationOutput(
  vcf: SharedFile,
  log: SharedFile
) extends ResultWithSharedFiles(vcf, log)

object DbsnpIDAnnotationInput {

  def apply(jar: File, referenceFasta: File, referenceFai: File, referenceDict: File, dbsnpvcf: File)(implicit components: TaskSystemComponents): DbsnpIDAnnotationInput = {

    DbsnpIDAnnotationInput(
      gatkjar = Some(SharedFile(jar)),
      reference = Some(FastaWithIndex(
        fasta = SharedFile(referenceFasta),
        fai = SharedFile(referenceFai),
        dict = SharedFile(referenceDict)
      )),
      vcf = None,
      dbsnpvcf = Some(SharedFile(dbsnpvcf))
    )

  }

  def updateDbsnpIDAnnotationInput: UpdatePrerequisitive[DbsnpIDAnnotationInput] = {
    case (self, i: VCFFile) => self.copy(vcf = Some(i.file))
    case (self, i: HaplotypeCallerOutput) => self.copy(vcf = Some(i.vcf))
    case (self, i: VQSROutput) => self.copy(vcf = Some(i.vcf))
    case (self, i: VariantFiltrationOutput) => self.copy(vcf = Some(i.vcf))
    case (self, i: CombineVCFOutput) => self.copy(vcf = Some(i.vcf))

  }

}

object dbsnpAnnotation {
  def apply(
    in: DbsnpIDAnnotationInput,
    memory: Int,
    update: UpdatePrerequisitive[DbsnpIDAnnotationInput] = DbsnpIDAnnotationInput.updateDbsnpIDAnnotationInput orElse identity[DbsnpIDAnnotationInput]
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = memory)
    ) {

      case (
        DbsnpIDAnnotationInput(
          Some(vcf),
          Some(FastaWithIndex(referenceFasta, referenceFai, referenceDict)),
          Some(jarFile),
          Some(dbsnpvcf)), ce) =>
        import ce._

        //           java -Xmx60g -jar $GATK \
        // -R $GATK_BUNDLE/hg19/hg19.fasta \
        // -T DbsnpIDAnnotation \
        // -L /archive/epfl/fellay/Capture-kits/Agilent-v4/BED/S03723424_Regions.bed \
        // -noST \
        // -noEV \
        // -EV CountVariants \
        // -EV TiTvDbsnpIDAnnotationuator \
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

        val out = TempFile.createTempFile(".vcf")

        val cmd1 = s"java -Xmx${resourceAllocated.memory}m -jar ${jarFile.file.getCanonicalPath} -T VariantAnnotator -R ${fasta.getAbsolutePath} --out ${out.getCanonicalPath} -D ${dbsnpvcf.file.getCanonicalPath} --log_to_file ${out.getCanonicalPath + ".log"} -V ${vcf.file.getCanonicalPath} "

        val (stdout1, stderr1, succ1) = execGetStreamsAndCodeWithLog(cmd1, unsuccessfulOnErrorStream = false)

        if (!succ1) throw new RuntimeException("error in GATK during DbsnpIDAnnotation" + stdout1.mkString("\n") + stderr1.mkString("\n"))

        DbsnpIDAnnotationOutput(
          vcf = SharedFile(out, name = vcf.name + ".dbsnpannotate.report"),
          log = SharedFile(new File(out.getCanonicalPath + ".log"), name = vcf.name + ".dbsnpannotate.report.log")
        )

    }

}

