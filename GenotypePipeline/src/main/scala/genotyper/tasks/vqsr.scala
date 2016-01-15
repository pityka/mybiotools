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
import com.typesafe.config.Config
import mybiotools.workflows._

case class VQSRResource(
  file: SharedFile,
  name: String,
  known: Boolean,
  training: Boolean,
  truth: Boolean,
  prior: Double
) extends ResultWithSharedFiles(file)

object VQSRResource {
  def fromConfig(config: Config)(implicit components: TaskSystemComponents): VQSRResource = {

    // hapmap,known=false,training=true,truth=true,prior=15.0 hapmap_3.3.b37.sites.vcf
    val name = config.getString("name")
    val known = config.getBoolean("known")
    val training = config.getBoolean("training")
    val truth = config.getBoolean("truth")
    val prior = config.getDouble("prior")
    val file = config.getString("file")

    VQSRResource(file = SharedFile(new File(file)), name = name, known = known, training = training, truth = truth, prior = prior)
  }
}

case class VCFFile(file: SharedFile) extends ResultWithSharedFiles(file)

case class VQSRInput(
    vcf: Option[SharedFile],
    reference: Option[FastaWithIndex],
    resourceFiles: Option[List[VQSRResource]],
    expectedNumberOfResources: Option[Int],
    gatkjar: Option[SharedFile],
    modeArg: Option[String],
    extraArgs: Option[String],
    minNumBadVariants: Option[Int]
) extends Prerequisitive[VQSRInput] {
  override def persistent = this.copy(gatkjar = None)

  def ready = {
    reference.isDefined &&
      gatkjar.isDefined &&
      expectedNumberOfResources.isDefined &&
      vcf.isDefined &&
      extraArgs.isDefined &&
      modeArg.isDefined &&
      minNumBadVariants.isDefined &&
      resourceFiles.isDefined &&
      resourceFiles.get.size == expectedNumberOfResources.get
  }

}

case class VQSROutput(
  recal: SharedFile,
  tranches: SharedFile,
  rscript: SharedFile,
  vcf: SharedFile,
  logRecal: SharedFile,
  logApply: SharedFile,
  plot: Option[SharedFile]
) extends ResultWithSharedFiles(recal :: tranches :: rscript :: vcf :: logRecal :: logApply :: plot.toList: _*)

object VQSRInput {
  def empty = VQSRInput(None, None, None, None, None, None, None, None)

  def apply(jar: File, referenceFasta: File, referenceFai: File, referenceDict: File, extraArgs: String, modeArg: String, resources: List[VQSRResource], minNumBadVariants: Int)(implicit components: TaskSystemComponents): VQSRInput = {

    VQSRInput(
      gatkjar = Some(SharedFile(jar)),
      reference = Some(FastaWithIndex(
        fasta = SharedFile(referenceFasta),
        fai = SharedFile(referenceFai),
        dict = SharedFile(referenceDict)
      )),
      extraArgs = Some(extraArgs),
      expectedNumberOfResources = Some(resources.size),
      resourceFiles = Some(resources),
      modeArg = Some(modeArg),
      vcf = None,
      minNumBadVariants = Some(minNumBadVariants)
    )

  }

  def updateVQSRInput: UpdatePrerequisitive[VQSRInput] = {
    case (self, i: VCFFile) => self.copy(vcf = Some(i.file))
    case (self, i: HaplotypeCallerOutput) => self.copy(vcf = Some(i.vcf))
    case (self, i: VQSROutput) => self.copy(vcf = Some(i.vcf))
    case (self, i: CombineVCFOutput) => self.copy(vcf = Some(i.vcf))
    case (self, i: VariantFiltrationOutput) => self.copy(vcf = Some(i.vcf))
    case (self, i: DbsnpIDAnnotationOutput) => self.copy(vcf = Some(i.vcf))

  }

}

object vqsr {
  def apply(
    in: VQSRInput,
    memory: Int,
    cpu: Int,
    update: UpdatePrerequisitive[VQSRInput] = VQSRInput.updateVQSRInput orElse identity[VQSRInput]
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = cpu, memory = memory)
    ) {

      case (
        VQSRInput(
          Some(vcf),
          Some(FastaWithIndex(referenceFasta, referenceFai, referenceDict)),
          Some(resources),
          Some(_),
          Some(jarFile),
          Some(modeArg1),
          Some(extraArgs),
          Some(minNumBadVariants)), ce) =>
        import ce._

        val modeArg = " --mode " + modeArg1

        //       java -jar GenomeAnalysisTK.jar \ 
        // -T VariantRecalibrator \ 
        // -R reference.fa \ 
        // -input raw_variants.vcf \ 
        // -resource:hapmap,known=false,training=true,truth=true,prior=15.0 hapmap.vcf \ 
        // -resource:omni,known=false,training=true,truth=true,prior=12.0 omni.vcf \ 
        // -resource:1000G,known=false,training=true,truth=false,prior=10.0 1000G.vcf \ 
        // -resource:dbsnp,known=true,training=false,truth=false,prior=2.0 dbsnp.vcf \ 
        // -an DP \ 
        // -an QD \ 
        // -an FS \ 
        // -an MQRankSum \ 
        // -an ReadPosRankSum \ 
        // -mode SNP \ 
        // -tranche 100.0 -tranche 99.9 -tranche 99.0 -tranche 90.0 \ 
        // -numBad 1000 \ 
        // -recalFile recalibrate_SNP.recal \ 
        // -tranchesFile recalibrate_SNP.tranches \ 
        // -rscriptFile recalibrate_SNP_plots.R 
        val outrecal = TempFile.createTempFile(".recal")
        val outtranches = TempFile.createTempFile(".tranches")
        val outrscripts = TempFile.createTempFile(".R")

        val resourceString = resources.map {
          case VQSRResource(file, name, known, training, truth, prior) =>
            s"-resource:$name,known=$known,training=$training,truth=$truth,prior=$prior ${file.file.getCanonicalPath}"
        }.mkString(" ")

        val (fasta, fai, dict) = Helpers.putBesidesFasta(referenceFasta.file, referenceFai.file, referenceDict.file)

        val cmd1 = s"java -Xmx${resourceAllocated.memory}m -jar ${jarFile.file.getCanonicalPath} -T VariantRecalibrator -R ${fasta.getAbsolutePath} -input ${vcf.file.getCanonicalPath} $resourceString -an QD -an FS -an MQRankSum -an ReadPosRankSum ${modeArg} -tranche 100.0 -tranche 99.9 -tranche 99.0 -tranche 90.0 --minNumBadVariants ${minNumBadVariants} --log_to_file ${outrecal.getCanonicalPath + ".log"} -recalFile ${outrecal.getCanonicalPath} -tranchesFile ${outtranches.getCanonicalPath} -rscriptFile ${outrscripts.getCanonicalPath} -nt ${resourceAllocated.cpu} $extraArgs"

        val (stdout1, stderr1, succ1) = execGetStreamsAndCodeWithLog(cmd1, unsuccessfulOnErrorStream = false)

        if (!succ1) throw new RuntimeException("error in GATK during VQSR Recalibration" + stdout1.mkString("\n") + stderr1.mkString("\n"))

        // java -jar GenomeAnalysisTK.jar \ 
        // -T ApplyRecalibration \ 
        // -R reference.fa \ 
        // -input raw_variants.vcf \ 
        // -mode SNP \ 
        // --ts_filter_level 99.0 \ 
        // -recalFile recalibrate_SNP.recal \ 
        // -tranchesFile recalibrate_SNP.tranches \ 
        // -o recalibrated_snps_raw_indels.vcf 

        val outvcf = TempFile.createTempFile(".vcf")

        val cmd2 = s"java -Xmx${resourceAllocated.memory}m -jar ${jarFile.file.getCanonicalPath} -T ApplyRecalibration -R ${fasta.getAbsolutePath} -input ${vcf.file.getCanonicalPath} $modeArg --ts_filter_level 99.0 --log_to_file ${outvcf.getCanonicalPath + ".log"} -recalFile ${outrecal.getCanonicalPath} -tranchesFile ${outtranches.getCanonicalPath} -o ${outvcf.getCanonicalPath} -nt ${resourceAllocated.cpu} "

        val (stdout2, stderr2, succ2) = execGetStreamsAndCodeWithLog(cmd2, unsuccessfulOnErrorStream = false)

        if (!succ2) throw new RuntimeException("error in GATK during VQSR Apply" + stdout2.mkString("\n") + stderr2.mkString("\n"))

        VQSROutput(
          recal = SharedFile(outrecal, name = vcf.name + s"$modeArg1.recal"),
          tranches = SharedFile(outtranches, name = vcf.name + s"$modeArg1.tranches"),
          rscript = SharedFile(outrscripts, name = vcf.name + s"$modeArg1.vqsr.R"),
          vcf = SharedFile(outvcf, name = vcf.name + s"$modeArg1.vqsr.vcf", canMoveAway = true),
          logRecal = SharedFile(new File(outrecal.getCanonicalPath + ".log"), name = vcf.name + s"$modeArg1.vqsr.recal.log"),
          logApply = SharedFile(new File(outvcf.getCanonicalPath + ".log"), name = vcf.name + s"$modeArg1.vqsr.apply.log"),
          plot = if (new File(outrscripts.getAbsolutePath + ".pdf").canRead) Some(SharedFile(new File(outrscripts.getAbsolutePath + ".pdf"), name = vcf.name + s"$modeArg1.vqsr.R.pdf")) else None
        )

    }

}

