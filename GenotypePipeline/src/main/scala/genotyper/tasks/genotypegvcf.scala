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
import mybiotools.gwascommons._
import genotyper.Helpers
import htsjdk.variant.vcf._
import htsjdk.variant.variantcontext.writer.VariantContextWriterFactory
import htsjdk.variant.variantcontext.VariantContext
import collection.JavaConversions._
import genotyper._

case class GenotypeGVCFInput(
    gvcfs: Option[Set[(SharedFile, SharedFile)]],
    expectedNumberOfGvcfs: Option[Int],
    reference: Option[FastaWithIndex],
    interval: Option[Set[Region]],
    gatkjar: Option[SharedFile],
    outName: Option[String],
    extraArgs: Option[String],
    dbsnpVCF: Option[SharedFile]
) extends Prerequisitive[GenotypeGVCFInput] {
  override def persistent = this.copy(gatkjar = None)

  def ready = {
    reference.isDefined &&
      interval.isDefined &&
      gatkjar.isDefined &&
      expectedNumberOfGvcfs.isDefined &&
      outName.isDefined &&
      extraArgs.isDefined
    gvcfs.isDefined &&
      gvcfs.get.size == expectedNumberOfGvcfs.get
  }

}

case class GenotypeGVCFOutput(
  vcf: SharedFile,
  log: SharedFile
) extends ResultWithSharedFiles(vcf, log)

object GenotypeGVCFInput {
  def empty = GenotypeGVCFInput(None, None, None, None, None, None, None, None)

  def apply(jar: File, referenceFasta: File, referenceFai: File, referenceDict: File, interval: Set[Region], outName: String, expectedGvcfs: Int, extraArgs: String, dbsnp: File)(implicit components: TaskSystemComponents): GenotypeGVCFInput = this(jar, referenceFasta, referenceFai, referenceDict, interval, outName, expectedGvcfs, extraArgs, dbsnp, Nil)

  def apply(jar: File, referenceFasta: File, referenceFai: File, referenceDict: File, interval: Set[Region], outName: String, expectedMoreGvcfs: Int, extraArgs: String, dbsnp: File, gvcfs: List[(File, File)])(implicit components: TaskSystemComponents): GenotypeGVCFInput = {

    assert((expectedMoreGvcfs + gvcfs.size) > 0, "gvcf input is empty")

    GenotypeGVCFInput(
      gatkjar = Some(SharedFile(jar)),
      reference = Some(FastaWithIndex(
        fasta = SharedFile(referenceFasta),
        fai = SharedFile(referenceFai),
        dict = SharedFile(referenceDict)
      )),
      gvcfs = Some(gvcfs.map(f => SharedFile(f._1) -> SharedFile(f._2)).toSet),
      expectedNumberOfGvcfs = Some(expectedMoreGvcfs + gvcfs.size),
      interval = Some(interval),
      outName = Some(outName),
      extraArgs = Some(extraArgs),
      dbsnpVCF = Some(SharedFile(dbsnp))
    )

  }

  private def updateBamList(input: GenotypeGVCFInput, file: SharedFile, idx: SharedFile): GenotypeGVCFInput = {
    val updated = input.gvcfs match {
      case None => Some(Set((file, idx)))
      case Some(xs) => Some(xs + ((file, idx)))
    }
    input.copy(gvcfs = updated)

  }

  def updateGenotypeGVCFInput: UpdatePrerequisitive[GenotypeGVCFInput] = {
    case (self, i: HaplotypeCallerOutput) => updateBamList(self, i.vcf, i.vcfidx)
    case (self, i: VCFWithIndex) => updateBamList(self, i.vcf, i.vcfidx)

  }

}

object genotypeGVCF {
  def apply(
    in: GenotypeGVCFInput,
    memory: Int,
    cpu: Int,
    update: UpdatePrerequisitive[GenotypeGVCFInput] = GenotypeGVCFInput.updateGenotypeGVCFInput orElse identity[GenotypeGVCFInput]
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = cpu, memory = memory)
    ) {

      case (
        GenotypeGVCFInput(
          Some(gvcfs),
          _,
          Some(FastaWithIndex(referenceFasta, referenceFai, referenceDict)),
          Some(interval),
          Some(jarFile),
          Some(outName),
          Some(extraArgs),
          Some(dbsnp)), ce) =>
        import ce._

        val (fasta, fai, dict) = Helpers.putBesidesFasta(referenceFasta.file, referenceFai.file, referenceDict.file)

        // copy the gvcfs to tmp disk
        val tmpvcfs = gvcfs.map { gvcfile =>
          val invcftrunk = mybiotools.putBesides(gvcfile._1.file -> ".vcf" :: gvcfile._2.file -> ".vcf.idx" :: Nil: _*)
          new File(invcftrunk.getAbsolutePath + ".vcf")
        }

        val intervalString = if (interval.isEmpty) "" else {
          val tmp = TempFile.createTempFile(".bed")
          writeToFile(tmp.getAbsolutePath, interval.map(_.toLine).mkString("\n"))
          s" -L ${tmp.getCanonicalPath} "
        }

        //       java -Xmx2g -jar GenomeAnalysisTK.jar \
        // -R ref.fasta \
        // -T GenotypeGVCFs \
        // --variant gvcf1.vcf \
        // --variant gvcf2.vcf \
        // -o output.vcf
        val tmpout = TempFile.createTempFile(".vcf")

        val cmd = s"java -Xmx${resourceAllocated.memory}m -jar ${jarFile.file.getCanonicalPath} -T GenotypeGVCFs -R ${fasta.getAbsolutePath} ${tmpvcfs.map(x => "-V " + x.getAbsolutePath).mkString(" ")} $intervalString -o ${tmpout.getCanonicalPath} -nt ${resourceAllocated.cpu} ${extraArgs} -A Coverage -A QualByDepth -A DepthPerSampleHC -A FisherStrand -A MappingQualityRankSumTest -A HardyWeinberg -A LikelihoodRankSumTest -A ReadPosRankSumTest -A StrandBiasBySample -A VariantType --log_to_file ${tmpout.getCanonicalPath + ".log"} -disable_auto_index_creation_and_locking_when_reading_rods"

        val (stdout, stderr, succ) = execGetStreamsAndCodeWithLog(cmd, unsuccessfulOnErrorStream = false)

        if (!succ) throw new RuntimeException("error in GATK during GenotypeGVCFs" + stdout.mkString("\n") + stderr.mkString("\n"))

        tmpvcfs.foreach(_.delete)

        GenotypeGVCFOutput(SharedFile(tmpout, name = outName + ".vcf", canMoveAway = true), SharedFile(new File(tmpout.getCanonicalPath + ".log"), name = outName + ".GenotypeGVCFs.log", canMoveAway = true))

    }

}

