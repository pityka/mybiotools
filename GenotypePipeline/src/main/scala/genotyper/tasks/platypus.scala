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
import htsjdk.variant.vcf._
import htsjdk.variant.variantcontext.writer.VariantContextWriterFactory
import htsjdk.variant.variantcontext.VariantContext
import collection.JavaConversions._
import genotyper._
import mybiotools.workflows._

case class GenotypeWithPlatypusInput(
    bamfiles: Option[Set[(SharedFile, SharedFile)]],
    expectedNumberOfBamFiles: Option[Int],
    reference: Option[FastaWithIndex],
    interval: Option[Set[Region]],
    platypusfolder: Option[File],
    outName: Option[String],
    extraArgs: Option[String]
) extends Prerequisitive[GenotypeWithPlatypusInput] {
  override def persistent = this.copy(platypusfolder = None)

  def ready = {
    reference.isDefined &&
      interval.isDefined &&
      platypusfolder.isDefined &&
      expectedNumberOfBamFiles.isDefined &&
      outName.isDefined &&
      extraArgs.isDefined
    bamfiles.isDefined &&
      bamfiles.get.size == expectedNumberOfBamFiles.get
  }

}

case class GenotypeWithPlatypusOutput(
  vcf: SharedFile,
  log: SharedFile
) extends ResultWithSharedFiles(vcf, log)

object GenotypeWithPlatypusInput {

  def apply(platypusfolder: File, referenceFasta: File, referenceFai: File, referenceDict: File, interval: Set[Region], outName: String, expectedNumberOfBamFiles: Int, extraArgs: String, bamfiles: List[(File, File)])(implicit components: TaskSystemComponents): GenotypeWithPlatypusInput = {

    assert((expectedNumberOfBamFiles + bamfiles.size) > 0, "gvcf input is empty")

    GenotypeWithPlatypusInput(
      platypusfolder = Some((platypusfolder)),
      reference = Some(FastaWithIndex(
        fasta = SharedFile(referenceFasta),
        fai = SharedFile(referenceFai),
        dict = SharedFile(referenceDict)
      )),
      bamfiles = Some(bamfiles.map(f => SharedFile(f._1) -> SharedFile(f._2)).toSet),
      expectedNumberOfBamFiles = Some(expectedNumberOfBamFiles + bamfiles.size),
      interval = Some(interval),
      outName = Some(outName),
      extraArgs = Some(extraArgs)
    )

  }

  private def updateBamList(input: GenotypeWithPlatypusInput, file: SharedFile, idx: SharedFile): GenotypeWithPlatypusInput = {
    val updated = input.bamfiles match {
      case None => Some(Set((file, idx)))
      case Some(xs) => Some(xs + ((file, idx)))
    }
    input.copy(bamfiles = updated)

  }

  def updateGenotypeWithPlatypusInput: UpdatePrerequisitive[GenotypeWithPlatypusInput] = {
    case (self, i: BamWithBai) => updateBamList(self, i.bam, i.bai)

  }

}

object platypusGenotype {
  def apply(
    in: GenotypeWithPlatypusInput,
    memory: Int,
    cpu: Int,
    update: UpdatePrerequisitive[GenotypeWithPlatypusInput] = GenotypeWithPlatypusInput.updateGenotypeWithPlatypusInput orElse identity[GenotypeWithPlatypusInput]
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = cpu, memory = memory)
    ) {

      case (
        GenotypeWithPlatypusInput(
          Some(bams),
          _,
          Some(FastaWithIndex(referenceFasta, referenceFai, referenceDict)),
          Some(interval),
          Some(folder),
          Some(outName),
          Some(extraArgs)), ce) =>
        import ce._

        val (fasta, fai, dict) = Helpers.putBesidesFasta(referenceFasta.file, referenceFai.file, referenceDict.file)

        // copy the gvcfs to tmp disk
        val tmpbams = bams.map { bam =>
          val invcftrunk = mybiotools.putBesides(bam._1.file -> ".bam" :: bam._2.file -> ".bam.bai" :: Nil: _*)
          new File(invcftrunk.getAbsolutePath + ".bam")
        }

        val intervalString = if (interval.isEmpty) "" else {
          val tmp = TempFile.createTempFile(".txt")
          mybiotools.writeToFile(
            tmp,
            interval.map(_.toGATKStringWithChr).mkString("\n")
          )
          s"--regions ${tmp.getAbsolutePath}"
        }

        //       java -Xmx2g -jar GenomeAnalysisTK.jar \
        // -R ref.fasta \
        // -T GenotypeWithPlatypuss \
        // --variant gvcf1.vcf \
        // --variant gvcf2.vcf \
        // -o output.vcf
        val tmpout = TempFile.createTempFile(".vcf")

        val logf = TempFile.createTempFile(".log")

        val cpu = resourceAllocated.cpu

        val cmd = s"python ${folder.getAbsolutePath}/Platypus.py callVariants $intervalString --refFile=${fasta.getAbsolutePath} --bamFiles=${tmpbams.map(x => x.getAbsolutePath).mkString(",")} --output=${tmpout.getAbsolutePath} --nCPU=$cpu --logFileName=${logf.getAbsolutePath} --filterDuplicates=1 "

        println(cmd)

        val (stdout, stderr, succ) = execGetStreamsAndCodeWithLog(cmd, unsuccessfulOnErrorStream = false)

        if (!succ) throw new RuntimeException("error in platypus" + stdout.mkString("\n") + stderr.mkString("\n"))

        tmpbams.foreach(_.delete)

        GenotypeWithPlatypusOutput(SharedFile(tmpout, name = outName + ".vcf", canMoveAway = true), SharedFile(logf, name = outName + ".platypus.log", canMoveAway = true))

    }

}

