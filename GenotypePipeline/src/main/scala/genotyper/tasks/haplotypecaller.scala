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

case class ExpectedNumberOfBamFiles(count: Int) extends Result

case class HaplotypeCallerInput(
    bamsWithIndex: Option[Set[(SharedFile, SharedFile)]],
    expectedNumberOfBamFiles: Option[Int],
    reference: Option[FastaWithIndex],
    interval: Option[Set[Region]],
    gatkjar: Option[SharedFile],
    outName: Option[String],
    extraArgs: Option[String]
) extends Prerequisitive[HaplotypeCallerInput] {
  override def persistent = this.copy(gatkjar = None)

  def ready = {
    reference.isDefined &&
      interval.isDefined &&
      gatkjar.isDefined &&
      expectedNumberOfBamFiles.isDefined &&
      outName.isDefined &&
      extraArgs.isDefined
    bamsWithIndex.isDefined &&
      bamsWithIndex.get.size == expectedNumberOfBamFiles.get
  }

}

case class HaplotypeCallerOutput(
  vcf: SharedFile,
  vcfidx: SharedFile,
  log: SharedFile
) extends ResultWithSharedFiles(vcf, vcfidx, log)

object HaplotypeCallerInputFactory {
  def apply(jar: SharedFile, referenceFasta: SharedFile, referenceFai: SharedFile, referenceDict: SharedFile, interval: Iterable[Region], outName: String, expectedBams: Int, extraArgs: String)(implicit components: TaskSystemComponents): HaplotypeCallerInput = {

    assert(expectedBams > 0)

    HaplotypeCallerInput(
      gatkjar = Some((jar)),
      reference = Some(FastaWithIndex(
        fasta = (referenceFasta),
        fai = (referenceFai),
        dict = (referenceDict)
      )),
      bamsWithIndex = None,
      expectedNumberOfBamFiles = Some(expectedBams),
      interval = Some(interval.toSet),
      outName = Some(outName),
      extraArgs = Some(extraArgs)
    )

  }
}

object HaplotypeCallerInput {
  def empty = HaplotypeCallerInput(None, None, None, None, None, None, None)

  def apply(jar: File, referenceFasta: File, referenceFai: File, referenceDict: File, interval: Iterable[Region], outName: String, expectedBams: Int, extraArgs: String)(implicit components: TaskSystemComponents): HaplotypeCallerInput = {

    assert(expectedBams > 0)

    HaplotypeCallerInput(
      gatkjar = Some(SharedFile(jar)),
      reference = Some(FastaWithIndex(
        fasta = SharedFile(referenceFasta),
        fai = SharedFile(referenceFai),
        dict = SharedFile(referenceDict)
      )),
      bamsWithIndex = None,
      expectedNumberOfBamFiles = Some(expectedBams),
      interval = Some(interval.toSet),
      outName = Some(outName),
      extraArgs = Some(extraArgs)
    )

  }

  private def updateBamList(input: HaplotypeCallerInput, pair: (SharedFile, SharedFile)): HaplotypeCallerInput = {
    val updated = input.bamsWithIndex match {
      case None => Some(Set(pair))
      case Some(xs) => Some(xs + pair)
    }
    input.copy(bamsWithIndex = updated)

  }

  def updateHaplotypeCallerInput: UpdatePrerequisitive[HaplotypeCallerInput] = {
    case (self, i: BamWithBai) => updateBamList(self, (i.bam, i.bai))
    case (self, i: ExpectedNumberOfBamFiles) if i.count > 0 => self.copy(expectedNumberOfBamFiles = Some(i.count))
  }

}

object haplotypecaller {
  def apply(
    in: HaplotypeCallerInput,
    memory: Int,
    cpu: Int,
    update: UpdatePrerequisitive[HaplotypeCallerInput] = HaplotypeCallerInput.updateHaplotypeCallerInput orElse identity[HaplotypeCallerInput]
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = cpu, memory = memory)
    ) {

      case (
        HaplotypeCallerInput(
          Some(bamsWithIndex),
          _,
          Some(FastaWithIndex(referenceFasta, referenceFai, referenceDict)),
          Some(interval),
          Some(jarFile),
          Some(outName),
          Some(extraArgs)), ce) =>
        import ce._

        {

          val bamsBesideBais = bamsWithIndex.map {
            case (bamfile, indexfile) =>
              Helpers.putBesidesBam(bamfile.file, indexfile.file)
          }

          val (fasta, fai, dict) = Helpers.putBesidesFasta(referenceFasta.file, referenceFai.file, referenceDict.file)

          val intervalString = if (interval.isEmpty) "" else {
            val tmp = TempFile.createTempFile(".bed")
            writeToFile(tmp.getAbsolutePath, interval.map(_.toLine).mkString("\n"))
            s" -L ${tmp.getCanonicalPath} "
          }

          //       java -jar GenomeAnalysisTK.jar \ 
          // -T HaplotypeCaller \ 
          // -R reference.fa \ 
          // -I preprocessed_reads.bam \  # can be reduced or not
          // -L 20 \ 
          // --genotyping_mode DISCOVERY \ 
          // -stand_emit_conf 10 \ 
          // -stand_call_conf 30 \ 
          // -o raw_variants.vcf 
          val tmpout = TempFile.createTempFile(".g.vcf")

          val cmd = s"java -Xmx${resourceAllocated.memory}m -jar ${jarFile.file.getCanonicalPath} -T HaplotypeCaller -R ${fasta.getAbsolutePath} ${bamsBesideBais.map(x => "-I " + x._1.getAbsolutePath).mkString(" ")} $intervalString -o ${tmpout.getCanonicalPath} --genotyping_mode DISCOVERY -stand_emit_conf 10 -stand_call_conf 30 -nct ${resourceAllocated.cpu} ${extraArgs} -A Coverage -A QualByDepth -A DepthPerSampleHC -A FisherStrand -A MappingQualityRankSumTest -A HardyWeinberg -A LikelihoodRankSumTest -A ReadPosRankSumTest -A StrandBiasBySample -A VariantType -A InbreedingCoeff -A GCContent -A LowMQ --log_to_file ${tmpout.getCanonicalPath + ".log"}"

          val (stdout, stderr, succ) = execGetStreamsAndCodeWithLog(cmd, unsuccessfulOnErrorStream = false)

          if (!succ) throw new RuntimeException("error in GATK during haplotypecaller" + stdout.mkString("\n") + stderr.mkString("\n"))

          HaplotypeCallerOutput(SharedFile(tmpout, name = outName + ".g.vcf", canMoveAway = true), SharedFile(new File(tmpout.getAbsolutePath + ".idx"), name = outName + ".vcf.idx", canMoveAway = true), SharedFile(new File(tmpout.getCanonicalPath + ".log"), name = outName + ".haplotypecaller.log", canMoveAway = true))

        }

    }

}

