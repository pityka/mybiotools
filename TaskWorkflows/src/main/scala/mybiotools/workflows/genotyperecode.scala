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
import mybiotools.gwascommons._
import mybiotools.stringstore._
import mybiotools.tasks._
import java.io.File
import akka.actor.{ ActorRefFactory, ActorContext }

import hdfdosage.{ DosageFileFormat, HDFDosage, PDose, PGenotypeProbabilities, BIGSNPMajor, TPed }
import hdfdosage.FileSets
import hdfdosage.FileSets._
import mybiotools.sharedfiletypes._
import mybiotools.sharedfiletypes.SharedFileSets._

case class RecodeParameters(
  inputSorted: Boolean,
  recodeToRecessive: Boolean,
  blockSize: Int,
  ldprune: Boolean,
  pruneWindow: Int,
  pruneThreshold: Double,
  pruneKeepSnps: Set[String],
  pruneAwaySnps: Set[String],
  minimumMAF: Double,
  maximumMAF: Double,
  grmBatchSize: Int,
  maximumMissingnessRate: Double,
  includeIndividuals: Option[Set[Individual]]
) extends Result

case class RecodeInput(
  outname: Option[String],
  inputfile: Option[SharedFileSet],
  parameters: Option[RecodeParameters],
  genomicMap: Option[Option[SharedFile]] = Some(None),
  includeSNPs: Option[Option[SharedFile]] = Some(None)
) extends SimplePrerequisitive[RecodeInput]

object RecodeInput {

  def apply(
    fileset: GenotypeFileSet,
    parameters: RecodeParameters,
    genomicMap: Option[File],
    outname: String,
    includeSNPs: Option[File]
  )(implicit components: TaskSystemComponents): RecodeInput = {

    RecodeInput(
      inputfile = Some(SharedFileSets.fromFileSet(fileset)),
      outname = Some(outname),
      parameters = Some(parameters),
      includeSNPs = Some((includeSNPs.map(f => SharedFile(f)))),
      genomicMap = Some((genomicMap.map(f => SharedFile(f))))
    )
  }

}

trait RecodeStub[R <: Result] {

  def packageUp(tmpFileStub: File, outname: String)(implicit ce: TaskSystemComponents): R

  def outputFormat: DosageFileFormat

  def runStub(in: (RecodeInput, ComputationEnvironment)) = in match {

    case (
      RecodeInput(
        Some(outname),
        Some(sharedinputfileset),
        Some(parameters),
        Some(genomicmapfile),
        Some(includeSNPs)
        ), ce) =>
      import ce._
      import parameters._

      {

        val inputfile = sharedinputfileset.toFileSet

        val genomicmap = NodeLocalCache.getItemBlocking(s"genomicmap${genomicmapfile.map(_.name)}") {
          inputfile match {
            case FileSets.VCFFile(file) => {
              val r = vcfhelpers.VCFHelpers.openVCF(file, None)
              val x = vcfhelpers.VCFHelpers.getGenomicMap(r)
              r.close
              x
            }
            case FileSets.BedFile(_, bim, _) => getGenomicMapFromBimFile(bim.getCanonicalPath)
            case _ => genomicmapfile.map(x => getGenomicMapFromBimFile(x.file.getCanonicalPath)).getOrElse(collection.Map[String8, GenomicLocation]())
          }

        }

        val snpIncludeFilter =
          includeSNPs.map(sf => openSource(sf.file.getCanonicalPath)(_.getLines.toSet))

        val output = TempFile.createTempFile("")

        val counter = hdfdosage.Recode.run(
          inputfile,
          output,
          ldprune,
          pruneThreshold,
          pruneWindow,
          pruneKeepSnps,
          pruneAwaySnps,
          genomicmap,
          snpIncludeFilter,
          outputFormat,
          recodeToRecessive,
          blockSize,
          minimumMAF,
          maximumMAF,
          grmBatchSize,
          resourceAllocated.cpu,
          maximumMissingnessRate,
          includeIndividuals
        )

        packageUp(output, outname)
      }
  }

}

object Recode2GRMImpl extends RecodeStub[GRMFile] {

  def packageUp(tmpFileStub: File, outname: String)(implicit components: TaskSystemComponents) = {

    val fastlmm = FastlmmGRMFile(SharedFile(new File(tmpFileStub.getCanonicalPath + ".fastlmmsim"), name = outname + ".fastlmmsim", canMoveAway = true))
    val gctagz = SharedFile(new File(tmpFileStub.getCanonicalPath + ".grm.gz"), name = outname + ".grm.gz", canMoveAway = true)
    val gctaid = SharedFile(new File(tmpFileStub.getCanonicalPath + ".grm.id"), name = outname + ".grm.id", canMoveAway = true)
    val eval = SharedFile(new File(tmpFileStub.getCanonicalPath + ".eval"), name = outname + ".eval", canMoveAway = true)
    val evec = SharedFile(new File(tmpFileStub.getCanonicalPath + ".evec"), name = outname + ".evec", canMoveAway = true)
    val pcaplot1 = SharedFile(new File(tmpFileStub.getCanonicalPath + ".pcaplot.1:2.png"), name = outname + ".pcaplot.1:2.png", canMoveAway = true)
    val pcaplot2 = SharedFile(new File(tmpFileStub.getCanonicalPath + ".pcaplot.1:3.png"), name = outname + ".pcaplot.1:3.png", canMoveAway = true)
    val pcaplot3 = SharedFile(new File(tmpFileStub.getCanonicalPath + ".pcaplot.2:3.png"), name = outname + ".pcaplot.2:3.png", canMoveAway = true)
    val grmplot = SharedFile(new File(tmpFileStub.getCanonicalPath + ".grm.png"), name = outname + ".grm.png", canMoveAway = true)
    val snps = SharedFile(new File(tmpFileStub.getCanonicalPath + ".snps"), name = outname + ".snps", canMoveAway = true)

    GRMFile(
      fastlmm, gctagz, gctaid, eval, evec, pcaplot1, pcaplot2, pcaplot3, grmplot, snps
    )
  }

  def outputFormat = hdfdosage.GRMMatrix

}

object recode2grm {
  def apply(
    in: RecodeInput,
    update: UpdatePrerequisitive[RecodeInput] = identity[RecodeInput],
    cpu: Int,
    memory: Int = 70000
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = cpu, memory = memory)
    ) { (in, ce) =>
      Recode2GRMImpl.runStub((in, ce))
    }
}
