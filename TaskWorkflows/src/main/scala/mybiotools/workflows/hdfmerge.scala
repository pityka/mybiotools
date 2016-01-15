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
import mybiotools.tasks._
import java.io.File
import mybiotools.gwascommons._
import mybiotools.stringstore._
import scala.io.Source
import akka.actor.{ ActorRefFactory, ActorContext }
import hdfdosage._
import mybiotools.sharedfiletypes._
import mybiotools.sharedfiletypes.SharedFileSets._

import hdfdosage.{ DosageFileFormat, HDFDosage, PDose, PGenotypeProbabilities, BIGSNPMajor, TPed }

case class BIGFileSet(big: SharedFile, bim: SharedFile, fam: SharedFile) extends ResultWithSharedFiles(big, bim, fam)
case class GRMFile(fastlmm: FastlmmGRMFile, gctagz: SharedFile, gctaid: SharedFile, eval: SharedFile, evec: SharedFile, pcaplot1: SharedFile, pcaplot2: SharedFile, pcaplot3: SharedFile, grmplot: SharedFile, snps: SharedFile) extends ResultWithSharedFiles(fastlmm.file, gctagz, gctaid, eval, evec, pcaplot1, pcaplot2, pcaplot3, grmplot, snps)
case class FastlmmGRMFile(file: SharedFile) extends ResultWithSharedFiles(file)

case class JarFile(file: SharedFile) extends ResultWithSharedFiles(file)
case class DosageToolInputFiles(elems: Seq[SharedFile]) extends ResultWithSharedFiles(elems: _*)
case class DosageToolIndividualList(file: Option[SharedFile]) extends ResultWithSharedFiles(file.toList: _*)
case class DosageToolIncludeIndividualList(file: Option[SharedFile]) extends ResultWithSharedFiles(file.toList: _*)
case class DosageToolExcludeIndividualList(file: Option[SharedFile]) extends ResultWithSharedFiles(file.toList: _*)
case class DosageToolIncludeSNPList(file: Option[SharedFile]) extends ResultWithSharedFiles(file.toList: _*)
case class DosageToolExcludeSNPList(file: Option[SharedFile]) extends ResultWithSharedFiles(file.toList: _*)

case class DosageToolParameters(
  minimumSNPCoverage: Double = 0.0,
  minimumMAF: Double = 0.0,
  sortOutput: Boolean = true,
  hardCallThreshold: Double = 0.1,
  cacheSize: Int = 5000,
  blockSize: Int = 1
) extends Result

case class MergeHDFInput(
  outname: Option[String],
  parameters: Option[DosageToolParameters],
  inputlist: Option[Set[SharedFileSet]],
  includeIndividuals: Option[Option[SharedFile]] = Some(None),
  excludeIndividuals: Option[Option[SharedFile]] = Some(None),
  includeSNPs: Option[Option[SharedFile]] = Some(None),
  excludeSNPs: Option[Option[SharedFile]] = Some(None),
  genomicMap: Option[Option[SharedFile]] = Some(None)
) extends SimplePrerequisitive[MergeHDFInput]

object MergeHDFInput {

  def apply(
    files: Seq[FileSets.GenotypeFileSet],
    parameters: DosageToolParameters,
    genomicMap: File,
    outname: String
  )(implicit components: TaskSystemComponents): MergeHDFInput = {

    MergeHDFInput(
      inputlist = Some((files.map(s => SharedFileSets.fromFileSet(s))).toSet),
      outname = Some(outname),
      parameters = Some(parameters),
      genomicMap = Some((Some(SharedFile(genomicMap))))
    )
  }

  def fromSharedInput(
    files: Seq[SharedFileSet],
    parameters: DosageToolParameters,
    genomicMap: File,
    outname: String
  )(implicit components: TaskSystemComponents): MergeHDFInput = {

    MergeHDFInput(
      inputlist = Some((files.toSet)),
      outname = Some(outname),
      parameters = Some(parameters),
      genomicMap = Some((Some(SharedFile(genomicMap))))
    )
  }

}

trait DosagetoolStub[R <: Result] {

  def packageUp(report: HDFDosageFile.MergeDosageReport, file: File, outname: String)(implicit ce: TaskSystemComponents): R

  def outputformatstring: String

  def runStub(in: (MergeHDFInput, ComputationEnvironment)) = in match {

    case (
      MergeHDFInput(
        Some(outname),
        Some(parameters),
        Some(inputfiles),
        Some(includeIndividuals),
        Some(excludeIndividuals),
        Some(includeSNPs),
        Some(excludeSNPs),
        Some(genomicMapFile)), ce) =>
      import ce._

      import parameters._

      val output = TempFile.createTempFile(outname)

      val inputfilesseq = inputfiles.toSeq.map(_.toFileSet)

      val individualIDFiles: Seq[Option[Source]] = inputfilesseq.map(x => None)

      val sortByCHR = sortOutput

      val format = outputformatstring match {
        case x if x == "hdfdosage" => HDFDosage
        case x if x == "pdose" => PDose
        case x if x == "bigind" => BIGIndividualMajor
        case x if x == "big" || x == "bigsnp" => BIGSNPMajor
        case x if x == "pgenotypeprobabilities" => PGenotypeProbabilities
        case x if x == "impute" => IMPUTEFile
        case x if x == "missing" => Missingness
        case x if x == "tped" => {
          TPed(hardCallThreshold, '-')
        }
        case x if x == "grm" => GRMMatrix
      }

      val recodeToRecessive = false

      val genomicmap = NodeLocalCache.getItemBlocking(s"genomicmap:${genomicMapFile.map(_.name)}") {
        genomicMapFile.map(x => getGenomicMapFromBimFile(x.file.getCanonicalPath)).getOrElse(collection.Map[String8, GenomicLocation]())
      }

      val report = HDFDosageFile.mergeSNPMajors(
        inputfilesseq,
        output,
        minimumSNPCoverage,
        includeSNPs = includeSNPs.map(sf => openSource(sf.file)(_.getLines.map(x => StringStore(x))).toSet).getOrElse(Set[String8]()),
        excludeSNPs = excludeSNPs.map(sf => openSource(sf.file)(_.getLines.map(x => StringStore(x))).toSet).getOrElse(Set[String8]()),
        includeIndividuals = includeIndividuals.map(sf => openSource(sf.file)(getIndividualsFromFamFile(_).toSet)).getOrElse(Set[Individual]()),
        excludeIndividuals = excludeIndividuals.map(sf => openSource(sf.file)(getIndividualsFromFamFile(_).toSet)).getOrElse(Set[Individual]()),
        cacheSize,
        format,
        minimumMAF,
        genomicmap,
        sortByCHR = sortByCHR,
        blockSize = if (sortByCHR && genomicmap.size > 0) blockSize else 1,
        recodeToRecessive = false
      )
      if (!report.writeSuccessful) {

        openFileWriter(new File(output.getAbsolutePath + ".mismatchedSNPs")) { writer =>
          report.mismatchedSNPs.foreach {
            _.foreach { snp =>
              writer.write(snp.snpName.toString)
              writer.write("\n")
            }
          }
        }
        throw new RuntimeException("Merge unsuccessful. Mismatched SNPs written to: " + new File(output.getAbsolutePath + ".mismatchedSNPs").getAbsolutePath)

      }

      packageUp(report, output, outname)

  }

}

object HardCallImpl extends DosagetoolStub[SharedBedSet] {

  def packageUp(report: HDFDosageFile.MergeDosageReport, tmpFileStub: File, outname: String)(implicit components: TaskSystemComponents) = {

    val tfam = report.file.get.asInstanceOf[FileSets.TPed].fam
    val tped = report.file.get.asInstanceOf[FileSets.TPed].tped

    val output = TempFile.createTempFile("").getAbsolutePath

    val (stdout2, stderr2, succ2) = execGetStreamsAndCode(s"plink --tfam ${tfam} --tped ${tped} --make-bed --noweb --out $output")

    if (!succ2) throw new RuntimeException("error in plink" + stdout2.mkString("\n") + stderr2.mkString("\n"))

    tped.delete
    tfam.delete

    val bim = (SharedFile(new File(output + ".bim"), name = outname + ".bim", canMoveAway = true))
    val bed = (SharedFile(new File(output + ".bed"), name = outname + ".bed", canMoveAway = true))
    val fam = (SharedFile(new File(output + ".fam"), name = outname + ".fam", canMoveAway = true))

    SharedBedSet(bed, bim, fam)
  }

  def outputformatstring: String = "tped"

}

object dosagetoolhardcall {
  def apply(
    in: MergeHDFInput,
    update: UpdatePrerequisitive[MergeHDFInput] = identity[MergeHDFInput],
    memory: Int = 70000
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = memory)
    ) { (in, ce) =>
      HardCallImpl.runStub((in, ce))
    }
}

object dosagetoolmerge2hdf {

  def apply(
    in: MergeHDFInput,
    update: UpdatePrerequisitive[MergeHDFInput] = identity[MergeHDFInput],
    memory: Int = 70000
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = memory)
    ) { (in, ce) =>
      Merge2HDFImpl.runStub((in, ce))
    }
}

object Merge2HDFImpl extends DosagetoolStub[SharedHDFDosage] {

  def packageUp(report: HDFDosageFile.MergeDosageReport, tmpFileStub: File, outname: String)(implicit ce: TaskSystemComponents) = {
    val tmpFileStub = report.file.get.asInstanceOf[FileSets.HDFDosage].file

    val hdf = SharedFile(tmpFileStub, name = outname, canMoveAway = true)

    SharedHDFDosage(hdf)
  }

  def outputformatstring: String = "hdfdosage"
}

object dosagetoolmerge2pdose {
  def apply(
    in: MergeHDFInput,
    update: UpdatePrerequisitive[MergeHDFInput] = identity[MergeHDFInput],
    memory: Int = 70000
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = memory)
    ) { (in, ce) =>
      Merge2PDoseImpl.runStub((in, ce))
    }
}

object Merge2PDoseImpl extends DosagetoolStub[SharedBlockCompressedPDose] {

  def packageUp(report: HDFDosageFile.MergeDosageReport, tmpFileStub: File, outname: String)(implicit ce: TaskSystemComponents) = {

    val set = report.file.get.asInstanceOf[FileSets.BGZippedPDose]

    SharedBlockCompressedPDose(
      SharedFile(set.file, name = outname, canMoveAway = true),
      set.fam.map(
        f => SharedFile(f, name = outname + ".fam", canMoveAway = true)
      ),
      set.missingValue,
      SharedFile(set.index, name = outname + ".pidx", canMoveAway = true)
    )

  }

  def outputformatstring: String = "pdose"
}

object Merge2BIGImpl extends DosagetoolStub[BIGFileSet] {

  def packageUp(report: HDFDosageFile.MergeDosageReport, tmpFileStub: File, outname: String)(implicit ce: TaskSystemComponents) = {

    val bim = SharedFile(new File(tmpFileStub + ".bim"), name = outname + ".bim", canMoveAway = true)
    val big = SharedFile(new File(tmpFileStub + ".big"), name = outname + ".big", canMoveAway = true)
    val fam = SharedFile(new File(tmpFileStub + ".fam"), name = outname + ".fam", canMoveAway = true)

    BIGFileSet(big, bim, fam)

  }

  def outputformatstring: String = "big"
}

object dosagetoolmerge2big {
  def apply(
    in: MergeHDFInput,
    update: UpdatePrerequisitive[MergeHDFInput] = identity[MergeHDFInput],
    memory: Int = 70000
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = memory)
    ) { (in, ce) =>
      Merge2BIGImpl.runStub((in, ce))
    }
}

object Merge2GRMImpl extends DosagetoolStub[GRMFile] {

  def packageUp(report: HDFDosageFile.MergeDosageReport, tmpFileStub: File, outname: String)(implicit ce: TaskSystemComponents) = {

    val gctaid = SharedFile(new File(tmpFileStub + ".grm.id"), name = outname + ".grm.id", canMoveAway = true)
    val gctagz = SharedFile(new File(tmpFileStub + ".grm.gz"), name = outname + ".grm.gz", canMoveAway = true)
    val fastlmm = SharedFile(new File(tmpFileStub + ".fastlmmsim"), name = outname + ".fastlmmsim", canMoveAway = true)
    val evec = SharedFile(new File(tmpFileStub + ".evec"), name = outname + ".evec", canMoveAway = true)
    val eval = SharedFile(new File(tmpFileStub + ".eval"), name = outname + ".eval", canMoveAway = true)
    val p1 = SharedFile(new File(tmpFileStub + ".pcaplot.1:2.png"), name = outname + ".pcaplot.1:2.png", canMoveAway = true)
    val p2 = SharedFile(new File(tmpFileStub + ".pcaplot.1:3.png"), name = outname + ".pcaplot.1:3.png", canMoveAway = true)
    val p3 = SharedFile(new File(tmpFileStub + ".pcaplot.2:3.png"), name = outname + ".pcaplot.2:3.png", canMoveAway = true)
    val grmplot = SharedFile(new File(tmpFileStub.getCanonicalPath + ".grm.png"), name = outname + ".grm.png", canMoveAway = true)
    val snps = SharedFile(new File(tmpFileStub.getCanonicalPath + ".snps"), name = outname + ".snps", canMoveAway = true)

    GRMFile(FastlmmGRMFile(fastlmm), gctagz, gctaid, eval, evec, p1, p2, p3, grmplot, snps)

  }

  def outputformatstring: String = "grm"
}

object dosagetoolmerge2grm {
  def apply(
    in: MergeHDFInput,
    update: UpdatePrerequisitive[MergeHDFInput] = identity[MergeHDFInput],
    memory: Int = 70000
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = memory)
    ) { (in, ce) =>
      Merge2GRMImpl.runStub((in, ce))
    }
}