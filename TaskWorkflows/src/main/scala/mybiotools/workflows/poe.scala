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
import akka.actor.{ ActorRefFactory }
import com.typesafe.config.{ Config, ConfigFactory }
import collection.JavaConversions._
import org.saddle._
import org.saddle.scalar._
import mybiotools.gwascommons._
import scala.io.Source

import hdfdosage.{ DosageFileFormat, HDFDosage, PDose, PGenotypeProbabilities, BIGSNPMajor, TPed }

case class PoeParameters(
  config: Config
) extends Result

case class PoeInput(
  outname: Option[String],
  parameters: Option[PoeParameters],
  inputlist: Option[DosageToolInputFiles],
  genomicMap: Option[BimFile],
  covariateFile: Option[PlinkCovariateFile],
  dosagejarFile: Option[JarFile]
) extends SimplePrerequisitive[PoeInput]

case class PoeOutput(
  tests: SharedFile, count: Int
) extends ResultWithSharedFiles(tests)

object PoeInput {
  def empty = TracksInput(None, None, None, None, None, None)

  def apply(
    files: Seq[File],
    parameters: PoeParameters,
    genomicMap: File,
    outname: String,
    covariateFile: Option[File],
    dosagejarFile: File
  )(implicit components: TaskSystemComponents): PoeInput = {

    PoeInput(
      inputlist = Some(DosageToolInputFiles(files.map(s => SharedFile(s)))),
      outname = Some(outname),
      parameters = Some(parameters),
      genomicMap = Some(BimFile(SharedFile(genomicMap))),
      covariateFile = covariateFile.map(x => PlinkCovariateFile(SharedFile(x))),
      dosagejarFile = Some(JarFile(SharedFile(dosagejarFile)))
    )
  }
  def fromSharedFiles(
    files: Seq[SharedFile],
    parameters: PoeParameters,
    genomicMap: File,
    outname: String,
    covariateFile: Option[File],
    dosagejarFile: File
  )(implicit components: TaskSystemComponents): PoeInput = {

    PoeInput(
      inputlist = Some(DosageToolInputFiles(files)),
      outname = Some(outname),
      parameters = Some(parameters),
      genomicMap = Some(BimFile(SharedFile(genomicMap))),
      covariateFile = covariateFile.map(x => PlinkCovariateFile(SharedFile(x))),
      dosagejarFile = Some(JarFile(SharedFile(dosagejarFile)))
    )
  }
  def apply(
    list: File,
    parameters: PoeParameters,
    genomicMap: File,
    outname: String,
    covariateFile: Option[File],
    dosagejarFile: File
  )(implicit components: TaskSystemComponents): PoeInput = openSource(list.getCanonicalPath) { source =>
    apply(source.getLines.map(x => new File(x)).toList, parameters, genomicMap, outname, covariateFile, dosagejarFile)
  }

  def updatePlinkCovar: UpdatePrerequisitive[PoeInput] = {
    case (self, plinkc: PlinkCovariateFile) => self.copy(covariateFile = Some(plinkc))
  }

}

object poetask {
  def apply(
    in: PoeInput,
    update: UpdatePrerequisitive[PoeInput] = PoeInput.updatePlinkCovar orElse identity[PoeInput],
    memory: Int
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = memory)
    ) {

      case (
        PoeInput(
          Some(outname),
          Some(parameters),
          Some(inputfiles),
          Some(genomicMap),
          Some(covariateFile),
          Some(dosagejarFile)), ce) =>
        import ce._

        val workdir = {
          val folder = new File(TempFile.createTempFile("poeapp").getParent)
          val x = new File(folder, outname + "workdir")
          x.mkdir
          x
        }

        val covariates: Frame[Individual, String, Double] = mybiotools.stat.LinearRegression.readPlinkCovarFile(Source.fromFile(covariateFile.file.file.getCanonicalPath), "-9")

        val output = new File(workdir, outname)

        val inputlist = {
          val f = TempFile.createTempFile("filelist")
          writeToFile(f.getCanonicalPath, inputfiles.elems.map(_.file.getCanonicalPath).mkString("\n"))
          f.getCanonicalPath
        }

        val tmpimputefile = TempFile.createTempFile(".imp")

        try {
          val recodecmd = s"java -Xmx${resourceAllocated.memory}M -Dmerge.run=true -Dmerge.outputFormat=impute -Dmerge.cacheSize=1000 -Dmerge.inputs=${inputlist} -Dmerge.output=${tmpimputefile.getCanonicalPath} -Dmerge.genomicMap=${genomicMap.file.file.getCanonicalPath} -Dmerge.minimumSNPCoverage=0.98 -jar ${dosagejarFile.file.file.getCanonicalPath} -Djava.io.tmpdir=${System.getProperty("java.io.tmpdir")}"

          val (stdout1, stderr1, succ1) = execGetStreamsAndCodeWithLog(recodecmd)

          if (!succ1) throw new RuntimeException("error in recode to impute" + stdout1.mkString("\n") + stderr1.mkString("\n"))

          val poeout = TempFile.createTempFile(".poeout")
          val phenoname = parameters.config.getString("phenoname")
          val covarnames = parameters.config.getStringList("covarnames").toVector
          val covarstring = covarnames.map(x => s"--ncovar $x").mkString(" ", " ", " ")

          val individuals: Vector[Individual] = openSource(tmpimputefile.getCanonicalPath + ".individuals")(s => s.getLines.map { line =>
            val spl = fastSplitSeparator(line, ' ')
            Individual(spl(0), spl(1))
          }.toVector)

          val doBlomTransform = scala.util.Try(parameters.config.getBoolean("blomtransform")).getOrElse(false)

          val transformedCovariates = if (!doBlomTransform) covariates else {
            val (inds, phenos): (Seq[Individual], Seq[Double]) = covariates.firstCol(phenoname)(individuals: _*).toSeq.unzip
            val transformed = mybiotools.stat.BlomTransformation(phenos)
            val frame: Frame[Individual, String, Double] = Frame(phenoname -> Series((inds zip transformed): _*))

            covariates.filterIx(_ != phenoname).concat(frame)

          }

          def escape(x: Option[Double]): String = x match {
            case None => "-9"
            case Some(y) => y.toString
          }

          val covarsToWrite: Vector[Vector[String]] =
            (phenoname +: covarnames) +: individuals.map { x => (phenoname +: covarnames).map { n => escape(Scalar.scalarToOption(transformedCovariates.firstCol(n).first(x))) } }

          val covariateString = covarsToWrite.map(_.mkString(" ")).mkString("\n")

          val tmpcovar = TempFile.createTempFile(".covar")
          writeToFile(tmpcovar.getCanonicalPath, covariateString)

          val quicktestcmd = s"quicktest-static --geno ${tmpimputefile.getCanonicalPath} --pheno ${tmpcovar.getCanonicalPath} --missing-code -9 --npheno $phenoname $covarstring --method-poe --out ${poeout.getCanonicalPath}"

          val (stdout2, stderr2, succ2) = execGetStreamsAndCode(quicktestcmd)

          if (!succ2) throw new RuntimeException("error in quicktest" + stdout2.mkString("\n") + stderr2.mkString("\n"))

          val testsFile = SharedFile(poeout, name = outname + ".poe")

          PoeOutput(testsFile, count = individuals.size)
        } finally {
          tmpimputefile.delete
        }
    }

}

