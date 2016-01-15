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

package fastlmmwrapper

import com.typesafe.config.{ Config, ConfigFactory }
import mybiotools.tasks._
import java.io.File
import mybiotools.config.Config.configInstance
import scala.collection.JavaConversions._
import scala.concurrent.{ Future, Await }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import org.saddle._
import mybiotools._
import mybiotools.stat.LinearRegression.readPlinkCovarFile
import hdfdosage.FileSets._
import hdfdosage.FileSets
import mybiotools.workflows._
import mybiotools.gwascommons._
import mybiotools.gwascommons.gwas._
import mybiotools.gwascommons.genotypedata._
import mybiotools.stringstore._

trait FastlmmConfig {
  def covarfiles: List[File]
  def phenoname: String
  def covarnames: List[String]
  def outname: String
  def genotypefiles: List[File]
  def genotypefiletype: String
  def conditionmarkers: List[String]
  def fastlmmRAM: Int
  def fastlmmCPU: Int
  def kernel: Option[File]
  def generateKernelType: Option[String]
  def generateKernelList: List[String8]
  def generateKernelRandom: Option[Int]
  def fastlmmParameters: String
  def onlykernel: Boolean

}

object DefaultFastlmmConfig extends FastlmmConfig {
  val onlykernel = configInstance.getBoolean("fastlmm.onlykernel")
  val covarfiles: List[File] = ((configInstance.getString("fastlmm.covarfilelist") match {
    case x if x == "" => Nil
    case x => scala.io.Source.fromFile(x).getLines.toList
  }) ++ (configInstance.getString("fastlmm.covar") match {
    case x if x == "" => Nil
    case x => List(x)
  })).map(x => new File(x))
  val phenoname = configInstance.getString("fastlmm.pheno-name")
  val covarnames = configInstance.getString("fastlmm.covar-names") match {
    case x if x == "" => Nil
    case x => x.split(",").toList
  }
  val outname = configInstance.getString("fastlmm.outname")

  val genotypefiles = {
    val x = configInstance.getString("fastlmm.genotypefilelist")

    openSource(x)(_.getLines.map(x => new File(x)).toList)
  }
  val genotypefiletype = configInstance.getString("fastlmm.genotypefiletype")
  val conditionmarkers = ((configInstance.getString("fastlmm.condition-list") match {
    case x if x == "" => Nil
    case x => scala.io.Source.fromFile(x).getLines.toList
  }) ++ (configInstance.getString("fastlmm.condition") match {
    case x if x == "" => Nil
    case x => x.split(",").toList
  })).toList
  val fastlmmRAM = configInstance.getInt("fastlmm.RAM")
  val fastlmmCPU = configInstance.getInt("fastlmm.CPU")
  val kernel = {
    val x = configInstance.getString("fastlmm.kernel")
    if (x == "-") None
    else Some(new File(x))
  }
  val fastlmmParameters = configInstance.getString("fastlmm.extraArgs")
  val generateKernelType = if (kernel.isDefined) None else Some(configInstance.getString("fastlmm.generateKernel.type"))
  val generateKernelList = if (generateKernelType.get.toLowerCase == "list") openSource(configInstance.getString("fastlmm.generateKernel.list"))(_.getLines.toList.map(x => StringStore(x))) else Nil
  val generateKernelRandom = if (generateKernelType.get.toLowerCase == "random") Some(configInstance.getInt("fastlmm.generateKernel.numberOfSnps")) else None
}

class FastlmmWrapper(ts: TaskSystem, fastlmmConfig: FastlmmConfig) {

  def run: Unit = {
    import ts._
    import fastlmmConfig._
    implicit val fs = components.fs
    implicit val actorsystem = components.actorsystem
    if (ts.hostConfig.myRole == MASTER) {
      val log = ts.getLogger(this)

      ts.registerApplicationFileLogger(new File(configInstance.getString("tasks.fileServiceBaseFolder") + "/logfile"))
      log.info("Badge:\n" + (mybiotools.config.Config.prettyPrintVersion("FastlmmWrapper", fastlmmwrapper.Reflected.version)))
      log.info(s"Config: $fastlmmConfig")

      val genotypefilelist: List[GenotypeFileSet] = {
        genotypefiles.map { f =>
          genotypefiletype match {
            case "pdose" => PDose(f, -9f, None)
            case "tped" => TPed(new File(f.getAbsolutePath + ".fam"), new File(f.getAbsolutePath + ".tped"), '0')
            case "pgenotypeprobabilities" => FileSets.PGenotypeProbabilities(f, -9f, None)
            case "hdfdosage" => FileSets.HDFDosage(f)
          }
        }
      }

      val covariates: Frame[Individual, String, Double] = {

        val covs = {
          covarfiles.map(f => useResource(scala.io.Source.fromFile(f))(source => readPlinkCovarFile(source, "-9")))
        }.reduce(_ rconcat _).filterIx((str: String) => (phenoname +: covarnames).contains(str))

        val conditionSNPs: Frame[Individual, String, Double] = if (conditionmarkers.size > 0) {
          genotypefilelist.map { file =>
            FileSets.openFileSet(file, 0.0, Full) { snpmajoriterator =>
              val locusiter = snpmajoriterator.toLocusIteratorWithGenomicMap(Map())
              val inds: Seq[Individual] = locusiter.individuals
              val indIdx = Index(inds: _*)
              val snps: Seq[(String, Series[Individual, Double])] = locusiter.loci.filter(x => conditionmarkers.contains(x._1.name.value)).map {
                case (ld, geno) =>
                  ld.name.value -> mybiotools.gwascommons.gwas.GWAS.recodeGenotypesDropNA(indIdx, geno, Additive)
              }.toSeq
              Frame(snps: _*)
            }
          }.reduce(_ rconcat _)
        } else Frame[Individual, String, Double]()

        covs rconcat conditionSNPs
      }.rfilter(x => !x.filterIx(x => (covarnames ++ conditionmarkers).contains(x)).hasNA).filter(_.toVec.dropNA.toSeq.distinct.size > 1)

      log.info(s"Extract of covariates  (after removing any rows with NA and any columns with 0 variance): \n $covariates")

      val covariatefile = {
        val tmp = TempFile.createFileInTempFolderIfPossibleWithName(outname + ".mergedcovar")
        mybiotools.writeToFile(
          tmp.getAbsolutePath,
          FrameToPlink.frameToPlink(covariates, "-9")
        )
        tmp
      }

      val task = if (!onlykernel) {
        val input = FastlmmInput(
          outname = outname,
          parameters = fastlmmParameters,
          covariateFile = covariatefile,
          phenoname = phenoname,
          covariateNames = covarnames,
          expectedfiles = genotypefilelist.size
        )

        val task = fastlmmtask(input, fastlmmCPU, fastlmmRAM)

        genotypefilelist.foreach { f =>
          f match {
            case PDose(pdose, missingValue, fam) => task <~ PDoseFile(SharedFile(pdose))
            case TPed(fam, tped, missingValue) => task <~ TPedFileSet(TfamFile(SharedFile(fam)), TpedFile(SharedFile(tped)), missingValue)
            case _ => { throw new RuntimeException("the rest is not supported") }
          }
        }

        if (kernel.isDefined) {
          task <~ FastlmmGRMFile(SharedFile(kernel.get))
        } else {
          val kernelinput = FastlmmInput(
            outname = outname,
            parameters = fastlmmParameters,
            covariateFile = covariatefile,
            phenoname = phenoname,
            covariateNames = covarnames,
            expectedfiles = genotypefilelist.size
          )
          val kerneltask = fastlmmkerneltask(kernelinput, fastlmmCPU, fastlmmRAM)

          genotypefilelist.foreach { f =>
            f match {
              case PDose(pdose, missingValue, fam) => kerneltask <~ PDoseFile(SharedFile(pdose))
              case PGenotypeProbabilities(pgeno, missingValue, fam) => kerneltask <~ PGenoFile(SharedFile(pgeno))
              case FileSets.VCFFile(vcf) => kerneltask <~ workflows.VCFFile(SharedFile(vcf))
              case FileSets.HDFDosage(hdf) => kerneltask <~ workflows.HDFFile(SharedFile(hdf))
              case TPed(fam, tped, missingValue) => kerneltask <~ TPedFileSet(TfamFile(SharedFile(fam)), TpedFile(SharedFile(tped)), missingValue)
            }
          }

          generateKernelType.get.toLowerCase match {
            case "all" => kerneltask <~ UseAll
            case "random" => kerneltask <~ UseRandom(generateKernelRandom.get)
            case "list" => kerneltask <~ UseSpecific(generateKernelList)
          }

          kerneltask ~> task

        }

        task
      } else {
        val kernelinput = FastlmmInput(
          outname = outname,
          parameters = fastlmmParameters,
          covariateFile = covariatefile,
          phenoname = phenoname,
          covariateNames = covarnames,
          expectedfiles = genotypefilelist.size
        )
        val kerneltask = fastlmmkerneltask(kernelinput, fastlmmCPU, fastlmmRAM)

        genotypefilelist.foreach { f =>
          f match {
            case PDose(pdose, missingValue, fam) => kerneltask <~ PDoseFile(SharedFile(pdose))
            case TPed(fam, tped, missingValue) => kerneltask <~ TPedFileSet(TfamFile(SharedFile(fam)), TpedFile(SharedFile(tped)), missingValue)
            case _ => { throw new RuntimeException("the rest is not supported") }
          }
        }

        generateKernelType.get.toLowerCase match {
          case "all" => kerneltask <~ UseAll
          case "random" => kerneltask <~ UseRandom(generateKernelRandom.get)
          case "list" => kerneltask <~ UseSpecific(generateKernelList)
        }
        kerneltask
      }

      Await.ready(task.?, Duration.Inf)

      ts.shutdown

    }
  }

}

object FastlmmWrapperApp extends App {

  new FastlmmWrapper(defaultTaskSystem, DefaultFastlmmConfig).run
}
