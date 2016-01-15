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

package gwasapp

import gwasapp.tasks._
import mybiotools._
import mybiotools.gwascommons._
import gwascommons.genotypedata._
import gwascommons.gwas._
import gwascommons.gwas.GWAS._
import mybiotools.config.Config.configInstance
import mybiotools.stat.LinearRegression.readPlinkCovarFile
import collection.JavaConversions._
import _root_.ch.systemsx.cisd.hdf5._
import hdfdosage.HDFDosageIterator
import mybiotools.stringstore._
import mybiotools.tasks._
import scala.concurrent.Future
import mybiotools.saddlehelpers._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util._
import com.typesafe.config.Config
import org.saddle._
import java.io.File
import mybiotools.sharedfiletypes._
import mybiotools.sharedfiletypes.SharedFileSets._
import mybiotools.workflows.{ pcaTask, PCAParameters, PCATaskInput, SharedPCAResult }
import hdfdosage.FileSets
import hdfdosage.FileSets._
import mybiotools.stat._
import mybiotools.gwascommons.associationresults._
import mybiotools.eq._
import mybiotools.pcanew._
import akka.actor._

case class GWASEnvironmentConfig(gwasCPU: Int, numberOfChunks: Int, plotMemory: Int) extends Serializable

object GWASEnvironmentConfig {

  def apply(configInstance: Config): GWASEnvironmentConfig =
    GWASEnvironmentConfig(
      plotMemory = configInstance.getInt("plotMemory"),
      numberOfChunks = configInstance.getInt("chunks"),
      gwasCPU = configInstance.getInt("gwasCPU")
    )
}

case class GWASAppConfig(
    includePCAAxes: Int,
    genomicmap: Option[SharedFile],
    fromBP: Option[Int],
    toBP: Option[Int],
    chr: Option[Int],
    intervals: List[Region],
    keepPValueThreshold: Double,
    useOnlySignificantCovariatesPerPhenotype: Boolean,
    covarfiles: Set[SharedFile],
    covarfilemissingvalue: String,
    phenonames: List[String],
    covarnames: List[String],
    minimumMAF: Double,
    phenoscale: PhenotypeScale,
    models: List[GeneticModel],
    interactionmodels: List[InteractionModel],
    sharedGenotypeFileList: Set[SharedFileSet],
    conditionMarkers: Set[String],
    interactionMarkers: List[String],
    interactionCovariates: List[String],
    keepIndividuals: Set[Individual],
    createPlotsPriv: Boolean,
    disableCovariatePrint: Boolean,
    wholeGenomeEpistasis: Boolean,
    prefilterWithScoreTest: Boolean,
    ldPruningWindow: Int,
    ldPruningThreshold: Double,
    automaticallyDetermineLogisticPhenotype: Boolean,
    writeFullResults1: Boolean
) extends ResultWithSharedFiles(genomicmap.toList ++ covarfiles.toList ++ sharedGenotypeFileList.flatMap(_.files.toList): _*) {

  if (((chr.isDefined && toBP.isDefined && fromBP.isDefined) || !intervals.isEmpty) && !genomicmap.isDefined) {
    throw new RuntimeException("Chromosome position based filtering is not possible without a map file.")
  }
  if (phenonames.size == 0) {
    throw new RuntimeException("Please specify phenotype names with pheno-name or phenonamelist configuration keys.")
  }
  if (models.size == 0) {
    throw new RuntimeException("Please specify genetic models with model configuration key.")
  }

  if (models.size == 0) {
    throw new RuntimeException("Please specify genetic models with model configuration key.")
  }

  val createPlots = createPlotsPriv && genomicmap.isDefined
  val subsetregionset = if (chr.isDefined && toBP.isDefined && fromBP.isDefined && genomicmap.isDefined)
    Some(RegionSet[String8, GenomicLocation, Region](Region(chr.get, fromBP.get, toBP.get) :: intervals))
  else if (!intervals.isEmpty) Some(RegionSet[String8, GenomicLocation, Region](intervals))
  else None
}

object GWASAppConfig {

  def apply(configInstance: com.typesafe.config.Config)(implicit ce: TaskSystemComponents): GWASAppConfig =
    GWASAppConfig(
      includePCAAxes = configInstance.getInt("includePCAAxes"),
      useOnlySignificantCovariatesPerPhenotype = configInstance.getBoolean("useOnlySignificantCovariatesPerPhenotype"),
      wholeGenomeEpistasis = configInstance.getBoolean("wholeGenomeEpistasis"),
      writeFullResults1 = configInstance.getBoolean("writeFullResults"),
      genomicmap = configInstance.getString("map") match {
        case x if x == "" => None
        case x => Some(SharedFile(new File(x)))
      },
      prefilterWithScoreTest = configInstance.getBoolean("prefilterWithScoreTest"),
      ldPruningWindow = configInstance.getInt("ldPruningWindow"),
      ldPruningThreshold = configInstance.getDouble("ldPruningThreshold"),
      fromBP = configInstance.getInt("from-bp") match {
        case x if x < 0 => None
        case x => Some(x)
      },
      toBP = configInstance.getInt("to-bp") match {
        case x if x < 0 => None
        case x => Some(x)
      },
      chr = configInstance.getInt("chr") match {
        case x if x <= 0 => None
        case x => Some(x)
      },
      intervals = {
        val x = configInstance.getString("intervalsbed")
        if (x != "-" && x != "") openSource(x)(s => readBedInterval(s).toList) else Nil
      },
      keepPValueThreshold = configInstance.getDouble("keepPValueThreshold"),
      covarfiles = (((configInstance.getString("covarfilelist") match {
        case x if x == "" => Nil
        case x => openSource(x)(_.getLines.toList)
      }) ++ (configInstance.getString("covar").split(","))).filter(_ != "")).map(x => SharedFile(new File(x))).toSet,
      covarfilemissingvalue = configInstance.getString("covarfilemissing"),
      phenonames = (configInstance.getString("phenonamelist") match {
        case x if x == "" => Nil
        case x => scala.io.Source.fromFile(x).getLines.toList
      }) ++ (configInstance.getString("pheno-name") match {
        case x if x == "" => Nil
        case x => x.split(",").toList
      }),
      automaticallyDetermineLogisticPhenotype = configInstance.getBoolean("autoLogisticPhenotype"),
      covarnames = configInstance.getString("covar-names") match {
        case x if x == "" => Nil
        case x => x.split(",").toList
      },
      minimumMAF = configInstance.getDouble("minimumMAF"),
      phenoscale = scala.util.Try {
        configInstance.getString("phenoscale") match {
          case "linear" => Linear
          case "logistic" => Logistic
        }
      }.toOption.getOrElse { throw new RuntimeException("Please specify phenoscale configuration key. Should be linear or logistic. ") },
      models = configInstance.getString("model").split(",").toList.map(_ match {
        case "ADD" => Additive
        case "DOM" => Dominant
        case "REC" => Recessive
        case "HETADV" => HeterozygousAdvantage
        case "IMPERROR" => ImputationError
        case _ => Additive
      }).distinct,
      interactionmodels = {
        val x = configInstance.getString("interactionmodels")
        if (x == "") Nil
        else
          x.split(",").toList.map(_ match {
            case "PROD" => ProductInteraction
            case "PRODDOM" => ProductDominantInteraction
          }).distinct
      },
      sharedGenotypeFileList = hdfdosage.FileSets.parseConfig(configInstance).map(f =>
        SharedFileSets.fromFileSet(f)).toSet,
      conditionMarkers = ((configInstance.getString("condition-list") match {
        case x if x == "" => Nil
        case x => scala.io.Source.fromFile(x).getLines.toList
      }) ++ (configInstance.getString("condition") match {
        case x if x == "" => Nil
        case x => x.split(",").toList
      })).toSet,
      interactionMarkers = ((configInstance.getString("interaction") match {
        case x if x == "" => Nil
        case x => scala.io.Source.fromFile(x).getLines.toList
      }) ++ (configInstance.getString("interaction-list") match {
        case x if x == "" => Nil
        case x => x.split(",").toList
      })).toSet.toList,
      interactionCovariates = ((configInstance.getString("interactioncovariates") match {
        case x if x == "" => Nil
        case x => scala.io.Source.fromFile(x).getLines.toList
      }) ++ (configInstance.getString("interactioncovariates-list") match {
        case x if x == "" => Nil
        case x => x.split(",").toList
      })).toSet.toList,
      createPlotsPriv = configInstance.getBoolean("plot"),
      disableCovariatePrint = configInstance.getBoolean("disableCovariatePrintOnStdOut"),
      keepIndividuals = {
        val f = configInstance.getString("keepIndividuals")
        if (f == "" || f == "-") Set[Individual]()
        else openSource(f)(s => getIndividualsFromFamFile(s).toSet)
      }
    )
}

object GWASApp extends App {

  val taskSystem = mybiotools.tasks.defaultTaskSystem

  if (taskSystem.hostConfig.myRole == mybiotools.tasks.MASTER) {
    val log = taskSystem.getLogger(this)
    try {
      val config = GWASAppConfig(configInstance)(taskSystem.components)

      val env = GWASEnvironmentConfig(configInstance)

      val runner = new GWASRunner(config, env, taskSystem.components)
      val output = Try(configInstance.getString("output")).toOption.getOrElse("./logfile")
      taskSystem.registerApplicationFileLogger(new File(output + ".log"))

      runner.run
      log.info("Finished.")
    } catch {
      case e: Throwable => { log.error("Error in GWAS application. \n {}", e.toString + " " + helpers.stackTraceAsString(e)); throw e }
    } finally {
      taskSystem.shutdown
      Thread.sleep(5000)
    }
  }
}

class GWASRunner(config: GWASAppConfig, env: GWASEnvironmentConfig, taskSystem: mybiotools.tasks.TaskSystemComponents) {
  import env._
  def autoLogistic(s: Series[Individual, Double]): Series[Individual, Double] = {
    val values = s.dropNA.toVec.toSeq.distinct
    assert(values.size <= 2, s"Series supposed to have two states. Values: $values, $s ")
    val yesvalue = values.sorted.last
    val novalue = values.sorted.head
    s.mapValues(d => if (d == yesvalue) 2.0 else 1.0)
  }

  def run: PlinkAssocFile = {

    import config._
    implicit val components = taskSystem
    val log = createLogger(this)

    val genomicmapMemory = NodeLocalCache.getItemBlocking("genomicmap" + genomicmap.map(_.name)) {
      genomicmap.map(x => getGenomicMapFromBimFile(x.file.getCanonicalPath)).getOrElse(collection.Map[String8, GenomicLocation]())
    }(taskSystem.nodeLocalCache)

    val genotypefilelist: Seq[GenotypeFileSet] = {
      val f = sharedGenotypeFileList.map(x => x -> x.toFileSet).toList
      val (nonempty, empty) = f.partition(f => openFileSet(f._2)(_.snpIterator.hasNext))
      if (empty.size > 0) {
        log.warning(s"The following files are empty: ${empty.map(_._1)}")
      }
      nonempty.map(_._2).sortBy(f => openFileSet(f)(_.toLocusIteratorWithGenomicMap(genomicmapMemory).loci.next._1.genomicLocation.getOrElse(GenomicLocation(0, "0"))))
    }

    val filesubsets = genotypefilelist.map { f =>
      f match {
        case HDFDosage(file) => {
          if (numberOfChunks == 1) f -> List(Full)
          else {
            val reader = HDF5Factory.openForReading(file)
            val n = try { hdfdosage.HDFDosageFile.getNumberOfSNPs(reader) } finally { reader.close }

            val chunksize = n / numberOfChunks

            f -> (for (i <- 0 to n by chunksize) yield FileSubSet(fromIdx = i, toIdx = (if (i + chunksize <= n) i + chunksize else n))).filter(x => x.toIdx - x.fromIdx > 0)
          }
        }
        case BedFile(bed, bim, fam) => {
          if (numberOfChunks == 1) f -> List(Full)
          else {
            val n = openSource(bim.getCanonicalPath)(_.getLines.size)

            val chunksize = n / numberOfChunks

            f -> (for (i <- 0 to n by chunksize) yield FileSubSet(fromIdx = i, toIdx = (if (i + chunksize <= n) i + chunksize else n))).filter(x => x.toIdx - x.fromIdx > 0)
          }
        }
        case _ => f -> List(Full)
      }
    }.toMap

    log.info("Badge:\n" + (mybiotools.config.Config.prettyPrintVersion("GWASApp", gwasapp.Reflected.version)))
    log.info(s"Genotype files: ${sharedGenotypeFileList.mkString("\n")}")
    log.info(s"Genotype files: ${genotypefilelist.mkString("\n")}")
    log.info(s"Covariate files: $covarfiles")
    log.info(s"Files are processed in $numberOfChunks chunks.")
    log.info(s"Map file: $genomicmap")
    log.info(s"Covariate names: $covarnames")
    log.info(s"Phenotype scale: $phenoscale")
    log.info(s"Genetic mode: $models")
    log.info(s"Condition on SNPs: $conditionMarkers")
    log.info(s"Minimum MAF: $minimumMAF")
    log.info(s"Aggregate p-value threshold: $keepPValueThreshold")
    log.info(s"Subset to regions (None means no filter): $subsetregionset")
    log.info(s"Create plot: $createPlots")
    log.info(s"Interaction markers or covariates: ${interactionMarkers ++ interactionCovariates}")
    log.info(s"Interaction model: $interactionmodels")
    log.info(s"Full pairwise epistasis screen: $wholeGenomeEpistasis")
    log.info(s"LD pruning (threshold/window): $ldPruningThreshold/$ldPruningWindow")
    log.info(s"Write full results: $writeFullResults1")
    log.info(s"Prefilter with score test: $prefilterWithScoreTest")

    val writeFullResults = !wholeGenomeEpistasis && writeFullResults1

    if (ldPruningThreshold != 1.0) {
      log.warning(s"LD pruning requires a chromosome ordered genotype file. This is NOT verified!")
    }

    val individualsInGenotypes: Seq[Individual] = FileSets.getIndividuals(genotypefilelist.head)

    genotypefilelist.foreach { f =>
      assert(FileSets.getIndividuals(f) == individualsInGenotypes, "different individuals found in different genotype files")
    }

    log.info(s"# Individuals in genotype file: ${individualsInGenotypes.size}")

    log.info(s"Genomic map in memory: ${genomicmapMemory.size} lines")

    val covariatesFromCovariateFiles = Frame({
      covarfiles.map(f => useResource(scala.io.Source.fromFile(f.file))(source => readPlinkCovarFile(source, covarfilemissingvalue)))
    }.reduce(_ rconcat _).filterIx((str: String) => (phenonames ++ covarnames ++ interactionCovariates).contains(str)).toColSeq.map {
      case (colname, col) =>
        if (phenonames.contains(colname) && automaticallyDetermineLogisticPhenotype && phenoscale == Logistic) {
          (colname, autoLogistic(col))
        } else (colname, col)
    }: _*)

    covarnames.foreach { x =>
      if (!covariatesFromCovariateFiles.colIx.contains(x)) {
        log.error(s"covariate $x not found.")
        throw new RuntimeException(s"covariate $x not found.")
      }
    }

    val phenonamesAvailable = phenonames.filter(x => covariatesFromCovariateFiles.colIx.contains(x))
    phenonames.filterNot(x => covariatesFromCovariateFiles.colIx.contains(x)).foreach { x =>
      log.warning(s"Phenotype listed but not found: $x !")
    }
    log.info(s"Phenotypes: $phenonamesAvailable")

    val listOfFutureAssocFiles: Seq[Future[Seq[SharedAssocFile]]] =
      models.flatMap { model =>
        val covariatesShort: Frame[Individual, String, Double] = {

          log.info(s"Read ${covariatesFromCovariateFiles.numCols} covariates and/or phenotypes for ${covariatesFromCovariateFiles.numRows} individuals.")

          if ((covariatesFromCovariateFiles.colIx.toSeq.toSet & phenonames.toSet).isEmpty) {
            log.error("Phenotypes not found. Exiting.")
            throw new RuntimeException("Phenotypes not found. Exiting.")
          }

          val conditionSNPs: Frame[Individual, String, Double] = if ((conditionMarkers ++ interactionMarkers).size > 0) {
            genotypefilelist.map { file =>
              FileSets.openFileSet(file, minimumMAF, Full, (conditionMarkers ++ interactionMarkers)) { snpmajoriterator =>
                val locusiter = snpmajoriterator.toLocusIteratorWithGenomicMap(genomicmapMemory)

                val inds: Seq[Individual] = locusiter.individuals
                val indIdx = Index(inds: _*)
                val snps: Seq[(String, Series[Individual, Double])] = locusiter.loci.map {
                  case (ld, geno) =>
                    ld.name.value -> recodeGenotypesDropNA(indIdx, geno, if (conditionMarkers.contains(ld.name.value)) model else Additive)
                }.toSeq
                Frame(snps: _*)
              }
            }.reduce(_ rconcat _)
          } else Frame[Individual, String, Double]()

          val pca: Frame[Individual, String, Double] = if (includePCAAxes > 0 && genomicmap.isDefined) {
            log.info(s"Calculating $includePCAAxes PCA axes. Minimum MAF: 0.05, pruning with threshold 0.5. Using the last 1 million bp as window.")

            val pcaresult: PCAResult[Individual] = Await.result(
              pcaTask(PCATaskInput(
                Some(PCAParameters(
                  ldPruningThreshold = 0.5,
                  ldPruningWindowInBP = 1000000,
                  numberOfAxes = includePCAAxes
                )),
                Some(sharedGenotypeFileList),
                genomicmap
              ), gwasCPU, 5000).?[SharedPCAResult[Individual]].map(x => x.pcaresult),
              Duration.Inf
            )

            val frame = pcaresult.evecToFrame.mapColIndex(i => "__hostpca__" + i)

            SharedFile(
              writeToTempFile(pcaresult.eigenValues.mkString("\n")),
              name = "pca.evals.txt",
              canMoveAway = true
            )

            val plots = mybiotools.plots.ScatterPlot.createScatterPlotsFromFrame(frame).zipWithIndex.map(
              x =>
                SharedFile(
                  mybiotools.plots.pdfToFile(x._1),
                  name = s"pcaplots_${x._2}.pdf",
                  canMoveAway = true
                )
            )

            frame

          } else {
            if (includePCAAxes > 0 && genomicmap.isEmpty) {
              log.warning("PCA requested but no genomic map provided. Skip PCA.")
            }
            Frame[Individual, String, Double]()
          }

          covariatesFromCovariateFiles rconcat conditionSNPs rconcat pca
        }.rfilter(x => !x.filterIx(x => (covarnames ++ conditionMarkers).contains(x)).hasNA).rfilterIx((x: Individual) => individualsInGenotypes.contains(x) && (keepIndividuals.isEmpty || keepIndividuals.contains(x))).squeeze

        val pcaCovarNames = 0 until includePCAAxes map (i => "__hostpca__" + i)

        log.info(s"Regressions of phenotypes against covariates (${covarnames ++ conditionMarkers ++ pcaCovarNames}).")
        val significantCovariatesPerPhenotype: List[(String, List[String])] = phenonamesAvailable.map {
          case (phenoname) =>
            val data = covariatesShort.firstCol(phenoname)

            val phenotypehistogram = SharedFile(
              mybiotools.plots.pdfToFile(
                mybiotools.plots.HistogramPlot.createHistogramPlot(data.toVec.toSeq, main = phenoname)
              ),
              name = "histogram." + phenoname + ".pdf",
              canMoveAway = true
            )

            val (regressionTables, significantCovariates) = ((covarnames ++ conditionMarkers ++ pcaCovarNames).map { testingCov =>
              val reg: Try[RegressionResultOrFailure] = mybiotools.stat.Regression.regression(
                covariates = covariatesShort,
                covariateNames = List(testingCov),
                phenoscale = phenoscale,
                phenoName = phenoname
              )

              val string = reg.map(_ match {
                case x: RegressionResult => x.table(phenoname)
                case y => y
              }).toString + "\n"

              val result = reg match {
                case Failure(e) => { log.error(s"Phenotype vs covariate regression failed. $phenoname"); throw e; Nil }
                case Success(x) => {
                  x match {
                    case x: RegressionResult => x.covariates.filter(_._2._2.pValue < 0.05).toList.map(_._1)
                    case y => { log.error("Phenotype vs covariate regression failed. " + phenoname + " " + y); throw new RuntimeException(("Phenotype vs covariate regression failed. " + phenoname + " " + y)); Nil }
                  }
                }
              }
              (string, result)
            }).unzip

            SharedFile(
              writeToTempFile(regressionTables.mkString("\n")),
              name = "pheno.vs.covar.txt",
              canMoveAway = true
            )

            log.info(s"Significant covariates for phenotype $phenoname: ${significantCovariates.flatten}. Using only these: $useOnlySignificantCovariatesPerPhenotype")

            (phenoname, significantCovariates.flatten)

        }

        val individualsInCovariates: Set[Individual] = covariatesShort.rowIx.toSeq.toSet

        log.info(s"Dropped ${(individualsInGenotypes.toSet &~ individualsInCovariates).size} individuals because of missingness in covariates or conditioning snps, or missing from all phenotypes, or missing from keepIndividuals: " + (individualsInGenotypes.toSet &~ individualsInCovariates))

        // log.info(s"Non missing counts in covariates and phenotypes:" + covariatesShort.toColSeq.map(x => (x._1, x._2.dropNA.length)).toSeq)

        if (!((covarnames ++ interactionCovariates).toSet &~ covariatesShort.colIx.toSeq.toSet).isEmpty) {
          log.warning("Extracted covariates does not include all interaction terms or covariates.\n" + ((covarnames ++ interactionCovariates).toSet &~ covariatesShort.colIx.toSeq.toSet).toString)
        }
        if (((interactionMarkers ++ interactionCovariates).toSet & covariatesShort.colIx.toSeq.toSet).isEmpty && (interactionMarkers ++ interactionCovariates).toSet.size > 0) {
          log.error("No interaction term found. Exiting.\n" + covariatesShort.toString)
          throw new RuntimeException("No interaction term found. Exiting.")
        }

        val covariatesIter =
          if (wholeGenomeEpistasis) {
            genotypefilelist.iterator.flatMap { file =>
              val (snpmajoriterator, closable) = FileSets.getIteratorFromFileSet(file, minimumMAF, 1.0, Full, Set())
              val locusiter =
                (if (genomicmap.isDefined && ldPruningThreshold < 1.0) {
                  snpmajoriterator.prune(ldPruningThreshold, ldPruningWindow, genomicmapMemory)
                } else snpmajoriterator).toLocusIteratorWithGenomicMap(genomicmapMemory)

              val inds: Seq[Individual] = locusiter.individuals
              val indIdx = Index(inds: _*)
              locusiter.loci.grouped(1000).map { group =>
                val recoded = group.map {
                  case (ld, geno) =>
                    ld.name.value -> recodeGenotypesDropNA(indIdx, geno, Additive)
                }.toSeq
                val frame = Frame(recoded: _*).rfilterIx((x: Individual) => individualsInCovariates.contains(x)).squeeze
                if (frame.numCols > 0)
                  Some((frame rconcat covariatesShort, recoded.map(_._1)))
                else None

              }.filter(_.isDefined).map(_.get)
            }
          } else List((covariatesShort, Nil)).iterator

        covariatesIter.zipWithIndex.flatMap {
          case ((covariates, wholeGenomeEpistasisNames), covIdx) =>
            if (!disableCovariatePrint && !wholeGenomeEpistasis) {
              log.info(s"Extract of covariates for $model model (after removing any rows with NA (in condition and covariate columns) and any columns with 0 variance): \n $covariates")
            }

            if (covariates.numRows <= 1) {
              log.info(s"Empty covariate table. Skipping $model.")
              List()
            } else {

              if (!wholeGenomeEpistasis) {
                covariates.colIx.toSeq.toList
                  .filter(x => (covarnames ++ conditionMarkers ++ pcaCovarNames).contains(x))
                  .combinations(2).foreach {
                    case c1 :: c2 :: Nil =>
                      val a1 = covariates.firstCol(c1).toSeq.sortBy(_._1).map(_._2).toArray
                      val a2 = covariates.firstCol(c2).toSeq.sortBy(_._1).map(_._2).toArray
                      val r2 = math.pow(new org.apache.commons.math3.stat.correlation.PearsonsCorrelation().correlation(a1, a2), 2)
                      if (r2 > 0.4) {
                        log.warning(s"High correlation between covariates: $c1 vs $c2 , r2=$r2")
                      }
                    case _ => {}
                  }
              }

              // val covarenvelope = frameToEnvelope(covariates)
              val sharedcovar = SharedFile(
                mybiotools.writeToTempFile(
                  FrameToPlink.frameToPlink(covariates, "-9")
                ), name = "sharedcovariates", canMoveAway = true
              )

              val expectedassocfiles = genotypefilelist.map(f => filesubsets(f).size).sum

              val plotters = if (createPlots) {

                def makePlotter(name: Option[String], phenotype: List[Option[String]], maxp: Double) = {

                  val input = tasks.PlotTaskInput(expectedassocfiles, maxp, phenotype, name)
                  tasks.plotGWASInternal(input, memory = plotMemory)
                }

                makePlotter(Some("lowpval"), List(None), 1E-5) ::
                  phenonamesAvailable.grouped(10).toList.map { phn =>
                    makePlotter(None, phn.map(x => Some(x)), keepPValueThreshold)
                  }

              } else Nil

              val future: Future[Seq[SharedAssocFile]] = Future.sequence(genotypefilelist.flatMap { file =>

                filesubsets(file).map { filesubset =>

                  val sharedfile = SharedFileSets.fromFileSet(file)

                  val sharedmap = genomicmap

                  val gwas = GWASTask.startNew(gwasCPU)

                  val interactionNames = if (!wholeGenomeEpistasis) InteractionNames(((interactionMarkers ++ interactionCovariates).toSet & covariates.colIx.toSeq.toSet).toSeq) else InteractionNames(wholeGenomeEpistasisNames)

                  val phenotypeInfo = if (useOnlySignificantCovariatesPerPhenotype) significantCovariatesPerPhenotype.map(x => (x._1, phenoscale, x._2.toSet)).toSet else {
                    val covnames = ((covarnames ++ conditionMarkers ++ pcaCovarNames).toSet & covariates.colIx.toSeq.toSet).toList
                    phenonamesAvailable.map(x => (x, phenoscale, covnames.toSet)).toSet
                  }

                  ProxyTask.sendStartDataWithRetry(
                    gwas,
                    sharedfile ::
                      CovarData(sharedcovar) ::
                      interactionNames ::
                      InteractionModels(interactionmodels) ::
                      PhenotypeInfos(phenotypeInfo) ::
                      MaybeBIMFile(sharedmap) ::
                      MinimumMAF(minimumMAF) ::
                      SubsetRegion(subsetregionset) ::
                      Model(model) ::
                      KeepPValueThreshold(keepPValueThreshold) ::
                      FileSubsetWrapper(filesubset) ::
                      PreFilterWithScoreTest(prefilterWithScoreTest) ::
                      WholeGenomeEpistasisScreen(wholeGenomeEpistasis) ::
                      WriteFullResults(writeFullResults) ::
                      LDPruningParameters(ldPruningThreshold, ldPruningWindow) ::
                      OutNamePart(if (wholeGenomeEpistasis) s"epistasisbatch$covIdx" else "gwas") ::
                      Nil
                  )

                  plotters.foreach { p =>
                    ProxyTaskActorRef(gwas) ~> p
                  }

                  ProxyTask.getBackResultFuture(gwas).asInstanceOf[Future[SharedAssocFile]]

                }
              })

              val futurePlotters = Future.sequence(plotters.map(_.?[Plots]))

              List(future.flatMap { f =>
                futurePlotters.map { f2 =>
                  f
                }
              })

            }
        }
      }

    val listOfAssocFiles: Seq[SharedAssocFile] = Await.result(
      Future.sequence(listOfFutureAssocFiles).map(_.flatten),
      168 hours
    )

    val copytoplinktaskFull = {
      val interactionheader = if ((interactionMarkers ++ interactionCovariates).size > 0 && interactionmodels.size > 0 || wholeGenomeEpistasis) true else false
      val input = tasks.CopyToPlinkInput(listOfAssocFiles.size, "assoc.full.gz", interactionheader, listOfAssocFiles, true)
      tasks.copyToPlink(input).?[PlinkAssocFile]
    }

    val copytoplinktaskSnpOnly = {
      val interactionheader = if ((interactionMarkers ++ interactionCovariates).size > 0 && interactionmodels.size > 0 || wholeGenomeEpistasis) true else false
      val input = tasks.CopyToPlinkInput(listOfAssocFiles.size, "assoc.gz", interactionheader, listOfAssocFiles, false)
      tasks.copyToPlink(input).?[PlinkAssocFile]
    }

    val files: Seq[File] = listOfAssocFiles.map(_.snponly.file)
    val testsDone = listOfAssocFiles.map(_.testsDone).sum
    SharedFile(writeToTempFile(testsDone.toString), name = "testDone", canMoveAway = true)
    log.info("Total number of tests performed: " + testsDone)
    log.info(s"Copying association results to a plink file.")
    val list = Await.result(Future.sequence(List(copytoplinktaskSnpOnly, copytoplinktaskFull)), 168 hours)
    log.info("Full result file: " + list(1).file.file.getAbsolutePath)
    log.info("SNP result file: " + list(0).file.file.getAbsolutePath)

    Await.result(copytoplinktaskSnpOnly, 168 hours)

  }

}