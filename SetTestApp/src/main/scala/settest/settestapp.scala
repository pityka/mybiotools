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

package settest

import mybiotools.config.Config.configInstance
import hdfdosage.FileSets._
import mybiotools.gwascommons._
import mybiotools.gwascommons.genotypedata._
import mybiotools._
import mybiotools.stringstore._
import scala.collection.JavaConversions._
import org.saddle._
import mybiotools.stat.LinearRegression.readPlinkCovarFile
import scala.util._
import mybiotools.eq._
import java.io.File
import mybiotools.tasks._
import mybiotools.sharedfiletypes._
import mybiotools.sharedfiletypes.SharedFileSets._
import mybiotools.workflows.{ pcaTask, PCAParameters, PCATaskInput, SharedPCAResult }
import mybiotools.pcanew._
import scala.concurrent.Await
import scala.concurrent.duration._

sealed trait MaskRegionFile {
  def toList: List[SharedFile] = Nil
}
case object Empty extends MaskRegionFile
case object Exome extends MaskRegionFile
case class MaskFile(f: SharedFile) extends MaskRegionFile {
  override def toList = List(f)
}

case class SetTestEnvironmentConfig(
  threads: Int
)

object SetTestEnvironmentConfig {
  def apply(c: com.typesafe.config.Config): SetTestEnvironmentConfig = SetTestEnvironmentConfig(c.getInt("threads"))
}

case class SetTestConfig(
  includePCAAxes: Int,
  assemblePathwayAndQuit: Boolean,
  sharedgenotypefile: Set[SharedFileSet],
  genomicMapFile: SharedFile,
  maxMaf: Double,
  minMaf: Double,
  gmtfile: Option[SharedFile],
  excludeMaskRegionsFile: MaskRegionFile,
  includeMaskRegionsFile: MaskRegionFile,
  conditionNamesFile: Option[SharedFile],
  conditionMarkerNamesFile: Option[SharedFile],
  conditionMarkerNamesList: Set[String],
  excludeNamesFile: Option[SharedFile],
  includeOnlyNamesFile: Option[SharedFile],
  bedregionfile: Option[SharedFile],
  covariateFiles: Set[SharedFile],
  covarnames: Set[String],
  covarfilemissingvalue: String,
  phenotypeInfo: Map[String, PhenotypeScale],
  flankingBefore: Int,
  flankingAfter: Int,
  thurmansfile: Option[SharedFile],
  natarajanAssociationsFile: Option[SharedFile],
  natarajanBedFile: Option[SharedFile],
  natarajanGeneIDFile: Option[SharedFile],
  includeCodingBesideRegulators: Boolean,
  minimumShare: Int,
  preComputedTracksFile: Option[SharedFile],
  locationListMultiLine: Option[SharedFile],
  windowSize: Int,
  windowOverlap: Int,
  skatMethod: String,
  skatShape1: Double,
  skatShape2: Double,
  testStrings: List[String],
  keepIndividuals: Set[Individual],
  regiontypes: Seq[String]
)
    extends ResultWithSharedFiles(genomicMapFile :: (locationListMultiLine.toList ++
      preComputedTracksFile.toList ++
      natarajanGeneIDFile.toList ++
      natarajanBedFile.toList ++
      natarajanAssociationsFile.toList ++
      thurmansfile.toList ++
      covariateFiles.toList ++
      bedregionfile.toList ++
      includeOnlyNamesFile.toList ++
      excludeNamesFile.toList ++
      conditionMarkerNamesFile.toList ++
      conditionNamesFile.toList ++
      includeMaskRegionsFile.toList ++
      excludeMaskRegionsFile.toList ++
      gmtfile ++
      sharedgenotypefile.flatMap(_.files)): _*) with Serializable {
  def phenonames = phenotypeInfo.map(_._1)

  def selectedTests = testStrings.toList.map {
    case "SKAT" => tests.SkatTest(
      skatTestType = tests.Skat.SKATStandard,
      skatMethod = skatMethod,
      weights = skatShape1 -> skatShape2
    )
    case "SKATC" => tests.SkatTest(
      skatTestType = tests.Skat.SKATC,
      skatMethod = skatMethod,
      weights = skatShape1 -> skatShape2
    )
    case "BURDEN" => tests.BurdenTest //BURDEN
    case "GLOBALTEST" => {
      tests.GlobalTest(mybiotools.stat.GlobalTest.BetaPDFAtHalfMean(skatShape1, skatShape2))
    }
  } toSet
}

object SetTestConfig {

  def apply(configInstance: com.typesafe.config.Config)(implicit ce: TaskSystemComponents): SetTestConfig = SetTestConfig(
    includePCAAxes = configInstance.getInt("includePCAAxes"),
    regiontypes = configInstance.getStringList("regiontype").toList,
    genomicMapFile = SharedFile(new File(configInstance.getString("map"))),
    sharedgenotypefile = hdfdosage.FileSets.parseConfig(configInstance).map(x => SharedFileSets.fromFileSet(x)).toSet,
    maxMaf = configInstance.getDouble("maxMaf"),
    minMaf = configInstance.getDouble("minMaf"),
    gmtfile = {
      val s = configInstance.getString("gmt")
      if (s === "") None else Some(SharedFile(new File(s)))
    },
    conditionNamesFile = {
      val s = configInstance.getString("conditionNamesFile")
      if (s === "") None else Some(SharedFile(new File(s)))
    },
    conditionMarkerNamesFile = {
      val s = configInstance.getString("conditionMarkerNamesFile")
      if (s === "") None else Some(SharedFile(new File(s)))
    },
    conditionMarkerNamesList = configInstance.getStringList("conditionMarkerNames").toList.toSet,
    excludeNamesFile = {
      val s = configInstance.getString("excludeNamesFile")
      if (s === "") None else Some(SharedFile(new File(s)))
    },
    includeOnlyNamesFile = {
      val s = configInstance.getString("includeOnlyNamesFile")
      if (s === "") None else Some(SharedFile(new File(s)))
    },
    bedregionfile = {
      val s = configInstance.getString("bedregionfile")
      if (s === "") None else Some(SharedFile(new File(s)))
    },
    covariateFiles = configInstance.getStringList("covariateFiles").toList.map(x => SharedFile(new File(x))).toSet,
    covarnames = (configInstance.getStringList("covarnames")).toSet,
    covarfilemissingvalue = configInstance.getString("covarfilemissingvalue"),
    phenotypeInfo =
      configInstance.getStringList("phenotype").toList.grouped(2).map {
        case name :: "linear" :: Nil => ((name), Linear)
        case name :: "logistic" :: Nil => ((name), Logistic)
        case x => throw new RuntimeException("""phenotype configuration key should list phenotype names and their scale e.g. ["VL","linear","controller","logistic"] """ + x)
      }.toList.toMap,
    excludeMaskRegionsFile = {
      val s = configInstance.getString("excludeMaskRegionsFile")
      if (s === "EMPTY") Empty else MaskFile(SharedFile(new File(s)))
    },
    includeMaskRegionsFile = {
      val s = configInstance.getString("includeMaskRegionsFile")
      if (s === "EVERYTHING") Empty else if (s === "EXOME") Exome else MaskFile(SharedFile(new File(s)))
    },
    thurmansfile = {
      val s = configInstance.getString("thurmansFile")
      if (s === "") None else Some(SharedFile(new File(s)))
    },
    natarajanAssociationsFile = {
      val s = configInstance.getString("natarajanAssociationsFile")
      if (s === "") None else Some(SharedFile(new File(s)))

    },
    natarajanBedFile = {
      val s = configInstance.getString("natarajanBedFile")
      if (s === "") None else Some(SharedFile(new File(s)))
    },
    natarajanGeneIDFile = {
      val s = configInstance.getString("natarajanGeneIDFile")
      if (s === "") None else Some(SharedFile(new File(s)))

    },
    includeCodingBesideRegulators = configInstance.getBoolean("includeCodingBesideRegulators"),
    minimumShare = configInstance.getInt("minimumSharedRegulators"),
    preComputedTracksFile = {
      val s = configInstance.getString("preComputedTracksFile")
      if (s === "") None else Some(SharedFile(new File(s)))
    },
    locationListMultiLine = {
      val s = configInstance.getString("locationListMultiLine")
      if (s === "") None else Some(SharedFile(new File(s)))
    },
    testStrings = configInstance.getStringList("tests").toList,
    windowSize = configInstance.getInt("windowSize"),
    windowOverlap = configInstance.getInt("windowOverlap"),
    flankingBefore = configInstance.getInt("flankingBefore"),
    flankingAfter = configInstance.getInt("flankingAfter"),
    assemblePathwayAndQuit = configInstance.getBoolean("assemblePathwayAndQuit"),
    skatMethod = configInstance.getString("skatMethod"),
    skatShape1 = configInstance.getDouble("skatShape1"),
    skatShape2 = configInstance.getDouble("skatShape2"),
    keepIndividuals = {
      val f = configInstance.getString("keepIndividuals")
      if (f === "" || f === "-") Set[Individual]()
      else openSource(f)(s => getIndividualsFromFamFile(s).toSet)
    }
  )

}

object SetTestApp extends App {

  val taskSystem = mybiotools.tasks.defaultTaskSystem

  if (taskSystem.hostConfig.myRole == mybiotools.tasks.MASTER) {
    val log = taskSystem.getLogger(this)

    try {
      val config = SetTestConfig(configInstance.getConfig("tracks"))(taskSystem.components)
      val env = SetTestEnvironmentConfig(configInstance.getConfig("tracks"))

      val runner = new SetTestRunner(config, env, taskSystem.components)
      runner.run
      log.info("Finished.")
    } catch {
      case e: Throwable => { log.error("Error in SetTest application. \n {}", e.toString + " " + helpers.stackTraceAsString(e)); throw e }
    } finally {
      taskSystem.shutdown
      Thread.sleep(5000)
    }
  }

}

case class SetTestResultFiles(details: Map[(String, Test), (SharedFile, SharedFile)], tests: Option[SharedFile], listOfAllIncludedLoci: SharedFile, assembledRegionsBed: SharedFile, assembledRegionsBedWithLoci: SharedFile) extends ResultWithSharedFiles(listOfAllIncludedLoci :: assembledRegionsBed :: assembledRegionsBedWithLoci :: tests.toList ::: details.flatMap(x => x._2._1 :: x._2._2 :: Nil).toList: _*)

class SetTestRunner(config: SetTestConfig, env: SetTestEnvironmentConfig, taskSystem: TaskSystemComponents) {

  import config._
  import env._
  implicit val components = taskSystem
  val log = createLogger(this)

  val genomicMap = NodeLocalCache.getItemBlocking("genomicmap" + Some(genomicMapFile.name)) {
    getGenomicMapFromBimFile(genomicMapFile.file.getCanonicalPath)
  }

  def autoLogistic(s: Series[Individual, Double]): Series[Individual, Double] = {
    val values = s.dropNA.toVec.toSeq.distinct
    assert(values.size <= 2, s"Series supposed to have two states. Values: $values, $s ")
    val yesvalue = values.sorted.last
    val novalue = values.sorted.head
    s.mapValues(d => if (d === yesvalue) 2.0 else 1.0)
  }

  def verifyCovars(covardata: Frame[Individual, String, Double], covarnames: Iterable[String], phenonames: List[String]) {
    catchToLeft {

      phenonames.foreach { phenoname =>

        assert(covardata.colIx.toSeq contains phenoname, "missing from covarfile: " + phenoname)
      }

      covarnames.foreach { cname =>

        assert(covardata.colIx.toSeq contains cname, "missing from covarfile: " + cname)

      }
    }.left.map { e =>
      throw new RuntimeException(s"Incomplete covariate file. " + e)
    }
  }

  def run: SetTestResultFiles = {

    log.info(mybiotools.config.Config.prettyPrint(Some("tracks")))
    log.info("Size of genomic map: " + genomicMap.size)

    implicit val ec = mybiotools.concurrent.newExecutionContext("SetTestApp", 5)

    val genotypefile = sharedgenotypefile.map(_.toFileSet).toList

    val pcaNames = 0 until includePCAAxes map (i => "__hostpca__" + i)

    val covariatesFromCovariateFiles = {
      val covariatesFromCovariateFiles = Frame({
        covariateFiles.map(f => useResource(scala.io.Source.fromFile(f.file))(source => readPlinkCovarFile(source, covarfilemissingvalue)))
      }.reduce(_ rconcat _).filterIx((str: String) => (phenonames ++ covarnames).contains(str)).toColSeq.map {
        case (colname, col) =>
          if (phenonames.contains(colname) && phenotypeInfo(colname) === Logistic) {
            (colname, autoLogistic(col))
          } else (colname, col)
      }: _*)

      verifyCovars(covariatesFromCovariateFiles, covarnames, phenotypeInfo.map(_._1).toList)

      val pca: Frame[Individual, String, Double] = if (includePCAAxes > 0) {
        log.info(s"Calculating $includePCAAxes PCA axes. Minimum MAF: 0.05, pruning with threshold 0.5. Using the last 1 million bp as window.")

        val pcaresult: PCAResult[Individual] = Await.result(
          pcaTask(PCATaskInput(
            Some(PCAParameters(
              ldPruningThreshold = 0.5,
              ldPruningWindowInBP = 1000000,
              numberOfAxes = includePCAAxes
            )),
            Some(sharedgenotypefile),
            Some(genomicMapFile)
          ), 4, 5000).?[SharedPCAResult[Individual]].map(x => x.pcaresult),
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
        Frame[Individual, String, Double]()
      }

      covariatesFromCovariateFiles rconcat pca

    }

    val trackStrategy: TrackStrategy = {

      val excludeMask: GenericRegionSet = excludeMaskRegionsFile match {
        case MaskFile(f) => openSource(f.file)(s => RegionSet.fromBedLines(s.getLines))
        case _ => EmptyRegionSet
      }

      val includeMask: GenericRegionSet = includeMaskRegionsFile match {
        case Empty => EverythingRegionSet
        case Exome => using(scala.io.Source.fromURL(getClass.getResource("/grch37.ensemb.exons.bed"))) { s => RegionSet.fromBedLines(s.getLines) }
        case MaskFile(f) => openSource(f.file)(s => RegionSet.fromBedLines(s.getLines))
      }

      {
        val strategies: Seq[TrackStrategy] = regiontypes.map {
          case "gmt" => openSource(gmtfile.get.file)(s => UseGMT(s, flankingBefore, flankingAfter))
          case "humangenes" => UseHumanGenes(flankingBefore, flankingAfter)
          case "bed" => openSource(bedregionfile.get.file)(s => UseBed(s, flankingBefore, flankingAfter))
          case "bedmergenames" => openSource(bedregionfile.get.file)(s => UseBedMergeNames(s, flankingBefore, flankingAfter))
          case "thurmangeneregulators" => openSource(thurmansfile.get.file)(s => ThurmanGeneRegulators(s, includeCodingBesideRegulators))
          case "thurmandhsbasedpathways" => openSource(thurmansfile.get.file)(s => ThurmanDHSBasedPathways(s))
          case "thurmanWithGMT" => openSource(gmtfile.get.file)(s2 =>
            openSource(thurmansfile.get.file)(s1 =>
              ThurmanSharedRegulatorsByGMT(
                thurmansFile = s1,
                gmt = s2,
                minimumShare,
                includeCodingBesideRegulators
              )))
          case "natarajangeneregulators" => openSource(natarajanGeneIDFile.get.file)(s3 => openSource(natarajanBedFile.get.file)(s2 => openSource(natarajanAssociationsFile.get.file)(s1 => NatarajansExtendedGene(
            associations = s1,
            bed = s2,
            geneIDs = s3,
            includeCoding = includeCodingBesideRegulators
          ))))
          case "locationlist" => openSource(preComputedTracksFile.get.file)(s => LocationList(locations = s))
          case "locationlistmultiline" => openSource(locationListMultiLine.get.file)(s => LocationListMultiLine(locations = s))
          case "slidingwindow" => SlidingWindowMapDistance(windowSize, windowOverlap)
          case "slidingbed" => openSource(bedregionfile.get.file)(s => SlidingWindowBed(s, flankingBefore, flankingAfter, windowSize, windowOverlap))
        }
        strategies.reduce(_ + _)
      }.intersect(includeMask).remove(excludeMask)
    }

    val conditionPathwaysAsInConfig: Set[RegionSetName] =
      if (conditionNamesFile.isEmpty) Set()
      else (openSource(
        conditionNamesFile.get.file
      )(_.getLines.map(x => RegionSetName(StringStore(x))).toSet)).toSet

    val excludePathwaysAsInConfig: Set[RegionSetName] =
      if (excludeNamesFile.isEmpty) Set()
      else (openSource(excludeNamesFile.get.file)(_.getLines.map(x => RegionSetName(StringStore(x))).toSet)).toSet

    val includeOnlyPathwaysAsInConfig: Option[Set[RegionSetName]] =
      if (includeOnlyNamesFile.isEmpty) None
      else Some((openSource(includeOnlyNamesFile.get.file)(_.getLines.map(x => RegionSetName(StringStore(x))).toSet)).toSet)

    val regionsets: Map[RegionSetName, GenericRegionSet] = trackStrategy.regions.filterKeys(k => !excludePathwaysAsInConfig.contains(k)).filterKeys(k => includeOnlyPathwaysAsInConfig.map(_.contains(k)).getOrElse(true))

    val outputBedFile = TempFile.createTempFile(".bed")

    openFileWriter(outputBedFile) { writer =>
      regionsets.foreach {
        case (pathway, set) =>
          writer.write((set.toRegions.toSeq.sorted(genericRegionOrderingByStart).map { r =>
            s"${r.toLine}\t${pathway.value}\n"
          }).mkString(""))
      }
    }

    val conditionPathways: Set[RegionSetName] = regionsets.keySet & conditionPathwaysAsInConfig

    val excludePathways: Set[RegionSetName] = regionsets.keySet & excludePathwaysAsInConfig

    val lociByPathway: Map[RegionSetName, Vector[Locus]] = {

      val lociMafFiltered: Vector[Locus] = openFileSets(genotypefile, minMaf, maxMaf, Full, Set()) { snpmajoriterator =>
        snpmajoriterator.toLocusIteratorWithGenomicMap(genomicMap).loci.map(_._1.toLocus).filter(_.isDefined).map(_.get).toVector
      }

      log.info("Total loci passing MAF filters in genotype file: " + lociMafFiltered.size)

      buildPathWays(
        lociMafFiltered,
        regionsets
      )
    }

    log.info("Total number of pathways: " + lociByPathway.size)
    log.info("Total loci in pathways: " + lociByPathway.flatMap(_._2).toSet.size)

    val conditionMarkerNames: Set[String] = {

      val listedInConfig = (if (conditionMarkerNamesFile.isEmpty) Set()
      else openSource(conditionMarkerNamesFile.get.file)(_.getLines.toSet)) ++ conditionMarkerNamesList.toSet

      listedInConfig ++ conditionPathways.flatMap(s => lociByPathway.get(s).map(_.map(_.name.value).toSet).getOrElse(Set()))

    }

    val individualsInGenotypes: Seq[Individual] = openFileSets(genotypefile, 0.0, 1.0, Full, Set())(_.individuals)

    log.info("Individuals: " + individualsInGenotypes.size)

    val covariatesIncludingConditionSNPs: Frame[Individual, String, Double] = {

      val conditionSNPs: Frame[Individual, String, Double] = if ((conditionMarkerNames).size > 0) {
        openFileSets(genotypefile, minMaf, maxMaf, Full, conditionMarkerNames) { snpmajoriterator =>
          val snps = snpmajoriterator
            .toLocusIteratorWithGenomicMap(genomicMap)
            .recodedToGeneticModel(gwas.Additive)
            .map(x => x._1.name.value -> x._2)
            .toSeq

          Frame(snps: _*)
        }

      } else Frame[Individual, String, Double]()

      covariatesFromCovariateFiles rconcat conditionSNPs
    }.rfilter(x => !x.filterIx(x => (covarnames ++ conditionMarkerNames).contains(x)).hasNA).rfilterIx((x: Individual) => individualsInGenotypes.contains(x) && (keepIndividuals.isEmpty || keepIndividuals.contains(x))).squeeze.rdropNA

    log.info(covariatesIncludingConditionSNPs.toString)

    val outputAlloci = TempFile.createTempFile("alloci")

    openFileWriter(outputAlloci) { writer =>
      lociByPathway.flatMap(_._2.toSet).foreach { a =>
        writer.write(s"${a.name} ${a.genomicLocation.chromosome} ${a.genomicLocation.basePairPosition}\n")
      }
    }

    val outputSetsBed = TempFile.createTempFile(".bed")

    openFileWriter(outputSetsBed) { writer =>
      lociByPathway.foreach {
        case (pathway, loci) =>
          loci.foreach { a =>
            writer.write(s"${a.genomicLocation.chromosome}\t${a.genomicLocation.basePairPosition}\t${a.genomicLocation.basePairPosition + 1}\t${pathway.toString}\t${a.name.value}\n")
          }
      }
    }

    if (assemblePathwayAndQuit) {
      log.info("assemblePathwayAndQuit was true. quite.")
      SetTestResultFiles(
        details = Map(),
        tests = None,
        listOfAllIncludedLoci = SharedFile(outputAlloci, "alloci", canMoveAway = true),
        assembledRegionsBed = SharedFile(outputBedFile, "assembledBed", canMoveAway = true),
        assembledRegionsBedWithLoci = SharedFile(outputSetsBed, "assembledBedWithLoci", canMoveAway = true)
      )
    } else {

      val outputTestFile = TempFile.createTempFile(".tests")

      val flatListOfResult: Seq[(RegionSetName, String, Test, Try[TestResult])] =
        openUnbufferedFileWriter(outputTestFile) { outputTestWriter =>
          openRandomAccessFileSets(genotypefile) { snpquery =>

            val iter1 = lociByPathway
              .iterator
              .filterNot(x => conditionPathways.contains(x._1))

            val mapped = ParIterator.map(iter1, threads, false) {
              case (pathwayname, pathwayloci) =>

                val data: Frame[Individual, String, Double] = synchronized {

                  Frame(
                    pathwayloci.flatMap((x: Locus) =>
                      snpquery
                        .queryLocusRecodedToAdditive(x, genomicMap)
                        .filter(_._1.maf > 0.0)
                        .map(y => y._1.name.value -> y._2)): _*
                  ).rdropNA.rconcat(covariatesIncludingConditionSNPs, org.saddle.index.InnerJoin)

                }

                phenotypeInfo.toList.flatMap {
                  case (phenotypename, phenotypescale) =>

                    // I am not sure that hte last rdropna is needed, but can't hurt
                    val dataMissingPhenoRowsDropped = data.rfilter(r => !r.first(phenotypename).isNA).rdropNA

                    selectedTests.map { test: Test =>

                      val testresult: Try[TestResult] = test.runTest(
                        data = dataMissingPhenoRowsDropped,
                        phenoName = phenotypename,
                        phenoScale = phenotypescale,
                        covariateNames = ((covarnames ++ conditionMarkerNames ++ pcaNames).toSet & data.colIx.toSeq.toSet).toSeq,
                        snpNames = (pathwayloci.map(_.name.value).toSet & data.colIx.toSeq.toSet).toSeq
                      )

                      testresult match {
                        case Failure(e) => log.error(s"Error in pheno: {} test: {} pathway: {}. Cause:\n {}", phenotypename, test, pathwayname, helpers.stackTraceAsString(e))
                        case _ => {}
                      }

                      val shortline = s"${pathwayname.value} $phenotypename ${testresult.map(_.pValue).toOption.getOrElse("NaN")} $test ${testresult.map(_.numberOfSamples).toOption.getOrElse("NaN")}\n"

                      synchronized { outputTestWriter.write(shortline); outputTestWriter.flush }

                      (pathwayname, phenotypename, test, testresult)

                    }

                }

            }

            mapped.flatMap(x => x).toList
          }
        }

      val groupedByTestAndPhenotype = flatListOfResult.groupBy(x => (x._2, x._3))

      val perPhenotypeFiles = groupedByTestAndPhenotype.map {
        case ((phenotype, test), list) =>

          val qqplotfile = TempFile.createTempFile(phenotype + "." + test + ".qq.png") //new File(outfile + phenotype + "." + test + ".qq.png")
          val detailedTestFile = TempFile.createTempFile(phenotype + "." + test + ".detailed.txt")

          mybiotools.plots.QQPlot.writeQQPlot(qqplotfile.getAbsolutePath, list.iterator.flatMap(_._4.toOption.map(_.pValue)).filter(_ != 0.0))

          openFileWriter(detailedTestFile) { fw =>
            list.foreach {
              case (pathway, phenotype, test, testresult) =>
                val line = s"${pathway.value} $phenotype $test ${testresult.map(_.toLine)}\n"
                fw.write(line)

            }
          }
          (phenotype, test) -> (SharedFile(detailedTestFile, name = phenotype + "." + test + ".detailed.txt", canMoveAway = true) -> SharedFile(qqplotfile, name = phenotype + "." + test + ".qq.png", canMoveAway = true))
      }

      ec.shutdown

      SetTestResultFiles(
        details = perPhenotypeFiles.toMap,
        tests = Some(SharedFile(outputTestFile, name = "tests", canMoveAway = true)),
        listOfAllIncludedLoci = SharedFile(outputAlloci, "alloci", canMoveAway = true),
        assembledRegionsBed = SharedFile(outputBedFile, "assembledBed", canMoveAway = true),
        assembledRegionsBedWithLoci = SharedFile(outputSetsBed, "assembledBedWithLoci", canMoveAway = true)
      )

    }

  }
}