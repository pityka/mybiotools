package hivheritability
import com.typesafe.config.Config
import mybiotools.config.Config.configInstance
import java.io.File
import mybiotools._
import mybiotools.openSource
import mybiotools.gwascommons.genotypedata.GRM
import scala.collection.JavaConversions._
import mybiotools.pcanew.pcaFromSaddle
import jebl.evolution.io.NewickImporter
import mybiotools.gwascommons.gcta._
import mybiotools.stat.Resampling._
import mybiotools.tasks._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import mybiotools._
import mybiotools.gwascommons._
import hdfdosage.FileSets._
import org.ejml.simple.SimpleMatrix
import org.saddle._
import mybiotools.workflows.GRMGZPair
import mybiotools.eq._
import mybiotools.workflows.FastaFile
import scala.util.Try
import mybiotools.stat._
import mybiotools.stat.LogisticRegression._

case class SequenceAlignmentConfig(
    private val files: Map[String, (File, File, String)],
    private val keepIndividualsOnlyWithCommonData: Boolean
) {

  val duplicatedRegions: Map[String, Set[Int]] = Map(
    "GAG" -> (432 until 501 toSet),
    "VIF" -> (173 until 193 toSet),
    "VPR" -> (90 until 97 toSet),
    "TAT" -> (0 until 19 toSet)
  )

  val referenceNames = files.map(x => x._1 -> x._2._3)

  val referenceSequences = files.map(x => x._1 -> (openSource(x._2._2)(readFasta)).apply((x._2._3)).filter(c => mybiotools.isAminoAcid(c)))

  val allpositions = referenceSequences.map(x => x._1 -> ((0 until x._2.size).toSet &~ duplicatedRegions.get(x._1).getOrElse(Set[Int]())))

  val (nucleotides, individualsPresentWithAllGenes) = {
    val inMemoryFastas = files.map {
      case (gene, (nuf, aaf, refname)) =>

        val alnu =
          openSource(nuf)(s => readFasta(s))
            .filter(x => x._2.filterNot(a => a == '-' || a == 'N' || a == 'X').size > 50)
            .map(x => x._1 -> x._2.toUpperCase)

        val alaa =
          openSource(aaf)(s => readFasta(s))
            .map(x => x._1 -> x._2.toUpperCase)

        if (keepIndividualsOnlyWithCommonData) {
          val filteredaminoacid = PreprocessSequence.dropMissingIndividualsAndColumns(
            alaa,
            0.4
          )(isAminoAcid)
          println("removing individuals from " + gene + ": " + alnu.filterNot(x => filteredaminoacid.contains(x._1)).map(_._1))
          println(" percentage of ref sequence remaining for " + gene + " " + filteredaminoacid(refname).filter(isAminoAcid).size.toDouble / alaa(refname).filter(isAminoAcid).size.toDouble)

          (
            gene,
            alnu.filter(x => filteredaminoacid.contains(x._1) || x._1 === refname)
          )
        } else (
          gene,
          alnu
        )
    }

    val allindividuals: Set[String] = inMemoryFastas.map(x => x._2.keySet).toSet.flatten

    println(inMemoryFastas.map { x =>
      x._1 -> allindividuals.count(i => x._2.contains(i))
    })

    val individualsPresentWithAllGenes = allindividuals.filter { i =>
      inMemoryFastas.forall(x => x._2.contains(i))
    }

    println("allindividuals: " + allindividuals.size)
    println("individualsPresentWithAllGenes: " + individualsPresentWithAllGenes.size)

    (inMemoryFastas
      .map {
        case (gene, nu) =>
          gene -> nu.filterKeys(key => individualsPresentWithAllGenes.contains(key) || !keepIndividualsOnlyWithCommonData)
      }.toMap, individualsPresentWithAllGenes)
  }

  val summary = files.toString + "\n allpositions: " + allpositions.mapValues(_.size).toString

}
object SequenceAlignmentConfig {
  def apply(c: Config): SequenceAlignmentConfig = {

    val keepIndividualsOnlyWithCommonData = c.getBoolean("keepIndividualsOnlyWithCommonData")
    val pergenealignments = c.getStringList("pergenealignments").grouped(4).map { x =>
      (x(0) -> (new File(x(1)), new File(x(2)), x(3)))
    }.toMap

    if (!keepIndividualsOnlyWithCommonData) {
      println("!!!! including all individuals irrespective of coverage !!!!")
      println("!!!! including all individuals irrespective of coverage !!!!")
      println("!!!! including all individuals irrespective of coverage !!!!")
      println("!!!! including all individuals irrespective of coverage !!!!")
      println("!!!! including all individuals irrespective of coverage !!!!")
    }

    SequenceAlignmentConfig(pergenealignments, keepIndividualsOnlyWithCommonData)
  }
}

trait HIVHeritabilityConfig {
  def runraxml: Boolean
  def h2withlasso: Boolean
  def onlyCurrentG2GPositions: Boolean
  def phenotypeFile: File
  def covariateFile: Option[File]
  def qCovariateFile: Option[File]

  def bootstrapCount: Int
  def randomizationCount: Int
  def downsamples: Seq[Int]
  def outTrunk: String

  def outGroupNames: Set[String]
  def checkPositiveDefinite: Boolean
  def subcohortFiles: Map[String, File]

  def genotypeBedBimFamFiles: Map[String, (File, Option[File])]
  def genotypeFilesForLDPruning: Map[String, GenotypeFileSet]
  def genotypeFilesForLASSO: Option[(String, GenotypeFileSet)]
  def grmgztrunks: Map[String, File]
  def treeFiles: Map[String, File]
  def sequenceAlignments: Option[SequenceAlignmentConfig]
  def sequenceLDPruneThreshold: Double

  def outgroupnucleotideFiles: Map[String, File]
  def outgroupaminoacidFiles: Map[String, File]

  def grmgroups: Map[String, Set[String]]

  def gctaGRMMemory: Int
  def javaGRMMemory: Int
  def subcohortMixAndDownSample: Option[Int]
  def nosubcohortruns: Boolean
  def gctaMaxIterations: Int
  def ldthresholds: List[Double]
  def ldPruneKeepSnps: Set[String]
  def ldPruneAwaySnps: Set[String]
  def genomicMap: Option[File]
  def grmThreads: Int
  def grmBatchSize: Int

  def lanlepitopefile: Option[File]
  def g2gbestperaafile: Option[File]
  def g2gpthreshold: Double

  def listKernels: Boolean
}

object DefaultHIVHeritabilityConfig extends HIVHeritabilityConfig {
  val grmBatchSize = configInstance.getInt("hivheritability.grmBatchSize")
  val grmThreads = configInstance.getInt("hivheritability.grmThreads")
  val genomicMap = {
    val x = configInstance.getString("hivheritability.genomicMap")
    if (x == "") None else Some(new File(x))
  }
  val phenotypeFile = new File(configInstance.getString("hivheritability.phenotypeFile"))
  val covariateFile = {
    val x = configInstance.getString("hivheritability.covariateFile")
    if (x == "") None else Some(new File(x))
  }
  val qCovariateFile = {
    val x = configInstance.getString("hivheritability.qCovariateFile")
    if (x == "") None else Some(new File(x))
  }
  val listKernels = configInstance.getBoolean("hivheritability.listkernels")

  val bootstrapCount = configInstance.getInt("hivheritability.bootstrapCount")
  val outTrunk = configInstance.getString("hivheritability.outTrunk")

  val treeFiles = {
    val x = configInstance.getStringList("hivheritability.treeFiles")
    x.grouped(2).map { x =>
      x(0) -> new File(x(1))
    }.toMap
  }

  val g2gpthreshold = configInstance.getDouble("hivheritability.g2gpthreshold")

  val grmgztrunks = {
    val x = configInstance.getStringList("hivheritability.grmgztrunks")
    x.grouped(2).map { x =>
      x(0) -> new File(x(1))
    }.toMap
  }

  val outGroupNames = {
    val f = configInstance.getString("hivheritability.outGroupNamesFile")
    ((if (f != "") openSource(f)(_.getLines.toSeq) else Seq()) ++
      configInstance.getStringList("hivheritability.outGroupNames").toList).toSet
  }
  // val scaleTree = configInstance.getBoolean("hivheritability.scaleTree")
  // val useCorrelationMatrix = configInstance.getBoolean("hivheritability.useCorrelationMatrix")
  val checkPositiveDefinite = configInstance.getBoolean("hivheritability.checkPositiveDefinite")
  val randomizationCount = configInstance.getInt("hivheritability.randomizationCount")
  val downSamplingSteps = configInstance.getIntList("hivheritability.downSamplingSteps").toList
  val downSamplingReplicates = configInstance.getInt("hivheritability.downSamplingReplicates")
  val downsamples: Seq[Int] = downSamplingSteps.flatMap(i => List.fill(n = downSamplingReplicates)(i)).map(_.toInt).toList
  val subcohortFiles = {
    val x = configInstance.getString("hivheritability.subcohortFiles")
    if (x == "") Map[String, File]()
    else openSource(x)(_.getLines.map { line =>
      val spl = fastSplitSetSeparator(line, Set('\t', ' '))
      spl(0) -> new File(spl(1))
    }.toMap)
  }
  val genotypeBedBimFamFiles = {
    val list = configInstance.getStringList("hivheritability.bedbimbamfiles")
    list.grouped(3).map { spl =>
      val name = spl(0)
      val trunk = new File(spl(1))
      val restriction = scala.util.Try(new File(spl(2))).toOption
      name -> (trunk, restriction)
    }.toMap
  }

  val sequenceLDPruneThreshold = configInstance.getDouble("hivheritability.sequenceLDPruneThreshold")

  assert(phenotypeFile.canRead, "phenotypefile is not readable")

  val individualswithphenotype =
    Series(readLines(phenotypeFile)
      .map(_.fastSplit(Set(' ', '\t')))
      .map(x => Individual(x(0), x(1)) -> x(2).toDouble): _*).filter(!_.isNaN).index.toSeq

  val sequenceAlignments = if (configInstance.hasPath("hivheritability.sequenceAlignments")) Some(SequenceAlignmentConfig(configInstance.getConfig("hivheritability.sequenceAlignments"))) else None

  val genotypeFilesForLDPruning = {
    (if (configInstance.hasPath("hivheritability.snpmajorgenotypes"))
      hdfdosage.FileSets.parseConfig(configInstance.getConfig("hivheritability.snpmajorgenotypes"))
    else Nil).map(x => x.name -> x).toMap
  }

  val genotypeFilesForLASSO = {
    (if (configInstance.hasPath("hivheritability.lasso"))
      Some(hdfdosage.FileSets.parseConfig(configInstance.getConfig("hivheritability.lasso")).head)
    else None).map(x => x.name -> x)
  }

  val gctaGRMMemory = configInstance.getInt("hivheritability.gctaGRMMemory")
  val javaGRMMemory = configInstance.getInt("hivheritability.javaGRMMemory")
  val subcohortMixAndDownSample = {
    val x = configInstance.getInt("hivheritability.subcohortMixAndDownSample")
    if (x > 0) Some(x) else None
  }
  val nosubcohortruns = configInstance.getBoolean("hivheritability.nosubcohortruns")
  val gctaMaxIterations = configInstance.getInt("hivheritability.gctaMaxIterations")

  val ldthresholds: List[Double] = configInstance.getDoubleList("hivheritability.ldthresholds").toList.map(_.toDouble)

  val ldPruneKeepSnps: Set[String] = {
    val f = configInstance.getString("hivheritability.ldPruneKeepSnps")
    if (f == "") Set[String]()
    else openSource(f)(_.getLines.toSet)
  }

  val ldPruneAwaySnps: Set[String] = {
    val f = configInstance.getString("hivheritability.ldPruneAwaySnps")
    if (f == "") Set[String]()
    else openSource(f)(_.getLines.toSet)
  }

  val grmgroups: Map[String, Set[String]] = configInstance.getList("hivheritability.grmgroups").map { configvalue =>
    val list = configvalue.asInstanceOf[com.typesafe.config.ConfigList]
    val name = list.head.unwrapped.asInstanceOf[String]
    val rest = list.drop(1).map(_.unwrapped.asInstanceOf[String]).toSet
    name -> rest
  }.toMap

  grmgroups.values.flatten.foreach { name =>

    val fix = Set(
      "viaIBSFullgenomeG2GHits",
      "viaIBSFullgenomeNoG2GHits",
      "viaTreeFullgenomeAllpositions.withCorrelation",
      "viaTreeFullgenomeAllpositions.scaleToMedian",
      "viaTreeFullgenomeAllpositions.scaleToMax",
      "viaTreeFullgenomeG2GHits.withCorrelation",
      "viaTreeFullgenomeG2GHits.scaleToMedian",
      "viaTreeFullgenomeG2GHitsRandom.scaleToMedian",
      "viaTreeFullgenomeG2GHits.scaleToMax",
      "viaTreeFullgenomeNoG2GHits.withCorrelation",
      "viaTreeFullgenomeNoG2GHits.scaleToMedian",
      "viaTreeFullgenomeNoG2GHits.scaleToMax",
      "viaIBSWithLASSOFullgenomeG2GHits",
      "viaIBSWithLASSOFullgenomeNoG2GHits"
    )

    val genenamesTree = sequenceAlignments.toList.flatMap(c => c.referenceNames.toList.map(x => "viaTreeNoG2GHits" + x._1) ++ c.referenceNames.toList.map(x => "viaTreeG2GHits" + x._1))

    val genenamesIBS = sequenceAlignments.toList.flatMap(c => c.referenceNames.toList.map(x => "viaIBSG2GHits" + x._1) ++ c.referenceNames.toList.map(x => "viaIBSNoG2GHits" + x._1))

    val other = genotypeBedBimFamFiles.map(_._1) ++ genotypeFilesForLDPruning.flatMap(x => ldthresholds.map(l => x + ".ld" + l)) ++ treeFiles.flatMap(x => (x._1 + ".withCorrelation") :: (x._1 + ".scaleToMax") :: (x._1 + ".scaleToMedian") :: Nil) ++ grmgztrunks.map(_._1) ++ genotypeFilesForLASSO.map(_._1)
    assert((fix ++ genenamesTree.map(_ + ".scaleToMedian") ++ genenamesTree.map(_ + ".withCorrelation") ++ genenamesTree.map(_ + ".scaleToMax") ++ genenamesIBS ++ other).toSet.contains(name), "GRM group configuration error. Referenced GRM is not configured." + grmgroups + " " + name)

  }

  val runraxml: Boolean = configInstance.getBoolean("hivheritability.runraxml")
  val h2withlasso: Boolean = configInstance.getBoolean("hivheritability.h2withlasso")
  val onlyCurrentG2GPositions: Boolean = configInstance.getBoolean("hivheritability.onlyCurrentG2GPositions")
  val outgroupnucleotideFiles = configInstance.getStringList("hivheritability.outgroupNucleotides").grouped(2).map(x => x(0) -> new File(x(1))).toMap
  val outgroupaminoacidFiles = configInstance.getStringList("hivheritability.outgroupAminoacids").grouped(2).map(x => x(0) -> new File(x(1))).toMap

  val lanlepitopefile = Try(new File(configInstance.getString("hivheritability.lanlepitopefile"))).toOption

  val g2gbestperaafile = Try(new File(configInstance.getString("hivheritability.g2gbestperaafile"))).toOption

}

class HIVHeritabilityRun(config: HIVHeritabilityConfig, taskSystem: TaskSystem) {
  import taskSystem._
  import config._
  implicit val fs = components.fs
  implicit val context = components.actorsystem

  private val allcohortname = "all"
  private val mixdownname = "mixdown"

  def convert(g: String, p: Int) = (g, p) match {
    case ("PR", x) => ("POL", (x + 56))
    case ("RT", x) => ("POL", (x + 155))
    case ("INT", x) => ("POL", (x + 715))
    case ("RNASE", x) => ("POL", (x + 595))
    case (g, p) => (g, p)
  }

  def run: Unit = {

    val rnd = new org.apache.commons.math3.random.Well19937c

    taskSystem.registerApplicationFileLogger(new File(outTrunk + ".logfile"))
    implicit val log = taskSystem.getLogger(this)
    log.info((mybiotools.config.Config.prettyPrintVersion("HIV Heritability", hivheritability.Reflected.version)))
    log.info((mybiotools.config.Config.prettyPrint(Some("hivheritability"))))

    log.info("Individual IID fields should be 1.")

    log.info("Outgroup sequences: " + outGroupNames)

    val phenotypeinmemory =
      Series(readLines(phenotypeFile)
        .map(_.fastSplit(Set(' ', '\t')))
        .map(x => Individual(x(0), x(1)) -> x(2).toDouble): _*)

    log.info("Phenotypes for individual: " + phenotypeinmemory.index.toSeq)

    val covariatesInMemory: Frame[Individual, String, Double] = {
      val qcovar: Frame[Individual, String, Double] = qCovariateFile.map { qf =>
        Frame(
          readLines(qf)
            .map(_.fastSplit(Set(' ', '\t')))
            .map(x => Individual(x(0), x(1)) -> Series(x.drop(2).zipWithIndex.map(x => ("qcov" + x._2.toString) -> x._1.toDouble): _*)): _*
        ).T
      } getOrElse Frame[Individual, String, Double]()

      val covar = {
        val raw: Frame[Individual, String, String] = covariateFile.map { cf =>

          Frame(
            readLines(cf)
              .map(_.fastSplit(Set(' ', '\t')))
              .map(x => Individual(x(0), x(1)) -> Series(x.drop(2).zipWithIndex.map(x => ("fcov" + x._2.toString) -> x._1): _*)): _*
          ).T
        } getOrElse Frame[Individual, String, String]()

        Frame(raw.toColSeq.flatMap {
          case (colname, col) =>
            col.toSeq.map(_._2).distinct.drop(1).map { value =>
              colname + "_" + value -> col.mapValues(v => if (v === value) 1.0 else 0.0)
            }
        }: _*)

      }

      qcovar rconcat covar

    }

    log.info("Covariates for individual: " + covariatesInMemory.rowIx.toSeq)
    log.info("Covariates columns: " + covariatesInMemory.colIx.toSeq)

    def runGCTAClosure(grms: Map[String, File], pheno: File, test: WhichKernelToTest, name: String, includeDiscreteCovariates: Boolean) = runGCTA(grms, qcovar = qCovariateFile, covar = if (includeDiscreteCovariates) covariateFile else None, pheno, test, name, randomizationCount, bootstrapCount, downsamples, gctaMaxIterations)

    val grmgztrunkswithname: Map[String, File] = {

      val lasso = genotypeFilesForLASSO.map {
        case (name, fileset) =>
          name -> makeGRMWithLasso(fileset, phenotypeinmemory, covariatesInMemory)
      }

      val gzfromlasso: Option[(String, File)] = lasso.map(x => x._1 -> x._2._1)
      val datafromlasso: Option[Frame[Individual, String, Double]] = lasso.map(_._2._2.mapColIndex(_.value))

      val singlesnpregressionOfLassoSelected = datafromlasso.map { genotypeselected =>

        genotypeselected.toColSeq.sortBy(_._1).map {
          case (snpname, snpseries) =>

            val joined = (Frame(snpname -> snpseries).rdropNA rconcat covariatesInMemory.rdropNA rconcat Frame("PHENO" -> phenotypeinmemory)).rdropNA

            val lm = LinearRegression.linearRegression(
              data = joined,
              covariates = joined.colIx.toSeq.filterNot(_ === "PHENO"),
              yKey = "PHENO",
              mybiotools.stat.DropSample,
              0.0
            ).asInstanceOf[mybiotools.stat.LinearRegression.LinearRegressionResultMaps[_]]

            log.info(s"LM: VL ~ $snpname : " + lm.covariates(snpname) + " " + lm.r2)

            (snpname, lm.covariates(snpname), lm.r2)

        }
      }

      gzfromlasso.foreach {
        case (name, file) =>
          SharedFile(new File(file.getAbsolutePath + ".grm.gz"), name + ".grm.gz")
          SharedFile(new File(file.getAbsolutePath + ".grm.id"), name + ".grm.id")
      }

      val grmgzfromsequenceviatree: Map[String, File] = {
        val outgroupnucleotides: Map[String, FastaSequenceData] = outgroupnucleotideFiles.map {
          case (gene, file) =>
            gene -> openSource(file)(s => readFasta(s))
        }
        val outgroupaminoacids: Map[String, FastaSequenceData] = outgroupaminoacidFiles.map {
          case (gene, file) =>
            gene -> openSource(file)(s => readFasta(s))
        }
        val s = sequenceAlignments

        sequenceAlignments.foreach { s =>
          log.info("Sequence configuration: " + s.summary)
          writeToFile(outTrunk + ".individualsPresentWithAllGenes", s.individualsPresentWithAllGenes.mkString("\n"))

        }

        def logNonMissingPositions(f: FastaFile) = {
          log.info("Number of positions which are not completely N: " + f.file.name + " " + calculateMissingRate(openSource(f.file.file)(s => readFasta(s)))(isNucleotide).count(_ > 0.0))
        }

        s.map { s =>
          import s._

          writeToFile(outTrunk + ".allpositions", allpositions.mkString("\n"))

          val aligned = alignSelectConcatenate(
            nucleotides = nucleotides,
            outgroupNucleotides = outgroupnucleotides,
            outgroupAminoAcids = outgroupaminoacids,
            referenceNames = referenceNames,
            referenceAminoAcidSequences = referenceSequences,
            positions = allpositions,
            outname = "aligned.concatenated.allpositions"
          )

          val alignmentFullGenomeAminoAcid: Future[Seq[(String, FastaSequenceData)]] = aligned.map(x => x._2.map(x => x._1 -> x._3))

          alignmentFullGenomeAminoAcid.foreach { pergeneaa =>
            pergeneaa.foreach {
              case (gene, fasta) =>
                log.info("AA alignment: " + gene + " samples: " + fasta.size + ", columns: " + fasta.head._2.size)
            }
          }

          // Multiple Regressions
          // alignmentFullGenomeAminoAcid.foreach { pergenealignments =>
          //
          //   datafromlasso foreach { humangenotypes =>
          //     val indicators: Frame[Individual, (Int, Char, String), Option[Boolean]] = pergenealignments.map {
          //       case (gene, aminoacidalignment) =>
          //
          //         val indicators =
          //           fasta2indicators(aminoacidalignment, referenceNames(gene))(isAminoAcid).sortedRIx.sortedCIx.mapRowIndex(s => Individual(s, "1")).mapColIndex(x => (x._1, x._2, gene))
          //
          //         indicators.rdropNA
          //
          //     } reduce (_ rconcat _) rdropNA
          //
          //     val g2gregressionresults: Seq[(((Int, Char, String), String), Either[FailedRegression, LogisticRegressionResult])] =
          //       g2g(
          //         indicators,
          //         humangenotypes,
          //         covariatesInMemory)
          //
          //     val significantG2GAssociations =
          //       g2gregressionresults
          //         .filter((x: ((((Int, Char, String), String), Either[FailedRegression, LogisticRegressionResult]))) =>
          //           x._2.right.toOption.map(_.covariates(x._1._2)._2.pValue <= g2gpthreshold).getOrElse(false)
          //         )
          //
          //     val significantIndicators = significantG2GAssociations.map(_._1._1).distinct
          //     val significantSnps = significantG2GAssociations.map(_._1._2).distinct
          //
          //     log.info(s"Viral variants with g2g p <= $g2gpthreshold : (${significantIndicators.size}) " + significantIndicators.groupBy(_._3).map(_._2.sortBy(_._1)))
          //     log.info(s"Human variants with g2gp <= $g2gpthreshold : (${significantSnps.size}) " + significantSnps)
          //
          //     log.info("Signficant g2g associations against VL: \n" + significantG2GAssociations.flatMap {
          //       case ((indicatorname, snpname), g2gregression) =>
          //         val indicator = indicators.firstCol(indicatorname).mapValues(x => x.map(y => if (y) 1.0 else 0.0).getOrElse(Double.NaN))
          //         val humansnp = humangenotypes.col(snpname)
          //         val joined = (Frame(indicatorname.toString -> indicator).rdropNA rconcat humansnp.rdropNA rconcat covariatesInMemory.rdropNA rconcat Frame("PHENO" -> phenotypeinmemory)).rdropNA
          //
          //         log.info("Joined human x covariate x viral data. Phenotype distribution: " + SummaryStat(joined.firstCol("PHENO").toVec.toSeq))
          //
          //         val lmCovariateOnly = if (covariatesInMemory.numCols > 0) Some(LinearRegression.linearRegression(
          //           data = joined,
          //           covariates = joined.colIx.toSeq.filter(x => covariatesInMemory.colIx.toSeq.contains(x)),
          //           yKey = "PHENO",
          //           mybiotools.stat.DropSample,
          //           0.0).asInstanceOf[mybiotools.stat.LinearRegression.LinearRegressionResultMaps[_]])
          //         else None
          //
          //         val lmIndicatorOnly = LinearRegression.linearRegression(
          //           data = joined,
          //           covariates = joined.colIx.toSeq.filterNot(x => x === "PHENO" || x === snpname),
          //           yKey = "PHENO",
          //           mybiotools.stat.DropSample,
          //           0.0).asInstanceOf[mybiotools.stat.LinearRegression.LinearRegressionResultMaps[_]]
          //
          //         val lmSNPOnly = LinearRegression.linearRegression(
          //           data = joined,
          //           covariates = joined.colIx.toSeq.filterNot(x => x === "PHENO" || x === indicatorname.toString),
          //           yKey = "PHENO",
          //           mybiotools.stat.DropSample,
          //           0.0).asInstanceOf[mybiotools.stat.LinearRegression.LinearRegressionResultMaps[_]]
          //
          //         val lmBoth = LinearRegression.linearRegression(
          //           data = joined,
          //           covariates = joined.colIx.toSeq.filterNot(_ === "PHENO"),
          //           yKey = "PHENO",
          //           mybiotools.stat.DropSample,
          //           0.0).asInstanceOf[mybiotools.stat.LinearRegression.LinearRegressionResultMaps[_]]
          //
          //         s"LM VL ~ ${covariatesInMemory.rowIx.toSeq} : " + lmCovariateOnly ::
          //           s"LogReg $indicatorname ~ $snpname : " + g2gregression.right.toOption.map(_.covariates(snpname)) + " " + g2gregression.right.toOption.map(_.numberOfSamples) ::
          //           s"LM VL ~ $indicatorname : " + lmIndicatorOnly.covariates(indicatorname.toString) + " " + lmIndicatorOnly.numberOfSamples + " " + lmIndicatorOnly.r2 + " " + lmIndicatorOnly.adjR2 ::
          //           s"LM VL ~ $snpname : " + lmSNPOnly.covariates(snpname.toString) + " " + lmSNPOnly.numberOfSamples + " " + lmSNPOnly.r2 + " " + lmSNPOnly.adjR2 ::
          //           s"LM VL ~ $indicatorname + $snpname : " + lmBoth.covariates(indicatorname.toString) + " " + lmBoth.numberOfSamples + " " + lmBoth.covariates(snpname) + " " + lmBoth.r2 + " " + lmBoth.adjR2 ::
          //           "" :: Nil
          //
          //     }.mkString("\n"))
          //
          //     val joined = (indicators.col(significantIndicators: _*).mapColIndex(_.toString).mapValues(x => x.map(y => if (y) 1.0 else 0.0).getOrElse(Double.NaN)).rdropNA rconcat humangenotypes.col(significantSnps: _*).rdropNA rconcat covariatesInMemory.rdropNA rconcat Frame("PHENO" -> phenotypeinmemory)).rdropNA
          //
          //     val joinedpergene = pergenealignments.map(_._1).map(gene => gene -> (indicators.col(significantIndicators.filter(_._3 === gene): _*).mapColIndex(_.toString).mapValues(x => x.map(y => if (y) 1.0 else 0.0).getOrElse(0.0)).rdropNA rconcat humangenotypes.col(significantSnps: _*).rdropNA rconcat covariatesInMemory.rdropNA rconcat Frame("PHENO" -> phenotypeinmemory)).rdropNA)
          //
          //     val lmCovariateOnly = if (covariatesInMemory.numCols > 0) Some(LinearRegression.linearRegression(
          //       data = joined,
          //       covariates = joined.colIx.toSeq.filter(x => covariatesInMemory.colIx.toSeq.contains(x)),
          //       yKey = "PHENO",
          //       mybiotools.stat.DropSample,
          //       0.0).asInstanceOf[mybiotools.stat.LinearRegression.LinearRegressionResultMaps[_]])
          //     else None
          //
          //     val lmAllGenesVirusAndHuman = LinearRegression.linearRegressionPruneMulticollinear(
          //       data = joined,
          //       covariates = joined.colIx.toSeq.filterNot(x => x === "PHENO"),
          //       yKey = "PHENO",
          //       mybiotools.stat.DropSample,
          //       0.0).asInstanceOf[mybiotools.stat.LinearRegression.LinearRegressionResultMaps[_]]
          //
          //     val lmAllGenesOnlyVirus = LinearRegression.linearRegressionPruneMulticollinear(
          //       data = joined,
          //       covariates = joined.colIx.toSeq.filterNot(x => x === "PHENO" || significantSnps.contains(x)),
          //       yKey = "PHENO",
          //       mybiotools.stat.DropSample,
          //       0.0).asInstanceOf[mybiotools.stat.LinearRegression.LinearRegressionResultMaps[_]]
          //
          //     val lmAllGenesOnlyHuman = LinearRegression.linearRegressionPruneMulticollinear(
          //       data = joined,
          //       covariates = joined.colIx.toSeq.filterNot(x => x === "PHENO" || significantIndicators.map(_.toString).contains(x)),
          //       yKey = "PHENO",
          //       mybiotools.stat.DropSample,
          //       0.0).asInstanceOf[mybiotools.stat.LinearRegression.LinearRegressionResultMaps[_]]
          //
          //     log.info(s"Multiple regression with G2G significant viral and human positions (r2/adjr2) :\n covariate only: ${lmCovariateOnly.map(_.r2)} \n virus+human: ${lmAllGenesVirusAndHuman.r2}/${lmAllGenesVirusAndHuman.adjR2} \n virus only: ${lmAllGenesOnlyVirus.r2}/${lmAllGenesOnlyVirus.adjR2} \n human only: ${lmAllGenesOnlyHuman.r2}/${lmAllGenesOnlyHuman.adjR2}")
          //
          //     log.info(s"Multiple regression with G2G significant viral and human positions:\n virus+human: $lmAllGenesVirusAndHuman \n virus only: $lmAllGenesOnlyVirus \n human only: $lmAllGenesOnlyHuman")
          //
          //     joinedpergene.foreach {
          //       case (gene, joined) =>
          //
          //         val lmCovariateOnly = if (covariatesInMemory.numCols > 0) Some(LinearRegression.linearRegression(
          //           data = joined,
          //           covariates = joined.colIx.toSeq.filter(x => covariatesInMemory.colIx.toSeq.contains(x)),
          //           yKey = "PHENO",
          //           mybiotools.stat.DropSample,
          //           0.0).asInstanceOf[mybiotools.stat.LinearRegression.LinearRegressionResultMaps[_]])
          //         else None
          //
          //         val lmVirusAndHuman = LinearRegression.linearRegressionPruneMulticollinear(
          //           data = joined,
          //           covariates = joined.colIx.toSeq.filterNot(x => x === "PHENO"),
          //           yKey = "PHENO",
          //           mybiotools.stat.DropSample,
          //           0.0).asInstanceOf[mybiotools.stat.LinearRegression.LinearRegressionResultMaps[_]]
          //
          //         val lmOnlyVirus = LinearRegression.linearRegressionPruneMulticollinear(
          //           data = joined,
          //           covariates = joined.colIx.toSeq.filterNot(x => x === "PHENO" || significantSnps.contains(x)),
          //           yKey = "PHENO",
          //           mybiotools.stat.DropSample,
          //           0.0).asInstanceOf[mybiotools.stat.LinearRegression.LinearRegressionResultMaps[_]]
          //
          //         val lmOnlyHuman = LinearRegression.linearRegressionPruneMulticollinear(
          //           data = joined,
          //           covariates = joined.colIx.toSeq.filterNot(x => x === "PHENO" || significantIndicators.map(_.toString).contains(x)),
          //           yKey = "PHENO",
          //           mybiotools.stat.DropSample,
          //           0.0).asInstanceOf[mybiotools.stat.LinearRegression.LinearRegressionResultMaps[_]]
          //
          //         log.info(s"Multiple regression with G2G significant $gene and human positions (r2/adjr2) :\n covariate only: ${lmCovariateOnly.map(_.r2)} \n virus+human: ${lmVirusAndHuman.r2}/${lmVirusAndHuman.adjR2} \n virus only: ${lmOnlyVirus.r2}/${lmOnlyVirus.adjR2} \n human only: ${lmOnlyHuman.r2}/${lmOnlyHuman.adjR2}")
          //
          //         log.info(s"Multiple regression with G2G significant $gene and human positions:\n virus+human: $lmVirusAndHuman \n virus only: $lmOnlyVirus \n human only: $lmOnlyHuman")
          //
          //     }
          //
          //   }
          // }
          // Multiple Regressions End

          val g2gHits: Future[Map[String, Set[Int]]] = {

            val g2gHitsFromFile: Map[String, Set[Int]] = g2gbestperaafile map { file =>
              openSource(file)(s => s.getLines.map { line =>
                val spl = fastSplitSeparator(line, ' ')
                val p = spl(1).toDouble
                val pos = spl(0).toInt - 1
                val gene = spl(5)
                if (p < g2gpthreshold) Some((gene, pos)) else None
              }.filter(_.isDefined).map(_.get).map(x => convert(x._1, x._2)).toList.groupBy(_._1).map(x => x._1 -> x._2.map(_._2).toSet))
            } getOrElse Map()

            log.info("G2G hits from file: " + g2gHitsFromFile.mapValues(_.toSeq.sorted).mkString("\n"))

            val lanlEpitopes: Map[String, Set[Int]] = lanlepitopefile.map { l =>
              openSource(l)(_
                .getLines
                .drop(1)
                .map(_.fastSplit1Wide(',').map(_.drop(1).dropRight(1)))
                .map(x => x(1).toUpperCase -> ((x(2).toInt - 1) to (x(3).toInt - 1)).toSet)
                .toSeq.groupBy(_._1).map(x => x._1 -> x._2.map(_._2).toSet.flatten))
            } getOrElse Map()

            log.info("Lanl epitopes: " + lanlEpitopes.mapValues(_.toSeq.sorted).mkString("\n"))

            alignmentFullGenomeAminoAcid.map { pergenealignments =>

              if (onlyCurrentG2GPositions) {
                datafromlasso map { humangenotypes =>
                  pergenealignments.map {
                    case (gene, aminoacidalignment) =>

                      val indicators: Frame[Individual, (Int, Char), Option[Boolean]] =
                        fasta2indicators(aminoacidalignment, referenceNames(gene))(isAminoAcid).sortedRIx.sortedCIx.mapRowIndex(s => Individual(s, "1"))

                      val regressionresults: Seq[(((Int, Char), String), Either[FailedRegression, LogisticRegressionResult])] =
                        g2g(
                          indicators,
                          humangenotypes,
                          covariatesInMemory
                        )

                      log.info("Number of samples for G2G regression: " + gene + ": " + indicators.numRows + "/" + humangenotypes.numRows + "/" + covariatesInMemory.numRows)

                      val positions: Set[Int] =
                        regressionresults
                          .groupBy(_._1._1)
                          .filter((y: ((Int, Char), Seq[(((Int, Char), String), Either[FailedRegression, LogisticRegressionResult])])) =>

                            y._2.exists(x => x._2.right.toOption.map(_.covariates(x._1._2)._2.pValue <= g2gpthreshold).getOrElse(false)))
                          .map(_._1._1)
                          .toSet

                      log.info("G2G regression for " + gene + ": " + positions.toSeq.sorted)

                      gene -> positions
                  }.toMap

                } getOrElse Map()
              } else addMaps(g2gHitsFromFile, lanlEpitopes)(_ ++ _)

            }

          }

          if (h2withlasso) {

            genotypeFilesForLASSO.foreach { genotypeFilesForLASSO =>
              val lassoheritability = g2gHits.flatMap { g2gHits =>

                alignmentFullGenomeAminoAcid.map { alignmentFullGenomeAminoAcid =>
                  log.info("calculating h2 with lasso.. ")
                  log.info("lasso h2: " + Try(calculateHeritabilityWithLasso(
                    humangenotypes = readGenotypeFileSet(genotypeFilesForLASSO._2),
                    pergeneaminoacidindicators = alignmentFullGenomeAminoAcid.map(x => x._1 ->
                      fasta2indicators(x._2, referenceNames(x._1))(isAminoAcid)),
                    g2ghits = g2gHits,
                    covariates = covariatesInMemory,
                    phenotype = phenotypeinmemory
                  )))
                }
              }
              Await.result(lassoheritability, atMost = duration.Duration.Inf)
            }
          }

          val f = g2gHits.flatMap { g2gHits =>

            writeToFile(outTrunk + ".g2ghits.txt", g2gHits.flatMap(x => x._2.map(y => x._1 + " " + (y + 1).toString)).mkString("\n"))

            val fullgenomeG2GHits: Map[String, Set[Int]] = allpositions.map {
              case (gene, allpos) =>
                gene -> (allpos & g2gHits(gene))
            }

            val fullgenomeG2GHitsRandom: Map[String, Set[Int]] = allpositions.map {
              case (gene, allpos) =>
                log.info(s"Select random of $gene , total ${allpos.size}, select ${(allpos & g2gHits(gene)).size}")
                gene -> selectRandom(allpos, (allpos & g2gHits(gene)).size, 1)
            }

            val fullgenomeNoG2GHits: Map[String, Set[Int]] = allpositions.map {
              case (gene, allpos) =>
                gene -> (allpos &~ g2gHits(gene))
            }

            log.info("G2G positions: " + fullgenomeG2GHits.mapValues(_.size))
            log.info("NOG2G positions: " + fullgenomeNoG2GHits.mapValues(_.size))
            log.info("random G2G positions: " + fullgenomeG2GHitsRandom.mapValues(_.size))

            // val lassoSelectedViralPositions = alignmentFullGenomeAminoAcid.map { alignmentFullGenomeAminoAcid =>

            //   val selected = PreprocessSequence.selectIndicatorsWithLASSOFromPerGeneAlignments(
            //     alignmentFullGenomeAminoAcid.map(x => x._1 -> PreprocessSequence.maskMissing(x._2, isAminoAcid)).toMap,
            //     isAminoAcid,
            //     referenceNames,
            //     phenotypeinmemory,
            //     covariatesInMemory
            //   )

            //   log.info("viral lasso: " + selected.colIx)

            //   selected

            // }

            val alignmentFullGenomeAllpositions = aligned.map(_._1)

            alignmentFullGenomeAllpositions.foreach(logNonMissingPositions)

            val alignmentFullGenomeG2GHits = alignSelectConcatenate(
              nucleotides = nucleotides,
              outgroupNucleotides = outgroupnucleotides,
              outgroupAminoAcids = outgroupaminoacids,
              referenceNames = referenceNames,
              referenceAminoAcidSequences = referenceSequences,
              positions = fullgenomeG2GHits,
              outname = "aligned.concatenated.g2ghits"
            ).map(_._1)

            alignmentFullGenomeG2GHits.foreach(logNonMissingPositions)

            val alignmentFullGenomeG2GHitsRandom = alignSelectConcatenate(
              nucleotides = nucleotides,
              outgroupNucleotides = outgroupnucleotides,
              outgroupAminoAcids = outgroupaminoacids,
              referenceNames = referenceNames,
              referenceAminoAcidSequences = referenceSequences,
              positions = fullgenomeG2GHitsRandom,
              outname = "aligned.concatenated.g2ghits.random"
            ).map(_._1)

            alignmentFullGenomeG2GHitsRandom.foreach(logNonMissingPositions)

            val ldPrunedAlignmentFullGenomeNoG2GHits = alignmentFullGenomeG2GHits.flatMap { alignmentg2g =>
              alignSelectConcatenate(
                nucleotides = nucleotides,
                referenceAminoAcidSequences = referenceSequences,
                outgroupNucleotides = outgroupnucleotides,
                outgroupAminoAcids = outgroupaminoacids,
                referenceNames = referenceNames,
                positions = fullgenomeNoG2GHits,
                outname = "aligned.concatenated.nog2ghits"
              ).map(_._1).flatMap { alignmentnog2g =>

                pruneSecondFasta(
                  TwoFastas(Some(alignmentg2g), Some(alignmentnog2g), Some(sequenceLDPruneThreshold)),
                  1,
                  5000
                ).?[FastaFile]
              }
            }

            ldPrunedAlignmentFullGenomeNoG2GHits.foreach(logNonMissingPositions)

            val alignmentPerGeneG2GHits = referenceNames.toList.map {
              case (gene, ref) =>

                gene -> alignSelectConcatenate(
                  nucleotides = nucleotides.filter(_._1 == gene),
                  referenceAminoAcidSequences = referenceSequences.filter(_._1 == gene),
                  outgroupNucleotides = outgroupnucleotides.filter(_._1 == gene),
                  outgroupAminoAcids = outgroupaminoacids.filter(_._1 == gene),
                  referenceNames = referenceNames.filter(_._1 == gene),
                  positions = fullgenomeG2GHits.filter(_._1 == gene),
                  outname = "aligned." + gene + ".g2ghits"
                ).map(_._1)
            }

            alignmentPerGeneG2GHits.foreach(_._2.foreach(logNonMissingPositions))

            val ldPrunedAlignmentPerGeneNoG2GHits = referenceNames.map {
              case (gene, _) =>
                gene -> alignSelectConcatenate(
                  nucleotides = nucleotides.filter(_._1 == gene),
                  referenceAminoAcidSequences = referenceSequences.filter(_._1 == gene),
                  outgroupNucleotides = outgroupnucleotides.filter(_._1 == gene),
                  outgroupAminoAcids = outgroupaminoacids.filter(_._1 == gene),
                  referenceNames = referenceNames.filter(_._1 == gene),
                  positions = fullgenomeNoG2GHits.filter(_._1 == gene),
                  outname = "aligned." + gene + ".nog2ghits"
                ).map(_._1).flatMap { alignmentnog2g =>
                    alignmentPerGeneG2GHits.filter(_._1 == gene).head._2.map { alignmentg2g =>

                      val fasta = mybiotools.sequence.maskPositionsInSecondWhichAreCorrelatedInTheFirst(
                        openSource(alignmentg2g.file.file)(s => readFasta(s)),
                        openSource(alignmentnog2g.file.file)(s => readFasta(s)),
                        sequenceLDPruneThreshold
                      )
                      val tmp = TempFile.createTempFile(".fasta")
                      writeFasta(tmp.getAbsolutePath, fasta, 80)
                      FastaFile(SharedFile(tmp, alignmentnog2g.file.name + ".pruned.fasta"))

                    }
                  }
            }

            ldPrunedAlignmentPerGeneNoG2GHits.foreach(_._2.foreach(logNonMissingPositions))

            Future.sequence(
              List(
                // lassoSelectedViralPositions.map { selected =>
                //   ("viaIBSWithLASSOFullgenomeG2GHits" -> makeIBSFromPerGeneIndicators(
                //     selected,
                //     fullgenomeG2GHits)) :: Nil
                // },

                // lassoSelectedViralPositions.map { selected =>
                //   ("viaIBSWithLASSOFullgenomeNoG2GHits" -> makeIBSFromPerGeneIndicators(
                //     selected,
                //     fullgenomeNoG2GHits)) :: Nil
                // },
                {
                  alignmentFullGenomeG2GHits.flatMap { fasta =>
                    makeIBS(
                      nucleotides = fasta,
                      threads = grmThreads
                    ).map { trunk: File =>
                      ("viaIBSFullgenomeG2GHits" -> trunk) :: Nil
                    }
                  }
                },

                {
                  ldPrunedAlignmentFullGenomeNoG2GHits.flatMap { fasta =>
                    makeIBS(
                      nucleotides = fasta,
                      threads = grmThreads
                    ).map { trunk: File =>
                      ("viaIBSFullgenomeNoG2GHits" -> trunk) :: Nil
                    }
                  }
                },

                {

                  Future.sequence(ldPrunedAlignmentPerGeneNoG2GHits.map {
                    case (gene, future) =>
                      future.flatMap { fasta =>
                        makeIBS(
                          fasta,
                          grmThreads
                        ).map { trunk =>
                          (("viaIBSNoG2GHits" + gene) -> trunk) :: Nil
                        }

                      }
                  }).map(_.flatten)
                },

                {

                  Future.sequence(alignmentPerGeneG2GHits.map {
                    case (gene, future) =>
                      future.flatMap { fasta =>
                        makeIBS(
                          fasta,
                          grmThreads
                        ).map { trunk =>
                          (("viaIBSG2GHits" + gene) -> trunk) :: Nil
                        }

                      }
                  }).map(_.flatten)
                }
              ) ++ (
                  if (!runraxml) Nil else
                    List(
                      {
                        alignmentFullGenomeAllpositions.flatMap { fasta =>
                          makeTreeWithOutgroup(
                            fasta = fasta,
                            outname = "grmViaTreeFullgenomeAllpositions",
                            threads = grmThreads
                          ).map { treefile =>
                            ("viaTreeFullgenomeAllpositions.scaleToMedian" -> covarianceMatricesFromNewick(treefile, outGroupNames, ScaleToMedian, Set()).head) ::
                              ("viaTreeFullgenomeAllpositions.scaleToMax" -> covarianceMatricesFromNewick(treefile, outGroupNames, ScaleToMax, Set()).head) ::
                              ("viaTreeFullgenomeAllpositions.withCorrelation" -> covarianceMatricesFromNewick(treefile, outGroupNames, ConvertToCorrelationMatrix, Set()).head) :: Nil
                          }
                        }
                      },
                      {
                        alignmentFullGenomeG2GHits.flatMap { fasta =>
                          makeTreeWithOutgroup(
                            fasta = fasta,
                            outname = "grmViaTreeFullgenomeG2GHits",
                            threads = grmThreads
                          ).map { treefile =>
                            ("viaTreeFullgenomeG2GHits.scaleToMedian" -> covarianceMatricesFromNewick(treefile, outGroupNames, ScaleToMedian, Set()).head) ::
                              ("viaTreeFullgenomeG2GHits.scaleToMax" -> covarianceMatricesFromNewick(treefile, outGroupNames, ScaleToMax, Set()).head) ::
                              ("viaTreeFullgenomeG2GHits.withCorrelation" -> covarianceMatricesFromNewick(treefile, outGroupNames, ConvertToCorrelationMatrix, Set()).head) :: Nil
                          }
                        }
                      },
                      {
                        alignmentFullGenomeG2GHitsRandom.flatMap { fasta =>
                          makeTreeWithOutgroup(
                            fasta = fasta,
                            outname = "grmViaTreeFullgenomeG2GHitsRandom",
                            threads = grmThreads
                          ).map { treefile =>
                            ("viaTreeFullgenomeG2GHitsRandom.scaleToMedian" -> covarianceMatricesFromNewick(treefile, outGroupNames, ScaleToMedian, Set()).head) ::
                              ("viaTreeFullgenomeG2GHitsRandom.scaleToMax" -> covarianceMatricesFromNewick(treefile, outGroupNames, ScaleToMax, Set()).head) ::
                              ("viaTreeFullgenomeG2GHitsRandom.withCorrelation" -> covarianceMatricesFromNewick(treefile, outGroupNames, ConvertToCorrelationMatrix, Set()).head) :: Nil
                          }
                        }
                      },
                      {
                        ldPrunedAlignmentFullGenomeNoG2GHits.flatMap(fasta =>
                          makeTreeWithOutgroup(
                            fasta,
                            outname = "grmViaTreeFullgenomeNoG2GHits",
                            threads = grmThreads
                          ).map { treefile =>
                            ("viaTreeFullgenomeNoG2GHits.scaleToMedian" -> covarianceMatricesFromNewick(treefile, outGroupNames, ScaleToMedian, Set()).head) ::
                              ("viaTreeFullgenomeNoG2GHits.scaleToMax" -> covarianceMatricesFromNewick(treefile, outGroupNames, ScaleToMax, Set()).head) ::
                              ("viaTreeFullgenomeNoG2GHits.withCorrelation" -> covarianceMatricesFromNewick(treefile, outGroupNames, ConvertToCorrelationMatrix, Set()).head) :: Nil
                          })
                      }
                    //
                    // {
                    //
                    //   Future.sequence(ldPrunedAlignmentPerGeneNoG2GHits.map {
                    //     case (gene, future) =>
                    //       future.flatMap { fasta =>
                    //         makeTreeWithOutgroup(fasta,
                    //           outname = "grmviatreeNoG2GHits" + gene,
                    //           threads = grmThreads).map { treefile =>
                    //             ("viaTreeNoG2GHits" + gene + ".scaleToMedian" -> covarianceMatricesFromNewick(treefile, outGroupNames, ScaleToMedian, Set()).head) ::
                    //               ("viaTreeNoG2GHits" + gene + ".scaleToMax" -> covarianceMatricesFromNewick(treefile, outGroupNames, ScaleToMax, Set()).head) ::
                    //               ("viaTreeNoG2GHits" + gene + ".withCorrelation" -> covarianceMatricesFromNewick(treefile, outGroupNames, ConvertToCorrelationMatrix, Set()).head) ::
                    //               Nil
                    //           }
                    //
                    //       }
                    //   }
                    //   ).map(_.flatten)
                    // },
                    // {
                    //   Future.sequence(alignmentPerGeneG2GHits.map {
                    //     case (gene, future) =>
                    //       future.flatMap { fasta =>
                    //         makeTreeWithOutgroup(fasta,
                    //           outname = "grmviatreeG2GHits" + gene,
                    //           threads = grmThreads).map { treefile =>
                    //             ("viaTreeG2GHits" + gene + ".scaleToMedian" -> covarianceMatricesFromNewick(treefile, outGroupNames, ScaleToMedian, Set()).head) ::
                    //               ("viaTreeG2GHits" + gene + ".scaleToMax" -> covarianceMatricesFromNewick(treefile, outGroupNames, ScaleToMax, Set()).head) ::
                    //               ("viaTreeG2GHits" + gene + ".withCorrelation" -> covarianceMatricesFromNewick(treefile, outGroupNames, ConvertToCorrelationMatrix, Set()).head) ::
                    //               Nil
                    //           }
                    //
                    //       }
                    //   }).map(_.flatten)
                    // }
                    )
                )
            ).map(_.flatten)

          }
          Await.result(f, duration.Duration.Inf).toMap
        }.getOrElse(Map())
      }

      val gzfrombedbimfam: Map[String, File] = {
        val f = Future.sequence(genotypeBedBimFamFiles.toSeq.map {
          case (name, (trunk, extract)) =>
            makeGRM(trunk, extract, gctaGRMMemory) map { f => name -> Some(f) }
        }) map (_.toMap)
        Await.result(f, duration.Duration.Inf)
      }.filter(_._2.isDefined).map(x => x._1 -> x._2.get)

      val gzfromldprunings: Map[String, File] = {
        val f = Future.sequence(ldthresholds.flatMap { ldthreshold =>
          genotypeFilesForLDPruning.map {
            case (name, fileset) =>
              makeGRMWithPruning(fileset, javaGRMMemory, ldthreshold, genomicMap, name + ".ld" + ldthreshold, grmThreads, grmBatchSize, ldPruneKeepSnps, ldPruneAwaySnps) map { f => name + ".ld" + ldthreshold -> Some(f) }
          }
        }) map (_.toMap)
        Await.result(f, duration.Duration.Inf)
      }.filter(_._2.isDefined).map(x => x._1 -> x._2.get)

      val grmgzfromtrees = treeFiles.toList.flatMap {
        case (name, file) =>
          val trunkScalingToMedian = covarianceMatricesFromNewick(file, outGroupNames, ScaleToMedian, Set()).head
          val trunkScalingToMax = covarianceMatricesFromNewick(file, outGroupNames, ScaleToMax, Set()).head
          val trunkCorrelation = covarianceMatricesFromNewick(file, outGroupNames, ConvertToCorrelationMatrix, Set()).head
          (name + ".scaleToMedian" -> trunkScalingToMedian) ::
            (name + ".scaleToMax" -> trunkScalingToMax) ::
            (name + ".withCorrelation" -> trunkCorrelation) :: Nil
      }

      grmgztrunks ++ gzfrombedbimfam ++ gzfromldprunings ++ grmgzfromtrees ++ grmgzfromsequenceviatree ++ gzfromlasso
    }

    log.info(s"GRM trunks: \n\t ${grmgztrunkswithname.toList.mkString("\n\t")}")

    log.info(s"GRM groups: $grmgroups")

    if (!listKernels) {

      grmgztrunkswithname.foreach {
        case (name, filetrunk) =>
          try {
            val grmgz = new File(filetrunk.getAbsolutePath + ".grm.gz")
            val grmid = new File(filetrunk.getAbsolutePath + ".grm.id")
            val grm = openSource(grmgz) { sgrm =>
              openSource(grmid) { sgrmid =>
                GRM.read(sgrm, sgrmid)
              }
            }
            log.info(s"$name " + grm.numRows)
            val pca = mybiotools.pcanew.pcaFromGRM(grm)

            mybiotools.pcanew.plotPCAResultToFile(pca, 1, 2, new File(outTrunk + s"grm.pca.$name.12.png"))
            mybiotools.pcanew.plotPCAResultToFile(pca, 1, 3, new File(outTrunk + s"grm.pca.$name.13.png"))
            mybiotools.pcanew.plotPCAResultToFile(pca, 2, 3, new File(outTrunk + s"grm.pca.$name.23.png"))
            log.info(s"Smallest eigenvalue, $name:  " + pca.eigenValues.min)
            log.info(s"Number of eigenvalues > 1E-3, $name: " + pca.eigenValues.count(_ > 1E-3))
          } catch {
            case x: Throwable => log.error(s"PCA plot of GRM $name $filetrunk failed. " + x)
          }

      }

      val subcohortFilesWithEmpty: Map[String, Option[File]] = if (subcohortFiles.isEmpty) Map(allcohortname -> None) else Map(allcohortname -> None) ++ {
        subcohortMixAndDownSample.map { mixdown =>
          val lines = subcohortFiles.map {
            case (name, file) =>
              // val tmp = TempFile.createTempFile(".pheno")
              val lines = openSource(file.getAbsolutePath)(_.getLines.toIndexedSeq)
              resampledReplica(lines, mixdown, rnd)
          }.reduce(_ ++ _)
          val tmp = TempFile.createTempFile(".pheno")
          writeToFile(tmp.getAbsolutePath, lines.mkString("\n"))
          mixdownname -> Some(tmp)
        } ++ (if (!nosubcohortruns) { subcohortFiles mapValues Some.apply } else Map()) toMap
      }

      def doWithCohort(individualSubSet: Option[(String, Set[Individual])], grms: Map[String, File], grmgroups: Map[String, Set[String]], originalPhenoFile: File, pureCohort: Boolean): Option[Future[Seq[GCTAResampledResultSummary]]] = {

        (if (individualSubSet.isDefined) subsetPhenotypeFile(originalPhenoFile, individualSubSet.get._2) else Some(originalPhenoFile)).map { phenotypeFile =>

          val cohortname = individualSubSet.map(_._1).getOrElse("allinds")

          val futures = grmgroups.toSeq.map {
            case (groupname, grmnames) =>
              val selectedgrms: Map[String, File] = grms.filter(x => grmnames.contains(x._1))
              if (grmnames.size != selectedgrms.size) {
                log.error("config error grm not found: " + groupname + " " + grmnames)
                throw new RuntimeException("config error grm not found: " + groupname + " " + grmnames)
              }

              log.info(s"GRM group: $groupname, $grmnames, $selectedgrms")

              val f = runGCTAClosure(selectedgrms, phenotypeFile, NoLRT, s"$cohortname." + groupname, !pureCohort)

              f map { result =>
                val report = makeReport(result._1, s"With $groupname, $cohortname")
                log.info(report)
                mybiotools.writeToFile(outTrunk + s".report.$cohortname.$groupname", report)
                result._1
              }
          }

          Future.sequence(futures)
        }

      }

      val allFutures = Future sequence
        subcohortFilesWithEmpty.flatMap {
          case (cohortname, maybefile) =>
            val subset = maybefile.map(file => cohortname -> openSource(file.getAbsolutePath)(_.getLines.map(l => Individual.fromFamLine(l)).toSet))
            doWithCohort(subset, grmgztrunkswithname, grmgroups, phenotypeFile, subset.isDefined && cohortname != allcohortname && cohortname != mixdownname)
        }
      Await.result(allFutures, atMost = duration.Duration.Inf)
    }
  }
}

object HIVHeritabilityApp extends App {
  val taskSystem = defaultTaskSystem
  if (taskSystem.hostConfig.myRole == MASTER) {
    try {
      new HIVHeritabilityRun(DefaultHIVHeritabilityConfig, taskSystem).run
    } finally {
      taskSystem.shutdown
    }

  }
}
