import jebl.evolution.trees.{ RootedTree, RootedTreeUtils, Tree, RootedSubtree, Utils, SimpleRootedTree }
import jebl.evolution.graphs.{ Node, Edge }
import jebl.evolution.io.NewickImporter
import scala.collection.JavaConversions._
import org.saddle._
import java.io.File
import mybiotools.gwascommons.gcta._
import mybiotools.stat.Resampling._
import mybiotools.workflows._
import scala.concurrent._
import scala.concurrent.duration._
import mybiotools.tasks._
import scala.concurrent.ExecutionContext.Implicits.global
import mybiotools._
import mybiotools.gwascommons._
import hdfdosage.FileSets._
import mybiotools.pcanew._
import mybiotools.gwascommons.genotypedata.GRM
import mybiotools.eq._
import mybiotools.gwascommons.genotypedata._
import mybiotools.stringstore._
import mybiotools.sequence._
import mybiotools.stat.LogisticRegression._
import mybiotools.stat.LinearRegression._
import mybiotools.stat._

package object hivheritability {

  def selectRandom(allpos: Set[Int], num: Int, seed: Int): Set[Int] =
    (new scala.util.Random(seed)).shuffle(allpos.toSeq).take(num).toSet

  def fasta2indicators(f: FastaSequenceData, referenceName: String)(filter: Char => Boolean)(implicit log: akka.event.LoggingAdapter) = mafFilterOnIndicators(mybiotools.sequence.transformToIndicators(maskToSequenceKey(PreprocessSequence.maskMissing(f, isAminoAcid, referenceName), referenceName))(filter), 0.05)

  def getR2(
    data: Frame[Individual, String, Double],
    covariates: Frame[Individual, String, Double],
    phenotype: Series[Individual, Double]
  ) = {
    val joined = (
      data.filter(x => x.toVec.variance > 0 && x.toVec.mean > 0.05 && x.toVec.mean < 0.95)
      rconcat covariates
      rconcat Frame("PHENO" -> phenotype)
    ).rdropNA

    val scv = PenalizedRegressionWithCrossValidation.crossValidation(
      data = joined,
      covariates = joined.colIx.toSeq.filterNot(_ === "PHENO"),
      yKey = "PHENO",
      mybiotools.stat.DropSample,
      // crossValidationModeInner = KFold(5, 123, 1),
      crossValidationMode = KFold(5, 123, 1),
      optimizer = PenalizedRegressionWithCrossValidation.Search1D(
        FixedBoundsOnLogScale(-20, 20),
        MinimumPredictionError,
        BrentGridAndCMAES(CMAES(50), BrentGrid(1, 20))
      ),
      warmStart = true,
      generateCVSamplesOnThFly = false,
      threads = 10,
      unpenalized = "intercept" +: covariates.colIx.toSeq,
      standardize = true,
      implementation = Ridge
    )

    val scvtrainr2 = scv.map(_._1.fit.rSquared)

    val r = PenalizedRegressionWithCrossValidation.nestedCrossValidation(
      data = joined,
      covariates = joined.colIx.toSeq.filterNot(_ === "PHENO"),
      yKey = "PHENO",
      mybiotools.stat.DropSample,
      crossValidationModeInner = KFold(5, 123, 1),
      crossValidationModeOuter = KFold(5, 123, 1),
      optimizer = PenalizedRegressionWithCrossValidation.Search1D(
        FixedBoundsOnLogScale(-20, 20),
        MinimumPredictionError,
        BrentGridAndCMAES(CMAES(50), BrentGrid(1, 20))
      ),
      warmStart = true,
      generateCVSamplesOnThFly = false,
      threads = 10,
      unpenalized = "intercept" +: covariates.colIx.toSeq,
      standardize = true,
      implementation = Ridge
    )

    (scvtrainr2, r._1.finalFit.map(_.rSquared), r._1.overallR2, r._1.correctedOverallR2)
  }

  def calculateHeritabilityWithLasso(
    humangenotypes: Frame[Individual, String, Double],
    pergeneaminoacidindicators: Seq[(String, Frame[String, (Int, Char), Option[Boolean]])],
    g2ghits: Map[String, Set[Int]],
    covariates: Frame[Individual, String, Double],
    phenotype: Series[Individual, Double]
  ) = {

    val g2ghitsmatrix: Frame[Individual, String, Double] =
      pergeneaminoacidindicators.map(gene =>
        gene._2.mapValues(_.map(y => if (y) 1.0 else 0.0).getOrElse(Double.NaN))
          .filter(_.toVec.variance != 0.0)
          .filterIx(ix => g2ghits(gene._1).contains(ix._1))
          .mapColIndex(x => gene._1 + "X" + x._1 + "_" + x._2)
          .mapRowIndex(x => Individual(x, "1"))
          .sortedCIx
          .sortedRIx
          .rdropNA).reduce(_ rconcat _)

    val nog2ghitsmatrix: Frame[Individual, String, Double] =
      pergeneaminoacidindicators.map(gene =>
        gene._2.mapValues(_.map(y => if (y) 1.0 else 0.0).getOrElse(Double.NaN))
          .filter(_.toVec.variance != 0.0)
          .filterIx(ix => !g2ghits(gene._1).contains(ix._1))
          .mapColIndex(x => gene._1 + "X" + x._1 + "_" + x._2)
          .mapRowIndex(x => Individual(x, "1"))
          .sortedCIx
          .sortedRIx
          .rdropNA).reduce(_ rconcat _)

    List(
      "humanonly" -> getR2(humangenotypes, covariates, phenotype),
      "g2gonly" -> getR2(g2ghitsmatrix, covariates, phenotype),
      "nog2gonly" -> getR2(nog2ghitsmatrix, covariates, phenotype),
      "humanandg2g" -> getR2(humangenotypes rconcat g2ghitsmatrix, covariates, phenotype),
      "humanandnog2g" -> getR2(humangenotypes rconcat nog2ghitsmatrix, covariates, phenotype),
      "g2gandnog2g" -> getR2(g2ghitsmatrix rconcat nog2ghitsmatrix, covariates, phenotype),
      "allthree" -> getR2(g2ghitsmatrix rconcat nog2ghitsmatrix rconcat humangenotypes, covariates, phenotype)
    )
  }

  // def g2g[T](
  //   indicators: Frame[Individual, T, Option[Boolean]],
  //   humangenotypes: Frame[Individual, String, Double],
  //   covariates: Frame[Individual, String, Double]
  // ): Seq[((T, String), Either[FailedRegression, LogisticRegressionResult])] = {
  //
  //   humangenotypes.toColSeq.flatMap {
  //     case (humangenotypename, humangenotypeSeries) =>
  //       val mergedwithcovar: Frame[Individual, String, Double] = if (covariates.numCols > 0 && covariates.numRows > 0) Frame((humangenotypename, humangenotypeSeries) +: covariates.toColSeq: _*) else Frame((humangenotypename, humangenotypeSeries))
  //
  //       indicators.toColSeq.map {
  //         case (indicatorname, indicatorSeries) =>
  //           val nonmiss: Series[Individual, Boolean] = indicatorSeries.filter(_.isDefined).mapValues(_.get)
  //           val regression = logisticRegression(mergedwithcovar, nonmiss, mergedwithcovar.colIx.toSeq, DropSample, 50, 1E-6)
  //           if (regression.isLeft) {
  //             println("failed regression " + indicatorname + " " + humangenotypename + " " + regression)
  //           }
  //           (indicatorname, humangenotypename) -> regression
  //       }
  //   }
  //
  // }

  def makeReport(result: GCTAResampledResultSummary, title: String): String = {
    s"""
    |## $title ##
    |###${title.map(x => '#').mkString}###
    |
    |${result.prettyPrint}
    """.stripMargin
  }

  def makeReport(result: GCTAPooledResampledResults, title: String): String = {
    s"""
    |## $title ##
    |###${title.map(x => '#').mkString}###
    |
    |${result.prettyPrint}
    """.stripMargin
  }

  def subsetPhenotypeFile(infile: File, sub: Set[Individual]): Option[File] = {

    val subsettedlines = openSource(infile.getAbsolutePath)(_.getLines.filter { line =>
      val spl = fastSplitSetSeparator(line, Set(' ', '\t'))
      sub.contains(Individual(spl(0), spl(1)))
    }.toList)
    if (subsettedlines.size > 0) {
      val tmp = TempFile.createTempFile(".pheno")
      writeToFile(tmp.getAbsolutePath, subsettedlines.mkString("\n"))
      Some(tmp)
    } else None

  }

  def vcv2corr(cov: Frame[String, String, Double]): Frame[String, String, Double] = {
    val diagvalues = cov.rowIx.toVec.map(x => cov(x, x).raw(0, 0)).map(x => 1.0 / math.sqrt(x))
    val diag = org.saddle.mat.diag(diagvalues)

    val matrix = cov.toMat
    Frame(diag mult matrix mult diag, cov.rowIx, cov.colIx)
  }

  sealed trait TreeNormalization
  case object ScaleToMedian extends TreeNormalization
  case object ScaleToMax extends TreeNormalization
  case object ConvertToCorrelationMatrix extends TreeNormalization

  def covarianceMatricesFromNewick(
    tf: File,
    outGroupNames: Set[String],
    normalization: TreeNormalization,
    removeSamples: Set[String]
  ): List[File] = {
    mybiotools.openFileReader(tf) { reader =>
      new NewickImporter(reader, true).iterator.zipWithIndex.map {
        case (unrooted, idx) =>
          getRootedSubtree(unrooted, outGroupNames, removeSamples.toSet).flatMap { rooted =>
            val scale = normalization match {
              case ScaleToMedian => 1.0 / getMedianTreeHeight(rooted)
              case ScaleToMax => 1.0 / getMaxTreeHeight(rooted)
              case ConvertToCorrelationMatrix => 1.0
            }
            val frame = {
              val cov = iterToFrame(readCovarianceFromTree(rooted, scale))
              if (normalization === ConvertToCorrelationMatrix) {
                val diagvalues = cov.rowIx.toVec.map(x => cov(x, x).raw(0, 0)).map(x => 1.0 / math.sqrt(x))
                // writeToFile(outTrunk + ".tree." + idx + ".diagonals", diagvalues.toSeq.mkString("\n"))

                vcv2corr(cov)
              } else cov
            }

            // Check for positive semidefiniteness
            val pca = pcaFromGRM(frame)
            val minEigenValue = pca.eigenValues.min
            val total = pca.eigenValues.sum.toDouble

            if (minEigenValue < 0) None
            else {

              val hivgrmtrunk = TempFile.createTempFile("")
              openZippedFileWriter(new File(hivgrmtrunk.getAbsolutePath + ".grm.gz")) { grmwriter =>
                openFileWriter(new File(hivgrmtrunk.getAbsolutePath + ".grm.id")) { idwriter =>
                  GRM.write(frame, grmwriter, idwriter, (x: String) => x + " 1")
                }
              }
              // GRM.plotGRMToFile(frame, new File(hivgrmtrunk + ".grm.png"))

              // mybiotools.pcanew.plotPCAResultToFile(pca, 1, 2, new File(hivgrmtrunk + s".pca.12.png"))
              // mybiotools.pcanew.plotPCAResultToFile(pca, 1, 3, new File(hivgrmtrunk + s".pca.13.png"))
              // mybiotools.pcanew.plotPCAResultToFile(pca, 2, 3, new File(hivgrmtrunk + s".pca.23.png"))
              // mybiotools.pcanew.plotPCAResultToFile(pca, 1, 4, new File(hivgrmtrunk + s".pca.14.png"))
              // mybiotools.pcanew.plotPCAResultToFile(pca, 2, 4, new File(hivgrmtrunk + s".pca.24.png"))
              // mybiotools.pcanew.plotPCAResultToFile(pca, 3, 4, new File(hivgrmtrunk + s".pca.34.png"))
              // writeToFile(hivgrmtrunk + s".pca.eigenvalues.txt", pca.eigenValues.sorted.map(_ / total).mkString("\n"))
              // writeToFile(hivgrmtrunk + s".pca.projection.txt", pca.coordinates.map(x => x._1 + " " + x._2.take(5).mkString(" ")).mkString("\n"))

              Some(hivgrmtrunk)
            }
          }
      }
        .toList.filter(_.isDefined).map(_.get)
    }
  }

  def makeAminoacidAlignment(
    nucleotides: Map[String, FastaSequenceData],
    outgroupNucleotides: Map[String, FastaSequenceData],
    outgroupAminoAcids: Map[String, FastaSequenceData],
    referenceNames: Map[String, String],
    referenceAminoAcidSequences: Map[String, String],
    outname: String
  )(implicit components: TaskSystemComponents, log: akka.event.LoggingAdapter): Future[Seq[(String, FastaSequenceData, FastaSequenceData)]] = {
    import components._
    implicit val fs = components.fs
    implicit val context = components.actorsystem

    // muscle the amino acids together with outgroup and merge the nucleotides without align
    Future.sequence(nucleotides.keys.toSeq.map { gene =>

      val mergedNucleotides = addMaps(
        nucleotides(gene),
        outgroupNucleotides(gene)
      )((x, y) => x).map(x => x._1 -> x._2.filterNot(_ == '-'))

      val inputAminoAcid = mybiotools.sequence.translateFasta(nucleotides(gene), keepGapsInFrame = false) + (referenceNames(gene) -> referenceAminoAcidSequences(gene))

      val outgroupAminoAcid = outgroupAminoAcids(gene)
      val tmpMergedAminoAcid = TempFile.createTempFile("fasta")
      writeFasta(tmpMergedAminoAcid.getAbsolutePath, addMaps(inputAminoAcid, outgroupAminoAcid)((x, y) => x), 80)
      muscle(
        MuscleInput(
          Some(SharedFile(tmpMergedAminoAcid, name = outname + "." + gene + ".aminoAcid.unaligned.mergedwithoutgroup.fasta", canMoveAway = true))
        )
      ).?[FastaFile].map(x => (gene, mergedNucleotides, openSource(x.file.file)(s => readFasta(s))))
    })

  }

  def concatenateAndSelectTask(
    alignedWithOutgroup: Seq[(String, FastaSequenceData, FastaSequenceData)],
    referenceNames: Map[String, String],
    positions: Map[String, Set[Int]],
    outname: String
  )(implicit components: TaskSystemComponents, log: akka.event.LoggingAdapter) = {
    import components._
    implicit val fs = components.fs
    implicit val context = components.actorsystem

    val aminoAcids = alignedWithOutgroup.map(x => x._1 -> x._3).toMap
    val nucleotides = alignedWithOutgroup.map(x => x._1 -> x._2).toMap

    concatenateAndSelectPositions(
      ConcatenateAndSelectInput(
        nucleotides = Some(nucleotides.map(x => x._1 -> x._2.toSeq).toSeq),
        alignedAminoAcids = Some(aminoAcids.map(x => x._1 -> x._2.toSeq).toSeq),
        referenceNames = Some(referenceNames.toSeq),
        positions = Some(positions.toSeq),
        outname = Some(outname + ".nucleotides.selected.concatenated.aligned.mergedwithoutgroup.fasta")
      ),
      1,
      1000
    )
  }

  def alignSelectConcatenate(
    nucleotides: Map[String, FastaSequenceData],
    outgroupNucleotides: Map[String, FastaSequenceData],
    outgroupAminoAcids: Map[String, FastaSequenceData],
    referenceNames: Map[String, String],
    referenceAminoAcidSequences: Map[String, String],
    positions: Map[String, Set[Int]],
    outname: String
  )(implicit components: TaskSystemComponents, log: akka.event.LoggingAdapter): Future[(FastaFile, Seq[(String, FastaSequenceData, FastaSequenceData)])] = {
    import components._
    implicit val fs = components.fs
    implicit val context = components.actorsystem

    // muscle the amino acids together with outgroup and merge the nucleotides without align
    val alignedWithOutgroupFuture: Future[Seq[(String, FastaSequenceData, FastaSequenceData)]] = makeAminoacidAlignment(nucleotides, outgroupNucleotides, outgroupAminoAcids, referenceNames, referenceAminoAcidSequences, outname)

    alignedWithOutgroupFuture.flatMap { alignedWithOutgroup =>

      concatenateAndSelectTask(alignedWithOutgroup, referenceNames, positions, outname).?[FastaFile].map(x => x -> alignedWithOutgroup)

    }
  }

  def makeTreeWithOutgroup(
    fasta: FastaFile,
    outname: String,
    threads: Int
  )(implicit components: TaskSystemComponents, log: akka.event.LoggingAdapter): Future[File] = {
    import components._
    implicit val fs = components.fs
    implicit val context = components.actorsystem

    val fastadata = openSource(fasta.file.file)(s => readFasta(s))

    log.info("Number of retained individuals in " + fasta.file.name + " : " + fastadata.size)

    log.info("Number of valid nucleotides in " + fasta.file.name + " : " +
      ({
        val coverage = calculateMissingRate(fastadata)(isNucleotide)
        val (low, high) = coverage.partition(_ < 0.8)
        s"below 80% coverage: ${low.size}, above ${high.size}"
      }))

    // raxml tree
    val raxmltask = raxml(RAxMLInput(Some(fasta.file)), cpu = threads)

    raxmltask.?[NewickTreeFile].map(_.file.file)

  }

  def makeIBS(
    nucleotides: FastaFile,
    threads: Int
  )(implicit components: TaskSystemComponents): Future[File] = {
    import components._
    implicit val fs = components.fs
    implicit val context = components.actorsystem

    sequencetoibsmatrixTask(
      SequenceToIBSInput(
        alignment = Some(nucleotides),
        isNucleotide = Some(true)
      ), 1, 2000
    ).?[GRMGZPair].map { grm =>
        putBesides(grm.grmgz.file -> ".grm.gz", grm.grmid.file -> ".grm.id")
      }
  }

  def makeIBSFromPerGeneIndicators(
    indicators: Frame[String, (String, Int, Char), Option[Double]],
    positions: Map[String, Set[Int]]
  )(implicit log: akka.event.LoggingAdapter): File = {

    val grm = PreprocessSequence.createIBSMatrix(
      indicators.filterIx(x => positions(x._1).contains(x._2))
    )

    val trunk = TempFile.createTempFile("")
    val grmgz = new File(trunk.getAbsolutePath + ".grm.gz")
    val grmid = new File(trunk.getAbsolutePath + ".grm.id")
    openFileWriter(grmgz) { w1 =>
      openFileWriter(grmid) { w2 =>
        GRM.write(grm, w1, w2, (x: String) => x + " 1")
      }
    }

    trunk

  }

  def readGenotypeFileSet(fileset: GenotypeFileSet): Frame[Individual, String, Double] =
    hdfdosage.FileSets.openFileSet(fileset, 0.05, Full) { reader =>
      reader.toLocusIteratorWithGenomicMap(Map()).toFrame.mapColIndex(_.value).filter(_.toVec.variance != 0.0)
    }

  def makeGRMWithLasso(fileset: GenotypeFileSet, phenotype: Series[Individual, Double], covariates: Frame[Individual, String, Double])(implicit log: akka.event.LoggingAdapter): (File, Frame[Individual, String8, Double]) = {
    hdfdosage.FileSets.openFileSet(fileset, 0.05, Full) { reader =>
      makeGRMWithLasso(
        reader.toList,
        phenotype,
        covariates
      )
    }
  }

  def makeGRMWithLasso(
    snps: SNPMajorList,
    phenotype: Series[Individual, Double],
    covariates: Frame[Individual, String, Double]
  )(implicit log: akka.event.LoggingAdapter): (File, Frame[Individual, String8, Double]) = {
    import org.saddle.io.CsvImplicits._

    val data = snps.toLocusIteratorWithGenomicMap(Map()).toFrame.mapColIndex(_.value).filter(_.toVec.variance != 0.0)

    log.info("Samples in genotype file for LASSO: " + data.rowIx.toSeq)
    log.info("Number of samples in genotype file for LASSO: " + data.rowIx.toSeq.size)

    val joined = (data.dropNA rconcat covariates.rdropNA rconcat Frame("PHENO" -> phenotype)).rdropNA

    log.info("number of samples in LASSO: " + joined.numRows)
    log.info("number of variables in LASSO: " + (joined.numCols - 1))

    val cv = PenalizedRegressionWithCrossValidation.crossValidation(
      data = joined,
      covariates = joined.colIx.toSeq.filterNot(_ === "PHENO"),
      yKey = "PHENO",
      mybiotools.stat.DropSample,
      crossValidationMode = KFold(5, 123, 1),
      optimizer = PenalizedRegressionWithCrossValidation.Search1D(
        // FixedBoundsOnLogScale(-20, 20),
        FindBounds,
        MaxSelectedVariables(60),
        BrentGridAndCMAES(CMAES(10), BrentGrid(1, 10))
      ),
      warmStart = true,
      generateCVSamplesOnThFly = false,
      threads = 12,
      unpenalized = "intercept" +: covariates.colIx.toSeq,
      implementation = LASSO,
      standardize = true
    ).get

    log.info("LASSO fit " + cv.toString)

    val variables: Seq[String] = cv._3.toSeq.groupBy(x => math.abs((x._2 * 1000.0).round)).map(_._2.head._1).toSeq

    log.info("LASSO selected: \n" + variables.mkString("\n"))
    log.info("# of LASSO selected: " + variables.size)
    log.info("LASSO coeff: " + cv._3.toSeq.filter(x => x._2 != 0.0 && variables.contains(x._1)).sortBy(x => math.abs(x._2)).mkString("\n"))

    val selected = snps.filter(pd => variables.contains(pd.name.value)).toList

    val grm: Frame[Individual, Individual, Double] = GRM.getGRMFromAutosomes(selected, 1000, 1)._1

    val outtrunk = TempFile.createTempFile("")
    openZippedFileWriter(new File(outtrunk.getAbsolutePath + ".grm.gz")) { grmwriter =>
      openFileWriter(new File(outtrunk.getAbsolutePath + ".grm.id")) { idwriter =>
        GRM.write(grm, grmwriter, idwriter, (x: Individual) => x.toLine)
      }
    }

    outtrunk -> selected.toLocusIteratorWithGenomicMap(Map()).toFrame

  }

  def makeGRMWithPruning(fileset: GenotypeFileSet, javaGRMMemory: Int, ldthreshold: Double, genomicMap: Option[File], outname: String, threads: Int, batchSize: Int, pruneKeepSnps: Set[String], pruneAwaySnps: Set[String])(implicit components: TaskSystemComponents): Future[File] = {
    import components._
    implicit val fs = components.fs
    implicit val context = components.actorsystem
    val input = RecodeInput(
      fileset = fileset,
      parameters = RecodeParameters(
        inputSorted = false,
        recodeToRecessive = false,
        blockSize = 1,
        ldprune = true,
        pruneWindow = 5000000,
        pruneThreshold = ldthreshold,
        pruneKeepSnps = pruneKeepSnps,
        pruneAwaySnps = pruneAwaySnps,
        minimumMAF = 0.0,
        maximumMAF = 1.0,
        grmBatchSize = batchSize,
        maximumMissingnessRate = 0.01,
        includeIndividuals = None
      ),
      genomicMap = genomicMap,
      outname = outname,
      includeSNPs = None
    )
    recode2grm(input, memory = javaGRMMemory, cpu = threads).?[GRMFile] map { grmfile =>
      putBesides(grmfile.gctagz.file -> ".grm.gz", grmfile.gctaid.file -> ".grm.id")
    }
  }

  def makeGRM(trunk: File, extract: Option[File], memory: Int)(implicit components: TaskSystemComponents): Future[File] = {
    import components._
    implicit val fs = components.fs
    implicit val context = components.actorsystem
    val input = GCTAKernelInput(trunk, extract)
    gctakerneltask(input, memory = memory).?[GRMGZPair] map { grmgzpair =>
      putBesides(grmgzpair.grmgz.file -> ".grm.gz", grmgzpair.grmid.file -> ".grm.id")
    }
  }

  // def grmProduct(trunk1: String, trunk2: String, trunkout: String): Unit = {
  //   val grmgz1 = new File(trunk1 + ".grm.gz")
  //   val grmgz2 = new File(trunk2 + ".grm.gz")
  //   val grmid1 = new File(trunk1 + ".grm.id")
  //   val grmid2 = new File(trunk2 + ".grm.id")
  //   val grm1 = openSource(grmgz1) { sgrm =>
  //     openSource(grmid1) { sgrmid =>
  //       GRM.read(sgrm, sgrmid)
  //     }
  //   }

  //   val grm2 = openSource(grmgz2) { sgrm =>
  //     openSource(grmid2) { sgrmid =>
  //       GRM.read(sgrm, sgrmid)
  //     }
  //   }
  //   println("XXX")
  //   val product = grm1 * grm2
  //   println("YYY")

  //   val pca = pcaFromGRM(product)
  //   val total = pca.eigenValues.sum.toDouble

  //   GRM.plotGRMToFile(product, new File(trunkout + ".product.grm.png"))

  //   mybiotools.pcanew.plotPCAResultToFile(pca, 1, 2, new File(trunkout + s"grm.pca.product.12.png"))
  //   mybiotools.pcanew.plotPCAResultToFile(pca, 1, 3, new File(trunkout + s"grm.pca.product.13.png"))
  //   mybiotools.pcanew.plotPCAResultToFile(pca, 2, 3, new File(trunkout + s"grm.pca.product.23.png"))
  //   writeToFile(trunkout + s"grm.pca.product.eigenvalues.txt", pca.eigenValues.sorted.map(_ / total).mkString("\n"))

  //   openZippedFileWriter(new File(trunkout + ".grm.gz")) { grmwriter =>
  //     openFileWriter(new File(trunkout + ".grm.id")) { idwriter =>
  //       GRM.write(product, grmwriter, idwriter)
  //     }
  //   }

  // }

  def runGCTA(grmtrunks: Map[String, File], qcovar: Option[File], covar: Option[File], pheno: File, testLRT: WhichKernelToTest, outname: String, randomizations: Int, bootstraps: Int, resamples: Seq[Int], maxit: Int)(implicit components: TaskSystemComponents) = {

    val lrtstring = "--reml-no-lrt"
    val grms = grmtrunks.map {
      case (name, t) =>
        (new File(t.getAbsolutePath + ".grm.gz"), new File(t.getAbsolutePath + ".grm.id"), name)
    }.toSet

    val input = GCTAResampleInput(
      grms = grms,
      extraArgs = lrtstring + s" --reml-maxit $maxit ",
      outname = outname,
      covarFile = covar,
      qCovarFile = qcovar,
      phenoFile = pheno,
      randomizations = randomizations,
      totallyRandomPhenotypes = randomizations,
      bootstraps = bootstraps,
      resamples = resamples
    )

    val f = gctaresampletask(input).?[GCTAResampleOutput]

    f.map { output =>
      (GCTAResampledResultSummary(output.main.result, output.bootstrapsSummary, output.randomizationsSummary, output.randomPhenotypesSummary, output.resamplesSummary), output)
    }

  }

  def readTreesFromFile(f: File): List[Tree] = mybiotools.openFileReader(f) { reader =>
    (new NewickImporter(reader, true)).iterator.toList
  }

  def readTreesFromString(s: String): List[Tree] = {
    val reader = new java.io.StringReader(s)
    val r = (new NewickImporter(reader, true)).iterator.toList
    reader.close
    r
  }

  def getSumBranchLengthsToRoot(tree: RootedTree, n: Node) = {
    var s = tree.getLength(n)
    var p = tree.getParent(n)
    while (p != null) {
      s += tree.getLength(p)
      p = tree.getParent(p)
    }
    s
  }

  def getCommonAncestorNode(tree: RootedTree, n1: Node, n2: Node): Node = {
    def getNodesToRoot(acc: List[Node]): List[Node] = {
      val p = tree.getParent(acc.head)
      if (p == null) acc
      else getNodesToRoot(p :: acc)
    }
    val line1 = getNodesToRoot(List(n1))
    val line2 = getNodesToRoot(List(n2))

    def getLowestCommon(l1: List[Node], l2: List[Node], acc: Option[Node]): Node = (l1, l2) match {
      case (n1 :: ll1, n2 :: ll2) if n1 == n2 => getLowestCommon(ll1, ll2, Some(n1))
      case _ => acc.get
    }

    getLowestCommon(line1, line2, None)

  }

  def getAverageTreeHeight(tree: RootedTree): Double = {
    Vec(tree.getExternalNodes.map(n => getSumBranchLengthsToRoot(tree, n)).toSeq: _*).mean
  }

  def getMedianTreeHeight(tree: RootedTree): Double = {
    Vec(tree.getExternalNodes.map(n => getSumBranchLengthsToRoot(tree, n)).toSeq: _*).median
  }

  def getMaxTreeHeight(tree: RootedTree): Double = {
    Vec(tree.getExternalNodes.map(n => getSumBranchLengthsToRoot(tree, n)).toSeq: _*).max.get
  }

  def readCovarianceFromTree(tree: RootedTree): Iterator[(String, String, Double)] =
    readCovarianceFromTree(tree, 1.0)

  def readCovarianceFromTree(tree: RootedTree, scale: Double): Iterator[(String, String, Double)] = {
    val tips = tree.getExternalNodes.toList
    (tips.iterator.zipWithIndex.flatMap {
      case (t1, i1) =>
        tips.iterator.zipWithIndex.filter(_._2 <= i1).map { t2 =>
          (t1, t2._1)
        }
    }).flatMap {
      case (n1, n2) =>

        val covariance = getSumBranchLengthsToRoot(tree, getCommonAncestorNode(tree, n1, n2)) * scale
        val name1 = tree.getTaxon(n1).getName
        val name2 = tree.getTaxon(n2).getName

        val r1 = (name1, name2, covariance) :: Nil
        if (n1 != n2) (name2, name1, covariance) :: r1
        else r1
    }
  }

  def iterToFrame(i: Iterator[(String, String, Double)]): Frame[String, String, Double] = {
    Frame(i.toList.groupBy(_._1).toSeq.map(x => (x._1, Series(x._2.map(y => (y._2, y._3)): _*))): _*).sortedRIx.sortedCIx

  }

  private[hivheritability] def getLeavesRecursive(tree: RootedTree, n: Node): Set[Node] = if (tree.isExternal(n)) Set(n) else
    tree.getChildren(n).filter(n => tree.isExternal(n)).toSet ++ tree.getChildren(n).filter(n => !tree.isExternal(n)).flatMap(n => getLeavesRecursive(tree, n))

  def getRootedSubtree(tree: Tree, outgroupTaxonNames: Set[String], disabledTaxa: Set[String]): Option[RootedSubtree] = {
    val outgroupNodes: Set[Node] = tree.getExternalNodes.filter(n => outgroupTaxonNames.contains(tree.getTaxon(n).getName)).toSet
    val rootedTreeAtRandom = Utils.rootTheTree(tree)
    BipartitionSet(rootedTreeAtRandom).largestMonophyleticSplit(outgroupTaxonNames).map { bps =>
      val splittingEdge = bps.edge
      val rootedTree = bps.edge.map { splittingEdge =>
        val (outGroup, inGroup) = {
          val nodes = rootedTreeAtRandom.getNodes(splittingEdge)
          if (getLeavesRecursive(rootedTreeAtRandom, nodes(0)).map(n => rootedTreeAtRandom.getTaxon(n).getName).contains(outgroupTaxonNames.head))
            (nodes(0), nodes(1))
          else (nodes(1), nodes(0))
        }
        new SimpleRootedTree(tree, inGroup, outGroup, 0.0)
      }.getOrElse(rootedTreeAtRandom)

      val inTaxa = tree.getTaxa.filter(x => !outgroupTaxonNames.contains(x.getName) && !disabledTaxa.contains(x.getName))
      val subsettree = new RootedSubtree(rootedTree, inTaxa)
      subsettree
    }
  }

}
