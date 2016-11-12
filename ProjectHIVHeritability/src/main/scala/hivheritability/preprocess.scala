package hivheritability

import mybiotools.sequence.indexInGapped
import mybiotools.sequencedistance._
import mybiotools.pcanew._
import java.io.File
import org.saddle._
import mybiotools.gwascommons.genotypedata.GRM
import mybiotools._
import mybiotools.stat._
import mybiotools.sequence._
import mybiotools.workflows._

import mybiotools._
import mybiotools.tasks._
import java.io.File
import akka.actor.{ ActorRefFactory }
import scala.util.Try
import mybiotools.stat.ZMetaCombination._
import hivheritability._
import hdfdosage.FileSets._
import com.typesafe.config.ConfigFactory
import mybiotools.gwascommons._
import scala.collection.JavaConversions._
import org.saddle._
import org.saddle.scalar._
import mybiotools.workflows.{ MakeBedOut, BimFile, BedFile, FamFile }
import mybiotools.eq._
import mybiotools.sequence._

object PreprocessSequence {

  def dropMissingIndividualsAndColumns(alignedFasta: FastaSequenceData, factor: Double)(filter: Char => Boolean): FastaSequenceData = {
    val commonNucleotides: String = makeConsensus(
      alignment = alignedFasta.map(x => x._1 -> x._2.map(c => if (filter(c)) 'A' else 'N')),
      threshold = factor,
      noCall = 'N',
      missingNucleotides = List('X', '*', 'N', '-')
    )

    val commonNucleotidesIndices = 0 until commonNucleotides.size filter (i => commonNucleotides(i) === 'A')

    alignedFasta.filter(x =>
      commonNucleotidesIndices.filter(i => filter(x._2(i))).size >= commonNucleotidesIndices.size * factor).map(x => x._1 -> commonNucleotidesIndices.map(x._2).mkString)
  }

  def createSelectedConcatenatedSequence(
    nucleotides: Map[String, FastaSequenceData],
    alignedAminoAcids: Map[String, FastaSequenceData],
    referenceNames: Map[String, String],
    positions: Map[String, Set[Int]],
    nameforlog: String,
    threads: Int
  )(implicit log: akka.event.LoggingAdapter): FastaSequenceData = {
    val selectedPositions: Seq[FastaSequenceData] = nucleotides.toSeq.map {
      case (gene, nuc) =>
        val aa = alignedAminoAcids(gene)
        val ref = referenceNames(gene)
        val select = positions(gene)
        selectPositions(
          nucleotideFasta = nuc,
          alignedAminoAcidFasta = aa,
          referenceName = ref,
          selectAminoAcidColumns = select
        )
    }

    val concatenatedSelected = concatenateFastas(selectedPositions, 'N').map(x => x._1 -> x._2.toUpperCase)

    concatenatedSelected
  }

  def createIBSMatrix(
    sequenceAlignment: FastaSequenceData,
    filter: Char => Boolean,
    nameforlog: String
  )(implicit log: akka.event.LoggingAdapter): Frame[String, String, Double] = {

    val indicators: Frame[String, (Int, Char), Option[Double]] =
      transformToIndicators(sequenceAlignment)(filter)
        .mapValues(x => x.map(y => if (y) 1.0 else 0.0)).sortedCIx.sortedRIx

    log.info(s"Number of indicators $nameforlog: " + indicators.numCols)

    createIBSMatrix(indicators)
  }

  def createIBSMatrix[T](
    indicators: Frame[String, T, Option[Double]]
  )(implicit ST: ST[T], ord: Ordering[T]): Frame[String, String, Double] = {

    val standardized: Frame[String, T, Double] = Frame(indicators.toColSeq.map {
      case (colid, col) =>
        val mean = col.filter(_.isDefined).mapValues(_.get).mean
        val variance = col.filter(_.isDefined).mapValues(_.get).variance
        colid -> (col.mapValues(x => if (x.isDefined) x.get - mean else 0.0)) / math.sqrt(variance)
    }: _*)

    val pairwisenonmissing: Frame[String, String, Double] = {
      val missing: Frame[String, T, Double] = indicators.map(x => (x._1, x._2, if (x._3.isEmpty) 0.0 else 1.0))
      val m = missing.toMat
      Frame(m mult m.T, missing.rowIx, missing.rowIx)
    }

    val keepsamples: Seq[String] =
      pairwisenonmissing.toColSeq.filter(_._2.toVec.countif(_ == 0.0) == 0).map(_._1)

    val m = standardized.toMat
    (Frame(m mult m.T, standardized.rowIx, standardized.rowIx) / pairwisenonmissing).filterIx(s => keepsamples.contains(s)).rfilterIx(s => keepsamples.contains(s))

  }

  def selectIndicatorsWithLASSOFromPerGeneAlignments(
    aminoAcids: Map[String, FastaSequenceData],
    filter: Char => Boolean,
    referenceNames: Map[String, String],
    phenotype: Series[Individual, Double],
    covariates: Frame[Individual, String, Double]
  )(implicit log: akka.event.LoggingAdapter): Frame[String, (String, Int, Char), Option[Double]] = {
    aminoAcids.map {
      case (gene, alignment) =>
        PreprocessSequence.selectIndicatorsWithLASSO(
          alignment,
          referenceNames(gene),
          filter,
          phenotype,
          covariates
        ).mapColIndex(x => (gene, x._1, x._2))
    }.reduce(_ rconcat _)
  }

  def maskMissing(alignment: FastaSequenceData, filter: Char => Boolean, nameforlog: String)(implicit log: akka.event.LoggingAdapter): FastaSequenceData = {

    val clean1 = alignment.filter { x =>
      x._2.count(filter) / x._2.size.toDouble >= 0.60
    }
    log.info(s"clean/mask missing. $nameforlog. original " + alignment.size + " clean1 " + clean1.size)

    if (clean1.size == 0) clean1
    else {
      val missingCol1 = calculateMissingRate(clean1)(filter).zipWithIndex.filter(_._1 < 0.9).map(_._2).toSet
      val clean2 = maskColumnsOfAlignment(clean1, missingCol1)

      val coverage = calculateCoverage(clean2).map(_.maxBy(_._2)._1)
      clean2.map(x => x._1 -> x._2.zipWithIndex.map(y => (if (!filter(y._1)) coverage(y._2) else y._1)).mkString)
    }

  }

  def selectIndicatorsWithLASSO(
    sequenceAlignment: FastaSequenceData,
    referenceName: String,
    filter: Char => Boolean,
    phenotype: Series[Individual, Double],
    covariates: Frame[Individual, String, Double]
  )(implicit log: akka.event.LoggingAdapter): Frame[String, (Int, Char), Option[Double]] = {
    import org.saddle.io.CsvImplicits._

    val data: Frame[String, String, Option[Double]] =
      fasta2indicators(sequenceAlignment, referenceName)(isAminoAcid)
        .mapValues(x => x.map(y => if (y) 1.0 else 0.0)).mapColIndex(x => "X" + x._1 + "_" + x._2).mapRowIndex(_ + "_1").sortedCIx.sortedRIx

    val joined = (
      data.mapValues(_.getOrElse(Double.NaN)).filter(_.toVec.variance != 0.0)
      rconcat covariates.mapRowIndex(_.toString)
      rconcat Frame("PHENO" -> phenotype).mapRowIndex(_.toString)
    ).rdropNA

    val cv = PenalizedRegressionWithCrossValidation.crossValidation(
      data = joined,
      covariates = joined.colIx.toSeq.filterNot(_ === "PHENO"),
      yKey = "PHENO",
      mybiotools.stat.DropSample,
      crossValidationMode = KFold(4, 123, 1),
      optimizer = PenalizedRegressionWithCrossValidation.Search1D(
        FindBounds,
        MinimumPredictionError,
        BrentGridAndCMAES(CMAES(50), BrentGrid(1, 20))
      ),
      warmStart = true,
      generateCVSamplesOnThFly = false,
      threads = 12,
      unpenalized = "intercept" +: covariates.colIx.toSeq,
      implementation = LASSO
    ).get

    log.info("LASSO fit " + cv.toString)

    val variables = cv._2.filterNot(v => covariates.colIx.contains(v))

    data.col(variables.distinct: _*).mapColIndex { x =>
      val spl = x.drop(1).split("_")
      (spl(0).toInt -> spl(1).head)
    }

  }

}

case class ConcatenateAndSelectInput(
  nucleotides: Option[Seq[(String, Seq[(String, String)])]],
  alignedAminoAcids: Option[Seq[(String, Seq[(String, String)])]],
  referenceNames: Option[Seq[(String, String)]],
  positions: Option[Seq[(String, Set[Int])]],
  outname: Option[String]
) extends SimplePrerequisitive[ConcatenateAndSelectInput]

object concatenateAndSelectPositions {

  def apply(
    in: ConcatenateAndSelectInput,
    cpu: Int,
    memory: Int,
    update: UpdatePrerequisitive[ConcatenateAndSelectInput] = identity[ConcatenateAndSelectInput]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = cpu, memory = memory)
    ) {

      case (ConcatenateAndSelectInput(
        Some(nucleotides),
        Some(alignedAminoAcids),
        Some(referenceNames),
        Some(positions),
        Some(outname)
        ),
        ce) =>
        import ce._
        val fastadata = PreprocessSequence.createSelectedConcatenatedSequence(
          nucleotides.map(x => x._1 -> x._2.toMap).toMap,
          alignedAminoAcids.map(x => x._1 -> x._2.toMap).toMap,
          referenceNames.toMap,
          positions.toMap,
          outname,
          resourceAllocated.cpu
        )(log)

        val tmp = TempFile.createTempFile(".fasta")
        writeFasta(tmp.getAbsolutePath, fastadata, 80)
        FastaFile(SharedFile(tmp, name = outname, canMoveAway = true))
    }
}

case class SequenceToIBSInput(
  alignment: Option[FastaFile],
  isNucleotide: Option[Boolean]
) extends SimplePrerequisitive[SequenceToIBSInput]

object sequencetoibsmatrixTask {

  def apply(
    in: SequenceToIBSInput,
    cpu: Int,
    memory: Int,
    update: UpdatePrerequisitive[SequenceToIBSInput] = identity[SequenceToIBSInput]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = cpu, memory = memory)
    ) {

      case (SequenceToIBSInput(
        Some(fasta),
        Some(isNucleotide)
        ),
        ce) =>
        import ce._

        val sequenceAlignment = openSource(fasta.file.file)(s => readFasta(s))

        val aminoAcidAlignment = if (isNucleotide) translateFasta(sequenceAlignment, true) else sequenceAlignment

        val grm = PreprocessSequence.createIBSMatrix(
          aminoAcidAlignment,
          isAminoAcid,
          fasta.file.name
        )
        val trunk = TempFile.createTempFile("")
        val grmgz = new File(trunk.getAbsolutePath + ".grm.gz")
        val grmid = new File(trunk.getAbsolutePath + ".grm.id")
        openFileWriter(grmgz) { w1 =>
          openFileWriter(grmid) { w2 =>
            GRM.write(grm, w1, w2, (x: String) => x + " 1")
          }
        }
        GRMGZPair(
          SharedFile(grmgz, name = fasta.file.name + ".ibs.grm.gz", canMoveAway = true),
          SharedFile(grmid, name = fasta.file.name + ".ibs.grm.id", canMoveAway = true),
          fasta.file.name + ".ibs"
        )
    }
}

case class TwoFastas(
  g2g: Option[FastaFile],
  nog2g: Option[FastaFile],
  threhsold: Option[Double]
) extends SimplePrerequisitive[TwoFastas]

object pruneSecondFasta {

  def apply(
    in: TwoFastas,
    cpu: Int,
    memory: Int,
    update: UpdatePrerequisitive[TwoFastas] = identity[TwoFastas]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = cpu, memory = memory)
    ) {

      case (TwoFastas(
        Some(g2g),
        Some(nog2g),
        Some(sequenceLDPruneThreshold)
        ),
        ce) =>
        import ce._

        val fasta = mybiotools.sequence.maskPositionsInSecondWhichAreCorrelatedInTheFirst(
          openSource(g2g.file.file)(s => readFasta(s)),
          openSource(nog2g.file.file)(s => readFasta(s)),
          sequenceLDPruneThreshold
        )
        val tmp = TempFile.createTempFile(".fasta")
        writeFasta(tmp.getAbsolutePath, fasta, 80)
        FastaFile(SharedFile(tmp, nog2g.file.name + ".pruned.fasta", canMoveAway = true))

    }
}

