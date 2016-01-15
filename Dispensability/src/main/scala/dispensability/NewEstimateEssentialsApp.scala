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

package dispensability

import mybiotools._
import mybiotools.stat._
import java.io.File
import scala.collection.JavaConversions._
import org.saddle._
import mybiotools.stringstore._
import com.typesafe.config.Config
import mybiotools.tasks._
import dispensability.tasks._
import scala.concurrent.Future
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import org.apache.commons.math3.random.{ MersenneTwister, RandomDataGenerator }
import mybiotools.plots.ScatterPlot._
import mybiotools.plots._
import java.awt.Color
import de.erichseifert.gral.graphics._
import de.erichseifert.gral.plots.colors._
import de.erichseifert.gral.util.Insets2D;
import de.erichseifert.gral.plots.{ Plot, XYPlot };
import de.erichseifert.gral.plots.axes.AxisRenderer
import java.awt.Font
import de.erichseifert.gral.util.{ Orientation, Location, Dimension2D }
import de.erichseifert.gral.plots.legends.Legend
import java.awt.{ Stroke, BasicStroke }
import mybiotools.gwascommons._
import mybiotools.eq._
import mybiotools.stat.RegressionResult
import dispensability.tasks.OptimalParameter
import AbsoluteMutationRateModel.{ Parameters => AbsParam }
import RelativeMutationRateModel.{ Parameters => RelParam }

trait EstimateEssentialConfig2[T <: Annotation[T]] {

  def bootstrap: Int

  def memory: Int

  def output: String

  def maxAffected: Double
  def minAffected: Double
  def hweThreshold: Double

  def variantsWithGenotypeFiles: File
  def antoniosVariantFile: File

  def nutvars: List[File]
  def snpeffvcf: List[File]

  def gmtFiles: List[File]
  def geneNamesToRemoveFromGMT: Set[String]

  // (Name,logistic/linear,includeMissingASZero,file)
  def externalAnnotations: List[(String, PhenotypeScale, Boolean, File)]

  def samochaTable: File
  def annotationFormat: AnnotationFormat
  implicit def annotationScore: AnnotationScore[T]

  def seed: Int
}

class DefaultEstimateEssentialConfig2[T <: Annotation[T]](conf: Config, val annotationScore: AnnotationScore[T]) extends EstimateEssentialConfig2[T] {
  val annotationFormat = conf.getString("newestimateessential.annotationFormat").toLowerCase match {
    case "nutvar" => NutVar
    case "snpeff" => SnpEffVCF
  }
  val seed = conf.getInt("newestimateessential.seed")
  val samochaTable = new File(conf.getString("newestimateessential.samochaTable"))

  val memory = conf.getInt("newestimateessential.memory")
  val output = conf.getString("newestimateessential.output")

  val bootstrap = conf.getInt("newestimateessential.bootstrap")

  val externalAnnotations: List[(String, PhenotypeScale, Boolean, File)] = conf.getStringList("newestimateessential.externalAnnotations").grouped(4).map { g =>
    val scale = PhenotypeScale(g(1))
    val includemissing = g(2) == "yes"
    (g(0), scale, includemissing, new File(g(3)))
  }.toList

  val variantsWithGenotypeFiles: File = new File(conf.getString("newestimateessential.variantcountfile"))

  val antoniosVariantFile: File = new File(conf.getString("newestimateessential.antoniosVariantFile"))

  val nutvars: List[File] = {
    val file = {
      val p = conf.getString("newestimateessential.nutvar")
      if (p != "") Some(new File(p)) else None
    }
    val files = conf.getString("newestimateessential.nutvars")
    if (files != "") file.toList ::: openSource(files)(_.getLines.toList.map(f => new File(f))) else file.toList
  }
  val snpeffvcf: List[File] = {
    val file = {
      val p = conf.getString("newestimateessential.snpeff")
      if (p != "") Some(new File(p)) else None
    }
    val files = conf.getString("newestimateessential.snpeffs")
    if (files != "") file.toList ::: openSource(files)(_.getLines.toList.map(f => new File(f))) else file.toList
  }

  val hweThreshold = conf.getDouble("newestimateessential.hwethreshold")

  val maxAffected = conf.getDouble("newestimateessential.maxAffected")
  val minAffected = conf.getDouble("newestimateessential.minAffected")

  val gmtFiles: List[File] = {
    val file = {
      val p = conf.getString("newestimateessential.gmtfile")
      if (p != "") Some(new File(p)) else None
    }
    val files = conf.getStringList("newestimateessential.gmtfiles").toList.map(f => new File(f))
    file.toList ::: files
  }

  val geneNamesToRemoveFromGMT = {
    val p = conf.getString("newestimateessential.geneNamesToRemoveFromGMT")
    if (p == "") Set[String]() else openSource(p)(_.getLines.toSet)
  }
}

class NewEstimateEssentialRunner[AT <: AnnotationWithGene[AT]](conf: EstimateEssentialConfig2[AT], ts: TaskSystem) {
  import conf._
  import ts._
  val log = ts.getLogger(this)

  def run {
    ts.registerApplicationFileLogger(new File(output + ".logfile"))
    implicit val components = ts.components

    val rnd = new MersenneTwister(seed)

    log.info((mybiotools.config.Config.prettyPrintVersion("NewEstimateEssentialsApp", dispensability.Reflected.version)))
    log.info(mybiotools.config.Config.prettyPrintConfig(mybiotools.config.Config.configInstance, Some("newestimateessential")))

    val variantsPassingAFFilter: Map[VariantKey, VariantWithoutAnnotation] = openSource(variantsWithGenotypeFiles)(s => Input.readVariantCountsFile(s, hweThreshold).toSet).filter((x: VariantWithoutAnnotation) => x.affectIndividualCount <= maxAffected && x.affectIndividualCount >= minAffected).map(x => x.variantkey -> x).toMap

    val synonymCounts: Map[String8, ObservedSynonymCount] = openSource(antoniosVariantFile)(s => Input.getSynonymCounts(s, variantsPassingAFFilter.map(_._1).toSet)).map(x => x._1 -> ObservedSynonymCount(x._2))

    log.info("Genes with >0 synonymous variants: " + synonymCounts.size)

    val allgeneswithexpectedcounts: Seq[(Gene, ExpectedTruncationCount)] = {
      val genes: Seq[Gene] = openSource(samochaTable.getAbsolutePath)(s => Input.readSamochaSupplement(s)).toSeq

      log.info("Genes present in Samocha's table: " + genes.size)

      val commongenes = genes.map(_.hgnc).toSet & synonymCounts.keySet

      log.info("Genes with synonymous variants and Samocha result: " + commongenes.size)

      val (r, slope) = AbsoluteMutationRateModel.calculateNeutralExpectedTruncationsWithRegression(genes.filter(x => commongenes.contains(x.hgnc)), synonymCounts.filter(x => commongenes.contains(x._1)))
      log.info(s"Neutral expectation of the number of truncations of a gene is calculated as alpha*(probStop+probFS), where alpha is the slope of the regression line between the observed synonym count and probSynonym (without intercept). alpha = $slope ")
      log.info("After removing outliers from the regression, number of genes used in the analysis: " + r.size)

      r

    }
    val allgenes = allgeneswithexpectedcounts.map(_._1)

    val genesets: Map[String, Map[GeneSetKey, Set[Gene]]] = gmtFiles.map(f => f.getName -> openSource(f.getAbsolutePath)(s => Input.readGMT(s, allgenes.map(g => g.hgnc -> g).toMap)).filter(_._2.size > 0)).toMap
    val genesetsWithoutKinases: Map[String, Map[GeneSetKey, Set[Gene]]] = genesets.map(x => x._1 -> x._2.map(x => x._1 -> x._2.filterNot(g => geneNamesToRemoveFromGMT.contains(g.hgnc.value))))

    val allVariantsSumGenotypeSeq: Seq[VariantWithSumGenotype[AT]] = {
      val nutvarindex: Input.AnnotationIndex[AT] =
        (annotationFormat match {
          case NutVar => Input.readNutvarOutputs(nutvars, allgenes.map(x => x.name -> x).toMap)
          case SnpEffVCF => Input.readAntonioSnpEffFiles(nutvars, allgenes.map(x => x.name -> x).toMap)
        }).asInstanceOf[Input.AnnotationIndex[AT]]

      Input.attachAnnotation(variantsPassingAFFilter.map(_._2).iterator, nutvarindex)
    }.toSeq

    val expectedAndObservedCounts = DataForAbsoluteModel(allVariantsSumGenotypeSeq.map(_.annotation.gene.hgnc.value), allgeneswithexpectedcounts.map(x => x._1 -> x._2), synonymCounts)

    val expectedAndObservedCountsSevere = DataForAbsoluteModel(
      allVariantsSumGenotypeSeq
        .filter(_.score.get > 0.3)
        .map(_.annotation.gene.hgnc.value),
      allgeneswithexpectedcounts.map(x => x._1 -> x._2),
      synonymCounts
    )

    val expectedAndObservedCountsUniform = DataForAbsoluteModel(
      allVariantsSumGenotypeSeq
        .map(_.annotation.gene.hgnc.value), allgeneswithexpectedcounts.map(x => x._1.copy(probStop = 0.1, probFS = 0.0) -> ExpectedTruncationCount(1.0)), synonymCounts
    )

    val expectedAndObservedHomVarCounts = DataForAbsoluteModel(allVariantsSumGenotypeSeq.filter(_.homVarCount > 0).map(_.annotation.gene.hgnc.value), allgeneswithexpectedcounts.map(x => x._1 -> x._2), synonymCounts)

    val scaledProb = expectedAndObservedCounts.expectedRelativeFrequencies

    val totalSynonymCount = expectedAndObservedCounts.vector.map(_.observedSynonyms.value).sum

    // playground
    {

      // val ml = RelativeMutationRateModel.maximumLikelihood(expectedAndObservedCounts.dataForRelativeModel.rescale)
      // val cdf = RelativeMutationRateModel.cdfFit(expectedAndObservedCounts.dataForRelativeModel.rescale)
      // val ds = RelativeMutationRateModel.downsamplingFit(expectedAndObservedCounts.dataForRelativeModel.rescale, rnd)
      // val ml_truncated = RelativeMutationRateModel.maximumLikelihood(expectedAndObservedCounts.filter(_.observedTruncations.value > 3).dataForRelativeModel.rescale)

      // {
      //   val predictedtruncated = ScatterPlot.Label("ML (>3)", Color.black, circle(2.0), new BasicStroke(1.0f)) -> seq2(RelativeMutationRateModel.predictDensity(ml_truncated.optimum, expectedAndObservedCounts.dataForRelativeModel.rescale).map(x => x._1.toDouble -> x._2))
      //   val predicted = ScatterPlot.Label("ML", Color.gray, circle(2.0), new BasicStroke(1.0f)) -> seq2(RelativeMutationRateModel.predictDensity(ml.optimum, expectedAndObservedCounts.dataForRelativeModel.rescale).map(x => x._1.toDouble -> x._2))
      //   val predictedCDF = ScatterPlot.Label("CDF", Color.red, circle(2.0), new BasicStroke(1.0f)) -> seq2(RelativeMutationRateModel.predictDensity(cdf.optimum, expectedAndObservedCounts.dataForRelativeModel.rescale).map(x => x._1.toDouble -> x._2))
      //   val predictedDS = ScatterPlot.Label("DS", Color.green, circle(2.0), new BasicStroke(1.0f)) -> seq2(RelativeMutationRateModel.predictDensity(ds.optimum, expectedAndObservedCounts.dataForRelativeModel.rescale).map(x => x._1.toDouble -> x._2))

      //   val observedDataHistogram = ScatterPlot.Label("Observed counts", Color.blue, circle(2.0), new BasicStroke(1.0f)) -> HistogramData(expectedAndObservedCounts.observedCounts.filter(_.value <= 80).map(_.value.toDouble), step = 1.0).toScatter

      //   pdfToFile(output + ".tmp.density.mltruncated.pdf", nice(ScatterPlot.createScatterPlotFromMultiple(
      //     data = observedDataHistogram :: predictedtruncated :: predicted :: predictedCDF :: predictedDS :: Nil,
      //     xlab = "Number of truncation",
      //     ylab = "Number of genes",
      //     main = "Histogram of genes with given number of truncations",
      //     xlim = Some(0d -> 30d))))

      // }
      // println(ml)
      // println(ml_truncated)

      // {
      //   import de.erichseifert.gral.graphics._

      //   pdfToFile(new File(output + ".tmp2.pdf"), HistogramPlot.createHistogramPlot(expectedAndObservedCounts.vector.map(x => x.observedTruncations.value / x.expectedTruncations.value)))

      //   val colorMapRed = {
      //     val cm = new HeatMapColors
      //     cm.setRange(0.0, 1.0)
      //     cm
      //   }

      //   val meanParameter = AbsoluteMutationRateModel.maximumLikelihood(expectedAndObservedCounts).optimum //AbsoluteParameters(0.26, -0.59, 0.77)
      //   val model = AbsoluteMutationRateModel
      //   val data = expectedAndObservedCounts

      //   val dataForPlot = (zip(model.predictedCountsPlugBackPosterior(meanParameter, data), data.expectedCounts, data.observedCounts,
      //     model.posteriorOfHaploinsufficient(data, meanParameter)).toVector map {
      //       case (hi, exp: ExpectedTruncationCount, obs: ObservedTruncationCount, posterior) =>
      //         (hi, exp, obs, posterior.pHI)
      //     } filter (x => (x._4 < 0.2 || x._4 > 0.8 || true)) sortBy (x => x._4) zipWithIndex) map (x => (x._1._1, x._1._2, x._1._3, x._1._4, x._2))

      //   pdfToFile(new File(output + ".tmp.pdf"), ScatterPlot.createScatterPlotFromMultiple(
      //     data =
      //       Label("Neutral", Color.green, circle(1.0)) -> seq2(dataForPlot.map(x => (x._5.toDouble, (x._2.value / x._2.value)))) ::
      //         Label("Observed", colorMapRed, circle(1.0)) -> seq3(dataForPlot.map(x => (x._5.toDouble, (x._3.value.toDouble / x._2.value), x._4))) ::
      //         Label("PredictedHI", Color.black, circle(1.0)) -> seq2(dataForPlot.map(x => x._5.toDouble -> (x._1 / x._2.value))) ::
      //         // Label("PredictedHS", Color.blue) -> seq2(dataForPlot.map(x => x._5.toDouble -> x._2 / x._3.value)) ::
      //         Nil,
      //     ylim = Some(-0.1, 1.2)
      //   ))

      //   pdfToFile(new File(output + ".tmp.scatter.pdf"), ScatterPlot.createScatterPlotFromMultiple(
      //     data =
      //       Label("Observed", colorMapRed, circle(1.0)) -> seq3(dataForPlot.map(x => (x._2.value.toDouble, x._3.value.toDouble, x._4))) ::
      //         Nil
      //   ))

      //   pdfToFile(new File(output + ".tmp.scatter2.pdf"), ScatterPlot.createScatterPlotFromMultiple(
      //     data =
      //       Label("Observed", colorMapRed, circle(1.0)) -> seq3(dataForPlot.map(x => (x._1, x._3.value.toDouble, x._4))) ::
      //         Nil
      //   ))

      // }

      // {
      //   import de.erichseifert.gral.graphics._

      //   val colorMapRed = {
      //     val cm = new HeatMapColors
      //     cm.setRange(0.0, 1.0)
      //     cm
      //   }

      //   // val meanParameter = RelativeParameters(0.24, 0.25)
      //   val model = RelativeMutationRateModel
      //   val data = expectedAndObservedCounts.dataForRelativeModel.rescale
      //   val meanParameter = model.maximumLikelihood(data).optimum

      //   val dataForPlot = (zip(model.predictedCountsPlugBackPosterior(meanParameter, data), data.expectedCounts, data.observedCounts,
      //     model.posteriorOfHaploinsufficient(data, meanParameter)).toVector map {
      //       case (hi, exp: ExpectedTruncationCount, obs: ObservedTruncationCount, posterior) =>
      //         (hi, exp, obs, posterior.pHI)
      //     } filter (x => (x._4 < 0.2 || x._4 > 0.8 || true)) sortBy (x => x._4) zipWithIndex) map (x => (x._1._1, x._1._2, x._1._3, x._1._4, x._2))

      //   // println(dataForPlot.map(x => x._1 / x._2.value))

      //   pdfToFile(new File(output + ".tmp.relative.pdf"), ScatterPlot.createScatterPlotFromMultiple(
      //     data =

      //       Label("Observed", colorMapRed, circle(1.0)) -> seq3(dataForPlot.map(x => (x._5.toDouble, (x._3.value.toDouble / x._2.value), x._4))) ::
      //         Label("PredictedHI", Color.black, circle(1.0)) -> seq2(dataForPlot.map(x => x._5.toDouble -> (x._1 / x._2.value))) ::
      //         Label("Neutral", Color.green, circle(1.0)) -> seq2(dataForPlot.map(x => (x._5.toDouble, (x._2.value / x._2.value)))) ::
      //         // Label("PredictedHS", Color.blue) -> seq2(dataForPlot.map(x => x._5.toDouble -> x._2 / x._3.value)) ::
      //         Nil,
      //     ylim = Some(-0.1, 1.5)
      //   ))

      //   pdfToFile(new File(output + ".tmp.relative.scatter.pdf"), ScatterPlot.createScatterPlotFromMultiple(
      //     data =
      //       Label("Observed", colorMapRed, circle(1.0)) -> seq3(dataForPlot.map(x => (x._2.value.toDouble, x._3.value.toDouble, x._4))) ::
      //         Nil
      //   ))

      //   pdfToFile(new File(output + ".tmp.relative.scatter2.pdf"), ScatterPlot.createScatterPlotFromMultiple(
      //     data =
      //       Label("Observed", colorMapRed, circle(1.0)) -> seq3(dataForPlot.map(x => (x._1, x._3.value.toDouble, x._4))) ::
      //         Nil
      //   ))

      // }
      // // log.info("cdf fit: " + cdf)
      // // val predictedHIMutations = expectedAndObservedCounts.expectedCounts.map { e => AbsoluteMutationRateModel.lambdas(e, cdf.optimum).lambdaHI * cdf.optimum.fractionOfEssentials }.sum
      // // val predictedHSMutations = expectedAndObservedCounts.expectedCounts.map { e => AbsoluteMutationRateModel.lambdas(e, cdf.optimum).lambdaHS * (1.0 - cdf.optimum.fractionOfEssentials) }.sum
      // // log.info(predictedHIMutations.toString)
      // // log.info(predictedHSMutations.toString)
      // // log.info("expected HI / expected all " + predictedHIMutations / (predictedHIMutations + predictedHSMutations))
      // // log.info("fraction of 'false' variants" + expectedAndObservedCounts.expectedCounts.map(e => AbsoluteMutationRateModel.lambdas(e, cdf.optimum).lambdaHI).sum / (predictedHIMutations + predictedHSMutations))

      // // Plots.plotModel(model = AbsoluteMutationRateModel,
      // //   data = expectedAndObservedCounts,
      // //   parameters = cdfonlyerror :: Nil,
      // //   xAxisPoints = 1 to 1500000 by 10000 map (i => TotalNumberOfSynonymVariants(i)),
      // //   shortXAxisPoints = 1 to 500000 by 10000 map (i => TotalNumberOfSynonymVariants(i)),
      // //   uniformData = expectedAndObservedCounts,
      // //   homozygousData = expectedAndObservedHomVarCounts,
      // //   seed = 85215473,
      // //   xlab = "Number of synonym variants").foreach {
      // //     case (n, f) =>
      // //       com.google.common.io.Files.move(f, new File(output + ".absolute_only_error." + n + ".pdf"))
      // //   }
      // // log.info(AbsoluteMutationRateModel.cdfFit(expectedAndObservedCounts).toString)
    }
    // playground

    log.info("Total number of (variant,gene) pairs: " + allVariantsSumGenotypeSeq.size)
    log.info("Total number of truncating variants: " + allVariantsSumGenotypeSeq.map(_.variantkey).toSet.size)
    log.info("Total number of genes with at least 1 truncating variant: " + allVariantsSumGenotypeSeq.map(_.annotation.gene).toSet.size)
    log.info("Total number of homvar truncating variants: " + allVariantsSumGenotypeSeq.filter(_.homVarCount > 0).toSet.size)
    log.info("Total number of observed genes with homvar truncating: " + allVariantsSumGenotypeSeq.filter(_.homVarCount > 0).map(_.annotation.gene).toSet.size)
    log.info("Total number of observed genes with severe nutvar truncation: " + allVariantsSumGenotypeSeq.filter(_.score.get > 0.3).map(_.annotation.gene).toSet.size)
    log.info("Total number of synonymous variants: " + totalSynonymCount)

    log.info(s"Number of genes with at least 1 truncation after redistributing ${allVariantsSumGenotypeSeq.size} variants into ${expectedAndObservedCounts.vector.size} genes according to the relative frequencies of the neutral expectation: " + SummaryStat(RelativeSimulation.simulation(
      scaledProb,
      RelativeMutationRateModel.Parameters(0.0, 0.0),
      100, 1, List(expectedAndObservedCounts.totalTruncations), rnd
    ).flatMap(_._2).map(_.value)))

    log.info(s"Number of genes with at least 1 truncation after redistributing ${allVariantsSumGenotypeSeq.filter(_.score.get > 0.3).size} variants into ${expectedAndObservedCounts.vector.size} genes according to the relative frequencies of the neutral expectation: " + SummaryStat(RelativeSimulation.simulation(
      scaledProb,
      RelativeMutationRateModel.Parameters(0.0, 0.0),
      100, 1, List(expectedAndObservedCountsSevere.totalTruncations), rnd
    ).flatMap(_._2).map(_.value)))

    log.info(s"Number of genes with at least 1 truncation after simulating truncating variants according to their neutral expectation:" + SummaryStat(1 to 100 map (i => AbsoluteMutationRateModel.simulate(AbsoluteMutationRateModel.Parameters(1E-200, 1.0, 1.0 - 1E-10), expectedAndObservedCounts.expectedCounts, rnd).count(_.value > 0))))

    log.info("Expected number of truncating variants observed at 80-90-100k individuals:" + JackknifeProjection.projectIndividualsToVariants(allVariantsSumGenotypeSeq, 11546, List(40000, 60000, 70000, 80000, 90000, 100000, 120000, 130000, 140000, 150000)))

    mybiotools.writeToFile(output + ".data.txt", "observed\texpected\tobservedsynonym\tpTrunc\tpSyn\n" + expectedAndObservedCounts.vector.map(_.toLine).mkString("\n"))

    // bootstrap, fit, plot
    {
      val bootstraps = bootstrapFromCounts(BootstrapInputFromCounts(
        data = Some(expectedAndObservedCounts),
        replicas = Some(bootstrap),
        seed = Some(2572347)
      ), memory).?[BootstrappedData]

      val absoluteFits: Future[Seq[(DataForAbsoluteModel, OptimalParameter[AbsParam])]] =
        bootstraps.flatMap { bootstrappedDataList =>
          Future.sequence(bootstrappedDataList.list.map { bd =>
            fitModel(ModelInput[DataForAbsoluteModel, AbsParam](
              data = Some(bd),
              model = Some(AbsoluteMutationRateModelProxy)
            ), memory).?[OptimalParameter[AbsParam]].map(x => bd -> x)
          })
        }

      val absoluteFits_ML: Future[Seq[(DataForAbsoluteModel, OptimalParameter[AbsParam])]] =
        bootstraps.flatMap { bootstrappedDataList =>
          Future.sequence(bootstrappedDataList.list.map { bd =>
            fitModelML(ModelInput[DataForAbsoluteModel, AbsParam](
              data = Some(bd),
              model = Some(AbsoluteMutationRateModelProxy)
            ), memory).?[OptimalParameter[AbsParam]].map(x => bd -> x)
          })
        }

      val absoluteFits_DS: Future[Seq[(DataForAbsoluteModel, OptimalParameter[AbsParam])]] =
        bootstraps.flatMap { bootstrappedDataList =>
          Future.sequence(bootstrappedDataList.list.map { bd =>
            fitModelDownsampling(ModelInput[DataForAbsoluteModel, AbsParam](
              data = Some(bd),
              model = Some(AbsoluteMutationRateModelProxy)
            ), memory).?[OptimalParameter[AbsParam]].map(x => bd -> x)
          })
        }

      val absoluteFitsNoPenetrance: Future[Seq[(DataForAbsoluteModel, OptimalParameter[AbsParam])]] =
        bootstraps.flatMap { bootstrappedDataList =>
          Future.sequence(bootstrappedDataList.list.map { bd =>
            fitModelNoPenetrance(ModelInput[DataForAbsoluteModel, AbsParam](
              data = Some(bd),
              model = Some(AbsoluteMutationRateModelProxy)
            ), memory).?[OptimalParameter[AbsParam]].map(x => bd -> x)
          })
        }

      absoluteFitsNoPenetrance.foreach { l =>

        log.info("Absolute model fit assuming full penetrance: " + "\n\t phi=" +
          SummaryStat(l.map(_._2.optimum.fractionOfEssentials)) + "\n\t r=" +
          SummaryStat(l.map(_._2.optimum.falseRate)) + "\n\t pen=" +
          SummaryStat(l.map(_._2.optimum.penetrance)))
      }

      val absoluteDescriptions = absoluteFits.map { l =>

        log.info("Absolute model fit: " + "\n\t phi=" +
          SummaryStat(l.map(_._2.optimum.fractionOfEssentials)) + "\n\t r=" +
          SummaryStat(l.map(_._2.optimum.falseRate)) + "\n\t pen=" +
          SummaryStat(l.map(_._2.optimum.penetrance)))

        log.info("Absolute model, noisy fraction of observed truncations: " + SummaryStat(l.map(p => AbsoluteMutationRateModel.predictFractionOfNoisyObservedTruncations(p._2.optimum, p._1))))

        log.info("Absolute model, fraction of total observed truncations in HI: " + SummaryStat(l.map(p => AbsoluteMutationRateModel.predictFractionOfObservedTruncationsWhichAreInHIGenes(p._2.optimum, p._1))))

        log.info("Absolute model, fraction of observed truncations which are in HI and can't be attributed to 3% error rate: " + SummaryStat(l.map(p => AbsoluteMutationRateModel.predictFractionOfObservedTruncationsWhichDoesNotOriginateFromGivenErrorLevelAndAreInHI(p._2.optimum, p._1, 0.03))))

        val classifications = GeneDescription.makeClassifications(
          expectedAndObservedCounts,
          AbsoluteMutationRateModel,
          l.map(_._2),
          4565236,
          output + ".absolute",
          expectedAndObservedCounts
        )

        GeneDescription.descriptiveAnalysis(
          externalAnnotations,
          genesets,
          expectedAndObservedCounts,
          output + ".absolute",
          classifications
        )

        GeneDescription.descriptiveAnalysis(
          externalAnnotations,
          genesetsWithoutKinases,
          expectedAndObservedCounts,
          output + ".absolute_withoutkinases",
          classifications
        )
      }

      val relativeFits: Future[Seq[(ScaledProbabilitiesWithCounts, OptimalParameter[RelParam])]] =
        bootstraps.flatMap { bootstrappedDataList =>
          Future.sequence(bootstrappedDataList.list.map { bd =>
            fitModel(ModelInput[ScaledProbabilitiesWithCounts, RelParam](
              data = Some(bd.dataForRelativeModel.rescale),
              model = Some(RelativeMutationRateModelProxy)
            ), memory).?[OptimalParameter[RelParam]].map(x => bd.dataForRelativeModel.rescale -> x)
          })
        }

      val relativeFits_ML: Future[Seq[(ScaledProbabilitiesWithCounts, OptimalParameter[RelParam])]] =
        bootstraps.flatMap { bootstrappedDataList =>
          Future.sequence(bootstrappedDataList.list.map { bd =>
            fitModelML(ModelInput[ScaledProbabilitiesWithCounts, RelParam](
              data = Some(bd.dataForRelativeModel.rescale),
              model = Some(RelativeMutationRateModelProxy)
            ), memory).?[OptimalParameter[RelParam]].map(x => bd.dataForRelativeModel.rescale -> x)
          })
        }

      val relativeFits_DS: Future[Seq[(ScaledProbabilitiesWithCounts, OptimalParameter[RelParam])]] =
        bootstraps.flatMap { bootstrappedDataList =>
          Future.sequence(bootstrappedDataList.list.map { bd =>
            fitModelDownsampling(ModelInput[ScaledProbabilitiesWithCounts, RelParam](
              data = Some(bd.dataForRelativeModel.rescale),
              model = Some(RelativeMutationRateModelProxy)
            ), memory).?[OptimalParameter[RelParam]].map(x => bd.dataForRelativeModel.rescale -> x)
          })
        }

      val relativeFitComparisons: Future[List[SharedFile]] = for {
        cdf <- relativeFits
        ml <- relativeFits_ML
        ds <- relativeFits_DS
      } yield {
        {

          val (tmplog, tmpnotlog) = Plots.plotPredictionOfDifferentMethods(
            RelativeMutationRateModel,
            expectedAndObservedCounts.dataForRelativeModel.rescale,
            Map(
              ScatterPlot.Label("Prediction - CDF fit", Color.green, circle(2.0), new BasicStroke(1.0f), 1) -> cdf.map(_._2.optimum),
              ScatterPlot.Label("Prediction - ML fit", Color.red, circle(2.0), new BasicStroke(1.0f), 2) -> ml.map(_._2.optimum),
              ScatterPlot.Label("Prediction - direct fit", Color.blue, circle(2.0), new BasicStroke(1.0f), 3) -> ds.map(_._2.optimum)
            )
          )

          SharedFile(tmpnotlog, "relative.predictions.comparisonOfFits.pdf", canMoveAway = true) ::
            SharedFile(tmplog, "relative.predictions.comparisonOfFits.log.pdf", canMoveAway = true) :: Nil

        }
      }

      val relativeFitsNoPenetrance: Future[Seq[OptimalParameter[RelParam]]] =
        bootstraps.flatMap { bootstrappedDataList =>
          Future.sequence(bootstrappedDataList.list.map { bd =>
            fitModelNoPenetrance(ModelInput[ScaledProbabilitiesWithCounts, RelParam](
              data = Some(bd.dataForRelativeModel.rescale),
              model = Some(RelativeMutationRateModelProxy)
            ), memory).?[OptimalParameter[RelParam]]
          })
        }
      relativeFitsNoPenetrance.foreach { l =>

        log.info("Relative model fit assuming full penetrance: " + "\n\t phi=" +
          SummaryStat(l.map(_.optimum.fractionOfEssentials)) + "\n\t pf=" +
          SummaryStat(l.map(_.optimum.noise)))
      }

      val relativeDescriptions = relativeFits.map { l =>

        log.info("Relative model fit: " + "\n\t phi=" +
          SummaryStat(l.map(_._2.optimum.fractionOfEssentials)) + "\n\t pf=" +
          SummaryStat(l.map(_._2.optimum.noise))
          .toString)

        log.info("Relative model, fraction of total observed truncations which are in HI: " + SummaryStat(l.map(p => RelativeMutationRateModel.predictFractionOfObservedTruncationsWhichAreInHIGenes(p._2.optimum, p._1))))
        log.info("Relative model, fraction of observed truncations which are in HI and can't be attributed to 3% error rate: " + SummaryStat(l.map(p => RelativeMutationRateModel.predictFractionOfObservedTruncationsWhichDoesNotOriginateFromGivenErrorLevelAndAreInHI(p._2.optimum, p._1, 0.03))))

        log.info("Relative model, predictFractionOfGenesWithAtLeastOneTruncationNotFromThisErrorRate 2%: " + SummaryStat(l.map(p => RelativeMutationRateModel.predictFractionOfGenesWithAtLeastOneTruncationNotFromThisErrorRate(p._2.optimum, p._1, 0.02, TotalNumberOfTruncations(90000))).map(_.value)))
        log.info("Relative model, predictFractionOfGenesWithAtLeastOneTruncationNotFromThisErrorRate 6%: " + SummaryStat(l.map(p => RelativeMutationRateModel.predictFractionOfGenesWithAtLeastOneTruncationNotFromThisErrorRate(p._2.optimum, p._1, 0.06, TotalNumberOfTruncations(90000))).map(_.value)))

        log.info("Relative model, predictFractionOfGenesWithAtLeastOneTruncationFromThisErrorRate 2%: " + SummaryStat(l.map(p => RelativeMutationRateModel.predictFractionOfGenesWithAtLeastOneTruncationFromThisErrorRate(p._2.optimum, p._1, 0.02, TotalNumberOfTruncations(90000))).map(_.value)))
        log.info("Relative model, predictFractionOfGenesWithAtLeastOneTruncationFromThisErrorRate 6%: " + SummaryStat(l.map(p => RelativeMutationRateModel.predictFractionOfGenesWithAtLeastOneTruncationFromThisErrorRate(p._2.optimum, p._1, 0.06, TotalNumberOfTruncations(90000))).map(_.value)))

        val classifications = GeneDescription.makeClassifications(
          expectedAndObservedCounts.dataForRelativeModel.rescale,
          RelativeMutationRateModel,
          l.map(_._2),
          4565236,
          output + ".relative",
          expectedAndObservedCounts
        )

        GeneDescription.descriptiveAnalysis(
          externalAnnotations,
          genesets,
          expectedAndObservedCounts.dataForRelativeModel.rescale,
          output + ".relative",
          classifications
        )

        GeneDescription.descriptiveAnalysis(
          externalAnnotations,
          genesetsWithoutKinases,
          expectedAndObservedCounts.dataForRelativeModel.rescale,
          output + ".relative_withoutkinases",
          classifications
        )
      }

      val absolutePlots: Future[PlotResult] = absoluteFitsNoPenetrance.flatMap { paramlistNoPenetrance =>
        absoluteFits.flatMap { paramlist =>

          plotModel(PlotModelInput(
            model = Some(AbsoluteMutationRateModelProxy),
            data = Some(expectedAndObservedCounts),
            parameters = Some(paramlist.map(_._2)),
            parametersNoNoise = Some(paramlistNoPenetrance.map(_._2)),
            xAxisPoints = Some(1 to 1500000 by 10000 map (i => TotalNumberOfSynonymVariants(i))),
            shortXAxisPoints = Some(1 to 500000 by 10000 map (i => TotalNumberOfSynonymVariants(i))),
            uniformData = Some(expectedAndObservedCounts),
            severeData = Some(expectedAndObservedCountsSevere),
            homozygousData = Some(expectedAndObservedHomVarCounts),
            seed = Some(85215473),
            outname = Some("absolute"),
            xlab = Some("Number of synonym variants")
          ), memory).?[PlotResult]

        }
      }

      val absolutePlots_ML: Future[PlotResult] =
        absoluteFits_ML.flatMap { paramlist =>

          plotModel(PlotModelInput(
            model = Some(AbsoluteMutationRateModelProxy),
            data = Some(expectedAndObservedCounts),
            parameters = Some(paramlist.map(_._2)),
            parametersNoNoise = Some(paramlist.map(_._2)),
            xAxisPoints = Some(1 to 1500000 by 10000 map (i => TotalNumberOfSynonymVariants(i))),
            shortXAxisPoints = Some(1 to 500000 by 10000 map (i => TotalNumberOfSynonymVariants(i))),
            uniformData = Some(expectedAndObservedCounts),
            severeData = Some(expectedAndObservedCountsSevere),
            homozygousData = Some(expectedAndObservedHomVarCounts),
            seed = Some(85215473),
            outname = Some("absolute_ML"),
            xlab = Some("Number of synonym variants")
          ), memory).?[PlotResult]

        }

      val absolutePlots_DS: Future[PlotResult] =
        absoluteFits_DS.flatMap { paramlist =>

          plotModel(PlotModelInput(
            model = Some(AbsoluteMutationRateModelProxy),
            data = Some(expectedAndObservedCounts),
            parameters = Some(paramlist.map(_._2)),
            parametersNoNoise = Some(paramlist.map(_._2)),
            xAxisPoints = Some(1 to 1500000 by 10000 map (i => TotalNumberOfSynonymVariants(i))),
            shortXAxisPoints = Some(1 to 500000 by 10000 map (i => TotalNumberOfSynonymVariants(i))),
            uniformData = Some(expectedAndObservedCounts),
            severeData = Some(expectedAndObservedCountsSevere),
            homozygousData = Some(expectedAndObservedHomVarCounts),
            seed = Some(85215473),
            outname = Some("absolute_DS"),
            xlab = Some("Number of synonym variants")
          ), memory).?[PlotResult]

        }

      val relativePlots = relativeFitsNoPenetrance.flatMap { paramlistNoPenetrance =>
        relativeFits.flatMap { paramlist =>

          plotModel(PlotModelInput(
            model = Some(RelativeMutationRateModelProxy),
            data = Some(expectedAndObservedCounts.dataForRelativeModel.rescale),
            parameters = Some(paramlist.map(_._2)),
            parametersNoNoise = Some(paramlistNoPenetrance),
            xAxisPoints = Some(1 to 200000 by 500 map (i => TotalNumberOfTruncations(i))),
            shortXAxisPoints = Some(1 to 50000 by 250 map (i => TotalNumberOfTruncations(i))),
            uniformData = Some(expectedAndObservedCountsUniform.dataForRelativeModel.rescale),
            severeData = Some(expectedAndObservedCountsSevere.dataForRelativeModel.rescale),
            homozygousData = Some(expectedAndObservedHomVarCounts.dataForRelativeModel.rescale),
            seed = Some(seed),
            outname = Some("relative"),
            xlab = Some("Number of truncating variants")
          ), memory).?[PlotResult]

        }
      }

      val relativePlots_ML =
        relativeFits_ML.flatMap { paramlist =>

          plotModel(PlotModelInput(
            model = Some(RelativeMutationRateModelProxy),
            data = Some(expectedAndObservedCounts.dataForRelativeModel.rescale),
            parameters = Some(paramlist.map(_._2)),
            parametersNoNoise = Some(paramlist.map(_._2)),
            xAxisPoints = Some(1 to 200000 by 500 map (i => TotalNumberOfTruncations(i))),
            shortXAxisPoints = Some(1 to 50000 by 250 map (i => TotalNumberOfTruncations(i))),
            uniformData = Some(expectedAndObservedCountsUniform.dataForRelativeModel.rescale),
            severeData = Some(expectedAndObservedCountsSevere.dataForRelativeModel.rescale),
            homozygousData = Some(expectedAndObservedHomVarCounts.dataForRelativeModel.rescale),
            seed = Some(seed),
            outname = Some("relative_ML"),
            xlab = Some("Number of truncating variants")
          ), memory).?[PlotResult]

        }

      val relativePlots_DS =
        relativeFits_DS.flatMap { paramlist =>

          plotModel(PlotModelInput(
            model = Some(RelativeMutationRateModelProxy),
            data = Some(expectedAndObservedCounts.dataForRelativeModel.rescale),
            parameters = Some(paramlist.map(_._2)),
            parametersNoNoise = Some(paramlist.map(_._2)),
            xAxisPoints = Some(1 to 200000 by 500 map (i => TotalNumberOfTruncations(i))),
            shortXAxisPoints = Some(1 to 50000 by 250 map (i => TotalNumberOfTruncations(i))),
            uniformData = Some(expectedAndObservedCountsUniform.dataForRelativeModel.rescale),
            severeData = Some(expectedAndObservedCountsSevere.dataForRelativeModel.rescale),
            homozygousData = Some(expectedAndObservedHomVarCounts.dataForRelativeModel.rescale),
            seed = Some(seed),
            outname = Some("relative_DS"),
            xlab = Some("Number of truncating variants")
          ), memory).?[PlotResult]

        }

      Await.result(Future.sequence(relativeFitComparisons :: relativePlots :: relativePlots_ML :: relativePlots_DS :: absolutePlots :: absolutePlots_DS :: absolutePlots_ML :: absoluteDescriptions :: relativeDescriptions :: Nil), atMost = 168 hours)

    }

    // poisson vs simulation
    {

    }

  }

}

object NewEstimateEssentialApp extends App {
  val ts: TaskSystem = try {
    defaultTaskSystem
  } catch {
    case e: Throwable => { e.printStackTrace; println(e.getMessage); System.exit(1); ().asInstanceOf[TaskSystem] }
  }

  if (ts.hostConfig.myRole == mybiotools.tasks.MASTER) {
    val runner = mybiotools.config.Config.configInstance.getString("newestimateessential.annotationFormat") match {
      case "nutvar" => {
        new NewEstimateEssentialRunner[NutVarAnnotation](
          new DefaultEstimateEssentialConfig2(mybiotools.config.Config.configInstance, NutVarAnnotationScores.NUTvar_ProbabilityPathogenic),
          ts
        )
      }

      case "snpeff" => {
        new NewEstimateEssentialRunner[SnpEffAnnotation](
          new DefaultEstimateEssentialConfig2(mybiotools.config.Config.configInstance, NutVarAnnotationScores.SnpEffAnnotationScore),
          ts
        )
      }
    }

    runner.run
    ts.shutdown
  }

}