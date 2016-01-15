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

package rnaseqalign.analysis

import mybiotools._
import mybiotools.pcanew.PCAResultFrames
import org.saddle._
import org.saddle.io.CsvImplicits._
import mybiotools.plots._
import ScatterPlot._
import de.erichseifert.gral.graphics.Drawable
import java.io.File
import scala.util.Try
import org.apache.commons.math3.ml.distance._

/* concentration is in attomoles/microliter */
case class SpikeIn(id: String, concentrationMix1: Double, concentrationMix2: Double)

sealed trait SpikeInMix
case object Mix1 extends SpikeInMix
case object Mix2 extends SpikeInMix

case class QuickReportInFiles(
  normalizedOnLibrary: File,
  normalizedOnLibraryAndTranscriptLength: File,
  rpkm: File,
  counts: File,
  varianceStabilizedByLogNormalizedOnLibrary: File,
  totalReadsPerSample: File,
  detectedGenesNormalizedOnLibrarySize: File,
  meanVarPlotAfterVST: File,
  meanVarPlotAfterVSTPerBP: File,
  versusPlotsNormalizedOnLibraryDepth: List[File],
  pcaplots: List[(String, (File, File, Seq[(String, File, File, File)], File, Seq[(Int, File)]))],
  genewisepca: File,
  readCountPerBPHistogramNormalizedOnLibraryDepth: File,
  readCountPerBPHistogramExpressedNormalizedOnLibraryDepth: File,
  readCountRaw: File,
  sizeFactors: File,
  spikeInDiagnosticPlots: List[(String, Seq[File])],
  detailedPlot: Seq[File],
  houseKeepingGenes: File,
  geneClusters: List[(String, Seq[File], File)]
)

case class QuickReport(
    normalizedOnLibrary: Frame[String, String, Double],
    normalizedOnLibraryAndTranscriptLength: Frame[String, String, Double],
    rpkm: Frame[String, String, Double],
    counts: Frame[String, String, Long],
    varianceStabilizedByLogNormalizedOnLibrary: Frame[String, String, Double],
    totalReadsPerSample: Series[String, Long],
    detectedGenesNormalizedOnLibrarySize: Int,
    meanVarPlotAfterVST: Drawable,
    meanVarPlotAfterVSTPerBP: Drawable,
    versusPlotsNormalizedOnLibraryDepth: Seq[Drawable],
    pcaplots: List[(String, Drawable, PCAResultFrames[String, String], Seq[(Int, EnrichmentResultPlain)], Seq[(Int, Drawable)])],
    genewisepca: Drawable,
    readCountPerBPHistogramNormalizedOnLibraryDepth: Drawable,
    readCountPerBPHistogramExpressedNormalizedOnLibraryDepth: Drawable,
    readCountRaw: Drawable,
    sizeFactors: Series[String, Double],
    spikeInDiagnosticPlots: List[(String, List[Drawable])],
    detailedPlots: Seq[Drawable],
    normalization: NormalizationMethod,
    geneClusters: List[(String, Seq[Drawable], Seq[Set[String]])]
) {

  def toFiles = {
    val normalizedOnLibraryFile = TempFile.createTempFile(".csv")
    normalizedOnLibrary.writeCsvFile(normalizedOnLibraryFile.getAbsolutePath, withColIx = true, withRowIx = true)

    val normalizedOnLibraryAndTranscriptLengthFile = TempFile.createTempFile(".csv")
    normalizedOnLibraryAndTranscriptLength.writeCsvFile(normalizedOnLibraryAndTranscriptLengthFile.getAbsolutePath, withColIx = true, withRowIx = true)

    val varianceStabilizedByLogNormalizedOnLibraryFile = TempFile.createTempFile(".csv")
    varianceStabilizedByLogNormalizedOnLibrary.writeCsvFile(varianceStabilizedByLogNormalizedOnLibraryFile.getAbsolutePath, withColIx = true, withRowIx = true)

    val geneClusterFiles = geneClusters.map {
      case (name, plots, cl) =>
        val file = TempFile.createTempFile(".gmt")
        writeToFile(file, cl.zipWithIndex.map(gs => gs._1.mkString(gs._2 + "\t?\t", "\t", "")).mkString("\n"))
        val pfs = plots.map { plot =>
          val plotfile = TempFile.createTempFile(".png")
          writeBinaryToFile(
            plotfile.getAbsolutePath,
            renderToByteArray(plot, "image/png", 2)
          )
          plotfile
        }

        (name, pfs, file)
    }

    val rpkmFile = TempFile.createTempFile(".csv")
    rpkm.writeCsvFile(rpkmFile.getAbsolutePath, withColIx = true, withRowIx = true)

    val countsFile = TempFile.createTempFile(".csv")
    counts.writeCsvFile(countsFile.getAbsolutePath, withColIx = true, withRowIx = true)

    val totalReadsPerSampleFile = TempFile.createTempFile(".csv")
    totalReadsPerSample.writeCsvFile(totalReadsPerSampleFile.getAbsolutePath, withColIx = true, withRowIx = true)

    val sizeFactorsWholeLibraryFile = TempFile.createTempFile(".csv")
    sizeFactors.writeCsvFile(sizeFactorsWholeLibraryFile.getAbsolutePath, withColIx = true, withRowIx = true)

    val detectedGenesNormalizedOnLibrarySizeFile = TempFile.createTempFile(".txt")
    writeToFile(detectedGenesNormalizedOnLibrarySizeFile.getAbsolutePath, detectedGenesNormalizedOnLibrarySize.toString)

    val meanVarPlotAfterVSTPerBPFile = TempFile.createTempFile(".png")
    writeBinaryToFile(
      meanVarPlotAfterVSTPerBPFile.getAbsolutePath,
      renderToByteArray(meanVarPlotAfterVSTPerBP, "image/png", 1)
    )

    val versusPlotsNormalizedOnLibraryDepthFiles = versusPlotsNormalizedOnLibraryDepth.zipWithIndex.map {
      case (plot, idx) =>
        val tmp = TempFile.createTempFile(".png")
        writeBinaryToFile(
          tmp.getAbsolutePath,
          renderToByteArray(plot, "image/png", 1)
        )
        tmp
    }.toList

    val housekeepingGenesFile = TempFile.createTempFile(".txt")
    val housekeepingenes = normalization match {
      case HouseKeepingNormalization(genes) => genes.toList
      case _ => Nil

    }
    writeToFile(housekeepingGenesFile, housekeepingenes.mkString("\n"))

    val genewisepcafile = TempFile.createTempFile(".png")
    writeBinaryToFile(
      genewisepcafile.getAbsolutePath,
      renderToByteArray(genewisepca, "image/png", 1)
    )

    val meanVarPlotAfterVSTFile = TempFile.createTempFile(".png")
    writeBinaryToFile(
      meanVarPlotAfterVSTFile.getAbsolutePath,
      renderToByteArray(meanVarPlotAfterVST, "image/png", 1)
    )

    val detailedPlotFiles = detailedPlots.map { plot =>
      val detailedPlotFile = TempFile.createTempFile(".pdf")
      writeBinaryToFile(
        detailedPlotFile.getAbsolutePath,
        renderToByteArray(plot, "application/pdf", 1)
      )
      detailedPlotFile
    }

    val pcaplotsFiles = pcaplots.map {
      case (name, pcaplot, PCAResultFrames(projections, eigenvalues, _, loadingsmatrix), enrichments, detailedplots) =>
        val plots = TempFile.createTempFile(".pdf")
        val loadings = TempFile.createTempFile(".csv")
        val projectionsfile = TempFile.createTempFile(".csv")
        val gmt = TempFile.createTempFile(".gmt")
        val geneclusterplotfile = TempFile.createTempFile(".png")

        writeBinaryToFile(
          plots.getAbsolutePath,
          renderToByteArray(pcaplot, "application/pdf", 1)
        )

        loadingsmatrix.writeCsvFile(loadings.getAbsolutePath, withColIx = true, withRowIx = true)
        projections.writeCsvFile(projectionsfile.getAbsolutePath, withColIx = true, withRowIx = true)

        val erf = enrichments.map {
          case (idx, er) =>
            val f = TempFile.createTempFile(".txt")
            writeToFile(f, er.query.toSeq.sorted.mkString("\n"))
            (idx.toString + "." + er.db.name, er.toFile, f, er.plotToFile)
        }

        val detailedfiles = detailedplots.map {
          case (idx, drawable) =>
            val f = TempFile.createTempFile(".pdf")
            writeBinaryToFile(
              f.getAbsolutePath,
              renderToByteArray(drawable, "application/pdf", 1)
            )

            (idx, f)
        }

        name -> (plots, loadings, erf, projectionsfile, detailedfiles)
    }

    val spikeInDiagnosticPlotsFiles = spikeInDiagnosticPlots.map {
      case (name, plots) =>
        name -> plots.map {
          case (plot) =>
            val f = TempFile.createTempFile(".png")
            writeBinaryToFile(
              f.getAbsolutePath,
              renderToByteArray(plot, "image/png", 1)
            )
            f
        }

    }

    val readCountPerBPHistogramNormalizedOnLibraryDepthFile = TempFile.createTempFile(".png")
    writeBinaryToFile(
      readCountPerBPHistogramNormalizedOnLibraryDepthFile.getAbsolutePath,
      renderToByteArray(readCountPerBPHistogramNormalizedOnLibraryDepth, "image/png", 1)
    )

    val readCountPerBPHistogramExpressedNormalizedOnLibraryDepthFile = TempFile.createTempFile(".png")
    writeBinaryToFile(
      readCountPerBPHistogramExpressedNormalizedOnLibraryDepthFile.getAbsolutePath,
      renderToByteArray(readCountPerBPHistogramExpressedNormalizedOnLibraryDepth, "image/png", 1)
    )

    val readCountRawFile = TempFile.createTempFile(".png")
    writeBinaryToFile(
      readCountRawFile.getAbsolutePath,
      renderToByteArray(readCountRaw, "image/png", 1)
    )

    QuickReportInFiles(
      normalizedOnLibrary = normalizedOnLibraryFile,
      normalizedOnLibraryAndTranscriptLength = normalizedOnLibraryAndTranscriptLengthFile,
      rpkm = rpkmFile,
      counts = countsFile,
      varianceStabilizedByLogNormalizedOnLibrary = varianceStabilizedByLogNormalizedOnLibraryFile,
      totalReadsPerSample = totalReadsPerSampleFile,
      detectedGenesNormalizedOnLibrarySize = detectedGenesNormalizedOnLibrarySizeFile,
      meanVarPlotAfterVST = meanVarPlotAfterVSTFile,
      meanVarPlotAfterVSTPerBP = meanVarPlotAfterVSTPerBPFile,
      versusPlotsNormalizedOnLibraryDepth = versusPlotsNormalizedOnLibraryDepthFiles,
      pcaplots = pcaplotsFiles,
      readCountPerBPHistogramNormalizedOnLibraryDepth = readCountPerBPHistogramNormalizedOnLibraryDepthFile,
      readCountPerBPHistogramExpressedNormalizedOnLibraryDepth = readCountPerBPHistogramExpressedNormalizedOnLibraryDepthFile,
      readCountRaw = readCountRawFile,
      sizeFactors = sizeFactorsWholeLibraryFile,
      spikeInDiagnosticPlots = spikeInDiagnosticPlotsFiles,
      detailedPlot = detailedPlotFiles,
      houseKeepingGenes = housekeepingGenesFile,
      geneClusters = geneClusterFiles,
      genewisepca = genewisepcafile
    )

  }
}

sealed trait NormalizationMethod {
  def name: String
}
case object NoDepthNormalization extends NormalizationMethod {
  def name = "NoDepthNormalization"
}
case object DeSeqNormalization extends NormalizationMethod {
  def name = "DeSeqNormalization"
}
case object LibrarySizeNormalization extends NormalizationMethod {
  def name = "LibrarySizeNormalization"
}
case object SpikeInNormalization extends NormalizationMethod {
  def name = "SpikeInNormalization"
}
case class HouseKeepingNormalization(genes: Set[String]) extends NormalizationMethod {
  def name = "HouseKeepingNormalization"
}
case object RuvGNormalizationWithSpikeIns extends NormalizationMethod {
  def name = "RUVGWithSpikeIns"
}
case class RuvGNormalization(genes: Set[String]) extends NormalizationMethod {
  def name = "RUVGWithHouseKeepings"
}

case class DetailedPlotDescription(genes: Seq[String], xAxisDimension: Int, title: String)

object QuickReport {

  def fromCountTable(
    countTable: Frame[String, String, Long],
    lengths: Map[String, Int],
    spikeInPerSample: Map[String, SpikeInMix],
    spikeInData: Set[SpikeIn],
    doPCA: Boolean,
    pseudoCoverage: Double,
    thresholdDropIfCoverageBelowInAllSamples: Double,
    genesToPlot: Seq[DetailedPlotDescription],
    primaryDimensionReverse: Seq[(String, Set[String])],
    secondaryDimensionReverse: Seq[(String, Set[String])],
    normalization: NormalizationMethod,
    gmts: Seq[GeneSetDB],
    readLength: Int
  )(log: (String => Unit)): QuickReport = {

    val spikeInNames = spikeInData.map(_.id)

    // def createClusters(data: Frame[String, String, Double], distance: DistanceMeasure, title: String) = {

    //   val clusters = clusterGenesByKMeans(data, distance, 1, math.pow(2d, primaryDimensionReverse.size).toInt)

    //   val clusterplotalltogether = detailedGeneExpressionPlot(
    //     data.row(clusters.flatMap(_._2).toSeq: _*),
    //     primaryDimensionReverse,
    //     Seq(),
    //     clusters.toSeq.zipWithIndex.flatMap(x => x._1._2.toSeq.map(y => y -> x._2.toString)).toMap,
    //     title
    //   )
    //   val clusterplotsSeparate = clusters.zipWithIndex.map { cl =>

    //     detailedGeneExpressionPlot(
    //       data.row(cl._1._2.toSeq: _*),
    //       primaryDimensionReverse,
    //       secondaryDimensionReverse,
    //       Map(),
    //       title + " cluster" + cl._2
    //     )
    //   }

    //   val composite = mybiotools.plots.compositeTable(clusterplotalltogether :: clusterplotsSeparate.toList, 3)
    //   (title, composite :: Nil, clusters.map(_._2))
    // }

    // def createClustersDBSCAN(data: Frame[String, String, Double], distance: DistanceMeasure, eps: Double, min: Int, title: String) = {
    //   val clusters = clusterGenesByDBSCAN(data, distance, eps, min)
    //   println(clusters.size)
    //   println(clusters.map(_.size))
    //   val clusterplot = detailedGeneExpressionPlot(
    //     data.row(clusters.flatten.toSeq: _*),
    //     primaryDimensionReverse,
    //     Map("?" -> secondaryDimensionReverse.values.flatten.toSet),
    //     clusters.toSeq.zipWithIndex.flatMap(x => x._1.toSeq.map(y => y -> x._2.toString)).toMap,
    //     title
    //   )
    //   (title, clusterplot, clusters)
    // }

    def createGeneWisePCA(data: Frame[String, String, Double], title: String, useGenes: Seq[String], geneLabels: Map[String, Set[String]]) = {
      val (plot, pca) = mybiotools.pcanew.createPCAPlots[String, String](
        (data.row(useGenes: _*)),
        title = title,
        labels = geneLabels.zipWithIndex.map {
          case ((n, gs), idx) =>
            ScatterPlot.Label(n, mybiotools.plots.colorPick(idx, geneLabels.size)) -> gs.toSeq
        },
        maxAxes = 6
      )

      (title, plot, pca)
    }

    val countTableNAZero = replaceNAWithZero(countTable)
    val countTableNAZeroPlusPseudo = addPseudoCoverage(
      countTableNAZero,
      amount = pseudoCoverage,
      readLength = readLength,
      transcriptLengths = lengths
    )
    log("Replace NAs with 0.")
    log(s"Total analyzed read count per sample: \n" + countTableNAZero.sum.toSeq.mkString("\n"))

    def createSpikeInDiagnosticPlots(data: Frame[String, String, Double], title: String, log: Boolean): (String, List[Drawable]) = {
      // val foldChange = spikeInFoldChange(data, spikeInPerSample, spikeInData, title, log)
      val foldChangeVsIntensityPooled = spikeInFoldChangeVsIntensity(data, spikeInPerSample, spikeInData, title, log)
      val doseResponse = spikeInDoseResponse(data, spikeInPerSample, spikeInData, title, log)
      val spikeInPCA = scala.util.Try({
        val equilibrated = equilibrateSpikeIns(countTableNAZeroPlusPseudo, spikeInPerSample, spikeInData)
        val axes = estimateRuvGBatchAxes(equilibrated, spikeInData.map(_.id).toSeq.sorted, 6).mapColIndex(_.toString)
        mybiotools.pcanew.createPCAPlots[String, String](
          axes.T
        )
      }._1).toOption
      title -> (mybiotools.plots.compositeTable(foldChangeVsIntensityPooled :: ((doseResponse.values).map(_._1).toList), 3) :: spikeInPCA.toList)
    }

    val normalizedOnLibraryDepth = normalization match {
      case DeSeqNormalization => normalizeByDeSeqsMethod(countTableNAZero)
      case SpikeInNormalization => normalizeByTotalSpikeIns(countTableNAZero, spikeInData)
      case HouseKeepingNormalization(genes) => normalizeByHouseKeepingGenes(countTableNAZero, genes)
      case LibrarySizeNormalization => normalizeOnLibraryDepth(countTableNAZero)
      case NoDepthNormalization => countTableNAZero
      case RuvGNormalization(genes) => normalizeByRuvGHouseKeepings(
        countTableNAZeroPlusPseudo,
        genes, 3
      )
      case RuvGNormalizationWithSpikeIns => normalizeByRuvGSpikeIns(
        countTableNAZeroPlusPseudo,
        spikeInPerSample,
        spikeInData,
        3
      )
    }

    val sizeFactor = normalization match {
      case DeSeqNormalization => sizeFactorsDESeq(countTableNAZero)
      case SpikeInNormalization => sizeFactorsFromTotalCountsOfGenes(countTableNAZero, spikeInData.map(_.id))
      case HouseKeepingNormalization(genes) => sizeFactorsFromTotalCountsOfGenes(countTableNAZero, genes)
      case LibrarySizeNormalization => sizeFactorFromLibraryDepth(countTableNAZero)
      case NoDepthNormalization | RuvGNormalization(_) | RuvGNormalizationWithSpikeIns => Series(countTableNAZero.colIx.toSeq.map(x => x -> 1.0): _*)
    }
    log(s"Normalize differences between sample depths ($normalization). #Genes after normalization: " + normalizedOnLibraryDepth.numRows)

    val normalizedOnLibraryAndTranscriptLength = normalizeByTranscriptLengths(
      addPseudoCoverage(normalizedOnLibraryDepth, amount = pseudoCoverage, readLength = readLength, transcriptLengths = lengths),
      lengths
    )
    val normalizedOnLibraryAndTranscriptLengthWITHOUTPSEUDO = normalizeByTranscriptLengths(
      normalizedOnLibraryDepth,
      lengths
    )
    log(s"Add ${pseudoCoverage.toDouble / readLength} per bp to all cells.")
    log(s"Normalize differences between genes due to gene length (divide by gene length).")

    {
      val outliers = sizeFactor.filter(x => x >= 5.0 || x <= 0.2)
      if (outliers.length > 1) {
        log(s"Outlier size factors: $outliers")
      }
    }

    // val downsamplingDetectedGenes: Seq[(Int, Int)] = {
    //   def detectedGenes(counts: Frame[String, String, Long]) = {
    //     (discardBelowCountInAllSamples(normalizeByTranscriptLengths(normalizeByDeSeqsMethod(replaceNAWithZero(counts)), lengths), thresholdDropIfCoverageBelowInAllSamples / readLength).rowIx.toSeq.toSet & lengths.filter(_._2 >= readLength * 2).map(_._1).toSet).size
    //   }

    //   val r = 1 to 30 by 5 map { x =>
    //     val r = x -> detectedGenes(downsampleCounts(countTable, (x * 1E6).toInt))
    //     println(r)
    //     r
    //   }

    //   log(s"Downsampling: $r")

    //   r

    // }

    val expressedGenes = (
      (discardBelowCountInAllSamples(normalizedOnLibraryAndTranscriptLength, thresholdDropIfCoverageBelowInAllSamples / readLength).rowIx.toSeq.toSet) &
      lengths.filter(_._2 >= readLength * 2).map(_._1).toSet
    ).toSeq
    log(s"${expressedGenes.size} sufficiently long (longer than ${readLength * 2}) genes are expressed: have higher than $thresholdDropIfCoverageBelowInAllSamples/$readLength per bp normalized values in at least one sample.")

    val normalizedOnLibraryDepthExpressed = normalizedOnLibraryDepth.row(expressedGenes: _*)

    val normalizedOnLibraryDepthAndTranscriptLengthExpressed = normalizedOnLibraryAndTranscriptLength.row(expressedGenes: _*)

    val logNormalizedOnLibraryDepthExpressed = stabilizeVarianceWithLog(normalizedOnLibraryDepthExpressed)

    val logNormalizedOnLibraryDepthAndTranscriptLengthExpressed = stabilizeVarianceWithLog(normalizedOnLibraryDepthAndTranscriptLengthExpressed)

    val normalizedOnLibraryDepthAndTranscriptLengthExpressedWITHOUTPSEUDO = normalizedOnLibraryAndTranscriptLengthWITHOUTPSEUDO.row(expressedGenes: _*)

    val logNormalizedOnLibraryDepthAndTranscriptLengthExpressedWITHOUTPSEUDO = stabilizeVarianceWithLog(normalizedOnLibraryDepthAndTranscriptLengthExpressedWITHOUTPSEUDO)

    log("Variance stabilization by taking log10.")

    val highVarianceGenes = logNormalizedOnLibraryDepthExpressed.rfilterIx(r => !spikeInNames.contains(r)).T.reduce(_.toVec.variance).sorted.tail(10000)

    val highVarianceGenes5K = logNormalizedOnLibraryDepthExpressed.rfilterIx(r => !spikeInNames.contains(r)).T.reduce(_.toVec.variance).sorted.tail(5000)

    log(s"Take top 10k genes with respect to variance. Variance is computed from variance stabilized (log), normalized, for 'expressed' genes.")
    log("Start PCAs..")
    val pcaplots = {

      val bigpca = if (countTable.numCols > 1 && doPCA)
        createPCA(logNormalizedOnLibraryDepthAndTranscriptLengthExpressed, s"log10((${normalization.name}+$pseudoCoverage*lengthP$readLength)Plength)", highVarianceGenes.index.toSeq, primaryDimensionReverse, secondaryDimensionReverse, gmts) :: Nil
      else Nil

      val genestoplotpca = if (genesToPlot.size > 1) {
        if (logNormalizedOnLibraryDepthAndTranscriptLengthExpressed.row(genesToPlot.flatMap(_.genes).toSet.toSeq: _*).numCols > 1) createPCA(logNormalizedOnLibraryDepthAndTranscriptLengthExpressed, s"genestoplot_log10((${normalization.name}+$pseudoCoverage*lengthP$readLength)Plength)", genesToPlot.flatMap(_.genes).toSet.toSeq, primaryDimensionReverse, secondaryDimensionReverse, gmts) :: Nil else Nil
      } else Nil

      bigpca ::: genestoplotpca
    }
    log("PCAs finished.")

    val totalReadsPerSample = countTable.reduce(_.sum)

    val detectedGenesNormalizedOnLibrarySize = expressedGenes.size

    log("Draw diagnostic plots using spike-ins..")
    val spikeindiagnostics =
      createSpikeInDiagnosticPlots(normalizedOnLibraryDepthAndTranscriptLengthExpressed, s"log10((${normalization.name}(raw)+$pseudoCoverage*lengthP$readLength)Plength[expressed])", false) ::
        Nil

    log(s"Draw detailed plot of $genesToPlot genes.")
    val detailedPlot = genesToPlot.map {
      case DetailedPlotDescription(genesToPlot, xAxis, title) =>
        val xaxisData = if (xAxis == 0) primaryDimensionReverse else secondaryDimensionReverse
        val symbolData = if (xAxis == 0) secondaryDimensionReverse else primaryDimensionReverse
        detailedGeneExpressionPlot(normalizedOnLibraryAndTranscriptLengthWITHOUTPSEUDO.row(genesToPlot: _*), xaxisData, symbolData, genesToPlot.toSeq.map(x => x -> x).toMap,
          s"(${normalization.name}(raw)+${pseudoCoverage / readLength}*length)/length",
          title)

    }

    val readCountPerBPHistogramNormalizedOnLibraryDepth =
      mybiotools.plots.HistogramPlot.createHistogramPlotMultipleAsScatter(
        data = normalizedOnLibraryAndTranscriptLengthWITHOUTPSEUDO.toColSeq.zipWithIndex.map(x => (x._1._1, mybiotools.plots.colorPick(x._2, normalizedOnLibraryAndTranscriptLength.numCols), x._1._2.toVec.toSeq.map(x => Try(math.log10(x)).toOption).filter(_.isDefined).map(_.get))),
        main = s"log10(${normalization.name}(raw+$pseudoCoverage*length/$readLength)/length)",
        ylab = "Frequency of genes",
        xlab = "normalized per bp value"
      )

    val readCountPerBPHistogramExpressedNormalizedOnLibraryDepth =
      mybiotools.plots.HistogramPlot.createHistogramPlotMultipleAsScatter(
        data = logNormalizedOnLibraryDepthAndTranscriptLengthExpressedWITHOUTPSEUDO.toColSeq.zipWithIndex.map(x => (x._1._1, mybiotools.plots.colorPick(x._2, logNormalizedOnLibraryDepthAndTranscriptLengthExpressed.numCols), x._1._2.toVec.toSeq)),
        main = s"log10((${normalization.name}(raw)+$pseudoCoverage*length/$readLength)Plength[expressed])",
        ylab = "Frequency of genes",
        xlab = "normalized per bp value"
      )

    val readCountRaw = mybiotools.plots.HistogramPlot.createHistogramPlot(
      countTableNAZero.toMat.toVec.toSeq.map(x => math.log10(x)),
      main = s"log(raw counts) per gene \nin all samples ",
      relative = true
    )

    val meanVarPlotAfterVST = {
      val means = rowMeans(logNormalizedOnLibraryDepthExpressed)
      val vars = rowVars(logNormalizedOnLibraryDepthExpressed)
      val joined = means.joinMap(vars, how = index.InnerJoin)((x, y) => (x, y)).toVec.toSeq.filter(_ != null)
      ScatterPlot.createScatterPlot(
        data = joined,
        xlog = false,
        ylog = false,
        fit = true,
        main = s"mean-var log10((${normalization.name}(raw)+$pseudoCoverage*length/$readLength)[expressed])",
        xlab = "mean",
        ylab = "var"
      )

    }

    val meanVarPlotAfterVSTPerBP = {
      val means = rowMeans(logNormalizedOnLibraryDepthAndTranscriptLengthExpressed)
      val vars = rowVars(logNormalizedOnLibraryDepthAndTranscriptLengthExpressed)
      val joined = means.joinMap(vars, how = index.InnerJoin)((x, y) => (x, y)).toVec.toSeq.filter(_ != null)
      ScatterPlot.createScatterPlot(
        data = joined,
        xlog = false,
        ylog = false,
        fit = true,
        main = s"mean-var log10((${normalization.name}(raw)+$pseudoCoverage*length/$readLength)Plength[expressed])",
        xlab = "mean",
        ylab = "var"
      )

    }

    val versusPlotsNormalizedOnLibraryDepth = Nil
    // ScatterPlot.createScatterPlotsFromFrame(
    //   frame = normalizedOnLibraryDepth + 1.0,
    //   fit = false,
    //   rsquared = true,
    //   xlog = true,
    //   ylog = true,
    //   combined = false)

    val rpkm = calculateRPKM(countTable, lengths)
    log("Calculate RPKM (for future use).")

    log("Gene wise PCA on depth normalized, logged, length normalized values of the 5k expressed genes with highest variance.")
    val genewisepca = createGeneWisePCA(logNormalizedOnLibraryDepthAndTranscriptLengthExpressed, s"log10((${normalization.name}(raw)+$pseudoCoverage*lengthP$readLength)Plength[expressed])", highVarianceGenes5K.index.toSeq, Map())

    // log("Clustering of top 5k genes..")
    val clusters =
      // createClusters(logNormalizedOnLibraryDepthAndTranscriptLengthExpressed.row(highVarianceGenes5K.index.toSeq: _*), new EuclideanDistance, s"log10((${normalization.name}(raw)+$pseudoCoverage*lengthP$readLength)Plength[expressed&top5kvar]), clustering: kmeans++, L2") ::
      // createClusters(logNormalizedOnLibraryDepthAndTranscriptLengthExpressed.row(highVarianceGenes5K.index.toSeq: _*), CosineDistance, s"log10((${normalization.name}(raw)+$pseudoCoverage*lengthP$readLength)Plength[expressed&top5kvar]), clustering: kmeans++, cosine") ::
      Nil

    log("Report done. Please at a minimum check the following files: \n\t 1. density histogram of read count per bp in expressed and nonexpressed genes. \n\t 2. Mean-variance plot.")

    QuickReport(
      normalizedOnLibrary = normalizedOnLibraryDepth,
      normalizedOnLibraryAndTranscriptLength = normalizedOnLibraryAndTranscriptLength,
      counts = countTable,
      rpkm = rpkm,
      varianceStabilizedByLogNormalizedOnLibrary = logNormalizedOnLibraryDepthExpressed,
      totalReadsPerSample = totalReadsPerSample,
      detectedGenesNormalizedOnLibrarySize = detectedGenesNormalizedOnLibrarySize,
      meanVarPlotAfterVST = meanVarPlotAfterVST,
      meanVarPlotAfterVSTPerBP = meanVarPlotAfterVSTPerBP,
      versusPlotsNormalizedOnLibraryDepth = versusPlotsNormalizedOnLibraryDepth,
      pcaplots = pcaplots,
      readCountPerBPHistogramNormalizedOnLibraryDepth = readCountPerBPHistogramNormalizedOnLibraryDepth,
      readCountPerBPHistogramExpressedNormalizedOnLibraryDepth = readCountPerBPHistogramExpressedNormalizedOnLibraryDepth,
      readCountRaw = readCountRaw,
      sizeFactors = sizeFactor,
      spikeInDiagnosticPlots = spikeindiagnostics,
      detailedPlots = detailedPlot,
      normalization = normalization,
      geneClusters = clusters,
      genewisepca = genewisepca._2
    )

  }
}