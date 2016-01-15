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

package rnaseqalign

import org.saddle._
import org.saddle.scalar._
import mybiotools.stat._
import de.erichseifert.gral.graphics.Drawable
import mybiotools.plots._
import mybiotools.plots.ScatterPlot._
import java.awt.Color
import de.erichseifert.gral.util.Insets2D
import de.erichseifert.gral.plots.XYPlot
import de.erichseifert.gral.plots.axes.AxisRenderer
import org.apache.commons.math3.ml.clustering._
import org.apache.commons.math3.ml.distance._
import org.apache.commons.math3.ml.clustering.evaluation._
import scala.collection.JavaConversions._
import mybiotools._
import java.awt.Font

package object analysis {

  val significanceLevel = 1E-3
  val minimumLog2FoldChange = 2

  /* avogadro * atto * (total concentration from ERCC spreadsheet) */
  val ERCCMoleculesPerMicroliter = 6.233821313115978E10

  def downsampleCounts(countsPerGene: Series[String, Long], totalReads: Int): Series[String, Long] = {

    def countFake(list: => Stream[String]): Seq[(String, Long)] = {
      val mmap = scala.collection.mutable.AnyRefMap[String, Long]()
      list.foreach { l =>
        mmap.get(l) match {
          case None => mmap.update(l, 1)
          case Some(x) => mmap.update(l, x + 1)
        }
      }
      mmap.toSeq
    }

    val fakeReads: IndexedSeq[String] = countsPerGene.toSeq.flatMap {
      case (genename, count) =>
        0L until count map (i => genename)
    } toIndexedSeq

    mybiotools.stat.Resampling.resample(1, totalReads.toInt, fakeReads) { (listOfFakeReads, _) => Series(countFake(listOfFakeReads): _*) } head
  }

  def downsampleCounts(c: Frame[String, String, Long], t: Int): Frame[String, String, Long] = {
    Frame(c.toColSeq.map(x => x._1 -> downsampleCounts(x._2, t)): _*)
  }

  def getDESeqSizeFactors(countTable: Frame[String, String, Long]) =
    sizeFactorsDESeq(replaceNAWithZero(countTable))

  def getExpressedGenesAndSizeFactors(countTable: Frame[String, String, Long], lengths: Map[String, Int], thresholdDropIfCoverageBelowInAllSamples: Double, spikeInData: Set[SpikeIn], spikeInPerSample: Map[String, SpikeInMix], normalization: NormalizationMethod, readLength: Int) = {
    val countTableNAZero = replaceNAWithZero(countTable)

    val normalizedOnLibraryDepth = normalization match {
      case DeSeqNormalization => normalizeByDeSeqsMethod(countTableNAZero)
      case SpikeInNormalization => normalizeByTotalSpikeIns(countTableNAZero, spikeInData)
      case HouseKeepingNormalization(genes) => normalizeByHouseKeepingGenes(countTableNAZero, genes)
      case LibrarySizeNormalization => normalizeOnLibraryDepth(countTableNAZero)
      case NoDepthNormalization => countTableNAZero
      case RuvGNormalization(genes) => normalizeByRuvGHouseKeepings(countTableNAZero, genes, 3)
      case RuvGNormalizationWithSpikeIns => normalizeByRuvGSpikeIns(countTableNAZero, spikeInPerSample, spikeInData, 3)
    }

    val normalizedOnLibraryAndTranscriptLength = normalizeByTranscriptLengths(normalizedOnLibraryDepth, lengths)

    val expressed = (
      (discardBelowCountInAllSamples(normalizedOnLibraryAndTranscriptLength, thresholdDropIfCoverageBelowInAllSamples / readLength).rowIx.toSeq.toSet) &
      lengths.filter(_._2 >= readLength * 2).map(_._1).toSet
    ).toSeq

    val sizeFactor = normalization match {
      case DeSeqNormalization => sizeFactorsDESeq(countTableNAZero)
      case SpikeInNormalization => sizeFactorsFromTotalCountsOfGenes(countTableNAZero, spikeInData.map(_.id))
      case HouseKeepingNormalization(genes) => sizeFactorsFromTotalCountsOfGenes(countTableNAZero, genes)
      case LibrarySizeNormalization => sizeFactorFromLibraryDepth(countTableNAZero)
      case NoDepthNormalization => Series(countTableNAZero.colIx.toSeq.map(x => x -> 1.0): _*)
      case RuvGNormalization(_) | RuvGNormalizationWithSpikeIns => Series(countTableNAZero.colIx.toSeq.map(x => x -> 1.0): _*)

    }
    (expressed, sizeFactor, normalizedOnLibraryDepth)
  }

  def addPseudoCoverage(countTable: Frame[String, String, Double], amount: Double, readLength: Int, transcriptLengths: Map[String, Int]): Frame[String, String, Double] = Frame(countTable.toColSeq.map {
    case (c, col) =>
      c -> col.map {
        case (r, v) =>
          r -> transcriptLengths.get(r).map(n => v + amount * n / readLength).getOrElse(na.to[Double])
      }
  }: _*)

  def equilibrateSpikeIns(countTable: Frame[String, String, Double], spikeInPerSample: Map[String, SpikeInMix], spikeInData: Set[SpikeIn]) = {
    val spikeInByNames = spikeInData.map(x => x.id -> x).toMap
    Frame(countTable.toRowSeq.map {
      case (gene, series) =>
        spikeInByNames.get(gene) match {
          case None => (gene, series)
          case Some(SpikeIn(_, conc1, conc2)) => {
            (gene, {
              Series(series.toSeq.map {
                case (sample, value) =>
                  spikeInPerSample.get(sample) match {
                    case Some(Mix1) => (sample, value)
                    case Some(Mix2) => (sample, value * conc1 / conc2)
                    case None => (sample, na.to[Double])
                  }
              }: _*)
            })
          }
        }
    }: _*).T.rdropNA

  }

  def estimateRuvGBatchAxes(countTable: Frame[String, String, Double], negativeControlGenes: Seq[String], k: Int) = {
    mybiotools.pcanew.pcaFromFrame(stabilizeVarianceWithLog(countTable.row(negativeControlGenes: _*).T + 1), k).projectedSampleCoordinates
  }

  def ruvgResiduals(countTable: Frame[String, String, Double], estimatedBatchDesign: Frame[String, Int, Double]): Frame[String, String, Double] =
    Frame(Frame(countTable.toRowSeq.map {
      case (geneName: String, gene: Series[String, Double]) =>
        val dataframe: Frame[String, String, Double] = Frame(estimatedBatchDesign.mapColIndex(_.toString).toColSeq: _*)
        (geneName, mybiotools.stat.PoissonRegression.poissonRegression(
          data = dataframe,
          covariateNames = estimatedBatchDesign.colIx.toSeq.map(_.toString),
          outcomes = gene,
          missingMode = mybiotools.stat.DropSample
        ) match {
            case x: PoissonRegressionResult => Some(x.residuals)
            case x => None //throw new RuntimeException(x.toString + " " + geneName + " " + gene + " " + dataframe)
          }, gene)
    }.filter(_._2.isDefined).map { x =>
      (x._1, Series(x._2.get, x._3.index))
    }: _*).T.toRowSeq.map {
      case (gene, series) =>
        val min = series.min.get
        if (min < 0.0) (gene, series + min * (-1))
        else (gene, series)
    }: _*).T

  def normalizeByRuvGSpikeIns(countTable: Frame[String, String, Double], spikeInPerSample: Map[String, SpikeInMix], spikeInData: Set[SpikeIn], k: Int) = {
    val equilibrated = equilibrateSpikeIns(countTable, spikeInPerSample, spikeInData)
    val axes = estimateRuvGBatchAxes(equilibrated, spikeInData.map(_.id).toSeq.sorted, k)
    ruvgResiduals(equilibrated, axes)
  }

  def normalizeByRuvGHouseKeepings(countTable: Frame[String, String, Double], useGenes: Set[String], k: Int) = {
    val axes = estimateRuvGBatchAxes(countTable, useGenes.toSeq.sorted, k)
    ruvgResiduals(countTable, axes)
  }

  /** Provides per base counts. Divides counts with transcript length. */
  def normalizeByTranscriptLengths(countTable: Frame[String, String, Double], lengths: Map[String, Int]): Frame[String, String, Double] =
    Frame(countTable.toColSeq.map {
      case (c, col) =>
        c -> col.map {
          case (r, v) =>
            r -> (lengths.get(r) match {
              case Some(n) => v / n
              case None => na.to[Double]
            })
        }
    }: _*)

  /** The geometric mean based size factors of DESeq. */
  def sizeFactorsDESeq(countTable: Frame[String, String, Double]): Series[String, Double] = {
    val geometricmeans = countTable.T.reduce(_.toVec.geomean)
    countTable.reduce { (column: Series[String, Double]) =>
      (column.joinMap(geometricmeans)((count, mean) => count / mean).toVec.dropNA.filter(x => !x.isInfinite)).median
    }
  }

  def sizeFactorsFromTotalCountsOfGenes(counts: Frame[String, String, Double], spikeInNames: Set[String]): Series[String, Double] = {
    val totalSpikeIn = counts.row(spikeInNames.toList: _*).sum
    val avg = totalSpikeIn.median
    totalSpikeIn.mapValues(v => v / avg)
  }

  def sizeFactorFromLibraryDepth(counts: Frame[String, String, Double]): Series[String, Double] = {
    val medianLibrarySize = counts.reduce(_.mapValues(_.toDouble).sum).median
    counts.reduce { x =>
      val size = x.mapValues(_.toDouble).sum
      size / medianLibrarySize
    }
  }

  /**
   * Divide each count by a size factor. Size factor is chosen to equalize total spikein counts.
   *
   * @param countTable raw counts
   */
  def normalizeByTotalSpikeIns(countTable: Frame[String, String, Double], spikeInData: Set[SpikeIn]): Frame[String, String, Double] = {
    val sf = sizeFactorsFromTotalCountsOfGenes(countTable, spikeInData.map(_.id))
    Frame(countTable.toColSeq.map {
      case (c, col) =>
        c -> col.mapValues(_ / sf.get(c))
    }: _*)
  }

  /**
   * Divide each count by a size factor. Size factor is chosen to equalize total housekeeping gene counts.
   *
   * @param countTable raw counts
   */
  def normalizeByHouseKeepingGenes(countTable: Frame[String, String, Double], housekeepings: Set[String]): Frame[String, String, Double] = {
    val sf = sizeFactorsFromTotalCountsOfGenes(countTable, housekeepings)
    Frame(countTable.toColSeq.map {
      case (c, col) =>
        c -> col.mapValues(_ / sf.get(c))
    }: _*)
  }

  /**
   * Divide each count by a size factor. Size factor is the sum of each column.
   *
   * @param countTable raw counts
   */
  def normalizeOnLibraryDepth(countTable: Frame[String, String, Double]): Frame[String, String, Double] = {
    val sf = sizeFactorFromLibraryDepth(countTable)
    Frame(countTable.toColSeq.map {
      case (c, col) =>
        c -> col.mapValues(_ / sf.get(c))
    }: _*)
  }

  /**
   * DESeq library size normalization.
   *
   * @param countTable raw counts
   */
  def normalizeByDeSeqsMethod(countTable: Frame[String, String, Double]): Frame[String, String, Double] = {
    val sf = sizeFactorsDESeq(countTable)
    Frame(countTable.toColSeq.map {
      case (c, col) =>
        c -> col.mapValues(_ / sf.get(c))
    }: _*)
  }

  def calculateRPKM(countTable: Frame[String, String, Long], lengths: Map[String, Int]): Frame[String, String, Double] = {
    val totalcounts: Series[String, Double] = countTable.reduce(_.mapValues(_.toDouble).sum)
    Frame(countTable.toColSeq.map {
      case (c, col) =>
        c -> col.map {
          case (r, v) if ScalarTagLong.isMissing(v) => r -> 0.0
          case (r, v) =>
            r -> (lengths.get(r) match {
              case Some(l) => 1E9 * (v.toDouble / (l.toDouble * totalcounts.get(c)))
              case None => 0.0
            })
        }
    }: _*)
  }

  def replaceNAWithZero(countTable: Frame[String, String, Long]) = Frame(countTable.toColSeq.map {
    case (c, col) =>
      c -> col.map {
        case (r, v) =>
          r -> (if (org.saddle.scalar.ScalarTagLong.isMissing(v)) 0.0 else v.toDouble)
      }
  }: _*)

  def discardBelowCountInFractionOfSamples(countTable: Frame[String, String, Double], threshold: Double, fraction: Double): Frame[String, String, Double] =
    countTable.rfilter(column => column.countif(_ >= threshold) >= (column.length * fraction))

  def discardBelowCountInAllSamples(countTable: Frame[String, String, Double], threshold: Double): Frame[String, String, Double] =
    countTable.rfilter(row => row.countif(_ >= threshold) >= 1)

  def rowMeans(countTable: Frame[String, String, Double]): Series[String, Double] =
    countTable.rreduce(_.mean)

  def rowVars(countTable: Frame[String, String, Double]): Series[String, Double] =
    countTable.rreduce(_.variance)

  // def stabilizeVarianceWithLog[T](countTable: Frame[String, String, Double], discardBelow: Double, pseudoCounts: Double): Frame[String, String, Double] =
  //   (discardBelowCountInFractionOfSamples(countTable, discardBelow, 0.5) + pseudoCounts).mapValues(x => scala.math.log10(x))

  def stabilizeVarianceWithLog[T](countTable: Frame[String, String, Double]): Frame[String, String, Double] =
    (countTable).mapValues(x => scala.math.log10(x))

  def spikeInFoldChange(countTable: Frame[String, String, Double], spikeInPerSample: Map[String, SpikeInMix], spikeInData: Set[SpikeIn], name: String, countsAreLog: Boolean): Map[(String, String), (Drawable, RegressionResultOrFailure)] = {
    (countTable.row(spikeInData.map(_.id).toList: _*).toColSeq.filter(x => spikeInPerSample.contains(x._1)).combinations(2).map { l =>
      val (sample1name, series1) = l(0)
      val (sample2name, series2) = l(1)

      val sample1mix = spikeInPerSample(sample1name)
      val sample2mix = spikeInPerSample(sample2name)

      val expectedSample1 = Series((sample1mix match {
        case Mix1 => spikeInData.toList.map(x => x.id -> x.concentrationMix1)
        case Mix2 => spikeInData.toList.map(x => x.id -> x.concentrationMix2)
      }): _*)
      val expectedSample2 = Series((sample2mix match {
        case Mix1 => spikeInData.toList.map(x => x.id -> x.concentrationMix1)
        case Mix2 => spikeInData.toList.map(x => x.id -> x.concentrationMix2)
      }): _*)

      val expectedFold: Series[String, Double] = expectedSample1.joinMap(expectedSample2)(_ / _) mapValues (math.log10)
      val observedFold: Series[String, Double] = series1.filter(x => !x.isInfinite && !x.isNaN).joinMap(series2.filter(x => !x.isInfinite && !x.isNaN)) { (x, y) =>
        if (countsAreLog) x - y
        else math.log10(x) - math.log10(y)
      }

      val regression = LinearRegression.linearRegression(observedFold, expectedFold)

      val plot = mybiotools.plots.ScatterPlot.createScatterPlot(
        data = expectedFold.joinMap(observedFold)((x, y) => (x, y)).toVec.toSeq.toIndexedSeq.filter(x => x != null && !x._1.isInfinite && !x._2.isInfinite),
        main = s"$name\n$sample1name($sample1mix) vs \n $sample2name($sample2mix)",
        fit = true,
        xlim = Some(-1, 1),
        ylim = Some(-1, 1),
        xlab = "expected fold",
        ylab = "observed fold",
        draw1Line = true
      )

      (sample1name, sample2name) -> (plot, regression)

    }).toMap

  }

  def spikeInFoldChangeVsIntensity(countTable: Frame[String, String, Double], spikeInPerSample: Map[String, SpikeInMix], spikeInData: Set[SpikeIn], name: String, countsAreLog: Boolean): Drawable = {
    val persample = (countTable.row(spikeInData.map(_.id).toList: _*).toColSeq.filter(x => spikeInPerSample.contains(x._1)).combinations(2).map { l =>
      val (sample1name, series1) = l(0)
      val (sample2name, series2) = l(1)

      val sample1mix = spikeInPerSample(sample1name)
      val sample2mix = spikeInPerSample(sample2name)

      val expectedSample1 = Series((sample1mix match {
        case Mix1 => spikeInData.toList.map(x => x.id -> x.concentrationMix1)
        case Mix2 => spikeInData.toList.map(x => x.id -> x.concentrationMix2)
      }): _*)
      val expectedSample2 = Series((sample2mix match {
        case Mix1 => spikeInData.toList.map(x => x.id -> x.concentrationMix1)
        case Mix2 => spikeInData.toList.map(x => x.id -> x.concentrationMix2)
      }): _*)

      val expectedFold: Series[String, Double] = expectedSample1.joinMap(expectedSample2)(_ / _) mapValues (math.log10)
      val observedFold: Series[String, Double] = series1.filter(x => !x.isInfinite && !x.isNaN).joinMap(series2.filter(x => !x.isInfinite && !x.isNaN)) { (x, y) =>
        if (countsAreLog) x - y
        else math.log10(x) - math.log10(y)
      }

      val observedOverExpected: Series[String, Double] = observedFold.joinMap(expectedFold)(_ - _)
      val observedGeoMeanIntensity = series1.filter(x => !x.isInfinite && !x.isNaN).joinMap(series2.filter(x => !x.isInfinite && !x.isNaN)) { (x, y) =>
        if (countsAreLog) (x + y) / 2
        else (math.log10(x) + math.log10(y)) / 2
      }

      val regression = LinearRegression.linearRegression(observedGeoMeanIntensity, observedOverExpected)

      val dataforplot = observedGeoMeanIntensity.joinMap(observedOverExpected)((x, y) => (x, y)).toVec.toSeq.toIndexedSeq.filter(x => x != null && !x._1.isInfinite && !x._2.isInfinite)

      // val plot = mybiotools.plots.ScatterPlot.createScatterPlot(
      //   data = dataforplot,
      //   main = s"spike in fold change vs intensity \n $name\n$sample1name($sample1mix) vs \n $sample2name($sample2mix)",
      //   fit = true,
      //   xlab = "observed log mean intensity :log10(sqrt(s1*s2))",
      //   ylab = "log observed fold - log expected fold: log(s1/e1) - log(s2/e2) ",
      //   draw1Line = false
      // )

      (sample1name, sample2name) -> (1, regression, dataforplot)

    }).toMap

    val pooled = persample.values.flatMap(_._3).toIndexedSeq

    val plot = mybiotools.plots.ScatterPlot.createScatterPlot(
      data = pooled,
      main = s"$name\nall pairwise pooled",
      fit = false,
      xlab = "observed log mean intensity :log10(sqrt(s1*s2))",
      ylab = "log observed fold - log expected fold: log(s1/e1) - log(s2/e2) ",
      draw1Line = false
    )

    // (persample.map(x => x._1 -> (x._2._1, x._2._2)), plot)
    plot

  }

  def spikeInDoseResponse(countTable: Frame[String, String, Double], spikeInPerSample: Map[String, SpikeInMix], spikeInData: Set[SpikeIn], name: String, countsAreLog: Boolean): Map[String, (Drawable, RegressionResultOrFailure)] =
    countTable.row(spikeInData.map(_.id).toList: _*).toColSeq.filter(x => spikeInPerSample.contains(x._1)).map {
      case (sample, observed1) =>
        val observed =
          if (countsAreLog) observed1.filter(x => !x.isInfinite && !x.isNaN)
          else observed1.filter(x => !x.isInfinite && !x.isNaN).mapValues(math.log10)

        val expected = Series((spikeInPerSample(sample) match {
          case Mix1 => spikeInData.toList.map(x => x.id -> math.log10(x.concentrationMix1))
          case Mix2 => spikeInData.toList.map(x => x.id -> math.log10(x.concentrationMix2))
        }): _*)

        val regression = LinearRegression.linearRegression(observed, expected)

        val plot = mybiotools.plots.ScatterPlot.createScatterPlot(
          data = expected.joinMap(observed)((x, y) => (x, y)).toVec.toSeq.toIndexedSeq.filter(x => x != null && !x._1.isInfinite && !x._2.isInfinite),
          fit = true,
          main = sample + " " + name,
          xlab = "expected concentration log(arbitrary)",
          ylab = "observed concentration",
          xlim = Some(-3, 7),
          ylim = Some(-3, 7)
        )

        sample -> (plot, regression)

    }.toMap

  def detailedGeneExpressionPlot(
    counts: Frame[String, String, Double],
    primaryDimensionReverse: Seq[(String, Set[String])],
    secondaryDimensionReverse: Seq[(String, Set[String])],
    geneGroups: Map[String, String],
    ylab: String,
    main: String
  ) = {

    if (counts.numRows > 0) {

      val groupidx = geneGroups.values.toSeq.sorted.zipWithIndex.toMap

      val samplesWithoutSecondaryDimension = primaryDimensionReverse.map(_._2).flatten.toSet &~ secondaryDimensionReverse.map(_._2).flatten.toSet

      val plot = mybiotools.plots.ScatterPlot.createScatterPlotFromMultiple(
        xnames = primaryDimensionReverse.zipWithIndex.map(x => x._2.toDouble -> x._1._1).toMap,
        ylab = ylab,
        main = main,
        data = counts.toRowSeq.toIndexedSeq.flatMap {
        case ((gene, row)) =>

          (secondaryDimensionReverse.toSeq.sortBy(_._1) :+ ("?", samplesWithoutSecondaryDimension)).filter(_._2.size > 0).zipWithIndex.map {
            case ((k2, k2samples), k2idx) =>
              val data: Seq[(Double, Double)] = primaryDimensionReverse.zipWithIndex.flatMap {
                case ((k1, k1samples), k1idx) =>
                  (k2samples & k1samples).toSeq.map(sampleName => k1idx.toDouble -> (row.get(sampleName) match {
                    case NA => None
                    case x => Some(x.get)
                  })).filter(_._2.isDefined).map(x => x._1 -> x._2.get)
              }
              val color = {
                val c1 = mybiotools.plots.colorPick(geneGroups.get(gene).map(x => groupidx(x) + 1).getOrElse(0), groupidx.size + 1)
                new java.awt.Color(c1.getRed, c1.getGreen, c1.getBlue, 200)
              }
              ScatterPlot.Label(geneGroups.get(gene).getOrElse("?"), color, mybiotools.plots.shapePick(k2idx), new java.awt.BasicStroke(0.2f)) -> seq2(data.toIndexedSeq)
          }

      } ++
        (secondaryDimensionReverse.toSeq.sortBy(_._1) :+ ("?", samplesWithoutSecondaryDimension)).filter(_._2.size > 0).zipWithIndex.map {
          case ((k2, k2samples), k2idx) =>
            ScatterPlot.Label(":" + k2, Color.black, mybiotools.plots.shapePick(k2idx), new java.awt.BasicStroke(0.0f)) -> seq2(Vector[(Double, Double)]())
        }

      )

      plot.setInsets(new Insets2D.Double(100.0, 100.0, 300.0, 500.0));
      plot.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICKS_SPACING, 1.0)
      plot.getLegend.setSetting(de.erichseifert.gral.plots.legends.Legend.FONT, new Font(Font.SANS_SERIF, Font.PLAIN, 10))

      plot
    } else {
      mybiotools.plots.ScatterPlot.createScatterPlot(Vector(0.0 -> 0.0))
    }
  }

  def detailedGeneExpressionPlotSecondaryAsCompositePlot(
    counts: Frame[String, String, Double],
    primaryDimensionReverse: Seq[(String, Set[String])],
    secondaryDimensionReverse: Seq[(String, Set[String])],
    geneGroups: Map[String, String],
    ylab: String,
    title: String
  ) =
    mybiotools.plots.compositeTable(secondaryDimensionReverse.map {
      case (secondaryName, samples) =>
        detailedGeneExpressionPlot(
          counts.col(samples.toSeq: _*),
          primaryDimensionReverse,
          Seq(),
          geneGroups,
          ylab,
          title + "." + secondaryName
        )
    }, 1)

  private case class ClusterableGene(name: String, data: Array[Double]) extends Clusterable {
    def getPoint = data
  }

  def clusterGenesByKMeans(data: Frame[String, String, Double], distance: DistanceMeasure, k: Int): Seq[(Series[String, Double], Set[String])] = {

    val clusterer = new MultiKMeansPlusPlusClusterer[ClusterableGene](
      new KMeansPlusPlusClusterer(k, 1000, distance),
      50,
      new SumOfClusterVariances(distance)
    )

    val rawresult = clusterer.cluster(data.toRowSeq.map(x => ClusterableGene(x._1, x._2.toVec.contents)))

    rawresult.toSeq.map { cc =>
      Series(Vec(cc.getCenter.getPoint: _*), data.colIx) -> cc.getPoints.map(_.name).toSet
    }

  }

  def clusterGenesByKMeans(data: Frame[String, String, Double], distance: DistanceMeasure, kmin: Int, kmax: Int): Seq[(Series[String, Double], Set[String])] = {
    (kmin to kmax map { k =>
      val clusters = clusterGenesByKMeans(data, distance, k)
      val bic = kmeansbic(data, clusters)
      (bic, clusters)
    } minBy (_._1))._2
  }

  def kmeansbic(data: Frame[String, String, Double], clusters: Seq[(Series[String, Double], Set[String])]): Double = {
    // this is from the x-means paper: https://www.cs.cmu.edu/~dpelleg/download/xmeans.pdf
    val K = clusters.size
    val R = data.numRows
    val M = data.numCols
    val sigmasq = clusters.map {
      case (centroid, genes) =>
        val ui: Vec[Double] = centroid.sorted.toVec
        genes.map { g =>
          val xi: Series[String, Double] = data.first(g).sorted
          ui.dot(xi.toVec)
        }.sum
    }.sum * 1.0 / (R - K)
    val l = clusters.map {
      case (_, genes) =>
        val Rn = genes.size
        -1.0 * Rn / 2.0 * math.log(2 * math.Pi) -
          Rn * M / 2.0 * math.log(sigmasq) -
          (Rn - K) / 2.0 +
          Rn * math.log(Rn) -
          Rn * math.log(R)
    }.sum
    val pj = (K - 1) + (M * K) + 1
    l - pj / 2.0 * math.log(R)

  }

  // def clusterGenesByDBSCAN(data: Frame[String, String, Double], distance: DistanceMeasure, eps: Double, min: Int): Seq[Set[String]] = {

  //   val clusterer = new DBSCANClusterer[ClusterableGene](eps, min, distance)

  //   val rawresult = clusterer.cluster(data.toRowSeq.map(x => ClusterableGene(x._1, x._2.toVec.contents)))

  //   rawresult.toSeq.map { cc =>
  //     cc.getPoints.map(_.name).toSet
  //   }

  // }

  object CosineDistance extends DistanceMeasure {
    def compute(a: Array[Double], b: Array[Double]) = mybiotools.stat.cosineDistance(a, b)
  }
  object EuclideanSquared extends DistanceMeasure {
    def compute(a: Array[Double], b: Array[Double]) = mybiotools.stat.innerProduct(a, b)
  }

  def createPCA(
    data: Frame[String, String, Double],
    title: String,
    useGenes: Seq[String],
    primaryDimensionReverse: Seq[(String, Set[String])],
    secondaryDimensionReverse: Seq[(String, Set[String])],
    gmts: Seq[GeneSetDB]
  ) = {
    val subsetdata = data.row(useGenes: _*)
    val (plot, pca) = mybiotools.pcanew.createPCAPlots[String, String](
      (subsetdata).T,
      title = title,
      labels = {
      primaryDimensionReverse.zipWithIndex.flatMap {
        case ((k1, set1), idx1) =>
          secondaryDimensionReverse.toSeq.zipWithIndex.map {
            case ((k2, set2), idx2) =>
              val samples = set1 & set2
              ScatterPlot.Label(k1 + "+" + k2, mybiotools.plots.colorPick(idx1, primaryDimensionReverse.size), mybiotools.plots.shapePick(idx2)) -> samples.toSeq
          }
      }.toMap
    },
      maxAxes = 6

    )

    val enrichmentsOfTop500Loading: Seq[(Int, EnrichmentResultPlain)] = 0 until 6 flatMap { idx =>
      val mean = SummaryStat(pca.loadings.colAt(idx).toSeq.map(_._2)).mean
      val top500: Seq[(String, Double)] = pca.loadings.colAt(idx).toSeq.sortBy(x => math.abs(x._2) * (-1)).take(500)
      gmts.map { gmt =>
        idx -> EnrichmentResult(gmt, useGenes.toSet, top500.map(_._1).toSet, top500.map(x => x._1 -> (x._2 - mean)).toMap)
      }
    }

    val detailedPlotOfTop500: Seq[(Int, Drawable)] = 0 until 6 map { idx =>

      val top500: Seq[(String, Double)] = pca.loadings.colAt(idx).toSeq.sortBy(x => math.abs(x._2) * (-1)).take(500)

      val datatop500 = data.row(top500.map(_._1): _*)
      val clusters = clusterGenesByKMeans(datatop500, new EuclideanDistance, 3)

      val allcluster = detailedGeneExpressionPlot(
        datatop500,
        primaryDimensionReverse,
        Seq(),
        clusters.toSeq.zipWithIndex.flatMap(x => x._1._2.toSeq.map(y => y -> x._2.toString)).toMap,
        "",
        title
      )

      val separateClusters = clusters.zipWithIndex.map { cl =>
        detailedGeneExpressionPlot(
          datatop500.row(cl._1._2.toSeq: _*),
          primaryDimensionReverse,
          secondaryDimensionReverse,
          clusters.toSeq.zipWithIndex.flatMap(x => x._1._2.toSeq.map(y => y -> x._2.toString)).toMap,
          "",
          title
        )
      }
      idx -> mybiotools.plots.compositeTable(allcluster +: separateClusters, 3)
    }

    (title, plot, pca, enrichmentsOfTop500Loading, detailedPlotOfTop500)

  }

  def selectGenesWithAtMinMaxDifference(data: Frame[String, String, Double], threshold: Double): Seq[String] =
    data.toRowSeq.map {
      case (genename, series) =>
        val min = series.min.get
        val max = series.max.get
        genename -> (max - min)
    }.filter(_._2 >= threshold).map(_._1)

}