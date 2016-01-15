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

import de.erichseifert.gral.plots.Plot;
import mybiotools.SummaryStat
import mybiotools.plots.HistogramPlot._
import mybiotools.plots._
import java.awt.Color
import java.awt.{ Stroke, BasicStroke }
import de.erichseifert.gral.plots.XYPlot
import de.erichseifert.gral.data.DataSeries;
import de.erichseifert.gral.data.DataTable;
import de.erichseifert.gral.data.DataSource;
import de.erichseifert.gral.data.filters.Convolution;
import de.erichseifert.gral.data.filters.Filter;
import de.erichseifert.gral.data.filters.Kernel;
import de.erichseifert.gral.data.filters.KernelUtils;
import de.erichseifert.gral.data.filters.Median;
import de.erichseifert.gral.plots.Plot;
import de.erichseifert.gral.plots.XYPlot;
import de.erichseifert.gral.plots._
import de.erichseifert.gral.plots.legends.Legend;
import de.erichseifert.gral.plots.lines.DefaultLineRenderer2D;
import de.erichseifert.gral.ui.InteractivePanel;
import de.erichseifert.gral.util.GraphicsUtils;
import de.erichseifert.gral.util.Insets2D;
import de.erichseifert.gral.plots.PlotArea
import de.erichseifert.gral.util.Orientation;
import de.erichseifert.gral.plots.points._
import de.erichseifert.gral.plots.lines._
import de.erichseifert.gral.plots.axes._
import de.erichseifert.gral.plots.XYPlot.XYPlotArea2D
import de.erichseifert.gral.graphics.AbstractDrawable
import de.erichseifert.gral.util.Location
import scala.runtime.RichInt
import scala.runtime.RichFloat
import de.erichseifert.gral.graphics.DrawableContainer
import de.erichseifert.gral.graphics.TableLayout
import de.erichseifert.gral.plots.colors.QuasiRandomColors
import de.erichseifert.gral.plots.BarPlot
import de.erichseifert.gral.util.DataUtils;
import de.erichseifert.gral.util.PointND;
import de.erichseifert.gral.plots.BoxPlot.BoxWhiskerRenderer
import de.erichseifert.gral.plots.colors.ColorMapper;

import de.erichseifert.gral.graphics.EdgeLayout

import mybiotools.SummaryStat
import mybiotools.plots._
import mybiotools.gwascommons._
import mybiotools.gwascommons.associationresults._
import mybiotools.plots._
import de.erichseifert.gral.data.statistics.Histogram1D
import de.erichseifert.gral.data.EnumeratedData
import org.saddle._
import mybiotools.stat.ContingencyTable
import mybiotools._
import mybiotools.plots.ScatterPlot.Label

import scala.collection.JavaConversions._
import mybiotools.stat._
import de.erichseifert.gral.plots.XYPlot
import de.erichseifert.gral.data.DataTable;
import de.erichseifert.gral.plots.lines._
import de.erichseifert.gral.plots.lines.DefaultLineRenderer2D
import de.erichseifert.gral.plots.lines.LineRenderer
import java.awt.BasicStroke
import mybiotools.plots.ScatterPlot._
import dispensability.tasks.OptimalParameter
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
import org.apache.commons.math3.random.{ MersenneTwister, RandomDataGenerator, RandomGenerator }
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

object Plots {

  def plotPredictionOfDifferentMethods[Data <: PerGeneCountData, Param <: HasFractionOfEssentials with ParamForPlot, Indep <: IntVal](
    model: Model[Data, Param, Indep],
    data: Data,
    estimates: Map[ScatterPlot.Label, Seq[Param]]
  ) = {

    val curves = estimates.toSeq.flatMap(x => x._2.map(param => x._1 -> seq2(model.predictDensity(param, data).map(x => x._1.toDouble -> x._2))))

    val observedPoints = HistogramData(data.observedCounts.filter(_.value <= 80).map(_.value.toDouble), step = 1.0).toScatter

    val maxx = observedPoints.map(_.left.get).filter(_._2 == 0).map(_._1).min

    val observedDataHistogram = ScatterPlot.Label("Observed counts", Color.black, circle(2.0), new BasicStroke(2.0f)) -> observedPoints

    val log = pdfToFile(nice(ScatterPlot.createScatterPlotFromMultiple(
      data = (observedDataHistogram +: curves).reverse,
      xlab = "Number of truncation",
      ylab = "Number of genes",
      main = "Distribution of \n per gene truncation counts",
      xlim = Some(0d -> maxx),
      ylog = true
    )))

    val notlog = pdfToFile(nice(ScatterPlot.createScatterPlotFromMultiple(
      data = (observedDataHistogram +: curves).reverse,
      xlab = "Number of truncation",
      ylab = "Number of genes",
      main = "Distribution of \n per gene truncation counts",
      xlim = Some(0d -> maxx),
      ylog = false
    )))

    (log, notlog)

  }

  def writePlots(plots: Map[String, Drawable], title: String, output: String) =
    plots.foreach { case (name, d) => pdfToFile(output + ".title." + name + ".pdf", d) }

  def plotModel[Data <: PerGeneCountData, Param <: HasFractionOfEssentials with ParamForPlot, Indep <: IntVal](
    model: Model[Data, Param, Indep],
    data: Data,
    parameters: Seq[OptimalParameter[Param]],
    parametersWithoutNoise: Seq[OptimalParameter[Param]],
    xAxisPoints: Seq[Indep],
    shortXAxisPoints: Seq[Indep],
    uniformData: Data,
    homozygousData: Data,
    severeData: Data,
    seed: Int,
    xlab: String
  ): Map[String, File] = {
    {
      val rnd = new MersenneTwister(seed)

      val font = new java.awt.Font(Font.SANS_SERIF, Font.PLAIN, 25)
      val fontsmall = new java.awt.Font(Font.SANS_SERIF, Font.PLAIN, 20)
      val fontsmaller = new java.awt.Font(Font.SANS_SERIF, Font.PLAIN, 15)

      val shape = new java.awt.geom.Ellipse2D.Double(-4, -4, 8.0, 8.0)
      val shape2 = new java.awt.geom.Ellipse2D.Double(-2, -2, 4.0, 4.0)
      val shapesmall = new java.awt.geom.Ellipse2D.Double(-0.1, -0.1, 0.2f, 0.2f)

      val dashedstroke = new BasicStroke(
        4.0f,
        BasicStroke.CAP_BUTT,
        BasicStroke.JOIN_MITER,
        10.0f, Array[Float](10.0f), 0.0f
      )

      val filledStroke = new BasicStroke(8.0f)
      val filledStroke2 = new BasicStroke(4.0f)
      val filledStrokeThin = new BasicStroke(0.2f)

      val meanParameter = model.parameterMean(parameters.map(_.optimum))

      val meanParameterWithoutNoise = model.parameterMean(parametersWithoutNoise.map(_.optimum))

      def curveOfTruncatedGenesUniform(p: Param): Seq[(Double, Double)] = (xAxisPoints map (i => i.value.toDouble / 10000 -> model.predictNumberOfTruncatedGenes(p, uniformData, i).value))

      def curveOfTruncatedGenes(p: Param): Seq[(Double, Double)] = (xAxisPoints map (i => i.value.toDouble / 10000 -> model.predictNumberOfTruncatedGenes(p, data, i).value))

      def shortCurveOfTruncatedGenes(p: Param): Seq[(Double, Double)] = (shortXAxisPoints map (i => i.value.toDouble / 10000 -> model.predictNumberOfTruncatedGenes(p, data, i).value))

      def makeCurveOfObserved(d: Data, refPoints: Seq[Indep]): Seq[(Double, Double)] = (1 to 10 flatMap (i => refPoints) filter (_.value <= model.maxObservedX(d).value) map (i => i.value.toDouble / 10000 -> model.observedTruncatedGenesInDownsampledData(d, i, rnd).value.toDouble))

      lazy val curveOfObservedTruncatedGenes: Seq[(Double, Double)] = makeCurveOfObserved(data, xAxisPoints)

      lazy val curveOfObservedSevereTruncatedGenes: Seq[(Double, Double)] = makeCurveOfObserved(severeData, xAxisPoints)

      lazy val shortCurveOfObservedTruncatedGenes: Seq[(Double, Double)] = makeCurveOfObserved(data, shortXAxisPoints)

      lazy val shortCurveOfObservedSevereTruncatedGenes: Seq[(Double, Double)] = makeCurveOfObserved(severeData, shortXAxisPoints)

      lazy val curveOfObservedHomozygousTruncatedGenes: Seq[(Double, Double)] = makeCurveOfObserved(homozygousData, shortXAxisPoints)

      def curveOfTruncatedHaplosufficientGenes(p: Param): Seq[(Double, Double)] = (xAxisPoints map (i => i.value.toDouble / 10000 -> model.predictNumberOfHaploSufficientTruncatedGenes(p, data, i).value))

      def curveOfTruncatedHaploinsufficientGenes(p: Param): Seq[(Double, Double)] = (xAxisPoints map (i => i.value.toDouble / 10000 -> model.predictNumberOfHaploInsufficientTruncatedGenes(p, data, i).value))

      def curveOfDeltaTruncatedHIOverDeltaTotal(p: Param): Seq[(Double, Double)] = (xAxisPoints filter (_.value > 0) map { i =>
        val xval = i.value.toDouble / 10000
        val hiAtI = model.predictNumberOfHaploInsufficientTruncatedGenes(p, data, i).value
        val hiAtIminus1 = model.predictNumberOfHaploInsufficientTruncatedGenes(p, data, model.indepFactory(i.value - 1)).value
        val totalAtI = model.predictNumberOfTruncatedGenes(p, data, i).value
        val totalAtIminus1 = model.predictNumberOfTruncatedGenes(p, data, model.indepFactory(i.value - 1)).value
        xval -> ((hiAtI - hiAtIminus1) / (totalAtI - totalAtIminus1))
      })

      val colorMapRed = {
        val cm = new LinearGradient(new Color(227, 196, 195, 150), new Color(229, 36, 32, 150))
        cm.setRange(0.0, 1.0)
        cm
      }

      val colorMapRedLight = {
        val cm = new LinearGradient(new Color(227, 196, 195), new Color(229, 142, 140))
        cm.setRange(0.0, 1.0)
        cm
      }

      val colorMapGreyScale = {
        val cm = new LinearGradient(new Color(132, 132, 132), new Color(230, 230, 230))
        cm.setRange(0.0, 1.0)
        cm
      }

      val colorBlueEnd = new Color(56, 72, 153)

      val colorLighBlueEnd = new Color(127, 158, 255)
      val colorLightRedEnd = new Color(229, 142, 140)
      val colorRedEnd = new Color(229, 36, 32)

      val colorMapBlue = {
        val cm = new LinearGradient(new Color(196, 200, 227), new Color(56, 72, 153))
        cm.setRange(0.0, 1.0)
        cm
      }

      val colorMapBlueLight = {
        val cm = new LinearGradient(new Color(196, 200, 227), new Color(127, 158, 255))
        cm.setRange(0.0, 1.0)
        cm
      }

      val colorMapGreen = {
        val cm = new LinearGradient(new Color(202, 227, 197), new Color(72, 153, 55))
        cm.setRange(0.0, 1.0)
        cm
      }

      lazy val meanCurve = Label(s"Best-fit prediction, with noise", new Color(143, 56, 93), shapesmall, filledStroke2) -> curveOfTruncatedGenes(meanParameter)

      lazy val meanCurveWithoutNoise = Label(s"Best-fit prediction, without noise", new Color(143, 56, 93), shapesmall, filledStroke2) -> curveOfTruncatedGenes(meanParameterWithoutNoise)

      lazy val curves = parameters.distinct.par.map {
        case OptimalParameter(p, _) =>
          Label(s"", new Color(200, 200, 200), shapesmall, filledStrokeThin, 1) -> curveOfTruncatedGenes(p)
      }.seq

      lazy val curvesWithoutNoise = parametersWithoutNoise.distinct.par.map {
        case OptimalParameter(p, _) =>
          Label(s"", new Color(200, 200, 200), shapesmall, filledStrokeThin, 1) -> curveOfTruncatedGenes(p)
      }.seq

      lazy val haploSufficientCurves = parameters.distinct.par.map {
        case OptimalParameter(p, _) =>
          Label(s"", new Color(200, 200, 200), shapesmall, filledStrokeThin, 2) -> curveOfTruncatedHaplosufficientGenes(p)
      }.seq

      lazy val haploSufficientMeanCurve = Label(s"Non-haploinsufficient genes", colorBlueEnd, shapesmall, filledStroke) -> curveOfTruncatedHaplosufficientGenes(meanParameter)

      lazy val haploInsufficientMeanCurve = Label(s"Haploinsufficient genes", colorRedEnd, shapesmall, filledStroke) -> curveOfTruncatedHaploinsufficientGenes(meanParameter)

      lazy val haploinsufficientCurves = parameters.distinct.par.map {
        case OptimalParameter(p, _) =>
          Label(s"", new Color(200, 200, 200), shapesmall, filledStrokeThin, 3) -> curveOfTruncatedHaploinsufficientGenes(p)
      }.seq

      lazy val haploinsufficientDeltaCurves = parameters.distinct.par.map {
        case OptimalParameter(p, _) =>
          Label(s"", new Color(200, 200, 200), shapesmall, filledStrokeThin, 3) -> curveOfDeltaTruncatedHIOverDeltaTotal(p)
      }.seq

      lazy val haploinsufficientDeltaMeanCurve = Label(s"Probability of false discovery", new Color(143, 56, 93), shapesmall, filledStroke2) -> curveOfDeltaTruncatedHIOverDeltaTotal(meanParameter)

      lazy val neutral = Label("Neutral model", Color.gray, shape, filledStroke) -> curveOfTruncatedGenes(model.copyParameterNeutral(meanParameter))

      lazy val neutralUniform = Label("Uniform neutral model", Color.gray, shape, filledStroke) -> curveOfTruncatedGenesUniform(model.copyParameterNeutral(meanParameter))

      lazy val neutralshort = Label("Neutral model", Color.gray, shape, filledStroke) -> shortCurveOfTruncatedGenes(model.copyParameterNeutral(meanParameter))

      lazy val observed = Label("Observed truncations", new Color(0, 150, 0), shape) -> curveOfObservedTruncatedGenes

      lazy val observedhomozygous = Label("Observed homozygous truncations", Color.blue, shape) -> curveOfObservedHomozygousTruncatedGenes

      lazy val observedshort = Label("Observed truncations", new Color(0, 150, 0), shape) -> shortCurveOfObservedTruncatedGenes

      lazy val observedshortSevere = Label("Observed severe truncations", Color.orange, shape) -> shortCurveOfObservedSevereTruncatedGenes

      lazy val observedSevere = Label("Observed severe truncations", Color.orange, shape) -> curveOfObservedSevereTruncatedGenes

      val zoomin = Plots.plotDownSamplingMultiple(
        (

        observedshort ::
        neutralshort ::
        observedshortSevere ::
        observedhomozygous ::
        Nil
      ).reverse,
        data.numberOfGenes,
        relative = true,
        maxX = shortXAxisPoints.map(_.value).max / 10000 + 1,
        maxY = data.numberOfGenes,
        xlab = xlab + " \n (x10,000)",
        ylab = "Fraction of genes"
      )

      val observedplotUntil50kWithoutSevere = Plots.plotDownSamplingMultiple(
        (
        observed ::
        neutral ::
        (Label("Observed severe truncations", Color.orange, shape) -> Vector[(Double, Double)]()) ::
        (Label("Observed homozygous truncations", Color.blue, shape) -> Vector[(Double, Double)]()) ::
        Nil
      ).reverse,
        data.numberOfGenes,
        true,
        maxX = model.maxObservedX(data).value / 10000 + 1,
        maxY = data.numberOfGenes,
        xlab = xlab + " \n (x10,000)",
        ylab = "Fraction of genes"
      )

      val observedplotWithFit = Plots.plotDownSamplingMultiple(
        (

        observed ::
        neutral ::
        meanCurve ::
        curves.toList
      ).reverse,
        data.numberOfGenes,
        true,
        maxX = xAxisPoints.map(_.value / 10000).max,
        maxY = data.numberOfGenes,
        xlab = xlab + " \n (x10,000)",
        ylab = "Fraction of genes"
      )

      val observedplotAlone = Plots.plotDownSamplingMultiple(
        (
        observed :: Nil
      ).reverse,
        data.numberOfGenes,
        true,
        maxX = xAxisPoints.map(_.value / 10000).max,
        maxY = data.numberOfGenes,
        xlab = xlab + " \n (x10,000)",
        ylab = "Fraction of genes"
      )

      val neutralPlotWithObservedUniformOnly = Plots.plotDownSamplingMultiple(
        (
        observed ::
        neutralUniform :: Nil
      ).reverse,
        data.numberOfGenes,
        true,
        maxX = xAxisPoints.map(_.value / 10000).max,
        maxY = data.numberOfGenes,
        xlab = xlab + "\n (x10,000)",
        ylab = "Fraction of genes"
      )

      val neutralPlotWithObservedUniformAndNonUniform = Plots.plotDownSamplingMultiple(
        (

        observed ::
        neutralUniform ::
        neutral :: Nil
      ).reverse,
        data.numberOfGenes,
        true,
        maxX = xAxisPoints.map(_.value / 10000).max,
        maxY = data.numberOfGenes,
        xlab = xlab + " \n (x10,000)",
        ylab = "Fraction of genes"
      )

      val neutralPlotWithObserved = Plots.plotDownSamplingMultiple(
        (

        observed ::
        neutral ::
        (Label("Observed severe truncations", Color.orange, shape) -> Vector[(Double, Double)]()) ::
        (Label("Observed homozygous truncations", Color.blue, shape) -> Vector[(Double, Double)]()) ::
        Nil
      ).reverse,
        data.numberOfGenes,
        true,
        maxX = xAxisPoints.map(_.value / 10000).max,
        maxY = data.numberOfGenes,
        xlab = xlab + "\n (x10,000)",
        ylab = "Fraction of genes"
      )

      val neutralPlotWithObservedAndObservedSevere = Plots.plotDownSamplingMultiple(
        (
        observedSevere ::
        observed ::
        neutral ::
        meanCurveWithoutNoise ::
        curvesWithoutNoise.toList

      ).reverse,
        data.numberOfGenes,
        true,
        maxX = xAxisPoints.map(_.value / 10000).max,
        maxY = data.numberOfGenes,
        xlab = xlab + "\n (x10,000)",
        ylab = "Fraction of genes"
      )

      val fitplotNoNoise = Plots.plotDownSamplingMultiple(
        (
        observed +:
        meanCurveWithoutNoise +:
        curvesWithoutNoise
      ).reverse,
        data.numberOfGenes,
        true,
        maxX = xAxisPoints.map(_.value / 10000).max,
        maxY = data.numberOfGenes,
        xlab = xlab + " \n (x10,000)",
        ylab = "Fraction of genes"
      )

      val fitplot = Plots.plotDownSamplingMultiple(
        (
        observed +:
        meanCurve +:
        curves
      ).reverse,
        data.numberOfGenes,
        true,
        maxX = xAxisPoints.map(_.value / 10000).max,
        maxY = data.numberOfGenes,
        xlab = xlab + " \n (x10,000)",
        ylab = "Fraction of genes"
      )

      val deltaPlot = Plots.plotDownSamplingMultiple(
        (
        haploinsufficientDeltaMeanCurve +:
        haploinsufficientDeltaCurves
      ).reverse,
        data.numberOfGenes,
        false,
        maxX = xAxisPoints.map(_.value / 10000).max,
        maxY = 1,
        xlab = xlab + " \n (x10,000)",
        ylab = "Conditional probability",
        lines = List((Label("", Color.black, new BasicStroke(0.5f)), 0, 0.5, xAxisPoints.map(_.value / 10000).max, 0.5))
      )

      val haplosufficientplot = Plots.plotDownSamplingMultiple(
        (

          (haploSufficientCurves ++:
            haploinsufficientCurves :+ haploInsufficientMeanCurve :+ haploSufficientMeanCurve)
        ),
        data.numberOfGenes,
        true,
        maxX = xAxisPoints.map(_.value / 10000).max,
        maxY = data.numberOfGenes,
        xlab = xlab + " \n (x10,000)",
        ylab = "Fraction of genes"
      )

      val haplosufficientplotfused = Plots.plotDownSamplingMultiple(
        (

          curves ++
          (haploSufficientCurves ++:
            haploinsufficientCurves :+ haploInsufficientMeanCurve :+ haploSufficientMeanCurve :+ meanCurve :+ observed)
        ),
        data.numberOfGenes,
        true,
        maxX = xAxisPoints.map(_.value / 10000).max,
        maxY = data.numberOfGenes,
        xlab = xlab + " \n (x10,000)",
        ylab = "Fraction of genes"
      )

      val boxplotinset = BoxPlotWrapper.createBoxPlot(List(parameters.map(_.optimum.fractionOfEssentials)), xlab = "", ylab = "Estimated size of haploinsufficient genome")

      val individualplotinfiles = List(
        "boxplotinset" -> boxplotinset,
        "haplosufficientplotfused" -> haplosufficientplotfused,
        "observedplotWithFit" -> observedplotWithFit,
        "neutralPlotWithObservedUniformOnly" -> neutralPlotWithObservedUniformOnly,
        "neutralPlotWithObservedUniformAndNonUniform" -> neutralPlotWithObservedUniformAndNonUniform,
        "neutralPlotWithObserved" -> neutralPlotWithObserved,
        "observedplotWithFit" -> observedplotWithFit,
        "zoomin" -> zoomin,
        "observedplotAlone" -> observedplotAlone,
        "observedplotUntil50kWithoutSevere" -> observedplotUntil50kWithoutSevere,
        "fitplot" -> fitplot,
        "fitplotNoNoise" -> fitplotNoNoise,
        "haplosufficientplot" -> haplosufficientplot,
        "deltaPlot" -> deltaPlot
      ).map {
          case (n, p) =>
            val tmp = TempFile.createTempFile(".pdf")
            pdfToFile(tmp, p)
            n -> tmp
        }.toMap

      def plotParameterDistribution2D(values: Seq[OptimalParameter[Param]]) = {

        val dimensions = values.head.optimum.extractForPlot.keys.toSet.toList

        val scatters = dimensions.combinations(2).toList.map { dims =>
          val dim1 = dims(0)
          val dim2 = dims(1)

          val scatter = nice(ScatterPlot.createScatterPlotFromMultiple(
            data = List(
              Label("Parameter estimates", Color.black) -> values.toIndexedSeq.map(x => (x.optimum.extractForPlot(dim1), x.optimum.extractForPlot(dim2), x.residual))

            ),
            xlab = dim1,
            ylab = dim2,
            main = "Joint distribution of parameter estimates"
          // xlim = Some((0.0, 1.0)),
          // ylim = Some((0.0, 1.0))
          ))
          ("estimates.scatter." + dim1 + ".vs." + dim2) -> scatter
        }

        val histograms = dimensions.map { dim =>
          "estimates.histogram." + dim -> HistogramPlot.createHistogramPlot(values.map(_.optimum.extractForPlot(dim)), main = "Marginal density of parameter estimates: " + dim)
        }

        scatters ++ histograms
      }

      val parameterplots = plotParameterDistribution2D(parameters).toMap

      val parameterplotfiles = parameterplots.map {
        case (n, p) =>
          val tmp = TempFile.createTempFile(".pdf")
          pdfToFile(tmp, p)
          n -> tmp
      }

      val densityplot = {
        val predicted = parameters.map { p =>
          ScatterPlot.Label("Predicted", Color.gray, circle(2.0), new BasicStroke(1.0f)) -> seq2(model.predictDensity(p.optimum, data).map(x => x._1.toDouble -> x._2))
        }
        val observedDataHistogram = ScatterPlot.Label("Observed counts", Color.blue, circle(2.0), new BasicStroke(1.0f)) -> HistogramData(data.observedCounts.filter(_.value <= 80).map(_.value.toDouble), breaks = 81).toScatter

        nice(ScatterPlot.createScatterPlotFromMultiple(
          data = observedDataHistogram +: predicted,
          xlab = "Number of truncation",
          ylab = "Number of genes",
          main = "Histogram of genes with given number of truncations"
        ))

      }

      val compositeOnlyObserved = {

        val observedplotUntil50k = observedplotUntil50kWithoutSevere

        nice(observedplotUntil50k)
        nice(zoomin)

        zoomin.getLegend.clear
        zoomin.setInsets(new Insets2D.Double(2.0, 125.0, 125.0, 2.0));
        zoomin.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL, "")
        zoomin.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL, "")
        zoomin.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICKS_SPACING, 2000.0)
        zoomin.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS_SPACING, 0.05)
        zoomin.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS_AUTO_SPACING, false)
        zoomin.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICKS_AUTO_SPACING, false)
        zoomin.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL_FONT, fontsmall)
        zoomin.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL_FONT, fontsmall)
        zoomin.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS_FONT, fontsmaller)
        zoomin.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICKS_FONT, fontsmaller)
        zoomin.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICK_LABELS_OUTSIDE, false)
        zoomin.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICK_LABELS_OUTSIDE, true)
        zoomin.getAxis(XYPlot.AXIS_Y).setMin(0.000)

        zoomin.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS, true)

        observedplotUntil50k.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL, "Fraction of genes")
        observedplotUntil50k.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS, true)
        observedplotUntil50k.setInsets(new Insets2D.Double(2.0, 125.0, 450.0, 125.0));
        observedplotUntil50k.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL, "Number of truncating variants \n (x10,000)")

        val container = new DrawableContainer(new FreeLayout)

        container.add(observedplotUntil50k)
        // container.add(zoomin)

        observedplotUntil50k.setBounds(0.0, 0.0, 645.0, 900.0)
        // zoomin.setBounds(160.0, 210.0, 350.0, 350.0)

        val bounds = new java.awt.geom.Rectangle2D.Double(0.0, 0.0, 645.0, 900.0)
        container.setBounds(bounds)
        container
      }

      val composite2 = {

        zoomin.getLegend.clear
        zoomin.setInsets(new Insets2D.Double(2.0, 125.0, 125.0, 2.0));
        zoomin.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL, "")
        zoomin.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL, "")
        zoomin.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICKS_SPACING, 2000.0)
        zoomin.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS_SPACING, 0.2)
        zoomin.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS_AUTO_SPACING, false)
        zoomin.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICKS_AUTO_SPACING, false)
        zoomin.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL_FONT, fontsmall)
        zoomin.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL_FONT, fontsmall)
        zoomin.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS_FONT, fontsmaller)
        zoomin.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICKS_FONT, fontsmaller)
        zoomin.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICK_LABELS_OUTSIDE, false)
        zoomin.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICK_LABELS_OUTSIDE, true)
        zoomin.getAxis(XYPlot.AXIS_Y).setMin(0.000)

        zoomin.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS, true)

        boxplotinset.getLegend.clear
        boxplotinset.setInsets(new Insets2D.Double(2.0, 125.0, 125.0, 2.0));
        boxplotinset.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL, "")
        boxplotinset.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL, "")
        boxplotinset.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS, true)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICKS, false)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICKS_SPACING, 0.2)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS_SPACING, 0.2)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICKS_AUTO_SPACING, false)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS_AUTO_SPACING, false)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICK_LABELS_FORMAT, new java.text.DecimalFormat("0.0"))
        boxplotinset.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL_FONT, fontsmall)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL_FONT, fontsmall)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS_FONT, fontsmaller)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICKS_FONT, fontsmaller)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICK_LABELS_OUTSIDE, false)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICK_LABELS_OUTSIDE, false)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL_DISTANCE, -2.4)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL_DISTANCE, -2.1)
        // boxplotinsetTwoParam.getAxis(XYPlot.AXIS_Y).setMin(0.0001)
        // boxplotinsetTwoParam.getAxis(XYPlot.AXIS_X).setMin(0.0001)
        boxplotinset.getAxis(XYPlot.AXIS_Y).setMax(0.5)
        boxplotinset.getAxis(XYPlot.AXIS_Y).setMin(0.01)
        boxplotinset.getAxis(XYPlot.AXIS_X).setMin(0.3)

        neutralPlotWithObservedAndObservedSevere.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL, "Fraction of genes")
        neutralPlotWithObservedAndObservedSevere.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS, true)
        neutralPlotWithObservedAndObservedSevere.setInsets(new Insets2D.Double(2.0, 125.0, 450.0, 5.0));

        fitplot.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL, "")
        fitplot.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS, false)
        fitplot.setInsets(new Insets2D.Double(2.0, 10.0, 450.0, 5.0));

        haplosufficientplot.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL, "")
        haplosufficientplot.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS, false)
        haplosufficientplot.setInsets(new Insets2D.Double(2.0, 10.0, 450.0, 5.0));

        neutralPlotWithObserved.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL, "")
        haplosufficientplot.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL, "")
        fitplot.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL, xlab + "(x10,000)")

        val container = new DrawableContainer(new FreeLayout)

        container.add(neutralPlotWithObservedAndObservedSevere)
        container.add(fitplot)
        container.add(haplosufficientplot)
        container.add(zoomin)
        container.add(boxplotinset)

        neutralPlotWithObservedAndObservedSevere.setBounds(0.0, 0.0, 525.0, 900.0)
        fitplot.setBounds(525.0, 0.0, 400.0, 900.0)
        haplosufficientplot.setBounds(925.0, 0.0, 400.0, 900.0)
        zoomin.setBounds(135.0, 180.0, 380.0, 380.0)
        boxplotinset.setBounds(535.0, 180.0, 380.0, 380.0)

        val bounds = new java.awt.geom.Rectangle2D.Double(0.0, 0.0, 1525.0, 900.0)
        container.setBounds(bounds)
        container
      }

      val compososite2File = pdfToFile(composite2)

      val composite4NoNoise = {

        neutralPlotWithObservedAndObservedSevere.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL, "Fraction of truncated genes")
        neutralPlotWithObservedAndObservedSevere.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS, true)
        neutralPlotWithObservedAndObservedSevere.setInsets(new Insets2D.Double(2.0, 125.0, 450.0, 5.0));

        fitplotNoNoise.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL, "")
        fitplotNoNoise.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS, false)
        fitplotNoNoise.setInsets(new Insets2D.Double(2.0, 10.0, 450.0, 5.0));

        haplosufficientplotfused.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL, "")
        haplosufficientplotfused.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS, false)
        haplosufficientplotfused.setInsets(new Insets2D.Double(2.0, 10.0, 450.0, 5.0));

        neutralPlotWithObservedAndObservedSevere.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL, xlab + "(x10,000)")
        neutralPlotWithObservedAndObservedSevere.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL_POSITION, 0.9)
        haplosufficientplotfused.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL, "")
        fitplotNoNoise.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL, xlab + "(x10,000)")
        // fitplotNoNoise.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL_POSITION, 1.0)

        val container = new DrawableContainer(new FreeLayout)

        container.add(neutralPlotWithObservedAndObservedSevere)
        container.add(haplosufficientplotfused)

        neutralPlotWithObservedAndObservedSevere.setBounds(0.0, 0.0, 525.0, 900.0)
        haplosufficientplotfused.setBounds(525.0, 0.0, 400.0, 900.0)

        val bounds = new java.awt.geom.Rectangle2D.Double(0.0, 0.0, 1125.0, 900.0)
        container.setBounds(bounds)
        container
      }

      val compososite4File = pdfToFile(composite4NoNoise)

      val composite3 = {

        zoomin.getLegend.clear
        zoomin.setInsets(new Insets2D.Double(2.0, 125.0, 125.0, 2.0));
        zoomin.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL, "")
        zoomin.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL, "")
        zoomin.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICKS_SPACING, 2000.0)
        zoomin.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS_SPACING, 0.2)
        zoomin.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS_AUTO_SPACING, false)
        zoomin.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICKS_AUTO_SPACING, false)
        zoomin.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL_FONT, fontsmall)
        zoomin.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL_FONT, fontsmall)
        zoomin.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS_FONT, fontsmaller)
        zoomin.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICKS_FONT, fontsmaller)
        zoomin.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICK_LABELS_OUTSIDE, false)
        zoomin.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICK_LABELS_OUTSIDE, true)
        zoomin.getAxis(XYPlot.AXIS_Y).setMin(0.000)

        zoomin.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS, true)

        boxplotinset.getLegend.clear
        boxplotinset.setInsets(new Insets2D.Double(2.0, 125.0, 125.0, 2.0));
        boxplotinset.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL, "")
        boxplotinset.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL, "")
        boxplotinset.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS, true)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICKS, false)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICKS_SPACING, 0.2)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS_SPACING, 0.2)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICKS_AUTO_SPACING, false)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS_AUTO_SPACING, false)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICK_LABELS_FORMAT, new java.text.DecimalFormat("0.0"))
        boxplotinset.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL_FONT, fontsmall)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL_FONT, fontsmall)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS_FONT, fontsmaller)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICKS_FONT, fontsmaller)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICK_LABELS_OUTSIDE, false)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICK_LABELS_OUTSIDE, false)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL_DISTANCE, -2.4)
        boxplotinset.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL_DISTANCE, -2.1)
        // boxplotinsetTwoParam.getAxis(XYPlot.AXIS_Y).setMin(0.0001)
        // boxplotinsetTwoParam.getAxis(XYPlot.AXIS_X).setMin(0.0001)
        boxplotinset.getAxis(XYPlot.AXIS_Y).setMax(0.5)
        boxplotinset.getAxis(XYPlot.AXIS_Y).setMin(0.01)
        boxplotinset.getAxis(XYPlot.AXIS_X).setMin(0.3)

        neutralPlotWithObserved.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL, "Fraction of truncated genes")
        neutralPlotWithObserved.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS, true)
        neutralPlotWithObserved.setInsets(new Insets2D.Double(2.0, 125.0, 450.0, 5.0));

        fitplot.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL, "")
        fitplot.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS, false)
        fitplot.setInsets(new Insets2D.Double(2.0, 10.0, 450.0, 5.0));

        haplosufficientplot.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL, "")
        haplosufficientplot.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS, false)
        haplosufficientplot.setInsets(new Insets2D.Double(2.0, 10.0, 450.0, 5.0));

        neutralPlotWithObserved.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL, "")
        haplosufficientplot.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL, "")
        fitplot.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL, xlab + "(x10,000)")

        val container = new DrawableContainer(new FreeLayout)

        container.add(neutralPlotWithObserved)
        container.add(fitplot)
        container.add(haplosufficientplot)
        container.add(zoomin)
        container.add(boxplotinset)

        neutralPlotWithObserved.setBounds(0.0, 0.0, 525.0, 900.0)
        fitplot.setBounds(525.0, 0.0, 400.0, 900.0)
        haplosufficientplot.setBounds(925.0, 0.0, 400.0, 900.0)
        zoomin.setBounds(135.0, 180.0, 380.0, 380.0)
        boxplotinset.setBounds(535.0, 180.0, 380.0, 380.0)

        val bounds = new java.awt.geom.Rectangle2D.Double(0.0, 0.0, 1525.0, 900.0)
        container.setBounds(bounds)
        compositeTable(neutralPlotWithObserved :: fitplot :: haplosufficientplot :: densityplot :: parameterplots.toList.map(_._2), 3)
      }

      val (perGenePlotRelativeToNeutralFile,
        perGenePlotFile,
        scatterVsNeutralFile,
        scatterVsPredictedFile) = {
        import de.erichseifert.gral.graphics._

        val colorMapRed = {
          val cm = new HeatMapColors
          cm.setRange(0.0, 1.0)
          cm
        }

        val dataForPlot = (zip(model.predictedCountsPlugBackPosterior(meanParameter, data), data.expectedCounts, data.observedCounts,
          model.posteriorOfHaploinsufficient(data, meanParameter)).toVector map {
            case (hi, exp: ExpectedTruncationCount, obs: ObservedTruncationCount, posterior) =>
              (hi, exp, obs, posterior.pHI)
          } filter (x => (x._4 < 0.2 || x._4 > 0.8 || true)) sortBy (x => x._4) zipWithIndex) map (x => (x._1._1, x._1._2, x._1._3, x._1._4, x._2))

        // println(dataForPlot.map(x => x._1 / x._2.value))

        val p1 = pdfToFile(ScatterPlot.createScatterPlotFromMultiple(
          data =

          Label("Observed", colorMapRed, circle(1.0)) -> seq3(dataForPlot.map(x => (x._5.toDouble, (x._3.value.toDouble / x._2.value), x._4))) ::
            Label("PredictedHI", Color.black, circle(1.0)) -> seq2(dataForPlot.map(x => x._5.toDouble -> (x._1 / x._2.value))) ::
            Label("Neutral", Color.green, circle(1.0)) -> seq2(dataForPlot.map(x => (x._5.toDouble, (x._2.value / x._2.value)))) ::
            // Label("PredictedHS", Color.blue) -> seq2(dataForPlot.map(x => x._5.toDouble -> x._2 / x._3.value)) ::
            Nil,
          ylim = Some(-0.1, 1.5)
        ))

        val p2 = pdfToFile(ScatterPlot.createScatterPlotFromMultiple(
          data =

          Label("Observed", colorMapRed, circle(1.0)) -> seq3(dataForPlot.map(x => (x._5.toDouble, (x._3.value.toDouble), x._4))) ::
            Label("PredictedHI", Color.black, circle(1.0)) -> seq2(dataForPlot.map(x => x._5.toDouble -> (x._1))) ::
            Label("Neutral", Color.green, circle(1.0)) -> seq2(dataForPlot.map(x => (x._5.toDouble, (x._2.value)))) ::
            // Label("PredictedHS", Color.blue) -> seq2(dataForPlot.map(x => x._5.toDouble -> x._2 / x._3.value)) ::
            Nil,
          ylim = Some(-0.1, 30)
        ))

        val p3 = pdfToFile(ScatterPlot.createScatterPlotFromMultiple(
          data =
          Label("Observed", colorMapRed, circle(1.0)) -> seq3(dataForPlot.map(x => (x._2.value.toDouble, x._3.value.toDouble, x._4))) ::
            Nil
        ))

        val p4 = pdfToFile(ScatterPlot.createScatterPlotFromMultiple(
          data =
          Label("Observed", colorMapRed, circle(1.0)) -> seq3(dataForPlot.map(x => (x._1, x._3.value.toDouble, x._4))) ::
            Nil
        ))

        (p1, p2, p3, p4)

      }

      (Map(
        "composite.onlyobserved" -> compositeOnlyObserved,
        "composite3" -> composite3,
        "densityplot" -> densityplot
      )).map {
          case (n, p) =>
            val tmp = TempFile.createTempFile(".pdf")
            pdfToFile(tmp, p)
            n -> tmp
        } ++ individualplotinfiles ++ parameterplotfiles ++
        Map(
          "composite2" -> compososite2File,
          "composite4" -> compososite4File,
          "perGenePlot" -> perGenePlotFile,
          "perGenePlotRelativeToNeutral" -> perGenePlotRelativeToNeutralFile,
          "scatterVsNeutral" -> scatterVsNeutralFile,
          "scatterVsPredicted" -> scatterVsPredictedFile
        )

    }
  }

  def plotNewError(data: Seq[(Label, IndexedSeq[(Double, Double)])], verticalLines: Seq[Double], colormap: ColorMapper, ylab: String = "Conditional probability"): Plot = {
    val shape = new java.awt.geom.Ellipse2D.Double(-1.5, -1.5, 3.0, 3.0)
    val shapesmall = new java.awt.geom.Ellipse2D.Double(-0.5, -0.5, 1.5, 1.5)
    val shapesmaller = new java.awt.geom.Ellipse2D.Double(-0.1, -0.1, 0.2, 0.2)

    val dataForPlots: Seq[(Label, IndexedSeq[D2vD3])] = data.map(x => x._1 -> seq2(x._2))

    val max = data.flatMap(_._2.map(_._1)).max

    val plot = mybiotools.plots.ScatterPlot.createScatterPlotFromMultiple(
      data = (dataForPlots).reverse,
      xlab = "Number of truncating variants",
      ylab = ylab,
      main = "",
      join = false,
      lines = verticalLines.map(x => (Label("Saturation of \n haplosufficient genome", Color.black, shapesmall, new BasicStroke(3.0f)), x, 0.0, x, 1.0))
    )
    val axis_X = plot.getAxis(XYPlot.AXIS_X)
    val axis_Y = plot.getAxis(XYPlot.AXIS_Y)
    axis_X.setMin(0.0)
    axis_X.setMax(20)
    axis_Y.setMin(0.0)
    axis_Y.setMax(1.0)
    plot
  }

  def plotDownSamplingMultiple(data: Seq[(Label, Seq[(Double, Double)])], totalCount: Int, relative: Boolean,
    maxY: Int = 25000,
    maxX: Int = 25000,
    lines: Seq[(Label, Double, Double, Double, Double)] = Nil,
    xlab: String,
    ylab: String): Plot = {

    val dataForPlots: Seq[(Label, IndexedSeq[D2vD3])] = data.map(z => z._1 -> seq2(z._2.map(y => y._1.toDouble -> (if (relative) y._2.toDouble / totalCount else y._2.toDouble)).toIndexedSeq))

    val plot = mybiotools.plots.ScatterPlot.createScatterPlotFromMultiple(
      data = dataForPlots,
      xlab = xlab,
      ylab = ylab,
      main = "",
      join = false,
      lines = lines,
      compressPoints = true
    )
    val axis_X = plot.getAxis(XYPlot.AXIS_X)
    val axis_Y = plot.getAxis(XYPlot.AXIS_Y)
    axis_X.setMin(0.0)
    axis_X.setMax(maxX.toDouble)
    axis_Y.setMin(0.0)
    if (relative)
      axis_Y.setMax(maxY.toDouble / totalCount)
    else
      axis_Y.setMax(maxY.toDouble)
    // val maxline = new DataTable(classOf[java.lang.Double], classOf[java.lang.Double])
    // maxline.add(0.0, totalCount.toDouble)
    // maxline.add(25000.0, totalCount.toDouble)
    // plot.add(maxline)
    // val lr = new DefaultLineRenderer2D();
    // lr.setSetting(LineRenderer.COLOR, new java.awt.Color(58, 95, 205))
    // lr.setSetting(LineRenderer.STROKE, new BasicStroke(1.5f))
    // plot.setLineRenderer(maxline, lr)
    // plot.setPointRenderer(maxline, null);

    nice(plot)
  }

}