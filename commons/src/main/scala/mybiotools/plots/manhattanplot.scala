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

package mybiotools.plots

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
import java.awt._;
import scala.runtime.RichInt
import scala.runtime.RichFloat
import de.erichseifert.gral.graphics.DrawableContainer
import de.erichseifert.gral.graphics.TableLayout
import de.erichseifert.gral.graphics.EdgeLayout

import mybiotools.SummaryStat
import mybiotools.gwascommons._
import mybiotools._
import mybiotools.gwascommons.associationresults._

import scala.collection.JavaConversions._

object ManhattanPlot {

  val ChromosomeLengths = Map(
    1 -> 248000000,
    2 -> 243000000,
    3 -> 200000000,
    4 -> 192000000,
    5 -> 181000000,
    6 -> 171000000,
    7 -> 159000000,
    8 -> 147000000,
    9 -> 141000000,
    10 -> 136000000,
    11 -> 135000000,
    12 -> 133000000,
    13 -> 115000000,
    14 -> 107000000,
    15 -> 101000000,
    16 -> 89000000,
    17 -> 79000000,
    18 -> 76000000,
    19 -> 64000000,
    20 -> 63000000,
    21 -> 47000000,
    22 -> 50000000,
    23 -> 155000000, // X
    24 -> 58000000 // Y
  )

  def plotToPNG[T <: HasName with HasGenomicLocation with HasPValue](
    data: Iterator[T],
    bonferroniLine: Option[Double] = None,
    maxY: Option[Double] = None,
    maxChr: Int = 24,
    scale: Double = 2
  ): Array[Byte] =
    renderToByteArray(
      plot(data, bonferroniLine, maxY, maxChr),
      "image/png", scale
    )

  def plot[T <: HasName with HasGenomicLocation with HasPValue](
    data: Iterator[T], bonferroniLine: Option[Double], maxY: Option[Double], maxChromosomeToPlot: Int = 24
  ): AbstractDrawable = {

    def cumulativeChrLength(i: Int): Double = (1 to i) map ChromosomeLengths map (_.toDouble) sum

    var lowestPValue = 1.0

    var datasize = 0L

    val pvaluedatatablesByChromosome: Map[Int, DataTable] = {
      val tables = collection.mutable.Map((1 to maxChromosomeToPlot).map((chr: Int) =>
        chr -> new DataTable(classOf[java.lang.Double], classOf[java.lang.Double])).toSeq: _*)

      data.foreach { r =>

        val pos = cumulativeChrLength(chromosomeNumberFromString(r.genomicLocation.chromosome) - 1) + r.genomicLocation.basePairPosition
        val p = math.log10(r.pValue) * -1.0
        if (r.pValue < lowestPValue) {
          lowestPValue = r.pValue
        }

        datasize += 1

        if (chromosomeNumberFromString(r.genomicLocation.chromosome) > 0 && chromosomeNumberFromString(r.genomicLocation.chromosome) <= maxChromosomeToPlot) {
          tables(chromosomeNumberFromString(r.genomicLocation.chromosome)).add((pos).toDouble, (p))
        }

      }
      tables.filter(_._2.getRowCount > 0).toMap
    }

    val plot = new XYPlot(pvaluedatatablesByChromosome.values.toSeq: _*)

    val bonferroniLineYValue = bonferroniLine match {
      case Some(x) => -1.0 * math.log10(x)
      case None => -1.0 * math.log10(0.05 / datasize.toDouble)
    }

    val bonferroniLineTable = {
      val table = new DataTable(classOf[java.lang.Double], classOf[java.lang.Double])
      table.add(0.0, bonferroniLineYValue)
      table.add(ChromosomeLengths.filterKeys(_ <= maxChromosomeToPlot).values.map(_.toDouble).sum, bonferroniLineYValue)
      table
    }

    plot.add(bonferroniLineTable)

    // Customization

    val bonferroniLineRenderer = new DefaultLineRenderer2D();
    plot.setPointRenderer(bonferroniLineTable, null);
    bonferroniLineRenderer.setSetting(LineRenderer.COLOR, Color.black);
    bonferroniLineRenderer.setSetting(LineRenderer.STROKE, new BasicStroke(
      2.0f,
      BasicStroke.CAP_ROUND,
      BasicStroke.JOIN_MITER,
      1.0f,
      Array(8.0f, 8.0f),
      0.0f
    ))
    plot.setLineRenderer(bonferroniLineTable, bonferroniLineRenderer);

    val pointrenderersOdd = pvaluedatatablesByChromosome.filterKeys(_ % 2 != 0).values.map(x => plot.getPointRenderer(x)).toSeq
    val pointrenderersEven = pvaluedatatablesByChromosome.filterKeys(_ % 2 == 0).values.map(x => plot.getPointRenderer(x)).toSeq

    (pointrenderersEven ++ pointrenderersOdd).foreach { x =>
      x.setSetting(PointRenderer.SHAPE, new java.awt.geom.Ellipse2D.Double(0, 0, 5, 5))
    }

    val evenColor = new java.awt.Color(58, 95, 205)
    val oddColor = new java.awt.Color(205, 55, 0)

    (pointrenderersEven).foreach { x =>
      x.setSetting(PointRenderer.COLOR, evenColor)
    }

    (pointrenderersOdd).foreach { x =>
      x.setSetting(PointRenderer.COLOR, oddColor)
    }

    val plotarea = plot.getPlotArea
    plotarea.setSetting(XYPlotArea2D.GRID_MAJOR_X, false)
    plotarea.setSetting(XYPlotArea2D.GRID_MAJOR_Y, false)
    plotarea.setSetting(XYPlotArea2D.GRID_MINOR_X, false)
    plotarea.setSetting(XYPlotArea2D.GRID_MINOR_Y, false)

    plotarea.setSetting(PlotArea.BORDER, new BasicStroke(0.0f))

    val axis_X = plot.getAxis(XYPlot.AXIS_X)
    val axis_Y = plot.getAxis(XYPlot.AXIS_Y)

    axis_X.setMin(0)
    axis_X.setMax(ChromosomeLengths.filterKeys(_ <= maxChromosomeToPlot).values.map(_.toDouble).sum)

    if (maxY.isDefined) {
      axis_Y.setMax(maxY.get)
    } else {
      axis_Y.setMax(-1.0 * math.log10(lowestPValue) + 0.5)
    }

    val rendererY = plot.getAxisRenderer(XYPlot.AXIS_Y);
    val rendererX = plot.getAxisRenderer(XYPlot.AXIS_X);

    val chrTicks: java.util.Map[Double, String] = ChromosomeLengths.map {
      case (chr, length) =>
        (cumulativeChrLength(chr - 1) + length / 2) -> chr.toString
    }

    rendererX.setSetting(AxisRenderer.TICKS_MINOR, false)
    rendererX.setSetting(AxisRenderer.TICKS, true)
    rendererX.setSetting(AxisRenderer.TICKS_AUTO_SPACING, false)
    rendererX.setSetting(AxisRenderer.TICKS_SPACING, ChromosomeLengths.values.map(_.toDouble).sum)
    rendererX.setSetting(AxisRenderer.TICKS_CUSTOM, chrTicks)
    rendererX.setSetting(AxisRenderer.TICKS_LENGTH, 0.3)
    rendererX.setSetting(AxisRenderer.TICKS_ALIGNMENT, 0.0)
    rendererX.setSetting(AxisRenderer.LABEL, "human chromosomes")

    rendererY.setSetting(AxisRenderer.LABEL, "-log10 p-values")
    rendererY.setSetting(AxisRenderer.TICKS_MINOR, false)
    rendererY.setSetting(AxisRenderer.INTERSECTION, -Double.MaxValue);
    rendererY.setSetting(AxisRenderer.TICKS_LENGTH, 0.3)
    rendererY.setSetting(AxisRenderer.TICKS_ALIGNMENT, 0.0)

    val fontbig = new java.awt.Font(Font.SANS_SERIF, Font.PLAIN, 20)
    val fontsmall = new java.awt.Font(Font.SANS_SERIF, Font.PLAIN, 12)

    rendererX.setSetting(AxisRenderer.LABEL_FONT, fontbig)
    rendererY.setSetting(AxisRenderer.LABEL_FONT, fontbig)
    rendererX.setSetting(AxisRenderer.TICKS_FONT, fontsmall)
    rendererY.setSetting(AxisRenderer.TICKS_FONT, fontsmall)

    plot.setInsets(new Insets2D.Double(10.0, 80.0, 80.0, 10.0));

    val bounds = new java.awt.geom.Rectangle2D.Double(0.0, 0.0, 1300.0, 500.0)
    plot.setBounds(bounds)

    plot
  }

}