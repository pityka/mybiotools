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

import mybiotools._
import java.io.File
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
import de.erichseifert.gral.graphics.Drawable
import de.erichseifert.gral.util.Location
import java.awt.Color
import java.awt.BasicStroke
import scala.runtime.RichInt
import scala.runtime.RichFloat
import de.erichseifert.gral.graphics.DrawableContainer
import de.erichseifert.gral.graphics.TableLayout
import de.erichseifert.gral.plots.colors.QuasiRandomColors
import de.erichseifert.gral.plots.BarPlot
import de.erichseifert.gral.util.DataUtils;
import de.erichseifert.gral.util.PointND;

import de.erichseifert.gral.graphics.EdgeLayout

import mybiotools.SummaryStat
import mybiotools.plots._
import mybiotools.plots.ScatterPlot._
import de.erichseifert.gral.graphics._

case class DataForEnrichmentPlot(pValue: Double, avgde: Double, representativeCategory: String)
object EnrichmentPlot {

  def createPlot(data: Seq[DataForEnrichmentPlot]): Drawable = {
    if (data.isEmpty) new XYPlot
    else {
      val representativeCategoryBounds: Map[String, (Double, Double)] = {
        val sortedBySize: Seq[(String, Int)] = data.groupBy(x => x.representativeCategory).toSeq.map(x => x._1 -> x._2.size).sortBy(_._1)
        val total = sortedBySize.map(_._2).sum
        val sortedByRelativeSize: Seq[(String, Double)] = sortedBySize.map(x => x._1 -> x._2.toDouble / total)

        {
          val cdf = sortedByRelativeSize.foldLeft(List[(String, Double, Double)]()) {
            case (list, (t0, w0)) =>
              list match {
                case Nil => (t0, 0.0, w0) :: list
                case (t1, start, end) :: xs => (t0, end, end + w0) :: (t1, start, end) :: xs
              }
          }.toIndexedSeq.reverse
          assert(math.abs(cdf.last._3 - 1.0) < 1E-8, cdf)
          val last = cdf.last

          cdf.dropRight(1) :+ (last._1, last._2, 1.0)
        }.map(x => x._1 -> (x._2, x._3)).toMap
      }

      val categoryColors: Map[String, Color] = representativeCategoryBounds.zipWithIndex.map(x => x._1._1 -> colorPick(x._2, representativeCategoryBounds.size)).toMap

      val pvaluefunction = (pvalue: Double) => math.min(30.0, (math.log10(pvalue) * (-1)) * 3)

      val sets: Seq[(Label, IndexedSeq[D2vD3])] = data.map {
        case DataForEnrichmentPlot(pvalue, avgdiff, category) =>
          val color = categoryColors(category)
          val (xstart, xend) = representativeCategoryBounds(category)
          val xwidth = xend - xstart
          val size = pvaluefunction(pvalue)
          val xpos = math.abs(DataForEnrichmentPlot(pvalue, avgdiff, category).hashCode.toDouble / Int.MaxValue.toDouble) * xwidth + xstart

          val ypos = avgdiff
          val shape = new java.awt.geom.Ellipse2D.Double(-size / 2.0, -size / 2.0, size, size)
          Label("", new java.awt.Color(color.getRed, color.getGreen, color.getBlue, 150), shape) -> seq2(Vector((xpos -> ypos)))
      }.toSeq

      val max = data.map(_.avgde).max
      val min = data.map(_.avgde).min
      val plotmax = math.max(math.abs(max), math.abs(min)) * 1.1

      val borderlines = representativeCategoryBounds.toSeq.sortBy(_._2._1).dropRight(1).map {
        case (_, (_, end)) =>
          (Label("", new Color(200, 200, 200), new BasicStroke(
            1.0f,
            BasicStroke.CAP_BUTT,
            BasicStroke.JOIN_MITER,
            10.0f, Array(10f), 0.0f
          )), end, -100d, end, 100d)
      }.toSeq

      val plot = createScatterPlotFromMultiple(
        data = sets,
        legend = false,
        xnames = representativeCategoryBounds.map(x => (x._2._1 + x._2._2) / 2 -> x._1),
        xlim = Some(0.0000001, 1.05),
        ylim = Some(-1 * plotmax -> plotmax),
        ylab = "avg log2 fold change",
        lines = borderlines :+ (Label("", Color.BLACK, new BasicStroke(1.0f)), 0.0, 0.0, 2.0, 0.0)
      )

      val rendererx = plot.getAxisRenderer(XYPlot.AXIS_X)
      val renderery = plot.getAxisRenderer(XYPlot.AXIS_Y)
      val plotarea = plot.getPlotArea

      rendererx.setSetting(AxisRenderer.TICKS_SPACING, 100000)
      rendererx.setSetting(AxisRenderer.TICKS_LENGTH, 0)
      // rendererx.setSetting(AxisRenderer.TICK_LABELS_DISTANCE, 18)
      rendererx.setSetting(AxisRenderer.TICK_LABELS_ROTATION, 0)
      rendererx.setSetting(AxisRenderer.TICK_LABELS_ROTATION, 90)
      renderery.setSetting(AxisRenderer.TICKS_MINOR, false)
      renderery.setSetting(AxisRenderer.TICK_LABELS_DISTANCE, 0.5)
      renderery.setSetting(AxisRenderer.LABEL_DISTANCE, 0.5)

      plotarea.setSetting(XYPlotArea2D.GRID_MAJOR_X, false)
      plotarea.setSetting(XYPlotArea2D.GRID_MAJOR_Y, false)
      plotarea.setSetting(XYPlotArea2D.GRID_MINOR_X, false)
      plotarea.setSetting(XYPlotArea2D.GRID_MINOR_Y, false)
      // renderery.setSetting(AxisRenderer.INTERSECTION, 0.0);
      // rendererx.setSetting(AxisRenderer.INTERSECTION, 0.0);

      val legend = new GenericLegend {
        add(new AbstractDrawable {

          def draw(context: DrawingContext): Unit = {
            import de.erichseifert.gral.plots.Label
            val pixelYMin = getBounds.getMinY + 10
            val pixelYMax = pixelYMin + 100
            val pixelYRange = pixelYMax - pixelYMin

            val z = -3.0
            def d(z: Double): Unit = {
              val y1 = pixelYMax - pixelYRange * math.abs(z) / 10.0
              val r = pvaluefunction(math.pow(10.0, z))
              GraphicsUtils.fillPaintedShape(
                context.getGraphics(), new java.awt.geom.Ellipse2D.Double(getBounds.getMinX + 30 - r / 2.0, y1, r, r), Color.black, null
              )

              val label = new de.erichseifert.gral.plots.Label("1E" + z.toString);
              // label.setSetting(Label.ALIGNMENT_X, getBounds.getMinX + 100);
              // label.setSetting(Label.ALIGNMENT_Y, y1);
              // label.setSetting(Label.ROTATION, 0.0);
              label.setBounds(getBounds.getMinX + 70, y1 + r / 2.0 - 5, 10, 10)

              label.draw(context);
            }
            d(-3)
            d(-5)
            d(-10)

          }
        }, Location.CENTER)
      }
      legend.setBounds(new java.awt.geom.Rectangle2D.Double(0.0, 0.0, 120.0, 500.0))
      plot.setLegend(legend)
      plot.setSetting(Plot.LEGEND, true)
      plot.setSetting(Plot.LEGEND_LOCATION, Location.EAST)
      // legend.setBounds(plot.getBounds.getWidth * 0.91, plot.getHeight * 0.5, 30, 100)

      // val container =
      //   new DrawableContainer(new FreeLayout);

      // container.setBounds(plot.getBounds)
      // container.add(plot)
      // container.add(legend)
      // plot.setBounds(0, 0, 900, 600)

      // container
      plot
      // legend
    }
  }
}