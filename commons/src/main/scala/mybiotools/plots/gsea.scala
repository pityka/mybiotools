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
import java.awt.{ BasicStroke, Font, Color };
import scala.runtime.RichInt
import scala.runtime.RichFloat
import de.erichseifert.gral.graphics.DrawableContainer
import de.erichseifert.gral.graphics.TableLayout
import de.erichseifert.gral.plots.colors.QuasiRandomColors

import de.erichseifert.gral.graphics.EdgeLayout

import mybiotools.SummaryStat
import mybiotools.plots._
import mybiotools.gwascommons._
import mybiotools.gwascommons.associationresults._
import mybiotools.plots._
import de.erichseifert.gral.data.statistics.Histogram1D
import de.erichseifert.gral.data.EnumeratedData

import scala.collection.JavaConversions._

import org.saddle._

object GSEAPlot {

  def apply(
    values: Map[String, Double],
    set: Set[String],
    main: String,
    xlab: String,
    p: Double = 0.0,
    xAxisIsRank: Boolean = true
  ): DrawableContainer = {

    val (membership, axisx) = {

      val bars = new DataTable(classOf[java.lang.Double], classOf[java.lang.Double]);

      values.toSeq.sortBy(_._2).zipWithIndex.foreach {
        case ((gene, value), idx) =>
          if (set.contains(gene)) {
            if (xAxisIsRank) {
              bars.add(idx, 0.0)
            } else {
              bars.add(value, 0.0)
            }

          }
      }

      val plot = new XYPlot(bars)

      plot.setSetting(Plot.TITLE, "")

      // plot.setBarWidth(0.075);

      // Customization

      val pointrenderer = plot.getPointRenderer(bars)

      pointrenderer.setSetting(PointRenderer.SHAPE, new java.awt.geom.Rectangle2D.Double(0, -500, 1, 500))

      pointrenderer.setSetting(PointRenderer.COLOR, new Color(0, 0, 0))

      val plotarea = plot.getPlotArea

      plotarea.setSetting(PlotArea.BORDER, new BasicStroke(1.0f))
      plotarea.setSetting(XYPlotArea2D.GRID_MAJOR_X, false)
      plotarea.setSetting(XYPlotArea2D.GRID_MAJOR_Y, false)
      plotarea.setSetting(XYPlotArea2D.GRID_MINOR_X, false)
      plotarea.setSetting(XYPlotArea2D.GRID_MINOR_Y, false)

      val axis_X = plot.getAxis(XYPlot.AXIS_X)
      val axis_Y = plot.getAxis(XYPlot.AXIS_Y)

      axis_Y.setMin(0.0)
      axis_Y.setMax(0.05)
      axis_X.setMin(0)
      if (xAxisIsRank) {
        axis_X.setMax(values.size - 1)
      } else {
        axis_X.setMax(values.map(_._2).max)
      }

      val rendererY = plot.getAxisRenderer(XYPlot.AXIS_Y);
      // 
      val rendererX = plot.getAxisRenderer(XYPlot.AXIS_X);

      rendererX.setSetting(AxisRenderer.LABEL, xlab)

      rendererY.setSetting(AxisRenderer.INTERSECTION, -Double.MaxValue);
      rendererX.setSetting(AxisRenderer.INTERSECTION, 0.0);
      rendererY.setSetting(AxisRenderer.TICKS, false)
      rendererY.setSetting(AxisRenderer.TICKS_MINOR, false)
      rendererX.setSetting(AxisRenderer.TICKS, false)
      rendererX.setSetting(AxisRenderer.TICKS_MINOR, false)

      // rendererX.setSetting(AxisRenderer.LABEL, "Ranks")

      plot.setInsets(new Insets2D.Double(00.0, 60.0, 0.0, 0.0));

      val bounds = new java.awt.geom.Rectangle2D.Double(0.0, 0.0, 700.0, 700.0)
      plot.setBounds(bounds)
      plot -> axis_X
    }
    val density = {

      val N = values.size
      val NH = set.size
      val NR = set.toSeq.map(values).map(x => math.pow(math.abs(x), p)).sum

      val sliding: Seq[(Double, Double)] = values.toSeq.sortBy(_._2).zipWithIndex.foldLeft(List[(Double, Double)]()) {
        (list, elem) =>
          val tag = elem._1._1
          val value = elem._1._2
          val idx = elem._2
          val last = list.headOption
          val increment =
            if (set.contains(tag)) math.pow(math.abs(value), p) / NR
            else (-1.0) / (N - NH).toDouble
          val xpos = if (xAxisIsRank) idx.toDouble else value
          val n = if (last.isEmpty) xpos -> increment
          else xpos -> (increment + last.get._2)
          n :: list
      }

      val bars = new DataTable(classOf[java.lang.Double], classOf[java.lang.Double]);

      sliding.foreach {
        case (x, y) =>
          bars.add(x, y)
      }

      val plot = new XYPlot(bars)

      // plot.setSetting(Plot.TITLE, main + " GSEA ")

      // plot.setBarWidth(0.075);

      // Customization

      val pointrenderer = plot.getPointRenderer(bars)

      pointrenderer.setSetting(PointRenderer.SHAPE, new java.awt.geom.Ellipse2D.Double(0, 0, 2, 2))

      pointrenderer.setSetting(PointRenderer.COLOR, new Color(0, 0, 0))

      val lr = new DefaultLineRenderer2D();
      lr.setSetting(LineRenderer.COLOR, new java.awt.Color(58, 95, 205))
      lr.setSetting(LineRenderer.STROKE, new BasicStroke(1.5f))
      plot.setLineRenderer(bars, lr)
      plot.setPointRenderer(bars, null)

      val plotarea = plot.getPlotArea

      plotarea.setSetting(PlotArea.BORDER, new BasicStroke(1.0f))
      plotarea.setSetting(XYPlotArea2D.GRID_MAJOR_X, false)
      plotarea.setSetting(XYPlotArea2D.GRID_MAJOR_Y, false)
      plotarea.setSetting(XYPlotArea2D.GRID_MINOR_X, false)
      plotarea.setSetting(XYPlotArea2D.GRID_MINOR_Y, false)

      // val axis_X = plot.getAxis(XYPlot.AXIS_X)
      val axis_Y = plot.getAxis(XYPlot.AXIS_Y)

      axis_Y.setMin(sliding.map(_._2).min)
      axis_Y.setMax(sliding.map(_._2).max)

      plot.setAxis(XYPlot.AXIS_X, axisx)

      val rendererY = plot.getAxisRenderer(XYPlot.AXIS_Y);
      // 
      val rendererX = plot.getAxisRenderer(XYPlot.AXIS_X);

      rendererX.setSetting(AxisRenderer.LABEL, xlab)

      rendererY.setSetting(AxisRenderer.INTERSECTION, 0.0);
      rendererX.setSetting(AxisRenderer.INTERSECTION, 0.0);
      rendererX.setSetting(AxisRenderer.TICKS, false)
      rendererX.setSetting(AxisRenderer.TICKS_MINOR, false)
      rendererY.setSetting(AxisRenderer.TICKS, true)
      rendererY.setSetting(AxisRenderer.TICKS_MINOR, false)

      // rendererY.setSetting(AxisRenderer.LABEL, "Running sum")

      plot.setInsets(new Insets2D.Double(00.0, 60.0, 0.0, 0.0));

      val bounds = new java.awt.geom.Rectangle2D.Double(0.0, 0.0, 200.0, 700.0)
      plot.setBounds(bounds)
      plot

    }

    val container = new DrawableContainer(new TableLayout(1))

    container.add(density)
    container.add(membership)

    val bounds = new java.awt.geom.Rectangle2D.Double(0.0, 0.0, 800.0, 800.0)
    container.setBounds(bounds)

    container

  }

}