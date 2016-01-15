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
import de.erichseifert.gral.plots.colors._

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

object Raster {

  def createArrayBackedRasterPlot[RX, CX](
    data: Frame[RX, CX, Double],
    main: String = "",
    fontSize: Int = 5,
    colormap: ColorMapper = defaultColorMap
  ) = {
    val max = data.toMat.toVec.max.get
    val min = data.toMat.toVec.min.get
    val range = max - min

    val dt = new ArrayDataSource(data.toMat.contents, data.numRows, data.numCols)

    val font = new java.awt.Font("Monospaced", java.awt.Font.PLAIN, math.min(math.max(500 / data.numRows, 1), 15))

    val plot = new ArrayRasterPlot(dt) {
      val l = new HeatMapLegend(colormap, 2)
      l.setBounds(new java.awt.geom.Rectangle2D.Double(0.0, 0.0, 120.0, 500.0))
      l.setSetting(Legend.FONT, new java.awt.Font("Monospaced", java.awt.Font.PLAIN, 15))
      setLegend(l)
    }

    plot.setSetting(Plot.TITLE, main)

    plot.setSetting(ArrayRasterPlot.COLORS, colormap)

    val rendererY = plot.getAxisRenderer(XYPlot.AXIS_Y);
    val rendererX = plot.getAxisRenderer(XYPlot.AXIS_X);

    val axis_Y = plot.getAxis(XYPlot.AXIS_Y)
    val axis_X = plot.getAxis(XYPlot.AXIS_X)
    axis_Y.setMin(0.0)
    axis_X.setMin(0.0)
    axis_Y.setMax(data.numRows)
    axis_X.setMax(data.numCols)

    rendererX.setSetting(AxisRenderer.TICKS_CUSTOM, mapAsJavaMap(data.colIx.toSeq.zipWithIndex.map(x => (x._2.toDouble, x._1.toString)).toMap))

    rendererX.setSetting(AxisRenderer.TICK_LABELS_ROTATION, 90)

    rendererY.setSetting(AxisRenderer.TICKS_CUSTOM, mapAsJavaMap(data.rowIx.toSeq.zipWithIndex.map(x => (x._2.toDouble, x._1.toString)).toMap))

    rendererX.setSetting(AxisRenderer.TICKS_FONT, font)
    rendererY.setSetting(AxisRenderer.TICKS_FONT, font)

    rendererX.setSetting(AxisRenderer.TICKS_FONT, font)
    rendererY.setSetting(AxisRenderer.TICKS_FONT, font)

    plot.setInsets(new Insets2D.Double(100.0, 200.0, 200.0, 100.0));
    val bounds = new java.awt.geom.Rectangle2D.Double(0.0, 0.0, 700.0, 700.0)
    plot.setBounds(bounds)
    plot.setSetting(Plot.LEGEND, true)
    plot.setSetting(Plot.LEGEND_LOCATION, Location.EAST)
    plot.getLegend.add(dt)

    plot

  }

  def defaultColorMap = new HeatMapColors
  def defaultColorMap(min: Double, max: Double) = {
    val c = new HeatMapColors
    c.setRange(min, max)
    c
  }

  def createRasterPlot[RX, CX](
    data: Frame[RX, CX, Double],
    main: String = "",
    fontSize: Int = 5,
    colormap: ColorMapper = defaultColorMap
  ) = {
    val max = data.toMat.toVec.max.get
    val min = data.toMat.toVec.min.get
    val range = max - min
    val dt = new DataTable(classOf[java.lang.Double], classOf[java.lang.Double], classOf[java.lang.Double])

    data.toColSeq.zipWithIndex.foreach {
      case ((cx, row), i) =>
        row.toSeq.zipWithIndex.foreach {
          case ((rx, value), j) =>

            dt.add(i - 0.5, j - 0.5, value)
        }
    }
    val font = new java.awt.Font("Monospaced", java.awt.Font.PLAIN, fontSize)

    val plot = new RasterPlot(dt) {
      val l = new HeatMapLegend(colormap, 2)
      l.setBounds(new java.awt.geom.Rectangle2D.Double(0.0, 0.0, 120.0, 500.0))
      l.setSetting(Legend.FONT, font)
      setLegend(l)
    }

    plot.setSetting(Plot.TITLE, main)

    plot.setSetting(RasterPlot.COLORS, colormap)

    val rendererY = plot.getAxisRenderer(XYPlot.AXIS_Y);
    val rendererX = plot.getAxisRenderer(XYPlot.AXIS_X);

    rendererX.setSetting(AxisRenderer.TICKS_CUSTOM, mapAsJavaMap(data.colIx.toSeq.zipWithIndex.map(x => (x._2.toDouble, x._1.toString)).toMap))

    rendererX.setSetting(AxisRenderer.TICK_LABELS_ROTATION, 90)

    rendererY.setSetting(AxisRenderer.TICKS_CUSTOM, mapAsJavaMap(data.rowIx.toSeq.zipWithIndex.map(x => (x._2.toDouble - 1, x._1.toString)).toMap))

    rendererX.setSetting(AxisRenderer.TICKS_FONT, font)
    rendererY.setSetting(AxisRenderer.TICKS_FONT, font)
    rendererX.setSetting(AxisRenderer.TICKS_MINOR, false)
    rendererX.setSetting(AxisRenderer.TICKS_SPACING, 1000)
    rendererX.setSetting(AxisRenderer.TICKS_AUTO_SPACING, false)

    plot.setInsets(new Insets2D.Double(100.0, 100.0, 100.0, 200.0));
    val bounds = new java.awt.geom.Rectangle2D.Double(0.0, 0.0, 900.0, 700.0)
    plot.setBounds(bounds)
    plot.setSetting(Plot.LEGEND, true)
    plot.setSetting(Plot.LEGEND_LOCATION, Location.EAST)
    plot.getLegend.add(dt)

    plot

  }
}