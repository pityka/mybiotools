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
import de.erichseifert.gral.data.Row;
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
import de.erichseifert.gral.graphics.DrawingContext
import de.erichseifert.gral.util.Location
import java.awt._
import scala.runtime.RichInt
import scala.runtime.RichFloat
import de.erichseifert.gral.graphics.DrawableContainer
import de.erichseifert.gral.graphics.TableLayout
import de.erichseifert.gral.plots.colors.QuasiRandomColors
import de.erichseifert.gral.plots.BarPlot
import de.erichseifert.gral.util.DataUtils;
import de.erichseifert.gral.util.PointND;
import de.erichseifert.gral.plots.legends.AbstractLegend
import de.erichseifert.gral.plots.legends.AbstractLegend.AbstractSymbol

import de.erichseifert.gral.graphics.EdgeLayout

import mybiotools.SummaryStat
import mybiotools.plots._
import mybiotools.gwascommons._
import mybiotools.gwascommons.associationresults._
import mybiotools.plots._
import de.erichseifert.gral.data.statistics.Histogram1D
import de.erichseifert.gral.data.EnumeratedData
import de.erichseifert.gral.plots.colors.ColorMapper

import scala.collection.JavaConversions._

class HistogramLegend(plot: XYPlot) extends XYPlot.XYLegend(plot) {
  override def getSymbol(row: Row): AbstractSymbol = {
    new AbstractSymbol(this) {

      def draw(context: DrawingContext) {
        val data = row.getSource();

        val bounds = getBounds();

        val shape = new java.awt.geom.Rectangle2D.Double(bounds.getCenterX(), bounds.getCenterY(), 10.0, 10.0)
        val color = plot.getPointRenderer(data).getSetting[ColorMapper](PointRenderer.COLOR).get(row.getIndex)

        GraphicsUtils.fillPaintedShape(
          context.getGraphics(), shape, color, null
        )

      }

    }
  }

}

class HistogramRenderer(plot: Plot) extends de.erichseifert.gral.plots.BarPlot.BarRenderer(plot) {
  override def getPointShape(data: PointData): Shape = {
    val colX = 0;
    val colY = 1;
    val colOrigin = 2;
    val colX1 = 3;

    val axisX = data.axes.get(0);
    val axisY = data.axes.get(1);
    val axisXRenderer = data.axisRenderers.get(0);
    val axisYRenderer = data.axisRenderers.get(1);
    val row = data.row;

    if (!row.isColumnNumeric(colX) || !row.isColumnNumeric(colY)) {
      null
    } else {

      val valueX = (row.get(colX).asInstanceOf[Number]).doubleValue();
      val valueX1 = (row.get(colX1).asInstanceOf[Number]).doubleValue();
      val barHeightInWorld = (row.get(colY).asInstanceOf[Number]).doubleValue();
      val axisYOrigin = (row.get(colOrigin).asInstanceOf[Number]).doubleValue();
      var barWidthInWorld = math.abs(valueX1 - valueX)

      val barXMin = axisXRenderer.worldToView(axisX, valueX, true)
      val barXMax = axisXRenderer.worldToView(axisX, valueX + barWidthInWorld, true)

      val barYOrigin = axisYRenderer.worldToView(axisY, axisYOrigin, true)
      val barYMax = axisYRenderer.worldToView(axisY, axisYOrigin + barHeightInWorld, true)
      val valueYInView = axisYRenderer.worldToView(axisY, barHeightInWorld, true)

      val barWidth = Math.abs(barXMax - barXMin);
      val barHeight = Math.abs(barYMax - barYOrigin);

      // position of the bar's left edge in screen coordinates
      val barX = axisXRenderer.getPosition(
        axisX, valueX, true, false
      ).get(PointND.X);
      // position of the bar's upper edge in screen coordinates
      // (the origin of the screen y axis is at the top)

      val shape = new java.awt.geom.Rectangle2D.Double(barXMin - barX, 0 - barYOrigin, barWidth + 0.1, barHeight + 0.1)

      shape
    }
  }
}