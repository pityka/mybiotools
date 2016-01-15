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
import de.erichseifert.gral.plots.legends.Legend;
import de.erichseifert.gral.plots.lines.{ DefaultLineRenderer2D, SmoothLineRenderer2D };
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
import de.erichseifert.gral.plots.colors.ColorMapper;
import java.awt.{ Stroke, BasicStroke }
import de.erichseifert.gral.plots.areas.{ DefaultAreaRenderer2D, LineAreaRenderer2D }
import de.erichseifert.gral.plots.areas.AreaRenderer

import de.erichseifert.gral.graphics.EdgeLayout

import mybiotools.SummaryStat
import mybiotools.plots._
import mybiotools.gwascommons._
import mybiotools.gwascommons.associationresults._
import mybiotools.plots._
import mybiotools.stat._
import mybiotools._
import scala.language.higherKinds

import scala.collection.JavaConversions._
import java.awt.Shape
import java.awt.Paint
import java.awt.geom.AffineTransform
import de.erichseifert.gral.plots.DataPoint
import de.erichseifert.gral.plots.lines.LineRenderer
import java.awt.geom.Path2D
import java.awt.geom.{ Point2D, Area }

class SimpleLineRenderer extends DefaultLineRenderer2D {
  override def getLineShape(points: java.util.List[DataPoint]): Shape = {
    // Construct shape
    val shape = new Path2D.Double(
      Path2D.WIND_NON_ZERO
    );
    points foreach { point =>
      val pos = point.position.getPoint2D();
      if (shape.getCurrentPoint() == null) {
        shape.moveTo(pos.getX(), pos.getY());
      } else {
        shape.lineTo(pos.getX(), pos.getY());
      }
    }
    val stroke: Stroke = getSetting(LineRenderer.STROKE).asInstanceOf[Stroke]
    stroke.createStrokedShape(shape)

  }
}