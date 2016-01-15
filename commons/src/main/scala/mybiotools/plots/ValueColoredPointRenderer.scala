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

import java.awt.BasicStroke;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.text.Format;
import java.text.NumberFormat;

import de.erichseifert.gral.data.Row;
import de.erichseifert.gral.graphics.AbstractDrawable;
import de.erichseifert.gral.graphics.Drawable;
import de.erichseifert.gral.graphics.DrawableContainer;
import de.erichseifert.gral.graphics.DrawingContext;
import de.erichseifert.gral.graphics.OuterEdgeLayout;
import de.erichseifert.gral.plots.Label;
import de.erichseifert.gral.plots.axes.Axis;
import de.erichseifert.gral.plots.axes.AxisRenderer;
import de.erichseifert.gral.plots.colors.ColorMapper;
import de.erichseifert.gral.plots.colors._;
import de.erichseifert.gral.util.DataUtils;
import de.erichseifert.gral.util.GraphicsUtils;
import de.erichseifert.gral.util.Location;
import de.erichseifert.gral.util.MathUtils;
import de.erichseifert.gral.util.PointND;
import de.erichseifert.gral.plots.points.DefaultPointRenderer2D
import de.erichseifert.gral.plots.points.DefaultPointRenderer2D._
import de.erichseifert.gral.plots.points.AbstractPointRenderer._
import de.erichseifert.gral.plots.points.PointRenderer._
import de.erichseifert.gral.plots.points._

class ValueColoredPointRenderer(colorColumn: Int) extends DefaultPointRenderer2D {

  override def getPoint(data: PointData, shape: Shape): Drawable = {
    new AbstractDrawable() {
      /** Version id for serialization. */
      private val serialVersionUID = 1915778739867091906L;

      def draw(context: DrawingContext): Unit = {

        val axisY = data.axes.get(1);
        val axisRendererY = data.axisRenderers.get(1);
        val row = data.row;
        val col = data.col;
        val value = row.get(colorColumn)

        val renderer = ValueColoredPointRenderer.this

        val colors: AnyRef = renderer.getSetting(COLOR);
        val paint: Paint = if (!value.isNaN) colors match {
          case cm: ContinuousColorMapper => cm.get(value)
          case cm: ColorMapper => cm.get(value.toInt)
          case p: Paint => p
        }
        else colors match {
          case p: Paint => p
          case cm: ColorMapper => cm.get(0)
        }

        GraphicsUtils.fillPaintedShape(
          context.getGraphics(), shape, paint, null
        );

        if (renderer.getSetting(VALUE_DISPLAYED).asInstanceOf[Boolean]) {
          val colValue = renderer.getSetting(VALUE_COLUMN).asInstanceOf[java.lang.Integer]
          drawValueLabel(context, shape, row, colValue);
        }

        if (renderer.getSetting(ERROR_DISPLAYED).asInstanceOf[Boolean]) {
          val colErrorTop =
            renderer.getSetting(ERROR_COLUMN_TOP).asInstanceOf[java.lang.Integer];
          val colErrorBottom =
            renderer.getSetting(ERROR_COLUMN_BOTTOM).asInstanceOf[java.lang.Integer];
          drawErrorBars(context, shape,
            row, col, colErrorTop, colErrorBottom,
            axisY, axisRendererY);
        }
      }
    }

  }
}