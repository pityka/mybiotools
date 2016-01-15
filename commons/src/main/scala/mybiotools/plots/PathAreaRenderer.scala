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

class PathAreaRenderer(lr: LineRenderer) extends DefaultAreaRenderer2D {
  override def getAreaShape(points: java.util.List[DataPoint]): Shape = {
    val areas = points
      .groupBy(x => scala.util.Try(x.data.row.get(3).asInstanceOf[Double]).toOption.getOrElse(0.0))
      .map(x => new Area(lr.getLineShape(x._2.toList)))
    val empty = new Area
    areas.foreach(x => empty.add(x))
    empty
  }
  // def getLineShape(points: List[DataPoint]): Shape = {
  //   val shape = new Path2D.Double()
  //   shape.moveTo(points.head.position.getPoint2D.getX, points.head.position.getPoint2D.getY)

  //   var p0: Point2D = null
  //   var p1: Point2D = null
  //   var p2: Point2D = null
  //   var p3: Point2D = null
  //   val ctrl1 = new Point2D.Double();
  //   val ctrl2 = new Point2D.Double();
  //   shape.moveTo(points.head.position.getPoint2D.getX, points.head.position.getPoint2D.getY)

  //   points.foreach { point =>

  //     p3 = point.position.getPoint2D();

  //     addCurve(shape, p0, p1, p2, p3, ctrl1, ctrl2, smoothness);

  //     p0 = p1;
  //     p1 = p2;
  //     p2 = p3;

  //   }
  //   addCurve(shape, p0, p1, p2, p3, ctrl1, ctrl2, smoothness);

  //   // shape.closePath

  //   punch(shape, points)

  // }

  // def addCurve(line: Path2D, p0: Point2D, p1: Point2D,
  //   p2: Point2D, p3: Point2D, ctrl1: Point2D, ctrl2: Point2D,
  //   smoothness: Double) {
  //   if (p1 == null) {
  //     ()
  //   } else if (p2 == null) {
  //     ()
  //   } else {
  //     getControlsPoints(p0, p1, p2, p3, ctrl1, ctrl2, smoothness);
  //     line.curveTo(
  //       ctrl1.getX(), ctrl1.getY(),
  //       ctrl2.getX(), ctrl2.getY(),
  //       p2.getX(), p2.getY());
  //   }
  // }
  // def getControlsPoints(pp0: Point2D, p1: Point2D,
  //   p2: Point2D, pp3: Point2D, ctrl1: Point2D, ctrl2: Point2D,
  //   smoothness: Double) {
  //   val p0 = if (pp0 == null) {
  //     p1
  //   } else pp0
  //   val p3 = if (pp3 == null) {
  //     p2
  //   } else pp3

  //   val c1 = new Point2D.Double(
  //     (p0.getX() + p1.getX()) / 2.0,
  //     (p0.getY() + p1.getY()) / 2.0);
  //   val c2 = new Point2D.Double(
  //     (p1.getX() + p2.getX()) / 2.0,
  //     (p1.getY() + p2.getY()) / 2.0);
  //   val c3 = new Point2D.Double(
  //     (p2.getX() + p3.getX()) / 2.0,
  //     (p2.getY() + p3.getY()) / 2.0);

  //   val len1 = p1.distance(p0);
  //   val len2 = p2.distance(p1);
  //   val len3 = p3.distance(p2);

  //   val k1 = len1 / (len1 + len2);
  //   val k2 = len2 / (len2 + len3);

  //   val m1 = new Point2D.Double(
  //     c1.getX() + (c2.getX() - c1.getX()) * k1,
  //     c1.getY() + (c2.getY() - c1.getY()) * k1);
  //   val m2 = new Point2D.Double(
  //     c2.getX() + (c3.getX() - c2.getX()) * k2,
  //     c2.getY() + (c3.getY() - c2.getY()) * k2);

  //   ctrl1.setLocation(
  //     m1.getX() + (c2.getX() - m1.getX()) * smoothness + p1.getX() - m1.getX(),
  //     m1.getY() + (c2.getY() - m1.getY()) * smoothness + p1.getY() - m1.getY()
  //   );
  //   ctrl2.setLocation(
  //     m2.getX() + (c2.getX() - m2.getX()) * smoothness + p2.getX() - m2.getX(),
  //     m2.getY() + (c2.getY() - m2.getY()) * smoothness + p2.getY() - m2.getY()
  //   );
  // }
}