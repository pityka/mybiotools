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

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.NoSuchElementException;
import de.erichseifert.gral.data._
import scala.collection.JavaConversions._
import de.erichseifert.gral.data.statistics.Statistics;

import de.erichseifert.gral.plots._

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Dimension2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

import de.erichseifert.gral.data.DataSource;
import de.erichseifert.gral.data.DataTable;
import de.erichseifert.gral.data.Row;
import de.erichseifert.gral.data.statistics.Statistics;
import de.erichseifert.gral.graphics.AbstractDrawable;
import de.erichseifert.gral.graphics.Drawable;
import de.erichseifert.gral.graphics.DrawingContext;
import de.erichseifert.gral.plots.axes.Axis;
import de.erichseifert.gral.plots.axes.AxisRenderer;
import de.erichseifert.gral.plots.colors.ColorMapper;
import de.erichseifert.gral.plots.colors.ContinuousColorMapper;
import de.erichseifert.gral.plots.colors.Grayscale;
import de.erichseifert.gral.plots.points.AbstractPointRenderer;
import de.erichseifert.gral.plots.points.PointData;
import de.erichseifert.gral.plots.settings.Key;
import de.erichseifert.gral.plots.settings.SettingsStorage;
import de.erichseifert.gral.util.GraphicsUtils;
import de.erichseifert.gral.util.PointND;

class ArrayDataSource(rowMajorData: Array[Double], val numRows: Int, val numCols: Int) extends DataSource with Serializable {

  @transient
  private var dataListeners = Set[DataListener]()

  @transient
  private var statistics: Option[Statistics] = None;

  private var name = ""

  def getColumn(col: Int) = new Column(this, col)

  def getRow(col: Int) = new Row(this, col)

  def getColumnTypes = 1 to numCols map (i => classOf[java.lang.Double]) toArray

  def get(row: Int, col: Int) = rowMajorData((row * numCols) + col)

  def getStatistics = {
    if (statistics == None) {
      statistics = Some(new Statistics(this))
    }
    statistics.get
  }

  def getRowCount = numRows

  def getColumnCount = numCols

  def isColumnNumeric(t: Int) = true

  def addDataListener(dataListener: DataListener) {
    dataListeners = dataListeners + (dataListener)
  }

  def removeDataListener(dataListener: DataListener) {
    dataListeners -= (dataListener)
  }

  def getName = name

  def setName(s: String) {
    name = s
  }

  def iterator = (0 until rowMajorData.size).iterator.map(i => new java.lang.Double(rowMajorData(i)))

}

object ArrayRasterPlot {
  /**
   * Key for specifying a {@link java.awt.geom.Point2D} instance which defines
   * the horizontal and vertical offset of the raster from the origin.
   */
  val OFFSET =
    new Key("arrayrasterplot.offset"); //$NON-NLS-1$

  /**
   * Key for specifying an instance of
   * {@link de.erichseifert.gral.plots.colors.ColorMapper} used for mapping the
   * pixel values to colors.
   */
  val COLORS =
    new Key("arrayrasterplot.color"); //$NON-NLS-1$
}

class ArrayRasterPlot(data: ArrayDataSource) extends XYPlot {

  private final val serialVersionUID = 5844862286358250831L;

  setPlotArea(new ArrayRasterPlotArea(this));

  setSettingDefault(ArrayRasterPlot.OFFSET, new Point2D.Double());
  setSettingDefault(ArrayRasterPlot.COLORS, new Grayscale());

  // getPlotArea().setSettingDefault(XYPlotArea2D.GRID_MAJOR_X, false);
  // getPlotArea().setSettingDefault(XYPlotArea2D.GRID_MAJOR_Y, false);
  //getAxisRenderer(AXIS_X).setSetting(AxisRenderer.TICKS, false);
  //getAxisRenderer(AXIS_Y).setSetting(AxisRenderer.TICKS, false);
  getAxisRenderer(XYPlot.AXIS_X).setSetting(
    AxisRenderer.INTERSECTION,
    -Double.MaxValue
  );
  getAxisRenderer(XYPlot.AXIS_Y).setSetting(
    AxisRenderer.INTERSECTION,
    -Double.MaxValue
  );

  // Store data
  add(data);

  // Adjust axes to the data series
  autoscaleAxes();

  def add(source: ArrayDataSource) {
    add(source, true);
  }

  override def add(source: DataSource) {
    if (source.isInstanceOf[ArrayDataSource]) {
      add(source, true);
    }
  }

  // override def autoscaleAxis(axisName: String) {
  //   if (AXIS_X.equals(axisName) || AXIS_Y.equals(axisName)) {
  //     Dimension2D dist = getSetting(DISTANCE);
  //     // In case we get called before settings defaults have been set,
  //     // just set distance to a sane default
  //     if (dist == null) {
  //       dist = new de.erichseifert.gral.util.Dimension2D.Double(1.0, 1.0);
  //     }

  //     val axis = getAxis(axisName);
  //     if (axis == null || !axis.isAutoscaled()) {
  //       return ;
  //     }

  //     double min = getAxisMin(axisName);
  //     double max = getAxisMax(axisName);
  //     if (AXIS_X.equals(axisName)) {
  //       axis.setRange(min, max + dist.getWidth());
  //     } else if (AXIS_Y.equals(axisName)) {
  //       axis.setRange(min - dist.getHeight(), max);
  //     }
  //   } else {
  //     super.autoscaleAxis(axisName);
  //   }
  // }

}

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.image.BufferedImage;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.Dimension2D;
import java.awt.geom.Line2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import de.erichseifert.gral.data.DataSource;
import de.erichseifert.gral.data.DummyData;
import de.erichseifert.gral.data.Row;
import de.erichseifert.gral.graphics.Drawable;
import de.erichseifert.gral.graphics.DrawingContext;
import de.erichseifert.gral.navigation.Navigable;
import de.erichseifert.gral.navigation.NavigationDirection;
import de.erichseifert.gral.navigation.Navigator;
import de.erichseifert.gral.plots.areas.AreaRenderer;
import de.erichseifert.gral.plots.axes.Axis;
import de.erichseifert.gral.plots.axes.AxisListener;
import de.erichseifert.gral.plots.axes.AxisRenderer;
import de.erichseifert.gral.plots.axes.LinearRenderer2D;
import de.erichseifert.gral.plots.axes.Tick;
import de.erichseifert.gral.plots.axes.Tick.TickType;
import de.erichseifert.gral.plots.legends.SeriesLegend;
import de.erichseifert.gral.plots.lines.LineRenderer;
import de.erichseifert.gral.plots.points.DefaultPointRenderer2D;
import de.erichseifert.gral.plots.points.PointData;
import de.erichseifert.gral.plots.points.PointRenderer;
import de.erichseifert.gral.plots.settings.Key;
import de.erichseifert.gral.plots.XYPlot.XYPlotArea2D;
import de.erichseifert.gral.util.GraphicsUtils;
import de.erichseifert.gral.util.Insets2D;
import de.erichseifert.gral.util.Orientation;
import de.erichseifert.gral.util.PointND;

/**
 * Class that represents the drawing area of an {@code XYPlot}.
 */
class ArrayRasterPlotArea(plot: ArrayRasterPlot) extends XYPlotArea2D(plot) {
  /** Version id for serialization. */
  private final val serialVersionUID = -3673157774425536428L;

  setSetting(XYPlotArea2D.GRID_MAJOR_X, false)
  setSetting(XYPlotArea2D.GRID_MAJOR_Y, false)
  setSetting(XYPlotArea2D.GRID_MINOR_X, false)
  setSetting(XYPlotArea2D.GRID_MINOR_Y, false)

  override def drawPlot(context: DrawingContext) {

    val graphics: java.awt.Graphics2D = context.getGraphics();

    val clipBoundsOld = graphics.getClip();
    val clipOffset = getSetting(PlotArea.CLIPPING).asInstanceOf[Insets2D];
    if (clipOffset != null) {
      // TODO Use real font size instead of fixed value
      val fontSize = 10.0;

      // Perform clipping
      var clipBounds: Shape = new Rectangle2D.Double(
        getX() + clipOffset.getLeft() * fontSize,
        getY() + clipOffset.getTop() * fontSize,
        getWidth() - clipOffset.getHorizontal() * fontSize,
        getHeight() - clipOffset.getVertical() * fontSize
      );
      // Take care of old clipping region. This is used when getting
      // scrolled in a JScrollPane for example.
      if (clipBoundsOld != null) {
        val clipBoundsNew = new Area(clipBoundsOld);
        clipBoundsNew.intersect(new Area(clipBounds));
        clipBounds = clipBoundsNew;
      }
      graphics.setClip(clipBounds);
    }

    val txOrig = graphics.getTransform();
    graphics.translate(getX(), getY());
    val txOffset = graphics.getTransform();

    val colormapper: ColorMapper = plot.getSetting(ArrayRasterPlot.COLORS);

    // Paint points and lines
    plot.getVisibleData.foreach { s1: DataSource =>
      if (s1.isInstanceOf[ArrayDataSource]) {
        val s = s1.asInstanceOf[ArrayDataSource];
        val image = new BufferedImage(s.numCols, s.numRows, BufferedImage.TYPE_INT_ARGB);
        var i = 0;
        var j = 0;
        while (i < s.numCols) {
          while (j < s.numRows) {
            image.setRGB(i, s.numRows - j - 1, colormapper.get(s.get(j, i)).asInstanceOf[Color].getRGB);
            j += 1;
          }
          j = 0;
          i += 1;
        }

        val tx = new AffineTransform();

        val maxX = plot.getAxisRenderer(XYPlot.AXIS_X).getPosition(plot.getAxis(XYPlot.AXIS_X), image.getWidth, true, false).get(PointND.X)
        val minX = plot.getAxisRenderer(XYPlot.AXIS_X).getPosition(plot.getAxis(XYPlot.AXIS_X), 0, true, false).get(PointND.X)

        val minY = plot.getAxisRenderer(XYPlot.AXIS_Y).getPosition(plot.getAxis(XYPlot.AXIS_Y), image.getHeight, true, false).get(PointND.Y)
        val maxY = plot.getAxisRenderer(XYPlot.AXIS_Y).getPosition(plot.getAxis(XYPlot.AXIS_Y), 0, true, false).get(PointND.Y)

        val width = math.abs(maxX - minX)
        val height = math.abs(maxY - minY)

        tx.translate(minX, minY)
        tx.scale(width / image.getWidth(), height / image.getHeight());

        val oldhints = graphics.getRenderingHints

        graphics.setRenderingHint(java.awt.RenderingHints.KEY_INTERPOLATION, java.awt.RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
        graphics.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_OFF)

        graphics.drawRenderedImage(image, tx);

        graphics.setRenderingHints(oldhints)
      }
    }

    // Reset transformation (offset)
    graphics.setTransform(txOrig);

    if (clipOffset != null) {
      // Reset clipping
      graphics.setClip(clipBoundsOld);
    }
  }
}
