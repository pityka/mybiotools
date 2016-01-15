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

import mybiotools.plots._
import de.erichseifert.gral.data.DataSeries;
import de.erichseifert.gral.data.DataTable;
import de.erichseifert.gral.data.filters.Convolution;
import de.erichseifert.gral.data.filters.Filter;
import de.erichseifert.gral.data.filters.Kernel;
import de.erichseifert.gral.data.filters.KernelUtils;
import de.erichseifert.gral.data.filters.Median;
import de.erichseifert.gral.plots.Plot;
import de.erichseifert.gral.plots.BarPlot;
import de.erichseifert.gral.plots.XYPlot;
import de.erichseifert.gral.plots.legends.Legend;
import de.erichseifert.gral.plots.lines.DefaultLineRenderer2D;
import de.erichseifert.gral.ui.InteractivePanel;
import de.erichseifert.gral.util.GraphicsUtils;
import de.erichseifert.gral.util.Insets2D;
import de.erichseifert.gral.util.Orientation;
import de.erichseifert.gral.plots.points._
import de.erichseifert.gral.plots.lines._
import de.erichseifert.gral.plots.axes._
import de.erichseifert.gral.plots.XYPlot.XYPlotArea2D
import de.erichseifert.gral.graphics.AbstractDrawable
import de.erichseifert.gral.plots.Label
import de.erichseifert.gral.data._
import java.awt.geom._

import de.erichseifert.gral.graphics.{ Layout, Container }
import java.awt.{ Shape, Paint, Color, Dimension, BorderLayout, BasicStroke, Font };
import scala.runtime.RichInt
import scala.runtime.RichDouble
import de.erichseifert.gral.util.Location

import de.erichseifert.gral.graphics.DrawableContainer
import de.erichseifert.gral.graphics.Drawable
import de.erichseifert.gral.graphics.StackedLayout
import de.erichseifert.gral.graphics.TableLayout
import de.erichseifert.gral.ui.InteractivePanel;
import de.erichseifert.gral.io.plots.DrawableWriterFactory
import de.erichseifert.gral.plots.colors.QuasiRandomColors
import de.erichseifert.gral.plots.colors.RandomColors
import de.erichseifert.gral.plots.PlotArea
import de.erichseifert.gral.plots.AbstractPlot
import de.erichseifert.gral.graphics._
import de.erichseifert.gral.plots.colors.{ ColorMapper }
import collection.JavaConversions._
import de.erichseifert.gral.util.PointND
import de.erichseifert.gral.plots.settings.Key
import de.erichseifert.gral.util.Location
import de.erichseifert.gral.plots.DataPoint

object TrackRenderer {
  val TrackHeight = new Key("trackHeight")
  val TrackWidthFactor = new Key("TrackWidthFactor")

}

class TrackRenderer(plot: TrackPlot) extends AbstractPointRenderer {

  setSettingDefault(TrackRenderer.TrackHeight, 10.0);
  setSettingDefault(TrackRenderer.TrackWidthFactor, 1.0);
  setSettingDefault(PointRenderer.VALUE_LOCATION, Location.NORTH)

  val plotArea = plot.getPlotArea

  def getPointShape(d: PointData): Shape = {

    val row = d.row

    val axis: Axis = d.axes.head

    val startCoord: Double = row.get(0).asInstanceOf[Double]
    val endCoord: Double = row.get(1).asInstanceOf[Double]
    val width = (endCoord - startCoord) * plotArea.getBounds.getWidth / axis.getRange

    new Rectangle2D.Double(0.0, 0.0, getSetting(TrackRenderer.TrackWidthFactor).asInstanceOf[Double] * width, getSetting(TrackRenderer.TrackHeight).asInstanceOf[Double])
  }

  def getPoint(data: PointData, shape: Shape) = getPoint(new PointData2(data.axes.toList, data.axisRenderers.toList, data.row, data.col, None), shape)

  def getPoint(data: PointData2, shape: Shape) = new AbstractDrawable {
    def draw(context: DrawingContext) {
      val row = data.row

      val colors = getSetting(PointRenderer.COLOR).asInstanceOf[ColorMapper]

      val paint: Paint = colors.get(row.getIndex());

      context.getAnnotationState.setId("plot_" + plot.getName + "-feature_track-datasource_" + row.getSource.getName + "-rowidx_" + row.getIndex)
      GraphicsUtils.fillPaintedShape(
        context.getGraphics(), shape, paint, null
      )
      context.getAnnotationState.setId(null)

      if (getSetting(PointRenderer.VALUE_DISPLAYED).asInstanceOf[Boolean] && data.label.isDefined) {

        drawValueLabel(context, data.label.get, shape.getBounds, row.getIndex)

      }

    }

  }

  def drawValueLabel(context: DrawingContext, text: String, shapeBounds: Rectangle2D, rowIndex: Int) {

    import PointRenderer._
    import de.erichseifert.gral.plots.Label
    import de.erichseifert.gral.util.MathUtils

    // Formatting

    // Visual settings
    val colors = getSetting(VALUE_COLOR).asInstanceOf[ColorMapper];
    val paint = colors.get(rowIndex);
    val font = getSetting(VALUE_FONT).asInstanceOf[Font];
    val fontSize = font.getSize2D();

    // Layout settings

    val location: Location = getSetting(VALUE_LOCATION).asInstanceOf[Location];
    val alignX = getSetting(VALUE_ALIGNMENT_X).asInstanceOf[Double];
    val alignY = getSetting(VALUE_ALIGNMENT_Y).asInstanceOf[Double];
    val rotation = getSetting(VALUE_ROTATION).asInstanceOf[Int];
    var distance = getSetting(VALUE_DISTANCE).asInstanceOf[Double];
    if (MathUtils.isCalculatable(distance)) {
      distance = distance * fontSize;
    } else {
      distance = 10.0;
    }

    // Create a label with the settings
    val label = new Label(text);
    label.setSetting(Label.ALIGNMENT_X, alignX);
    label.setSetting(Label.ALIGNMENT_Y, alignY);
    label.setSetting(Label.ROTATION, rotation);
    label.setSetting(Label.COLOR, paint);
    label.setSetting(Label.FONT, font);

    val boundsPoint = shapeBounds
    val labelContainer =
      new DrawableContainer(new OuterEdgeLayout(distance));
    labelContainer.setBounds(boundsPoint);
    labelContainer.add(label, location);

    labelContainer.draw(context);
  }
}

class PointData2(axes: List[Axis], renderers: List[_ <: AxisRenderer],
  row: Row, col: Int, val label: Option[String]) extends PointData(axes, renderers, row, col)

object TrackPlotArea {
  val TOPOFFSET = new Key("topoffset")
  val BOTTOMOFFSET = new Key("bottomoffset")
}

class TrackPlotArea(plot: TrackPlot) extends PlotArea {

  setSettingDefault(TrackPlotArea.TOPOFFSET, 30.0)
  setSettingDefault(TrackPlotArea.BOTTOMOFFSET, 30.0)

  def draw(context: DrawingContext) {

    drawBackground(context);
    drawBorder(context);
    drawPlot(context);
  }

  def drawPlot(context: DrawingContext) {
    val graphics = context.getGraphics();

    val clipBoundsOld: Shape = graphics.getClip();
    val clipOffset: Insets2D = getSetting(PlotArea.CLIPPING);
    if (clipOffset != null) {
      // TODO Use real font size instead of fixed value
      val fontSize = 1.0;

      // Perform clipping
      var clipBounds = new Rectangle2D.Double(
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
        clipBounds = clipBoundsNew.getBounds2D.asInstanceOf[Rectangle2D.Double];
      }
      graphics.setClip(clipBounds);
    }

    val txOrig: AffineTransform = graphics.getTransform();
    graphics.translate(getX(), getY());
    val txBase = graphics.getTransform

    // Get width and height of the plot area for relative sizes
    val bounds = getBounds();

    val topOffSet = getSetting(TrackPlotArea.TOPOFFSET).asInstanceOf[Double]
    val bottomOffSet = getSetting(TrackPlotArea.BOTTOMOFFSET).asInstanceOf[Double]

    val bandYPositions: Map[Int, Double] = {
      if (plot.maxBands > 1) {
        val division: Double = (bounds.getHeight - topOffSet - bottomOffSet) / (plot.maxBands - 1)
        (0 to (plot.maxBands - 1)).map(i => i -> ((i) * division + topOffSet)).toMap
      } else
        Map(0 -> (topOffSet + (bounds.getHeight - topOffSet - bottomOffSet) / 2))
    }

    bandYPositions.foreach {
      case (band, y) =>
        val stroke = plot.getSetting(TrackPlot.CENTERLINES).asInstanceOf[Map[Int, BasicStroke]].get(band)
        if (stroke.isDefined) {
          val line = new Line2D.Double(0.0, y, bounds.getWidth, y)

          GraphicsUtils.drawPaintedShape(
            graphics, line, Color.black, null, stroke.get
          )
        }

    }

    // Lines
    plot.getVisibleData.filter(_.getColumnCount == 4).foreach { (s: DataSource) =>
      val lineRenderer = plot.getLineRenderer(s).get

      val fromSource = plot.connectionSources(s)._1

      val toSource = plot.connectionSources(s)._2

      val axisFrom: Axis = plot.getAxis(plot.getMapping(s)(0));
      val axisTo: Axis = plot.getAxis(plot.getMapping(s)(1));

      val axisRenderer: AxisRenderer = plot.getAxisRenderer(plot.getMapping(s)(0));

      (0 until s.getRowCount).foreach { rowIndex =>

        val row = s.getRow(rowIndex);
        val startCoord: Double = row.get(0).asInstanceOf[Double]
        val startBand: Int = row.get(1).asInstanceOf[Double].toInt

        val endCoord: Double = row.get(2).asInstanceOf[Double]
        val endBand: Int = row.get(3).asInstanceOf[Double].toInt

        if (startCoord >= axisFrom.getMin.doubleValue &&
          startCoord <= axisFrom.getMax.doubleValue &&
          endCoord >= axisTo.getMin.doubleValue &&
          endCoord <= axisTo.getMax.doubleValue) {

          val drawable = {

            val transformedStartX = (startCoord - axisFrom.getMin.doubleValue) / axisFrom.getRange.doubleValue * bounds.getWidth

            val transformedEndX = (endCoord - axisTo.getMin.doubleValue) / axisTo.getRange.doubleValue * bounds.getWidth

            val trackHeightFrom = plot.getTrackRenderer(fromSource).get.getSetting(TrackRenderer.TrackHeight).asInstanceOf[Double]

            val trackHeightTo = plot.getTrackRenderer(toSource).get.getSetting(TrackRenderer.TrackHeight).asInstanceOf[Double]

            val pos1 = new PointND[java.lang.Double](
              transformedStartX, bandYPositions(startBand) + trackHeightFrom / 2
            );

            val pos2 = new PointND[java.lang.Double](
              transformedEndX, bandYPositions(endBand) - trackHeightTo / 2
            );

            val pointData = new PointData(
              List[Axis](), List[AxisRenderer](), null, 0
            );
            val dataPoint1 = new DataPoint(
              pointData, pos1, null, null
            );
            val dataPoint2 = new DataPoint(
              pointData, pos2, null, null
            );

            val pointList = List(dataPoint1, dataPoint2)
            val shape = lineRenderer.getLineShape(pointList);
            lineRenderer.getLine(pointList, shape)
          }

          context.getAnnotationState.setId("plot_" + plot.getName + "-feature_connection-datasource_" + s.getName + "-rowidx_" + rowIndex)
          drawable.draw(context);
          context.getAnnotationState.setId(null)
          graphics.setTransform(txBase);

        }
      }

    }

    // Tracks
    plot.getVisibleData.filter(_.getColumnCount == 3).foreach { (s: DataSource) =>

      val trackRenderer: TrackRenderer = plot.getTrackRenderer(s).get;

      val labels = plot.trackLabels(s)

      val axisNames: Array[String] = plot.getMapping(s);
      val axis: Axis = plot.getAxis(plot.getMapping(s)(0));

      val axes = collection.immutable.List(axis);
      val axisRenderers = collection.immutable.List[AxisRenderer]();
      (0 until s.getRowCount).foreach { rowIndex =>

        val row = s.getRow(rowIndex);
        val label: Option[String] = if (labels.size < rowIndex) labels(rowIndex) else None
        val startCoord: Double = row.get(0).asInstanceOf[Double]
        val endCoord: Double = row.get(1).asInstanceOf[Double]
        val band: Double = row.get(2).asInstanceOf[Double]

        if (endCoord >= axis.getMin.doubleValue && startCoord <= axis.getMax.doubleValue) {

          val drawable = {
            val pointData = new PointData2(
              axes, axisRenderers, row, 0, label
            );
            val shape = trackRenderer.getPointShape(pointData);
            trackRenderer.getPoint(pointData, shape)
          }

          val cappedStart: Double = if (startCoord < axis.getMin.doubleValue) axis.getMin.doubleValue else startCoord
          val cappedEnd: Double = if (endCoord > axis.getMax.doubleValue) axis.getMax.doubleValue else endCoord

          val transformedStart = (cappedStart - axis.getMin.doubleValue) / axis.getRange.doubleValue * bounds.getWidth

          val trackHeight = trackRenderer.getSetting(TrackRenderer.TrackHeight).asInstanceOf[Double]

          graphics.translate(transformedStart, bandYPositions(band.toInt) - trackHeight / 2);

          drawable.draw(context);
          graphics.setTransform(txBase);

        }
      }
    }

    graphics.setTransform(txOrig);

    if (clipOffset != null) {
      // Reset clipping
      graphics.setClip(clipBoundsOld);
    }

  }

}

object TrackPlot {
  val CENTERLINES = new Key("centerlines")
}

class TrackPlot(
    val maxBands: Int
) extends AbstractPlot {

  def addTrackData(datasource: DataSource, name: String, labels: IndexedSeq[Option[String]]) {
    if (datasource.getColumnCount == 3) {
      add(datasource)
      setMapping(datasource, name)
      datasource.setName(name)

      import de.erichseifert.gral.data.statistics.Statistics
      import de.erichseifert.gral.util.Orientation._

      setAxis(name, new Axis(datasource.getStatistics.get(Statistics.MIN, VERTICAL, 0), datasource.getStatistics.get(Statistics.MAX, VERTICAL, 1)))

      val trackRenderer = new TrackRenderer(this)
      trackRenderer.setSetting(PointRenderer.VALUE_DISPLAYED, true)
      trackRenderer.setSetting(PointRenderer.VALUE_ROTATION, 0)
      trackRenderers.update(datasource, trackRenderer)
      trackLabels.update(datasource, labels)
    } else throw new RuntimeException("Data needs 3 columns (start,end,band).")
  }

  def addConnectionLinesData(datasource: DataSource, name: String) {
    if (datasource.getColumnCount == 4) {
      add(datasource)
      datasource.setName(name)
      setMapping(datasource, name + "from", name + "to")
      setLineRenderer(datasource, new DefaultLineRenderer2D)
      setAxis(name + "from", new Axis)
      setAxis(name + "to", new Axis)

    } else throw new RuntimeException("Data needs 4 columns (fromBand,fromPoint,toBand,toPoint)")
  }

  def setDataSourcesOfConnectionLines(connections: DataSource, from: DataSource, to: DataSource) {
    setAxis(getMapping(connections)(0), getAxis(getMapping(from)(0)))
    setAxis(getMapping(connections)(1), getAxis(getMapping(to)(0)))
    connectionSources.update(connections, (from, to))
  }

  def getAxis(datasource: DataSource): Axis = {
    getAxis(getMapping(datasource)(0))
  }

  setSettingDefault(TrackPlot.CENTERLINES, Map[Int, BasicStroke]());

  setPlotArea(new TrackPlotArea(this));

  val trackRenderers = collection.mutable.Map[DataSource, TrackRenderer]()

  val lineRenderers = collection.mutable.Map[DataSource, DefaultLineRenderer2D]()

  val connectionSources = collection.mutable.Map[DataSource, (DataSource, DataSource)]()

  val trackLabels = collection.mutable.Map[DataSource, IndexedSeq[Option[String]]]()

  def getTrackRenderer(ds: DataSource) = trackRenderers.get(ds)

  def setTrackRenderer(ds: DataSource, tr: TrackRenderer) = trackRenderers.update(ds, tr)

  def getLineRenderer(ds: DataSource) = lineRenderers.get(ds)

  def setLineRenderer(ds: DataSource, lr: DefaultLineRenderer2D) = lineRenderers.update(ds, lr)

}