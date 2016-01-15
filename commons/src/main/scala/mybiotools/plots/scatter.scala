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

import org.saddle._
import scala.collection.generic._

object ScatterPlot {

  type D2 = (Double, Double)
  type D3 = (Double, Double, Double)
  type D2vD3 = Either[D2, D3]

  def _1(d: D2vD3) = d match {
    case Left(x) => x._1
    case Right(x) => x._1
  }
  def _2(d: D2vD3) = d match {
    case Left(x) => x._2
    case Right(x) => x._2
  }
  def mapY(d: D2vD3)(f: Double => Double): D2vD3 = d match {
    case Left(x) => (x._1, f(x._2))
    case Right(x) => (x._1, f(x._2), x._3)
  }

  case class Label(name: String, color: Either[Paint, ColorMapper], shape: Shape, join: Option[Stroke], lineGroup: Int)
  object Label {
    def apply(name: String, color: Paint, shape: Shape, join: Stroke): Label = Label(name, Left(color), shape, Some(join), -1)
    def apply(name: String, color: Paint): Label = Label(name, Left(color), new java.awt.geom.Ellipse2D.Double(-2, -2, 4.0, 4.0), None, -1)
    def apply(name: String, color: Paint, shape: Shape): Label = Label(name, Left(color), shape, None, -1)
    def apply(name: String, color: Paint, join: Stroke): Label = Label(name, Left(color), new java.awt.geom.Ellipse2D.Double(-2, -2, 4.0, 4.0), Some(join), -1)

    def apply(name: String, color: ColorMapper, shape: Shape, join: Stroke): Label = Label(name, Right(color), shape, Some(join), -1)
    def apply(name: String, color: ColorMapper): Label = Label(name, Right(color), new java.awt.geom.Ellipse2D.Double(-2, -2, 4.0, 4.0), None, -1)
    def apply(name: String, color: ColorMapper, shape: Shape): Label = Label(name, Right(color), shape, None, -1)
    def apply(name: String, color: ColorMapper, join: Stroke): Label = Label(name, Right(color), new java.awt.geom.Ellipse2D.Double(-2, -2, 4.0, 4.0), Some(join), -1)

    def apply(name: String, color: Paint, shape: Shape, join: Stroke, lineGroup: Int): Label = Label(name, Left(color), shape, Some(join), lineGroup)
    def apply(name: String, color: Paint, lineGroup: Int): Label = Label(name, Left(color), new java.awt.geom.Ellipse2D.Double(-2, -2, 4.0, 4.0), None, lineGroup)
    def apply(name: String, color: Paint, shape: Shape, lineGroup: Int): Label = Label(name, Left(color), shape, None, lineGroup)
    def apply(name: String, color: Paint, join: Stroke, lineGroup: Int): Label = Label(name, Left(color), new java.awt.geom.Ellipse2D.Double(-2, -2, 4.0, 4.0), Some(join), lineGroup)

    def apply(name: String, color: ColorMapper, shape: Shape, join: Stroke, lineGroup: Int): Label = Label(name, Right(color), shape, Some(join), lineGroup)
    def apply(name: String, color: ColorMapper, lineGroup: Int): Label = Label(name, Right(color), new java.awt.geom.Ellipse2D.Double(-2, -2, 4.0, 4.0), None, lineGroup)
    def apply(name: String, color: ColorMapper, shape: Shape, lineGroup: Int): Label = Label(name, Right(color), shape, None, lineGroup)
    def apply(name: String, color: ColorMapper, join: Stroke, lineGroup: Int): Label = Label(name, Right(color), new java.awt.geom.Ellipse2D.Double(-2, -2, 4.0, 4.0), Some(join), lineGroup)
  }

  // def reservoirSampling[T, CC[X] <: Traversable[X]](xs: CC[T], size: Int, rng: RandomGenerator)(implicit bf: CanBuildFrom[CC[T], T, CC[T]]): CC[T] = {

  implicit def tuple2ToEither(t: (Double, Double)): D2vD3 = Left(t)
  implicit def tuple3ToEither(t: (Double, Double, Double)): D2vD3 = Right(t)

  implicit def seq2[CC[X] <: IndexedSeq[X]](t: CC[D2]): IndexedSeq[D2vD3] = t.map(tuple2ToEither)
  implicit def seq3[CC[X] <: IndexedSeq[X]](t: CC[D3]): IndexedSeq[D2vD3] = t.map(tuple3ToEither)

  def createScatterPlotsFromFrame[RX, CX](
    frame: Frame[RX, CX, Double],
    xlog: Boolean = false,
    ylog: Boolean = false,
    fit: Boolean = false,
    rsquared: Boolean = false,
    combined: Boolean = true
  ): Seq[AbstractDrawable] = {
    val columns = frame.toColSeq
    val combinations = columns.combinations(2).toList
    val plots = combinations.map {
      case list =>
        val (cx1, s1) = list.head
        val (cx2, s2) = list(1)
        val joined = s1.joinMap(s2, how = index.InnerJoin)((x, y) => (x, y)).toVec.toSeq.filter(_ != null)
        createScatterPlot(joined.toIndexedSeq, xlog, ylog, fit, rsquared, main = cx1.toString + " vs \n" + cx2.toString, xlab = cx1.toString, ylab = cx2.toString)
    }
    if (combined) {
      val container = new DrawableContainer(new TableLayout(columns.size))
      plots.zipWithIndex.foreach {
        case (plot, idx) =>

          container.add(plot)

      }
      val bounds = new java.awt.geom.Rectangle2D.Double(0.0, 0.0, columns.size * 800.0, (combinations.size / columns.size) * 800.0)
      container.setBounds(bounds)
      container :: Nil
    } else plots.toList

  }
  def createScatterPlot(
    data: IndexedSeq[D2vD3],
    xlog: Boolean = false,
    ylog: Boolean = false,
    fit: Boolean = false,
    rsquared: Boolean = false,
    main: String = "",
    xlab: String = "",
    ylab: String = "",
    join: Boolean = false,
    legend: Boolean = true,

    xnames: Map[Double, String] = Map(),
    xlim: Option[(Double, Double)] = None,
    ylim: Option[(Double, Double)] = None,
    draw1Line: Boolean = false,
    lines: Seq[(Label, Double, Double, Double, Double)] = Nil,
    compressPoints: Boolean = false
  ): XYPlot = {

    createScatterPlotFromMultiple(
      List(Label("", Color.black) -> data),
      xlog,
      ylog,
      fit,
      rsquared,
      main,
      xlab,
      ylab,
      join,
      legend,
      xnames,
      xlim,
      ylim,
      draw1Line,
      lines,
      compressPoints
    )
  }

  def createScatterPlotFromMultiple(
    data: Seq[(Label, IndexedSeq[D2vD3])],
    xlog: Boolean = false,
    ylog: Boolean = false,
    fit: Boolean = false,
    rsquared: Boolean = false,
    main: String = "",
    xlab: String = "",
    ylab: String = "",
    join: Boolean = false,
    legend: Boolean = true,
    xnames: Map[Double, String] = Map(),
    xlim: Option[(Double, Double)] = None,
    ylim: Option[(Double, Double)] = None,
    draw1Line: Boolean = false,
    lines: Seq[(Label, Double, Double, Double, Double)] = Nil,
    compressPoints: Boolean = false
  ): XYPlot = {

    if (data.flatMap(_._2).isEmpty) new XYPlot
    else {

      def regression(data: Seq[(Double, Double)]) = {
        import org.saddle._
        import mybiotools.stat.LinearRegression._
        val outcome = Series(Vec(data.map(_._2).toArray))
        val predictor = Series(Vec(data.map(_._1).toArray))
        val r = linearRegression(outcome, predictor)
        r match {
          case r: RegressionResult => {
            val b = r.covariates("0")._1.slope
            val a = r.intercept._1.slope
            val c = r.covariates("0")._1.sd
            val d = r.covariates("0")._2.pValue
            (a, b, c, d)
          }
          case _: FailedRegression => (Double.NaN, Double.NaN, Double.NaN, Double.NaN)
        }

      }

      val datatransformed = data //data.map(x => ((if (xlog) math.log10(x._1) else x._1), (if (ylog) math.log10(x._2) else x._2)))

      def merge(in: Seq[(Label, IndexedSeq[D2vD3])]): (Label, IndexedSeq[(D2vD3, Double)]) =
        if (in.size == 1) (in.head._1, in.head._2.sortBy(_1).distinct.map(x => (x, 0.0)))
        else {
          val label = in.head._1
          label -> in.zipWithIndex.flatMap { case ((_, seq), idx) => seq.sortBy(_1).distinct.map(x => (x, idx.toDouble)) }.toIndexedSeq
        }

      val datatables = {
        val (nogroup, togroup) = datatransformed.zipWithIndex.partition(_._1._1.lineGroup < 0)

        val nogroup2 = nogroup.map(x => (x._1._1 -> x._1._2.sortBy(_1).distinct.map(x => (x, -1.0)), x._2))
        val grouped = togroup.groupBy(_._1._1.lineGroup).map(x => merge(x._2.map(_._1)) -> x._2.map(_._2).min)

        (nogroup2 ++ grouped).sortBy(_._2).map(_._1).map {
          case (label, data) =>
            val dt = new DataTable(classOf[java.lang.Double], classOf[java.lang.Double], classOf[java.lang.Double], classOf[java.lang.Double])

            data.foreach {
              case (Left((x, y)), group) =>
                dt.add(x, y, Double.NaN, group)
              case (Right((x, y, z)), group) =>
                dt.add(x, y, z, group)
            }

            label -> dt
        }.toSeq
      }

      val xmin = {
        val data = datatransformed.flatMap(_._2.map(_1)).min
        xlim.map(x => math.min(data, x._1)).getOrElse(data)
      }
      val xmax = {
        val d = datatransformed.flatMap(_._2.map(_1)).max
        xlim.map(x => math.max(x._2, d)).getOrElse(d)
      }
      val ymin = {
        val d = datatransformed.flatMap(_._2.map(_2)).min
        ylim.map(x => math.min(d, x._1)).getOrElse(d)
      }
      val ymax = {
        val d = datatransformed.flatMap(_._2.map(_2)).max
        ylim.map(x => math.max(x._2, d)).getOrElse(d)
      }

      val slopetable = if (fit) {

        val (intercept, slope, sd, p) = regression(data.head._2.map(x => _1(x) -> _2(x)))
        val slopetable = new DataTable(classOf[java.lang.Double], classOf[java.lang.Double])
        slopetable.add(xmin, xmin * slope + intercept)
        slopetable.add(xmax, (xmax) * slope + intercept)
        Some(slopetable, slope, intercept, sd, p)
      } else None

      val unitlineTable = if (draw1Line) {

        val slopetable = new DataTable(classOf[java.lang.Double], classOf[java.lang.Double])
        slopetable.add(xmin, xmin)
        slopetable.add(xmax, xmax)
        Some(slopetable)

      } else None

      val linesTable = lines.map {
        case (label, x1, y1, x2, y2) =>

          val slopetable = new DataTable(classOf[java.lang.Double], classOf[java.lang.Double])
          slopetable.add(x1, y1)
          slopetable.add(x2, y2)

          label -> slopetable

      }

      val rsq = if (rsquared) Some(math.pow((new org.apache.commons.math3.stat.correlation.PearsonsCorrelation).correlation(datatransformed.head._2.map(_1).toArray, datatransformed.head._2.map(_2).toArray), 2.0)) else None

      val plot = new XYPlot(datatables.map(_._2).toSeq ++ slopetable.map(_._1).toSeq ++ unitlineTable.toSeq ++ linesTable.map(_._2).toSeq: _*)

      plot.setSetting(Plot.TITLE, main + (if (rsquared) "\n rsq: " + mybiotools.formatDouble(rsq.get.toDouble) else "") + (if (fit) s"\n b: ${slopetable.get._2.format(3)}, sd:${slopetable.get._4.format(3)}, a:${slopetable.get._3.format(3)}, p:${slopetable.get._5.format(5)}" else ""))

      // Customization

      datatables.foreach {
        case (Label(name, color, shape, join1, linegroup), datatable) =>
          val pointrenderer = new ValueColoredPointRenderer(2)
          plot.setPointRenderer(datatable, pointrenderer)

          pointrenderer.setSetting(PointRenderer.SHAPE, shape)

          pointrenderer.setSetting(PointRenderer.COLOR, color.merge)

          val linecolor = color.merge match {
            case x: Color => x
            case x: ColorMapper => x.get(datatable.get(2, 0))
          }

          if ((datatable.getColumn(2).toList.filter(x => !x.isNaN).distinct.size <= 1 || color.merge.isInstanceOf[Color]) && compressPoints) {
            val ar = new PointAreaRenderer(pointrenderer)
            plot.setPointRenderer(datatable, null)
            ar.setSetting(AreaRenderer.COLOR, linecolor)
            plot.setAreaRenderer(datatable, ar)
          }

          if (join1.isDefined && linegroup < 0) {
            val lr = new SmoothLineRenderer2D();
            lr.setSetting(LineRenderer.COLOR, linecolor)
            lr.setSetting(LineRenderer.STROKE, join1.get)
            lr.setSetting(SmoothLineRenderer2D.SMOOTHNESS, 0.6)
            plot.setLineRenderer(datatable, lr)
            if (join1.get.isInstanceOf[BasicStroke] && join1.get.asInstanceOf[BasicStroke].getLineWidth >= shape.getBounds.getWidth) {
              plot.setPointRenderer(datatable, null)
              plot.setAreaRenderer(datatable, null)
            }

          }
          if (linegroup >= 0 && join1.isDefined) {
            val lr = new SimpleLineRenderer();
            lr.setSetting(LineRenderer.COLOR, linecolor)
            lr.setSetting(LineRenderer.STROKE, join1.get)
            // lr.setSetting(SmoothLineRenderer2D.SMOOTHNESS, 0.6)

            plot.setPointRenderer(datatable, null)

            val ar = new PathAreaRenderer(lr)
            ar.setSetting(AreaRenderer.COLOR, linecolor)
            plot.setAreaRenderer(datatable, ar)
          }

      }

      val plotarea = plot.getPlotArea

      plotarea.setSetting(PlotArea.BORDER, new BasicStroke(1.0f))

      val axis_X = plot.getAxis(XYPlot.AXIS_X)
      val axis_Y = plot.getAxis(XYPlot.AXIS_Y)

      if (xlog) {
        // axis_X.setMin(xmin / (math.pow((xmax / xmin), 0.01)))
        // axis_X.setMax(xmax * (math.pow((xmax / xmin), 0.01)))
      } else {
        axis_X.setMin(xmin - 0.1 * (xmax - xmin))
        axis_X.setMax(xmax + 0.1 * (xmax - xmin))
      }

      if (ylog) {
        // axis_Y.setMin(xmin / (math.pow((xmax / xmin), 0.01)))
        // axis_Y.setMax(xmax * (math.pow((xmax / xmin), 0.01)))
      } else {
        axis_Y.setMin(ymin - 0.1 * (ymax - ymin))
        axis_Y.setMax(ymax + 0.1 * (ymax - ymin))

      }

      xlim.foreach {
        case (min, max) =>
          axis_X.setMin(min)
          axis_X.setMax(max)
      }

      ylim.foreach {
        case (min, max) =>
          axis_Y.setMin(min)
          axis_Y.setMax(max)
      }

      slopetable.map(_._1).foreach { slopetable =>
        val lr = new DefaultLineRenderer2D();
        lr.setSetting(LineRenderer.COLOR, new java.awt.Color(58, 95, 205))
        lr.setSetting(LineRenderer.STROKE, new BasicStroke(1.5f))
        plot.setLineRenderer(slopetable, lr)
        plot.setPointRenderer(slopetable, null);
      }

      unitlineTable.foreach { slopetable =>
        val lr = new DefaultLineRenderer2D();
        lr.setSetting(LineRenderer.COLOR, new java.awt.Color(190, 190, 190))
        lr.setSetting(LineRenderer.STROKE, new BasicStroke(1.5f))
        plot.setLineRenderer(slopetable, lr)
        plot.setPointRenderer(slopetable, null);
      }

      linesTable.foreach {
        case (Label(name, color, shape, join, _), slopetable) =>
          val lr = new DefaultLineRenderer2D();
          val linecolor = color.merge match {
            case x: Color => x
            case x: ColorMapper => x.get(0)
          }
          lr.setSetting(LineRenderer.COLOR, linecolor)
          lr.setSetting(LineRenderer.STROKE, join.getOrElse(new BasicStroke(shape.getBounds.getWidth.toFloat)))
          plot.setLineRenderer(slopetable, lr)
          plot.setPointRenderer(slopetable, null);
      }

      val rendererY = if (ylog) {
        val r = new LogarithmicRenderer2D()
        plot.setAxisRenderer(XYPlot.AXIS_Y, r)
        r
      } else plot.getAxisRenderer(XYPlot.AXIS_Y);

      val rendererX = if (xlog) {
        val r = new LogarithmicRenderer2D()
        plot.setAxisRenderer(XYPlot.AXIS_X, r)
        r
      } else plot.getAxisRenderer(XYPlot.AXIS_X);

      rendererX.setSetting(AxisRenderer.LABEL, xlab)

      rendererY.setSetting(AxisRenderer.LABEL, ylab)

      rendererY.setSetting(AxisRenderer.INTERSECTION, -Double.MaxValue);
      rendererX.setSetting(AxisRenderer.INTERSECTION, -Double.MaxValue);

      val fontsmall = new java.awt.Font(Font.SANS_SERIF, Font.PLAIN, 12)

      rendererX.setSetting(AxisRenderer.LABEL_FONT, fontsmall)
      rendererY.setSetting(AxisRenderer.LABEL_FONT, fontsmall)
      rendererX.setSetting(AxisRenderer.TICKS_FONT, fontsmall)
      rendererY.setSetting(AxisRenderer.TICKS_FONT, fontsmall)

      rendererY.setSetting(AxisRenderer.LABEL_DISTANCE, 4)
      // rendererX.setSetting(AxisRenderer.LABEL_DISTANCE, 4)

      if (ylog) {
        rendererY.setSetting(AxisRenderer.TICKS_MINOR, false)
        rendererY.setSetting(AxisRenderer.TICKS_AUTO_SPACING, false)
        rendererY.setSetting(AxisRenderer.LABEL_DISTANCE, 4)
        rendererY.setSetting(AxisRenderer.TICKS_SPACING, 10)
        rendererY.setSetting(AxisRenderer.TICK_LABELS_ROTATION, 45)
      }
      if (xlog) {
        rendererX.setSetting(AxisRenderer.TICKS_MINOR, false)
        rendererX.setSetting(AxisRenderer.TICKS_AUTO_SPACING, false)
        rendererX.setSetting(AxisRenderer.LABEL_DISTANCE, 4)
        rendererX.setSetting(AxisRenderer.TICKS_SPACING, 10)
        rendererX.setSetting(AxisRenderer.TICK_LABELS_ROTATION, 45)
      }

      if (xnames.size > 0) {
        rendererX.setSetting(AxisRenderer.TICKS_CUSTOM, mapAsJavaMap(xnames))
        rendererX.setSetting(AxisRenderer.TICK_LABELS_ROTATION, 90)
        rendererX.setSetting(AxisRenderer.TICKS_MINOR, false)
      }

      plot.setInsets(new Insets2D.Double(100.0, 100.0, 200.0, 100.0));

      val bounds = new java.awt.geom.Rectangle2D.Double(0.0, 0.0, 700.0, 700.0)
      plot.setBounds(bounds)

      if (legend) {
        val legend = plot.getLegend()
        legend.clear()
        datatables.groupBy(_._1.name).values.map(_.head).toSeq.sortBy(_._1.name).foreach {
          case (Label(name, color, shape, join, _), dt) =>
            dt.setName(name)
            if (name != "") {
              legend.add(dt)
            }
        }
        linesTable.groupBy(_._1.name).values.map(_.head).toSeq.sortBy(_._1.name).foreach {
          case (Label(name, color, shape, join, _), dt) =>
            dt.setName(name)
            if (name != "") {
              legend.add(dt)
            }
        }
        plot.setSetting(Plot.LEGEND, true)
        plot.setSetting(Plot.LEGEND_DISTANCE, 1)
        plot.setSetting(Plot.LEGEND_LOCATION, Location.EAST)
        plot.setInsets(new Insets2D.Double(100.0, 100.0, 100.0, 300.0));
        val bounds = new java.awt.geom.Rectangle2D.Double(0.0, 0.0, 950.0, 700.0)
        plot.setBounds(bounds)
      }

      plot
    }
  }
}