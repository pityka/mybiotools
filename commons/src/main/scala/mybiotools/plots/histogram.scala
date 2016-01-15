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
import java.awt._
import scala.runtime.RichInt
import scala.runtime.RichFloat
import de.erichseifert.gral.graphics.DrawableContainer
import de.erichseifert.gral.graphics.TableLayout
import de.erichseifert.gral.plots.colors.QuasiRandomColors
import de.erichseifert.gral.plots.BarPlot
import de.erichseifert.gral.util.DataUtils;
import de.erichseifert.gral.util.PointND;
import mybiotools.plots.ScatterPlot._

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

case class HistogramData(bins: Map[(Double, Double, Double), Double], minX: Double, maxX: Double, maxY: Double, n: Int, lastBinUpperBound: Double) {

  def relative =
    HistogramData(
      bins.map(x => (x._1._1, x._1._2, x._1._3 / n) -> x._2 / n),
      minX,
      maxX,
      maxY / n,
      1,
      lastBinUpperBound
    )

  def filterBinsMinValue(m: Double) = this.copy(bins = bins.filter(_._2 >= m))

  def /(that: HistogramData): HistogramData = if ((this.bins.keys.toSet & that.bins.keys.toSet).size == 0) throw new IllegalArgumentException("Should have the same binning. " + this.toString + " / " + that.toString)
  else {
    val newbins = bins.map { bin1 =>
      val thisX = bin1._1._1
      val thisXEnd = bin1._1._2
      val thisY = bin1._1._3
      val thisValue = bin1._2
      val thatValue = that.bins.get(bin1._1)
      thatValue.map { tv =>
        (thisX, thisXEnd, thisY / tv) -> thisValue / tv
      }
    }.filter(_.isDefined).map(_.get).toMap
    HistogramData(
      newbins,
      minX,
      maxX,
      newbins.values.max,
      1,
      lastBinUpperBound
    )
  }

  def toScatterMiddle = seq2(bins.toSeq.map {
    case ((xstart, xend, ystart), height) =>
      ((xend - xstart) / 2.0 + xstart) -> (ystart + height)
  }.toIndexedSeq)

  def toScatter = seq2(bins.toSeq.map {
    case ((xstart, xend, ystart), height) =>
      (xstart) -> (ystart + height)
  }.toIndexedSeq)

}
object HistogramData {

  def makeBoundaries(step: Double, min: Double, max: Double): Seq[(Double, Double)] = {
    assert(max >= min, "max < min")
    assert(step > 0, "step < 0")
    val c = ((max - min) / step).toInt + 1
    0 to c map { i =>
      (min + step * i) -> (min + step * (i + 1))
    } toSeq
  }

  def apply(data: Seq[Double], binBoundaries: Seq[(Double, Double)]): HistogramData = {

    val min = binBoundaries.minBy(_._1)._1
    val max = binBoundaries.maxBy(_._2)._2
    val data2 = data.filter(x => !x.isNaN && !x.isInfinite).filter(z => z <= max && z >= min)

    val displayMax = max // binBoundaries.maxBy(_._2)._2

    val bins: Map[(Double, Double, Double), Double] = binBoundaries.map {
      case (x, y) =>
        (x, y, 0.0) -> (if (y == max) data2.filter(z => z <= y && z >= x).size.toDouble else data2.filter(z => z < y && z >= x).size.toDouble)
    }.toMap

    val datasize = data2.size

    assert(data2.size.toDouble == bins.map(_._2).sum)

    HistogramData(bins, min, displayMax, bins.values.max, datasize, displayMax)

  }

  def apply(data: Seq[Double], step: Double, min: Double, max: Double): HistogramData = {

    HistogramData(data, makeBoundaries(step, min, max))

  }

  def apply(data: Seq[Double], step: Double): HistogramData = {

    val data2 = data.filter(x => !x.isNaN && !x.isInfinite)
    if (data2.isEmpty) {
      HistogramData(bins = Map[(Double, Double, Double), Double](), minX = 1.0, maxX = 1.0, maxY = 0.0, n = 0, lastBinUpperBound = 1.0)
    } else {
      val min = data2.min
      val max = data2.max

      HistogramData(data2, step, min, max)
    }

  }
  def apply(data: Seq[Double], breaks: Int): HistogramData = {

    val step = getStep(data, breaks)

    apply(data, step)

  }

  def fromStratifiedData(
    data: Seq[Seq[Double]],
    step: Double,
    relativePerBin: Boolean
  ): Seq[HistogramData] = {
    val maxX = data.flatten.max
    val minX = data.flatten.min

    val data2 = data.map(_.filter(x => !x.isNaN && !x.isInfinite))

    val min = data2.flatten.min
    val max = data2.flatten.max

    val displayMax = max //(max - min) + step + min

    val baseBins = ((0 to (((max - min) / step).toInt + 1)) map { (idx: Int) =>
      ((min + idx * step), 0.0) -> 0.0
    }).toMap

    val bins: Seq[Map[(Double, Double), Double]] = data2.map { stratum =>
      baseBins ++ stratum.zipWithIndex.map {
        case (value, idx) =>
          val i = ((value - min) / step).toInt
          val bin = i * step + min
          bin -> value
      }.groupBy(_._1).map(x => (x._1, 0.0) -> x._2.map(_._2).size.toDouble).toMap
    }.foldLeft(Seq[Map[(Double, Double), Double]]()) {
      case (acc, binOfStratum) =>
        if (acc.isEmpty) Seq(binOfStratum)
        else {
          val head = acc.head
          val updatedBin = binOfStratum.map {
            case ((x, y), dens) =>
              (x, head.filter(_._1._1 == x).headOption.map(_._2).getOrElse(0.0) + head.filter(_._1._1 == x).headOption.map(_._1._2).getOrElse(0.0)) -> dens
          }.toMap
          updatedBin +: acc
        }
    }

    if (relativePerBin) {
      val perBinMaxY: Map[Double, Double] = bins.map(_.groupBy(_._1._1).map(x => x._1 -> x._2.map(_._2).sum)).reduce((x, y) => mybiotools.addMaps(x, y)(_ + _))

      val newbins = bins.map(_.map(x => (x._1._1, x._1._2 / perBinMaxY(x._1._1)) -> x._2 / perBinMaxY(x._1._1)))
      newbins.map { bin =>
        HistogramData(
          bin.map { case (x, v) => (x._1, x._1 + step, x._2) -> v },
          minX,
          maxX,
          1,
          1,
          displayMax
        )
      }
    } else {

      val maxY = if (bins.isEmpty) 0 else bins.map(_.map(x => x._1._1 -> x._2)).reduce((x, y) => mybiotools.addMaps(x, y)(_ + _)).values.max

      bins.map { bin =>
        HistogramData(
          bin.map { case (x, v) => (x._1, x._1 + step, x._2) -> v },

          minX, maxX, maxY, data.map(_.size).sum, displayMax
        )
      }
    }

  }

  def getStep(data: Seq[Double], breaks: Int) = {
    val data2 = data.filter(x => !x.isNaN && !x.isInfinite)
    val min = data2.min
    val max = data2.max
    if (max == min) 1.0
    else {
      val breaks2: Int = if (breaks <= 0) (math.pow(2 * data.size, 0.5).toInt + 1) else breaks
      val ret = (max - min) / breaks2
      assert(ret > 0, "step < 0 " + ret + " " + breaks2 + " " + data.toString + " breaks: " + breaks)
      ret
    }

  }
}

object HistogramPlot {

  def createHistogramPlot(
    data: Seq[Double],
    breaks: Int = -1,
    main: String = "",
    xlab: String = "",
    ylab: String = "",
    relative: Boolean = false,
    ylim: Option[Double] = None,
    xlim: Option[Double] = None
  ): Plot = {
    val hd = {
      val t = HistogramData(data, breaks)
      if (relative) t.relative else t
    }

    createHistogramPlotFromBins(Seq(("", new java.awt.Color(58, 95, 205), hd)), main, xlab, ylab, ylim, xlim)
  }

  def createHistogramPlotMultiple(
    data: Seq[(String, Color, Seq[Double])],
    breaks: Int = -1,
    main: String = "",
    xlab: String = "",
    ylab: String = "",
    relative: Boolean = false,
    ylim: Option[Double] = None,
    xlim: Option[Double] = None
  ): Plot = {
    val hd: Seq[(String, java.awt.Color, mybiotools.plots.HistogramData)] =
      {
        val step: Double = HistogramData.getStep(data.sortBy(_._3.size).last._3, breaks)
        val t: Seq[(String, java.awt.Color, mybiotools.plots.HistogramData)] = data.map(x => (x._1, x._2, HistogramData(x._3, step)))
        if (relative) t.map(x => x.copy(_3 = x._3.relative)) else t

      }

    createHistogramPlotFromBins(hd, main, xlab, ylab, ylim, xlim)
  }

  def createHistogramPlotFromStratified(
    data: Seq[(String, Color, Seq[Double])],
    breaks: Int,
    main: String,
    xlab: String,
    ylab: String,
    relative: Boolean,
    ylim: Option[Double],
    xlim: Option[Double]
  ): Plot = {
    val hd =
      {

        val seq = data.map(_._3).reverse
        val step = HistogramData.getStep(seq.flatten, breaks)
        val hds = HistogramData.fromStratifiedData(seq, step, relative)
        data zip hds map {
          case ((string, color, _), (hd)) =>
            (string, color, hd)
        }

      }

    createHistogramPlotFromBins(hd, main, xlab, ylab, ylim, xlim)
  }

  def histogramDataToScatter(
    data: Seq[(String, Color, HistogramData)]
  ): Seq[(ScatterPlot.Label, IndexedSeq[D2vD3])] = {
    data.map {
      case (label, color, hd) =>
        ScatterPlot.Label(label, color, new java.awt.geom.Ellipse2D.Double(-0.5, -0.5, 1.0, 1.0), new BasicStroke(1.0f)) -> hd.toScatter
    }
  }

  def createHistogramPlotMultipleAsScatter(
    data: Seq[(String, Color, Seq[Double])],
    breaks: Int = -1,
    main: String = "",
    xlab: String = "",
    ylab: String = "",
    relative: Boolean = false,
    ylim: Option[(Double, Double)] = None,
    xlim: Option[(Double, Double)] = None
  ): Plot = {
    val hd: Seq[(String, java.awt.Color, mybiotools.plots.HistogramData)] =
      {
        val step: Double = HistogramData.getStep(data.sortBy(_._3.size).last._3, breaks)
        val t: Seq[(String, java.awt.Color, mybiotools.plots.HistogramData)] = data.map(x => (x._1, x._2, xlim.map {
          case (min, max) =>
            HistogramData(x._3, step, min, max)
        }.getOrElse(HistogramData(x._3, step))))
        if (relative) t.map(x => x.copy(_3 = x._3.relative)) else t

      }

    ScatterPlot.createScatterPlotFromMultiple(histogramDataToScatter(hd), main = main, xlab = xlab, ylab = ylab, ylim = ylim, xlim = xlim)
  }

  def createHistogramPlotFromBins(
    data: Seq[(String, Color, HistogramData)],
    main: String,
    xlab: String,
    ylab: String,
    ylim: Option[Double],
    xlim: Option[Double]
  ): Plot = {

    val maxY = data.map(x => x._3.maxY).max

    val datatables = data.map {
      case (name, color, data) =>

        val datatable = {
          val datatable = new DataTable(classOf[java.lang.Double], classOf[java.lang.Double], classOf[java.lang.Double], classOf[java.lang.Double]);

          data.bins.foreach {
            case ((x, x1, y), density) =>
              datatable.add(x, density, y, x1)
          }
          datatable
        }

        name -> ((color, datatable))

    }

    val plot = new XYPlot() {
      val legend = new HistogramLegend(this)
      legend.clear()
      setLegend(legend)
    }
    val legend = plot.getLegend

    plot.setSetting(Plot.TITLE, main)
    plot.setSetting(Plot.LEGEND, true)
    plot.setSetting(Plot.LEGEND_DISTANCE, 1)
    plot.setSetting(Plot.LEGEND_LOCATION, Location.EAST)

    datatables.foreach {
      case (name, (color, dt)) =>
        plot.add(dt)

        // Customization

        val barrenderer = new HistogramRenderer(plot)
        barrenderer.setSetting(BarPlot.PAINT_ALL_BARS, false)

        plot.setPointRenderer(dt, barrenderer)

        barrenderer.setSetting(PointRenderer.COLOR, color)

        dt.setName(name)
        if (name != "") {
          legend.add(dt)
        }

    }

    val plotarea = plot.getPlotArea

    plotarea.setSetting(PlotArea.BORDER, new BasicStroke(1.0f))

    val axis_X = plot.getAxis(XYPlot.AXIS_X)
    val axis_Y = plot.getAxis(XYPlot.AXIS_Y)

    val rendererY = plot.getAxisRenderer(XYPlot.AXIS_Y);

    val rendererX = plot.getAxisRenderer(XYPlot.AXIS_X);

    rendererX.setSetting(AxisRenderer.LABEL, xlab)

    rendererY.setSetting(AxisRenderer.LABEL, ylab)

    val displayMax = data.map(_._3.lastBinUpperBound).max
    val minX = data.map(_._3.minX).min

    val cappedMaxY: Double = ylim.map(yl => math.min(yl, maxY)).getOrElse(maxY)
    axis_Y.setMax(cappedMaxY)
    axis_Y.setMin(0.0)
    val cappedMaxX: Double = xlim.map(xl => math.min(xl, displayMax)).getOrElse(displayMax)
    axis_X.setMax(cappedMaxX)
    axis_X.setMin(minX)

    rendererY.setSetting(AxisRenderer.INTERSECTION, -Double.MaxValue);
    rendererX.setSetting(AxisRenderer.INTERSECTION, -Double.MaxValue);

    plot.setInsets(new Insets2D.Double(80.0, 100.0, 60.0, 150.0));

    val bounds = new java.awt.geom.Rectangle2D.Double(0.0, 0.0, 700.0, 700.0)
    plot.setBounds(bounds)

    plot

  }
}