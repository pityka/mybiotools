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

import de.erichseifert.gral.data.DataSeries;
import de.erichseifert.gral.data.DataTable;
import de.erichseifert.gral.data.filters.Convolution;
import de.erichseifert.gral.data.filters.Filter;
import de.erichseifert.gral.data.filters.Kernel;
import de.erichseifert.gral.data.filters.KernelUtils;
import de.erichseifert.gral.data.filters.Median;
import de.erichseifert.gral.plots.Plot;
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
import java.awt._;
import scala.runtime.RichInt
import scala.runtime.RichFloat
import de.erichseifert.gral.graphics.DrawableContainer
import de.erichseifert.gral.graphics.TableLayout

import mybiotools.SummaryStat

object QQPlot {

  def writeQQPlot(fileName: String, pValues: Iterator[Double]) {
    mybiotools.writeBinaryToFile(
      fileName,
      renderToByteArray(
        plot(pValues),
        "image/png", 1000, 1000
      )
    )
  }

  def computeLambda(pvalues: Seq[Double], minimum: Double = 0.0, maximum: Double = 1.0): Double = {

    val medianObserved = SummaryStat(pvalues).median

    (((maximum - minimum) * 0.5) + minimum) / medianObserved
  }
  def plot(pvalues: Iterator[Double], disablePruning: Boolean = false, minimum: Option[Double] = None, maximum: Option[Double] = None): AbstractDrawable = {

    if (pvalues.hasNext) {

      // The sole purpose of this scope is to let GC collect the array of doubles.
      val (lambda, datatable, first99percentLineEnd, last100LineEnd, expectedMax, last100, ciLower, ciUpper) = {

        // Plot expected vs observed and put up a line with slope lambda
        val datatable = new DataTable(classOf[java.lang.Double], classOf[java.lang.Double])

        val ciLower = new DataTable(classOf[java.lang.Double], classOf[java.lang.Double])
        val ciUpper = new DataTable(classOf[java.lang.Double], classOf[java.lang.Double])

        val observed = pvalues.filter(x => minimum.map(m => m < x).getOrElse(true)).toArray.sorted

        val N: Int = observed.size

        val majority = 0.99

        val first99percent: Int = ((1 - majority) * N).toInt

        val last1percent: Int = (majority * N).toInt

        val last100: Option[Int] = if (N > 100) Some(100) else None

        val total = (N / (maximum.getOrElse(1.0) - minimum.getOrElse(0.0)))

        val expectedMax = -1 * math.log10(1 / (total + 1).toDouble)

        val lambda = computeLambda(observed, minimum.getOrElse(0.0), maximum.getOrElse(1.0))

        val d99 = observed(first99percent)
        val d100 = last100.map(x => observed(x))

        // take random sample of 30k except the largest 10k p values
        val rand = new scala.util.Random(1234)
        val maxPlotted = 30000
        val randFilter = 1.0 / (N / maxPlotted.toDouble)

        val start = if (minimum.isEmpty) 0 else (minimum.get * N + 1).toInt

        for (i <- 0 until N) {
          val expected: Double = -1 * math.log10((i + 1).toDouble / (total + 1) + minimum.getOrElse(0.0))
          val head = observed(i)
          if (disablePruning || i < 10000 || N < maxPlotted || rand.nextDouble < randFilter) {
            // mark for GC
            observed(i) = null.asInstanceOf[Double]

            // https://en.wikipedia.org/wiki/Order_statistic
            // the kth order statistic of the uniform distribution is a Beta random variable.[2][3]
            // ~ Beta(k,n+1-k).
            ciUpper.add(expected, -1 * math.log10(jdistlib.Beta.quantile(0.95, i, total + 1 - i, true, false)))
            ciLower.add(expected, -1 * math.log10(jdistlib.Beta.quantile(0.05, i, total + 1 - i, true, false)))

            datatable.add(expected, -1 * math.log10(head))
          }
        }

        (lambda, datatable, d99, d100, expectedMax, last100, ciLower, ciUpper)
      }

      val slope = new DataTable(classOf[java.lang.Double], classOf[java.lang.Double])
      slope.add(0.0, 0.0)
      // slope.add(expectedMax, (lambda * expectedMax))
      slope.add(expectedMax, (1.0 * expectedMax))

      val first99percentLineData = new DataTable(classOf[java.lang.Double], classOf[java.lang.Double])
      first99percentLineData.add(0.0, -1 * math.log10(first99percentLineEnd))
      first99percentLineData.add(expectedMax, -1 * math.log10(first99percentLineEnd))

      val plot = new XYPlot(datatable, slope, first99percentLineData, ciLower, ciUpper)

      if (last100.isDefined) {
        val last100LineData = new DataTable(classOf[java.lang.Double], classOf[java.lang.Double])
        last100LineData.add(0.0, -1 * math.log10(last100LineEnd.get))
        last100LineData.add(expectedMax, -1 * math.log10(last100LineEnd.get))
        plot.add(last100LineData)

        val bluelineRenderer = new DefaultLineRenderer2D();
        bluelineRenderer.setSetting(LineRenderer.COLOR, new Color(0.0f, 0.0f, 1.0f))
        plot.setLineRenderer(last100LineData, bluelineRenderer)
        plot.setPointRenderer(last100LineData, null)

      }

      // Customization of the plot

      plot.setSetting(Plot.TITLE, "lambda: " + lambda.toString)

      val pointrenderer = plot.getPointRenderer(datatable)
      pointrenderer.setSetting(PointRenderer.SHAPE, new java.awt.geom.Ellipse2D.Double(0, 0, 5, 5))

      val redlineRenderer = new DefaultLineRenderer2D();
      redlineRenderer.setSetting(LineRenderer.COLOR, new Color(1.0f, 0.0f, 0.0f))

      plot.setLineRenderer(slope, redlineRenderer)
      plot.setPointRenderer(slope, null)

      plot.setLineRenderer(first99percentLineData, redlineRenderer)
      plot.setPointRenderer(first99percentLineData, null)

      {
        plot.setPointRenderer(ciLower, null)
        plot.setPointRenderer(ciUpper, null)

        val lineRenderer = new DefaultLineRenderer2D();
        lineRenderer.setSetting(LineRenderer.COLOR, new Color(0.9f, 0.9f, 0.9f))
        plot.setLineRenderer(ciUpper, lineRenderer)
        plot.setLineRenderer(ciLower, lineRenderer)
      }

      val plotarea = plot.getPlotArea
      plotarea.setSetting(XYPlotArea2D.GRID_MAJOR_X, false)
      plotarea.setSetting(XYPlotArea2D.GRID_MAJOR_Y, false)
      plotarea.setSetting(XYPlotArea2D.GRID_MINOR_X, false)
      plotarea.setSetting(XYPlotArea2D.GRID_MINOR_Y, false)

      val rendererY = plot.getAxisRenderer(XYPlot.AXIS_Y);
      val rendererX = plot.getAxisRenderer(XYPlot.AXIS_X);

      rendererX.setSetting(AxisRenderer.LABEL, "Expected -log10 p-values")
      rendererY.setSetting(AxisRenderer.LABEL, "Observed -log10 p-values")

      rendererX.setSetting(AxisRenderer.TICKS_MINOR, false)
      rendererX.setSetting(AxisRenderer.TICKS_ALIGNMENT, 0.0)
      rendererY.setSetting(AxisRenderer.TICKS_MINOR, false)
      rendererY.setSetting(AxisRenderer.TICKS_ALIGNMENT, 0.0)

      plot.setInsets(new Insets2D.Double(80.0, 80.0, 80.0, 80.0));

      plot.setBounds(0.0, 0.0, 1000.0, 1000.0)

      plot
    } else {
      val plot = new XYPlot()
      plot.setSetting(Plot.TITLE, "Dataset was empty.")
      plot.setBounds(0.0, 0.0, 1000.0, 1000.0)
      plot

    }
  }

}