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

package mybiotools

import de.erichseifert.gral.graphics.Drawable
import de.erichseifert.gral.io.plots.DrawableWriterFactory
import de.erichseifert.gral.ui.InteractivePanel;
import de.erichseifert.gral.plots.colors.IndexedColorMapper
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
import de.erichseifert.gral.util.{ Orientation, Location, Dimension2D }
import java.awt.geom.Area
import de.erichseifert.gral.graphics.EdgeLayout

import java.awt._
import javax.swing._

package object plots {

  def circle(r: Double) = new java.awt.geom.Ellipse2D.Double(-1.0 * r / 2.0, -1.0 * r / 2.0, r, r)
  def square(r: Double) = new java.awt.geom.Rectangle2D.Double(-1.0 * r / 2.0, -1.0 * r / 2.0, r, r)

  private val random = new scala.util.Random(1232432);
  private val randomColorList = {
    def newRandomColor = {
      val hue = (1.0f + random.nextFloat())
      val saturation = (1.0f + random.nextFloat()) / 2.0f
      val brightness = (1.0f + random.nextFloat()) / 1.0f
      Color.getHSBColor(hue.toFloat, saturation, brightness)
    }
    def loop: Stream[Color] = newRandomColor #:: loop
    loop
  }
  private val colorList = Vector(
    new Color(250, 0, 0),
    new Color(237, 111, 9),
    new Color(50, 0, 250),
    new Color(9, 135, 237),
    new Color(32, 110, 27),
    new Color(84, 204, 204),
    new Color(139, 196, 81),
    new Color(171, 0, 250),
    new Color(176, 224, 0),
    new Color(250, 0, 158),
    new Color(224, 180, 0),
    new Color(196, 121, 0),
    new Color(0, 252, 164),
    new Color(255, 172, 120)
  )

  private val shapeList = Vector(
    new java.awt.geom.Ellipse2D.Double(-5, -5, 10, 10),
    new java.awt.geom.Rectangle2D.Double(-5, -5, 10, 10),
    {
      val p = new java.awt.Polygon(Array(-100, -100, (-100.0 + math.sqrt(3.0) * 100.0).toInt), Array(-100, 100, 0), 3)
      java.awt.geom.AffineTransform.getScaleInstance(0.08, 0.08).createTransformedShape(p)
    },
    {
      val p = new java.awt.geom.Rectangle2D.Double(-5, -5, 10, 10)
      java.awt.geom.AffineTransform.getRotateInstance(0.785398163).createTransformedShape(p)
    },
    {
      val t = 1.0f
      val l = 5.0f
      val SQRT2 = math.pow(2.0, 0.5).toFloat
      val p0 = new java.awt.geom.GeneralPath();
      p0.moveTo(-l - t, -l + t);
      p0.lineTo(-l + t, -l - t);
      p0.lineTo(0.0f, -t * SQRT2);
      p0.lineTo(l - t, -l - t);
      p0.lineTo(l + t, -l + t);
      p0.lineTo(t * SQRT2, 0.0f);
      p0.lineTo(l + t, l - t);
      p0.lineTo(l - t, l + t);
      p0.lineTo(0.0f, t * SQRT2);
      p0.lineTo(-l + t, l + t);
      p0.lineTo(-l - t, l - t);
      p0.lineTo(-t * SQRT2, 0.0f);
      p0.closePath();
      p0
    },
    {
      val t = 1.0f
      val l = 5.0f
      val SQRT2 = math.pow(2.0, 0.5).toFloat
      val p0 = new java.awt.geom.GeneralPath();
      p0.moveTo(-l - t, -l + t);
      p0.lineTo(-l + t, -l - t);
      p0.lineTo(0.0f, -t * SQRT2);
      p0.lineTo(l - t, -l - t);
      p0.lineTo(l + t, -l + t);
      p0.lineTo(t * SQRT2, 0.0f);
      p0.lineTo(l + t, l - t);
      p0.lineTo(l - t, l + t);
      p0.lineTo(0.0f, t * SQRT2);
      p0.lineTo(-l + t, l + t);
      p0.lineTo(-l - t, l - t);
      p0.lineTo(-t * SQRT2, 0.0f);
      p0.closePath();
      java.awt.geom.AffineTransform.getRotateInstance(0.785398163).createTransformedShape(p0)
    },
    new java.awt.geom.Rectangle2D.Double(-2, -5, 4, 10),
    new java.awt.geom.Rectangle2D.Double(-5, -2, 10, 4),
    {
      val large = new Area(new java.awt.geom.Rectangle2D.Double(-5, -5, 10, 10))
      val small = new java.awt.geom.Rectangle2D.Double(-2.5, -2.5, 5, 5)
      large subtract new Area(small)
      large
    },
    {

      val large = {
        val p = new java.awt.geom.Rectangle2D.Double(-5, -5, 10, 10)
        java.awt.geom.AffineTransform.getRotateInstance(0.785398163).createTransformedShape(p)
      }
      val small = {
        val p = new java.awt.geom.Rectangle2D.Double(-2.5, -2.5, 5, 5)
        java.awt.geom.AffineTransform.getRotateInstance(0.785398163).createTransformedShape(p)
      }
      val largeArea = new Area(large)
      largeArea subtract new Area(small)
      largeArea
    },
    {
      val large = new Area(new java.awt.geom.Ellipse2D.Double(-5, -5, 10, 10))
      val small = new java.awt.geom.Ellipse2D.Double(-2.5, -2.5, 5, 5)
      large subtract new Area(small)
      large
    }
  )

  def shapePick(idx: Int): Shape = shapeList(idx % shapeList.size)

  def colorPick(idx: Int, max: Int) = {
    if (max <= colorList.size && idx < colorList.size) colorList(idx)
    else synchronized {
      randomColorList(idx)
    }
  }
  def generateRandomColor(mix: java.awt.Color) = {

    var red = random.nextInt(256);
    var green = random.nextInt(256);
    var blue = random.nextInt(256);

    // mix the color
    if (mix != null) {
      red = (red + mix.getRed()) / 2;
      green = (green + mix.getGreen()) / 2;
      blue = (blue + mix.getBlue()) / 2;
    }

    new Color(red, green, blue)
  }

  class RandomColorMapper(mix: java.awt.Color) extends IndexedColorMapper {
    val random = new scala.util.Random(1)
    var map = Map[Int, Color]()

    def get(i: Int) = map.get(i) getOrElse {
      var red = random.nextInt(256);
      var green = random.nextInt(256);
      var blue = random.nextInt(256);

      red = (red + mix.getRed()) / 2;
      green = (green + mix.getGreen()) / 2;
      blue = (blue + mix.getBlue()) / 2;
      val color = new Color(red, green, blue)
      map = map.updated(i, color)
      color
    }
  }

  def show(plot: Drawable) {
    val ip = new InteractivePanel(plot);
    //1. Create the frame.
    val frame = new JFrame("");

    //2. Optional: What happens when the frame closes?
    frame.setDefaultCloseOperation(javax.swing.WindowConstants.HIDE_ON_CLOSE);

    //3. Create components and put them in the frame.
    //...create emptyLabel...
    frame.getContentPane().add(ip, BorderLayout.CENTER);

    //4. Size the frame.
    val b = plot.getBounds
    val d = new Dimension(b.getWidth.toInt, b.getHeight.toInt)
    frame.pack();
    frame.setSize(d);
    //5. Show it.
    frame.setVisible(true);
  }

  def renderToByteArray(plot: Drawable, mime: String, x: Int, y: Int): Array[Byte] = {
    val factory = DrawableWriterFactory.getInstance();
    val writer = factory.get(mime);
    val bs = new java.io.ByteArrayOutputStream()
    writer.write(plot, bs, 0.0, 0.0, x, y);
    bs.toByteArray
  }

  def renderToByteArray(plot: Drawable, mime: String, scale: Double): Array[Byte] = {
    val factory = DrawableWriterFactory.getInstance();
    val writer = factory.get(mime);
    val bs = new java.io.ByteArrayOutputStream()
    writer.write(plot, bs, scale);
    bs.toByteArray
  }

  def pdfToFile(f: java.io.File, plot: Drawable) = mybiotools.writeBinaryToFile(f, renderToByteArray(plot, "application/pdf", 1.0))
  def pdfToFile(f: String, plot: Drawable) = mybiotools.writeBinaryToFile(f, renderToByteArray(plot, "application/pdf", 1.0))
  def pdfToFile(plot: Drawable) = {
    val f = TempFile.createTempFile(".pdf")
    mybiotools.writeBinaryToFile(f, renderToByteArray(plot, "application/pdf", 1.0))
    f
  }

  def pngToFile(f: java.io.File, plot: Drawable) = mybiotools.writeBinaryToFile(f, renderToByteArray(plot, "image/png", 1.0))
  def pngToFile(f: String, plot: Drawable) = mybiotools.writeBinaryToFile(f, renderToByteArray(plot, "image/png", 1.0))
  def pngToFile(plot: Drawable) = {
    val f = TempFile.createTempFile(".png")
    mybiotools.writeBinaryToFile(f, renderToByteArray(plot, "image/png", 1.0))
    f
  }

  def nice(p: Plot) = {

    val font = new java.awt.Font(Font.SANS_SERIF, Font.PLAIN, 25)
    val fontsmall = new java.awt.Font(Font.SANS_SERIF, Font.PLAIN, 20)
    val fontsmaller = new java.awt.Font(Font.SANS_SERIF, Font.PLAIN, 15)

    p.getLegend.setSetting(Legend.ORIENTATION, Orientation.VERTICAL)
    p.getLegend.setSetting(Legend.FONT, fontsmall)
    p.getLegend.setSetting(Legend.BACKGROUND, new Color(255, 255, 255, 0))
    p.getLegend.setSetting(Legend.BORDER, null)
    p.getLegend.setSetting(Legend.GAP, new Dimension2D.Double(1.0, 0.5))
    p.getLegend.setSetting(Legend.SYMBOL_SIZE, new Dimension2D.Double(0.5, 0.5))
    p.getLegend.setSetting(Legend.TEXT_ALIGN, 0);
    p.setSetting(Plot.LEGEND_LOCATION, Location.SOUTH)
    p.setSetting(Plot.LEGEND_DISTANCE, 11)

    p.setInsets(new Insets2D.Double(2.0, 125.0, 450.0, 125.0));
    p.setSetting(Plot.TITLE_FONT, font)
    // p.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL, "Number of genes")
    // p.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL, "Truncation count")

    p.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS, true)
    p.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS_MINOR, true)
    p.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICKS_MINOR, true)

    p.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL_FONT, font)
    p.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL_FONT, font)
    p.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS_FONT, fontsmall)
    p.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICKS_FONT, fontsmall)

    p.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.LABEL_DISTANCE, 1.5)
    p.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL_DISTANCE, 0.3)
    p.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICK_LABELS_DISTANCE, 0.2)
    p.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICK_LABELS_DISTANCE, 0.3)

    p.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS_LENGTH, 0.25)
    p.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICKS_LENGTH, 0.25)
    p.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS_MINOR_LENGTH, 0.25)
    p.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICKS_MINOR_LENGTH, 0.25)
    p.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS_ALIGNMENT, 1.0)
    p.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICKS_ALIGNMENT, 1.0)

    p.getAxisRenderer(XYPlot.AXIS_Y).setSetting(AxisRenderer.TICKS_MINOR_ALIGNMENT, 1.0)
    p.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICKS_MINOR_ALIGNMENT, 1.0)

    p.setBounds(0.0, 0.0, 645.0, 900.0)
    // val numberformat = new java.text.DecimalFormat("0.#E0");

    // p.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.TICK_LABELS_FORMAT, numberformat)
    p
  }

  def compositeTable(plots: Seq[Drawable], tableWidth: Int): Drawable = {

    val container = new DrawableContainer(new TableLayout(tableWidth))
    plots.zipWithIndex.foreach {
      case (plot, idx) =>
        // plot.setBounds(0.0, 0.0, 900.0, 900.0)
        container.add(plot)

    }
    val bounds = new java.awt.geom.Rectangle2D.Double(0.0, 0.0, tableWidth * 1000.0, ((plots.size / tableWidth) + 1) * 1000.0)
    container.setBounds(bounds)
    container
  }
}