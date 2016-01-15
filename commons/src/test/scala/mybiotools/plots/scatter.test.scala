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
package plots
import ScatterPlot._
import mybiotools.gwascommons.associationresults._

import org.scalatest.FunSpec
import org.scalatest.Matchers

import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.OutputStream;

import javax.imageio.ImageIO;
import javax.imageio.ImageWriter;
import javax.imageio.stream.ImageOutputStream;
import java.awt.RenderingHints;
import de.erichseifert.gral.plots.XYPlot
import de.erichseifert.gral.plots.axes.AxisRenderer

import de.erichseifert.gral.graphics.Drawable;
import de.erichseifert.gral.graphics.DrawingContext;
import de.erichseifert.gral.io.IOCapabilities;
import de.erichseifert.gral.io.IOCapabilitiesStorage;
import de.erichseifert.gral.util.Messages;
import de.erichseifert.gral.plots.colors.LinearGradient
import java.awt.Color
import mybiotools.plots.ScatterPlot._

import org.saddle._

class ScatterPlotSpec extends FunSpec with Matchers {

  describe("ScatterPlot") {
    val data = (100, 2) :: (30000, 4) :: (5000, 6) :: (10000, 4) :: Nil map (x => (x._1.toDouble, x._2.toDouble))

    it("small") {
      val p = createScatterPlot(data.toIndexedSeq, xlog = true, main = "dfsdf", xlab = "x", ylab = "y", fit = true, rsquared = true)
      p.getAxisRenderer(XYPlot.AXIS_X).setSetting(AxisRenderer.LABEL_POSITION, 0.8)

      show(p)
      // val expected = Array[Byte](-119, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, 73, 72, 68, 82, 0, 0, 0, 7, 0, 0, 0, 3, 8, 2, 0, 0, 0, -48, -95, -126, -110, 0, 0, 0, 65, 73, 68, 65, 84, 120, -100, 99, -8, -7, -13, -25, 127, 24, -8, -9, -25, 55, 16, 1, 25, 12, 55, 54, 47, -67, -72, -27, -64, -107, 35, -41, -114, -51, 88, 112, 118, -57, -39, -77, -117, 86, -125, 68, 87, -58, 5, 119, -58, 53, -75, 7, 53, -10, 123, -92, 76, 13, -119, -98, 26, 21, 7, 20, 5, 0, -10, -68, 47, -6, -36, -47, 71, 102, 0, 0, 0, 0, 73, 69, 78, 68, -82, 66, 96, -126)
      // renderToByteArray(
      //   plot(data, Some(0.2), None),
      //   "image/png", 0.005) should equal(expected)
    }
    it("frame") {
      val frame = Frame("1" -> Series("1" -> 2.0, "2" -> 5.0, "3" -> 6.0), "2" -> Series("2" -> 2.0, "3" -> 3.0, "1" -> 4.0), "3" -> Series("1" -> -1.0, "2" -> 3.0, "3" -> 4.0))
      show(createScatterPlotsFromFrame(frame).head)
    }

    it("multiple") {
      show(createScatterPlotFromMultiple(List(Label("z", Color.blue) -> Vector(3.0 -> 3.0, 3.1 -> 3.1), Label("x", Color.red, new java.awt.BasicStroke(3.0f), 0) -> Vector(1.0 -> 1.0, 1.1 -> 1.1, 1.2 -> 1.0, 1.3 -> 0.95, 1.2 -> 0.8, 1.0 -> 1.0, 1.1 -> 1.0, 1.3 -> 1.4), Label("y", Color.black) -> Vector(2.0 -> 2.0, 2.2 -> 2.2)), xlog = false))
    }

    it("multiple colorscale") {
      val lm = new LinearGradient(Color.green, Color.black)
      lm.setRange(20.0, 21.0)
      show(createScatterPlotFromMultiple(List(
        Label("z", Color.blue) -> Vector(3.0 -> 3.0, 3.1 -> 3.1),
        Label("x", Color.red) -> Vector(1.0 -> 1.0, 1.1 -> 1.1),
        Label("y", lm) -> Vector((2.0, 2.0, 20.3), (2.2, 2.2, 20.6))
      ), xlog = false))
    }

  }
}