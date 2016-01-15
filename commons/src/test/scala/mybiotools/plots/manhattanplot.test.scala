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
import ManhattanPlot._
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

import de.erichseifert.gral.graphics.Drawable;
import de.erichseifert.gral.graphics.DrawingContext;
import de.erichseifert.gral.io.IOCapabilities;
import de.erichseifert.gral.io.IOCapabilitiesStorage;
import de.erichseifert.gral.util.Messages;

class ManhattanPlotSpec extends FunSpec with Matchers {

  describe("ManhattanPlot") {
    val data: Iterator[AssociationResult] = readAssociationResultsFromPlinkLinearAssoc(io.Source.fromFile(getClass.getResource("/plots/plink.assoc.linear").getPath), None)

    ignore("10k") {
      // show(plot(data, Some(1E-2), Some(12), 22))
      val expected = Array[Byte](-119, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, 73, 72, 68, 82, 0, 0, 0, 7, 0, 0, 0, 3, 8, 2, 0, 0, 0, -48, -95, -126, -110, 0, 0, 0, 65, 73, 68, 65, 84, 120, -100, 99, -8, -7, -13, -25, 127, 24, -8, -9, -25, 55, 16, 1, 25, 12, 55, 54, 47, -67, -72, -27, -64, -107, 35, -41, -114, -51, 88, 112, 118, -57, -39, -77, -117, 86, -125, 68, 87, -58, 5, 119, -58, 53, -75, 7, 53, -10, 123, -92, 76, 13, -119, -98, 26, 21, 7, 20, 5, 0, -10, -68, 47, -6, -36, -47, 71, 102, 0, 0, 0, 0, 73, 69, 78, 68, -82, 66, 96, -126)
      renderToByteArray(
        plot(data, Some(0.2), None),
        "image/png", 0.005
      ) should equal(expected)
    }
  }
}