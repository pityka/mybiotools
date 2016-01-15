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

import de.erichseifert.gral.util.Insets2D;
import java.awt.geom._
import java.awt._

import de.erichseifert.gral.graphics.{ Layout, Container, Drawable }
class FreeLayout extends Layout {
  import collection.JavaConversions._

  def layout(container: Container) {
  }

  def getPreferredSize(container: Container): Dimension2D = {
    val width = container.map(_.getX).max - container.map(_.getX).min
    val height = container.map(_.getY).max - container.map(_.getY).min
    new de.erichseifert.gral.util.Dimension2D.Double(width, height)
  }
}

class FreeRelativeLayout extends Layout {
  import collection.JavaConversions._

  val zeroBound = new Rectangle2D.Double()

  def layout(container: Container) {
    if (container.getBounds != zeroBound) {
      val origBoundingWidth = container.map(_.getBounds.getX).max - container.map(_.getBounds.getX).min + container.maxBy(_.getBounds.getX).getBounds.getWidth
      val origBoundingHeight = container.map(_.getBounds.getY).max - container.map(_.getBounds.getY).min + container.maxBy(_.getBounds.getY).getBounds.getHeight
      val origBoundingX = container.map(_.getBounds.getX).min
      val origBoundingY = container.map(_.getBounds.getY).min

      val newbounds = container.getBounds();
      var insets = container.getInsets();
      if (insets == null) {
        insets = new Insets2D.Double();
      }

      val newBoundingX = newbounds.getX() + insets.getLeft();
      val newBoundingY = newbounds.getY() + insets.getTop();
      val newBoundingWidth = newbounds.getWidth - insets.getRight
      val newBoundingHeight = newbounds.getHeight - insets.getBottom

      val f1 = newBoundingWidth / origBoundingWidth
      val f2 = newBoundingHeight / origBoundingHeight

      container.foreach { component =>

        val x = (component.getX - origBoundingX) * f1
        val y = (component.getY - origBoundingY) * f2
        val w = component.getWidth * f1
        val h = component.getHeight * f2
        component.setBounds(x, y, w, h)

      }
    }
  }

  def getPreferredSize(container: Container): Dimension2D = {
    val width = container.map(_.getX).max - container.map(_.getX).min + container.maxBy(_.getBounds.getX).getBounds.getWidth
    val height = container.map(_.getY).max - container.map(_.getY).min + container.maxBy(_.getBounds.getY).getBounds.getHeight
    new de.erichseifert.gral.util.Dimension2D.Double(width, height)
  }
}
