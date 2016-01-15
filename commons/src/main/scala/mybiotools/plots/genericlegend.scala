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

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.Paint;
import java.awt.Stroke;
import java.awt.geom.Dimension2D;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import de.erichseifert.gral.data.DataSource;
import de.erichseifert.gral.data.Row;
import de.erichseifert.gral.graphics.AbstractDrawable;
import de.erichseifert.gral.graphics.Drawable;
import de.erichseifert.gral.graphics.DrawableContainer;
import de.erichseifert.gral.graphics.DrawingContext;
import de.erichseifert.gral.graphics.EdgeLayout;
import de.erichseifert.gral.graphics.Layout;
import de.erichseifert.gral.graphics.StackedLayout;
import de.erichseifert.gral.plots.Label;
import de.erichseifert.gral.plots.StylableContainer;
import de.erichseifert.gral.plots.settings.Key;
import de.erichseifert.gral.plots.settings.SettingChangeEvent;
import de.erichseifert.gral.plots.settings.SettingsStorage;
import de.erichseifert.gral.util.GraphicsUtils;
import de.erichseifert.gral.util.Insets2D;
import de.erichseifert.gral.util.Location;
import de.erichseifert.gral.util.Orientation;
import de.erichseifert.gral.plots._
import de.erichseifert.gral.plots.legends._
import de.erichseifert.gral.plots.legends.Legend._
import de.erichseifert.gral.plots.axes._
import scala.collection.JavaConversions._
import de.erichseifert.gral.plots.colors._

abstract class GenericLegend extends StylableContainer with Legend {

  setSettingDefault(BACKGROUND, Color.WHITE);
  setSettingDefault(BORDER, new BasicStroke(1f));
  setSettingDefault(FONT, Font.decode(null));
  setSettingDefault(COLOR, Color.BLACK);
  setSettingDefault(ORIENTATION, Orientation.VERTICAL);
  setSettingDefault(ALIGNMENT_X, 0.0);
  setSettingDefault(ALIGNMENT_Y, 0.0);
  setSettingDefault(GAP, new de.erichseifert.gral.util.Dimension2D.Double(2.0, 0.5));
  setSettingDefault(SYMBOL_SIZE, new de.erichseifert.gral.util.Dimension2D.Double(2.0, 2.0));
  setLayout(new EdgeLayout)

  def add(s: de.erichseifert.gral.data.DataSource): Unit = ()

  def clear(): Unit = ()
  def contains(s: de.erichseifert.gral.data.DataSource): Boolean = false
  def remove(x$1: de.erichseifert.gral.data.DataSource): Unit = ()

  def refresh: Unit = ()

  /**
   * Draws the {@code Drawable} with the specified drawing context.
   * @param context Environment used for drawing.
   */
  override def draw(context: DrawingContext): Unit = {
    drawBackground(context);
    drawBorder(context);
    drawComponents(context);
  }

  /**
   * Draws the background of this legend with the specified drawing context.
   * @param context Environment used for drawing.
   */
  protected def drawBackground(context: DrawingContext): Unit = {
    val bg: Color = getSetting(BACKGROUND)
    if (bg != null) {
      GraphicsUtils.fillPaintedShape(
        context.getGraphics(), getBounds(), bg, null
      );
    }
  }

  /**
   * Draws the border of this legend with the specified drawing context.
   * @param context Environment used for drawing.
   */
  protected def drawBorder(context: DrawingContext): Unit = {
    val stroke: Stroke = getSetting(BORDER);
    if (stroke != null) {
      val fg: Color = getSetting(COLOR);
      GraphicsUtils.drawPaintedShape(
        context.getGraphics(), getBounds(), fg, null, stroke
      );
    }
  }

}