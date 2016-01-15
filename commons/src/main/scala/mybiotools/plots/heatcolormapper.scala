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

import java.awt.Color;
import java.awt.Paint;

import de.erichseifert.gral.util.MathUtils;
import de.erichseifert.gral.plots.colors._

/**
 * Class that generates the colors of a rainbow.
 */
class HeatMapColors extends ScaledContinuousColorMapper {
  /** Version id for serialization. */

  /**
   * Returns the Paint according to the specified value.
   * @param value Value of color.
   * @return Paint.
   */
  override def get(value: Double): Paint = {
    val scaled = scale(value);
    val v = applyMode(scaled, 0.0, 1.0);
    if (!MathUtils.isCalculatable(v)) {
      return null;
    }

    def scaleHue(v: Double) = (2.0 / 3.0) - v * (2.0 / 3.0)

    val hue = scaleHue(v).floatValue();
    Color.getHSBColor(hue, 1f, 1f);
  }

}