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

package rnaseqalign.analysis

import mybiotools._
import java.io.File
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
import de.erichseifert.gral.graphics.Drawable
import de.erichseifert.gral.util.Location
import java.awt.Color
import java.awt.BasicStroke
import scala.runtime.RichInt
import scala.runtime.RichFloat
import de.erichseifert.gral.graphics._
import de.erichseifert.gral.graphics.TableLayout
import de.erichseifert.gral.plots.colors.QuasiRandomColors
import de.erichseifert.gral.plots.BarPlot
import de.erichseifert.gral.util.DataUtils;
import de.erichseifert.gral.util.PointND;

import de.erichseifert.gral.graphics.EdgeLayout

import mybiotools.SummaryStat
import mybiotools.plots._
import mybiotools.plots.ScatterPlot._

object EnrichmentPlot {
  def createPlot(up: EnrichmentResult, down: EnrichmentResult): Drawable = {
    def ex(er: EnrichmentResult) = er.db.setsByName.keys.map(k => (er.enrichmentPValues.get(k), SummaryStat(er.db.setsByName(k).genes.flatMap(g => er.query.differentialExpressions.get(g))).mean, er.db.representativeCategories(k))).toSeq.filter(_._1.isDefined).map(x => DataForEnrichmentPlot(x._1.get, x._2, x._3))

    createPlot(ex(up) ++ ex(down))
  }
  def createPlot(updown: EnrichmentResultPlain): Drawable = {
    def ex(er: EnrichmentResultPlain) = er.db.setsByName.keys.map(k => (er.enrichmentPValues.get(k), SummaryStat(er.db.setsByName(k).genes.flatMap(g => er.weights.get(g))).mean, er.db.representativeCategories(k))).toSeq.filter(_._1.isDefined).map(x => DataForEnrichmentPlot(x._1.get, x._2, x._3))

    createPlot(ex(updown))
  }
  def createPlot(data: Seq[DataForEnrichmentPlot]): Drawable = mybiotools.plots.EnrichmentPlot.createPlot(data)

}