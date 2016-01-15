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

package viralshortread

import mybiotools.plots.ScatterPlot._

object Plots {

  def plot1DMetric(d: IndexedSeq[Double], log: Boolean) = {
    val plot = mybiotools.plots.ScatterPlot.createScatterPlotFromMultiple(
      data = List(Label("", java.awt.Color.BLACK, new java.awt.BasicStroke(0.2f)) -> seq2(d.zipWithIndex.map(x => x._2.toDouble -> x._1))),
      ylog = log
    )
    plot.setBounds(0, 0, 6000, 400)
    plot
  }

  def plotFrequencyDistributions(frequencies: IndexedSeq[Map[Char, Int]]) = {
    val distinctChars = frequencies.flatMap(_.keys).distinct
    val plot = mybiotools.plots.ScatterPlot.createScatterPlotFromMultiple(
      data = distinctChars.zipWithIndex.map {
        case (char, charidx) =>
          Label(List(char).mkString, mybiotools.plots.colorPick(charidx, distinctChars.size), new java.awt.BasicStroke(0.2f)) -> seq2(frequencies.zipWithIndex.map(x => x._2.toDouble -> x._1.get(char).getOrElse(0).toDouble))
      },
      ylog = true
    )
    plot.setBounds(0, 0, 6000, 400)
    plot
  }

}