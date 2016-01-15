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

import mybiotools.gwascommons._
import mybiotools._
import mybiotools.config.Config.configInstance

object PlotQQApp extends App {

  val outfile = configInstance.getString("out")

  val minimum = scala.util.Try(configInstance.getDouble("minimum")).toOption

  val maximum = scala.util.Try(configInstance.getDouble("maximum")).toOption

  val pvalues = io.Source.stdin.getLines.map { x =>
    catchToLeft(x.toDouble).fold(_ => None, x => Some(x))
  }.filter(_.isDefined).map(_.get)

  writeBinaryToFile(outfile, mybiotools.plots.renderToByteArray(mybiotools.plots.QQPlot.plot(pvalues, minimum = minimum, maximum = maximum), "image/png", 1000, 1000))
}