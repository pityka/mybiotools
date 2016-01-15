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

import mybiotools.config.Config.configInstance

import mybiotools.{ writeToFile, fastSplitSeparator, openSource }

object DiscriminateToApp extends App {

  val file = configInstance.getString("ffile")

  val sep = configInstance.getString("sep") match {
    case "tab" => "\t"
    case x => x
  }

  val outPathTrunk = configInstance.getString("outPathTrunk")

  val header = configInstance.getBoolean("headerLine")

  val discriminatorField = configInstance.getInt("discriminatorField")

  val input = openSource(file)(_.getLines.map(x => fastSplitSeparator(x, sep.apply(0))).toSeq)

  val grouped: Map[String, Seq[Seq[String]]] = input.groupBy(x => x(discriminatorField))

  grouped.par.foreach { tup =>
    val discr = tup._1
    val lines = tup._2.map(x => x.mkString(sep)).mkString("\n")

    writeToFile(outPathTrunk + "_" + discr, lines)
  }

}