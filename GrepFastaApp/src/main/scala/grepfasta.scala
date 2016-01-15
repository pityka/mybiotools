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

import mybiotools._
import mybiotools.config.Config.configInstance

object GrepFasta extends App {

  val list: Set[String] = if (configInstance.getString("list") == "") Set[String]() else io.Source.fromFile(configInstance.getString("list")).getLines.toSet

  val pattern = if (configInstance.getString("pattern") != "") Some(configInstance.getString("pattern").r) else None

  val input = if (args.size > 0) io.Source.fromFile(args(0)) else io.Source.stdin

  val lineWidth = configInstance.getInt("width")

  val out = new java.io.OutputStreamWriter(System.out)
  writeFasta(
    out,
    fastaIterator(input).filter {
      case (head, _) =>
        (pattern.map(_ findFirstIn head.toString).getOrElse(None)).isDefined ||
          list.contains(head.toString)
    },
    lineWidth
  )
  out.close
}
