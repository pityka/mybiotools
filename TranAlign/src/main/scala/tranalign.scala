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

import mybiotools.sequence._
import mybiotools._
import mybiotools.config.Config.configInstance

object TranAlignApp extends App {

  val nuclFastaPath = configInstance.getString("nucl")

  val protFastaPath = configInstance.getString("alignedProtein")

  val outPath = configInstance.getString("out")

  val proteins = readFasta(scala.io.Source.fromFile(protFastaPath))

  val nucl = fastaIterator(scala.io.Source.fromFile(nuclFastaPath))

  val transformed = nucl.map { entry =>
    val id = entry._1
    val seq = entry._2
    val tranaligned: Option[String] = proteins.get(id).flatMap { protSeq =>
      tranalign(seq, protSeq)
    }
    tranaligned match {
      case None => None
      case Some(x) => Some(formatForFastaEntry(id.toString, x))
    }
  }.filter(_.isDefined).map(_.get)

  openFileWriter(new java.io.File(outPath)) { writer =>
    transformed.foreach { x =>
      writer.write(x.mkString("\n"))
      writer.write('\n')
    }
  }

}
