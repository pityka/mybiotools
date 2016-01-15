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

package dispensability

import mybiotools.config.Config.configInstance
import mybiotools.gwascommons.MAFReader._
import mybiotools.intervaltree._
import mybiotools._

object ProcessMAF extends App {
  val maffiles = configInstance.getString("mafs")
  openSource(maffiles) { lines =>
    lines.getLines.foreach { line =>
      openSource(line) { source =>
        mapToIntervalOfFirstSequence(readMAF(source)) { slines =>
          assert(slines.size == 2)
          slines.head.sequence zip slines(1).sequence count { case (x, y) => x.toUpper != y.toUpper }
        }.foreach {
          case (chr, IntervalWithPayLoad(from, to, diff)) =>
            println(s"$chr\t$from\t$to\t$diff")
        }
      }

    }
  }

}