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

package mybiotools.tasks

import mybiotools.config.Config.configInstance
import scala.collection.JavaConversions._
import java.io.File

object SharedFileTestApp extends App {
  val ts = defaultTaskSystem
  import ts._
  if (ts.hostConfig.myRole == MASTER) {
  } else {
    0 to 10 foreach { i =>
      val tmp = mybiotools.TempFile.createTempFile("random.txt")
      mybiotools.writeToFile(tmp, scala.util.Random.nextInt.toString)
      println("A")
      SharedFile(tmp, name = "random" + i.toString, canMoveAway = true)
      println("B")
    }
    0 to 10 foreach { i =>
      println("C")

      val tmp = mybiotools.TempFile.createTempFile("fix.txt")
      mybiotools.writeToFile(tmp, i.toString)
      SharedFile(tmp, name = "fix" + i.toString, canMoveAway = true)
      println("D")

    }
    configInstance.getString("importfiles").split(",").map { f =>
      SharedFile(new File(f), name = new File(f).getName)
    }
    ts.shutdown
  }
  // t
}