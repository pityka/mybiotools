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

package mybiotools.workflows

import mybiotools._
import mybiotools.tasks._
import java.io.File
import akka.actor.{ ActorRefFactory }
import scala.util.Try
import mybiotools.stat.ZMetaCombination._

case class CatFilesOutput(file: SharedFile) extends ResultWithSharedFiles(file)

case class CatFilesInput(
    files: Option[List[SharedFile]],
    expectedFileCount: Option[Int],
    outname: Option[String]
) extends SimplePrerequisitive[CatFilesInput] {
  override def ready = {

    (expectedFileCount.isDefined && files.isDefined && outname.isDefined && files.get.size == expectedFileCount.get)

  }
}

object CatFilesInput {

  def apply(n: Int, outname: String): CatFilesInput = {

    CatFilesInput(
      expectedFileCount = Some(n),
      outname = Some(outname),
      files = None
    )
  }

  def updateCatFilesInput: UpdatePrerequisitive[CatFilesInput] = {
    case (self, i: MetaAnalyseOutput) => self.copy(files = if (self.files.isDefined) Some((i.file :: self.files.get).toSet.toList) else Some(List(i.file)))
  }
}

object catFiles {

  def apply(
    in: CatFilesInput,
    update: UpdatePrerequisitive[CatFilesInput] = CatFilesInput.updateCatFilesInput orElse identity[CatFilesInput]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = 1000)
    ) {
      case (CatFilesInput(Some(files), _, Some(outname)), ce) =>
        import ce._
        val tmp = TempFile.createTempFile("catted")
        mybiotools.cat(files.map(_.file), tmp)
        CatFilesOutput(SharedFile(tmp, name = outname, canMoveAway = true))
    }
}
