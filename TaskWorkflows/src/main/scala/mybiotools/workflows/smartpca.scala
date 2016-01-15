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
import mybiotools.pcanew._
import mybiotools.tasks._
import java.io.File
import akka.actor.{ ActorRefFactory }

case class BimFile(file: SharedFile) extends ResultWithSharedFiles(file)
case class FamFile(file: SharedFile) extends ResultWithSharedFiles(file)
case class BedFile(file: SharedFile) extends ResultWithSharedFiles(file)

case class SmartPCAOut(evec: SharedFile, eval: SharedFile, plot12: SharedFile, plot13: SharedFile, plot23: SharedFile, par: SharedFile) extends ResultWithSharedFiles(evec, eval, plot12, plot13, plot23, par)

case class SmartPCAInput(
  bim: Option[BimFile],
  bed: Option[BedFile],
  fam: Option[FamFile]
) extends SimplePrerequisitive[SmartPCAInput]

object SmartPCAInput {
  def empty = SmartPCAInput(None, None, None)
  def apply(s: String)(implicit components: TaskSystemComponents): SmartPCAInput = {

    SmartPCAInput(
      bim = Some(BimFile(SharedFile(new File(s + ".bim")))),
      bed = Some(BedFile(SharedFile(new File(s + ".bed")))),
      fam = Some(FamFile(SharedFile(new File(s + ".fam"))))
    )
  }

  def updateSmartPCAInputFromPlinkFiles: UpdatePrerequisitive[SmartPCAInput] = {
    case (self, i: MakeBedOut) => self.copy(bim = Some(i.bim), bed = Some(i.bed), fam = Some(i.fam))
    case (self, i: BimFile) => self.copy(bim = Some(i))
    case (self, i: FamFile) => self.copy(fam = Some(i))
    case (self, i: BedFile) => self.copy(bed = Some(i))
  }
}

object smartPCATask {

  def apply(
    in: SmartPCAInput,
    update: UpdatePrerequisitive[SmartPCAInput] = SmartPCAInput.updateSmartPCAInputFromPlinkFiles
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update
    ) {
      case (SmartPCAInput(Some(bim), Some(bed), Some(fam)), ce) =>
        import ce._

        {

          val wd = {
            val folder = TempFile.createTempFile("smartpca").getParent
            val x = new File(folder, bim.file.name + "smartpca")
            x.mkdir
            x.getCanonicalPath + "/"
          }

          gwascommons.runSmartPCA(bed.file.file.getCanonicalPath, bim.file.file.getCanonicalPath, fam.file.file.getCanonicalPath, wd + "/evecout.txt", wd)

          plotPCAResultToFile(PCAResult.fromSmartPCA(io.Source.fromFile(wd + "/evecout.txt")), 1, 2, new File(wd + "/pca12.png"))
          plotPCAResultToFile(PCAResult.fromSmartPCA(io.Source.fromFile(wd + "/evecout.txt")), 2, 3, new File(wd + "/pca23.png"))
          plotPCAResultToFile(PCAResult.fromSmartPCA(io.Source.fromFile(wd + "/evecout.txt")), 1, 3, new File(wd + "/pca13.png"))

          val eval = SharedFile(new File(wd + "/eval.txt"), name = bed.file.name + ".eval.txt")
          val evecout = SharedFile(new File(wd + "/evecout.txt"), name = bed.file.name + ".evecout.txt")
          val pca12 = SharedFile(new File(wd + "/pca12.png"), name = bed.file.name + ".pca12.png")
          val pca23 = SharedFile(new File(wd + "/pca23.png"), name = bed.file.name + ".pca23.png")
          val pca13 = SharedFile(new File(wd + "/pca13.png"), name = bed.file.name + ".pca13.png")
          val par = SharedFile(new File(wd + "/par.txt"), name = bed.file.name + ".par.txt")
          SmartPCAOut(evec = evecout, eval = eval, pca12, pca13, pca23, par)
        }
    }

}

