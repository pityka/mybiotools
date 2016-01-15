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

case class TfamFile(file: SharedFile) extends ResultWithSharedFiles(file)
case class TpedFile(file: SharedFile) extends ResultWithSharedFiles(file)

case class TPedFileSet(tfam: TfamFile, tped: TpedFile, missingValue: Char) extends ResultWithSharedFiles((tfam.files ++ tped.files): _*)

case class MakeBedOut(bed: BedFile, bim: BimFile, fam: FamFile, log: SharedFile) extends ResultWithSharedFiles(log +: (bed.files ++ bim.files ++ fam.files): _*)

case class MakeBedInput(
  tped: Option[TpedFile],
  tfam: Option[TfamFile]
) extends SimplePrerequisitive[MakeBedInput]

object MakeBedInput {
  def empty = MakeBedInput(None, None)
  def apply(s: String)(implicit components: TaskSystemComponents): MakeBedInput = apply(s + ".tped", s + ".tfam")
  def apply(tped: String, tfam: String)(implicit components: TaskSystemComponents): MakeBedInput = {

    MakeBedInput(
      tped = Some(TpedFile(SharedFile(new File(tped)))),
      tfam = Some(TfamFile(SharedFile(new File(tfam))))

    )
  }

  def updateMakeBedInputFromPlinkFiles: UpdatePrerequisitive[MakeBedInput] = {
    case (self, i: TfamFile) => self.copy(tfam = Some(i))
    case (self, i: TpedFile) => self.copy(tped = Some(i))
  }
}

object makebed {
  def apply(
    in: MakeBedInput,
    update: UpdatePrerequisitive[MakeBedInput] = MakeBedInput.updateMakeBedInputFromPlinkFiles
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update
    ) {
      case (MakeBedInput(Some(tped), Some(tfam)), ce) =>
        import ce._

        val wd = TempFile.createTempFile("makebed").getParent + "/" + tped.file.name

        val (stdout, stderr, succ) = mybiotools.execGetStreamsAndCode(s"plink --tfam ${tfam.file.file} --tped ${tped.file.file} --make-bed --noweb --out $wd")

        if (!succ) throw new RuntimeException("error in plink" + stdout.mkString("\n") + stderr.mkString("\n"))

        val bim = BimFile(SharedFile(new File(wd + ".bim"), name = tped.file.name))
        val bed = BedFile(SharedFile(new File(wd + ".bed"), name = tped.file.name))
        val fam = FamFile(SharedFile(new File(wd + ".fam"), name = tped.file.name))
        val log = SharedFile(new File(wd + ".log"))

        MakeBedOut(bed, bim, fam, log)
    }

}

