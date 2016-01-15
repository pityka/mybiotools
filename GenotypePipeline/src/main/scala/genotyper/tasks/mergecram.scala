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

package genotyper.tasks

import mybiotools.tasks._
import java.io.File
import mybiotools._
import mybiotools.gwascommons._
import genotyper.Helpers
import mybiotools.workflows._

case class MergeCramInput(
    crams: Option[Set[SharedFile]],
    expected: Option[Int],
    reference: Option[SharedFile],
    outname: Option[String]
) extends Prerequisitive[MergeCramInput] {

  def ready = {
    reference.isDefined &&
      crams.isDefined &&
      expected.isDefined &&
      expected.get == crams.get.size
  }

}

object MergeCramInput {

  def apply(crams: List[File], ref: File, outname: String)(implicit components: TaskSystemComponents): MergeCramInput = {

    MergeCramInput(
      crams = Some(crams.toSet.map((f: File) => SharedFile(f, name = f.getName))),
      reference = Some(SharedFile(ref)),
      expected = Some(crams.size),
      outname = Some(outname)
    )

  }

}

object mergecram {
  def apply(
    in: MergeCramInput,
    update: UpdatePrerequisitive[MergeCramInput] = identity[MergeCramInput]
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = 3000)
    ) {

      case (
        MergeCramInput(
          Some(crams),
          _,
          Some(reference),
          Some(outname)), ce) =>
        import ce._

        val tmp = TempFile.createTempFile(".bam")
        Helpers.mergeCram2Bam(crams.toList.map(_.file), reference.file, tmp)
        BamFile(SharedFile(tmp, outname, canMoveAway = true))

    }

}

