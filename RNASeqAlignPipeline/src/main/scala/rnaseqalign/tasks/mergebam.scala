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

package rnaseqalign.tasks

import mybiotools.tasks._
import java.io.File
import mybiotools.workflows._
import mybiotools._

case class MergeBamInput(
    bams: Option[Set[SharedFile]],
    expectedBams: Option[Int],
    outname: Option[String]
) extends Prerequisitive[MergeBamInput] {
  def ready = bams.isDefined && expectedBams.isDefined && outname.isDefined && bams.get.size == expectedBams.get
}

object MergeBamInput {

  def apply(expected: Int, outname: String): MergeBamInput = {

    MergeBamInput(
      expectedBams = Some(expected),
      outname = Some(outname),
      bams = None

    )

  }

  private def updateBamList(input: MergeBamInput, bam: SharedFile): MergeBamInput = {
    val updated = input.bams match {
      case None => Some(Set(bam))
      case Some(xs) => Some(xs + bam)
    }
    input.copy(bams = updated)

  }

  def updateMergeBamInput: UpdatePrerequisitive[MergeBamInput] = {
    case (self, i: AlignOut) => updateBamList(self, i.bam)
  }

}

object mergeUnsortedBams {
  def apply(
    in: MergeBamInput,
    memory: Int,
    update: UpdatePrerequisitive[MergeBamInput] = MergeBamInput.updateMergeBamInput orElse identity[MergeBamInput]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = memory)
    ) {

      case (
        MergeBamInput(
          Some(bams),
          _,
          Some(outname)), ce) =>
        import ce._

        {

          val out = TempFile.createTempFile(".bam")

          rnaseqalign.Helpers.mergeUnsortedBams(bams.map(_.file).toList, out)

          BamFile(
            SharedFile(out, name = outname + ".merged.bam", canMoveAway = true)
          )
        }

    }

}

