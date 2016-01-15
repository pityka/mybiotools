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
import mybiotools._
import mybiotools.workflows._
import scala.sys.process._
import scala.concurrent._
import scala.concurrent.duration._
import java.util.concurrent.Executors
import rnaseqalign.Helpers
import scala.util.{ Try, Success, Failure }
import htsjdk.samtools.SAMFileReader
import htsjdk.samtools.{ CigarOperator => SAMCO }
import scala.collection.JavaConversions._
import mybiotools.saddlehelpers._

import rnaseqalign._
import rnaseqalign.htseqcount._
import org.saddle._
import mybiotools.intervaltree._
import mybiotools.gwascommons._

case class ReadCountPlots(
  plots: List[(String, SharedFile)]
) extends ResultWithSharedFiles(plots.map(_._2).toList: _*)

case class ReadCountInput(
  bam: Option[SharedFile],
  bai: Option[SharedFile],
  regions: Option[List[(String, Region)]],
  strandedness: Option[Strandedness]
) extends SimplePrerequisitive[ReadCountInput]

object ReadCountInput {

  def apply(regions: List[(String, Region)], strandedness: Strandedness): ReadCountInput = {
    ReadCountInput(
      None,
      None,
      Some(regions),
      Some(strandedness)
    )
  }

  def updateMergeBamInput: UpdatePrerequisitive[ReadCountInput] = {
    case (self, i: AlignOut) => self.copy(bam = Some(i.bam))
    case (self, i: BamFile) => self.copy(bam = Some(i.file))
    case (self, i: BamIndexFile) => self.copy(bai = Some(i.file))
    case (self, i: BamWithBai) => self.copy(bai = Some(i.bai), bam = Some(i.bam))
  }

}

object readcountplot {
  def apply(
    in: ReadCountInput,
    update: UpdatePrerequisitive[ReadCountInput] = ReadCountInput.updateMergeBamInput orElse identity[ReadCountInput]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = 2000)
    ) {

      case (
        ReadCountInput(
          Some(bam),
          Some(bai),
          Some(regions),
          Some(strandedness)), ce) =>
        import ce._

        ReadCountPlots(regions.map {
          case (name, region) =>

            val plot = rnaseqalign.Helpers.readCountPlotFromBamFile(bam.file, bai.file, strandedness, region)
            val file = TempFile.createTempFile(".pdf")
            writeBinaryToFile(
              file.getAbsolutePath,
              mybiotools.plots.renderToByteArray(plot, "application/pdf", 1)
            )
            name -> SharedFile(file, name = bam.name + s".$name.pdf", canMoveAway = true)
        })

    }
}

