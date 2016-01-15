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
import mybiotools.sharedfiletypes._
import mybiotools.sharedfiletypes.SharedFileSets._

case class ExtractSignificantSNPsOutput(lines: SharedFile, snps: SharedFile) extends ResultWithSharedFiles(lines, snps)

case class ExtractSignificantSNPsInput(
  plink: Option[SharedFile],
  pThreshold: Option[Double]
) extends SimplePrerequisitive[ExtractSignificantSNPsInput]

object ExtractSignificantSNPsInput {

  def apply(pThreshold: Double): ExtractSignificantSNPsInput = {

    ExtractSignificantSNPsInput(
      plink = None,
      pThreshold = Some(pThreshold)
    )
  }

  def updateExtractSignificantSNPsInput: UpdatePrerequisitive[ExtractSignificantSNPsInput] = {
    case (self, i: PlinkAssocFile) => self.copy(plink = Some(i.file))
  }
}

object extractSignificantSnps {

  def apply(
    in: ExtractSignificantSNPsInput,
    update: UpdatePrerequisitive[ExtractSignificantSNPsInput] = ExtractSignificantSNPsInput.updateExtractSignificantSNPsInput orElse identity[ExtractSignificantSNPsInput]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = 1000)
    ) {
      case (ExtractSignificantSNPsInput(Some(plink), Some(pThreshold)), ce) =>
        import ce._
        val tmplines = TempFile.createTempFile("lines")
        val tmpsnp = TempFile.createTempFile("snps")

        openFileWriter(tmplines) { linewriter =>
          openFileWriter(tmpsnp) { snpwriter =>
            openSource(plink.file.getAbsolutePath) { source =>
              val lines = source.getLines
              val header = fastSplitSetSeparator(lines.next, Set(' ', '\t'))
              val snpidx = header.indexOf("SNP")
              val pidx = header.indexOf("P")
              lines.map { line =>
                val spl = fastSplitSetSeparator(line, Set(' ', '\t'))
                val snp = spl(snpidx)
                val p = scala.util.Try(spl(pidx).toDouble).toOption.getOrElse(Double.NaN)
                (p, snp, line)
              }.filter(_._1 <= pThreshold).foreach {
                case (_, snp, line) =>
                  linewriter.write(line + "\n")
                  snpwriter.write(snp + "\n")
              }

            }
          }

        }
        ExtractSignificantSNPsOutput(
          lines = SharedFile(tmplines, name = plink.name + ".sign." + pThreshold + ".lines", canMoveAway = true),
          snps = SharedFile(tmpsnp, name = plink.name + ".sign." + pThreshold + ".snps", canMoveAway = true)
        )
    }
}
