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
import mybiotools.workflows._

case class ValidateBamOutput(
  success: Boolean,
  report: SharedFile
) extends ResultWithSharedFiles(report)

object BamBaiHelper {
  def updateBamBaiInput: UpdatePrerequisitive[BamBaiInput] = {
    case (self, i: DeduplicateOutput) => self.copy(bam = Some(i.bam))
  }

}

object validatebam {
  def apply(
    in: BamBaiInput,
    memory: Int,
    update: UpdatePrerequisitive[BamBaiInput] = BamBaiInput.updateBamBaiInput orElse BamBaiHelper.updateBamBaiInput orElse identity[BamBaiInput]
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = memory)
    ) {

      case (
        BamBaiInput(
          Some(bamfile),
          Some(indexfile)), ce) =>
        import ce._

        import htsjdk.samtools.SamFileValidator
        import htsjdk.samtools.SAMFileReader

        val tmp = TempFile.createTempFile("validatereport")

        val valid = openFileWriter(tmp) { writer =>
          val printwriter = new java.io.PrintWriter(writer)
          val samfilereader = new SAMFileReader(bamfile.file, indexfile.file)
          samfilereader.setValidationStringency(htsjdk.samtools.ValidationStringency.SILENT)

          samfilereader.enableIndexCaching(true)
          val validator = new SamFileValidator(printwriter, 500)
          validator.setBisulfiteSequenced(false)
          validator.setValidateIndex(true)
          validator.setIgnoreWarnings(true)
          validator.validateSamFileSummary(samfilereader, null)

        }

        ValidateBamOutput(valid, SharedFile(tmp, name = bamfile.name + ".validationreport"))
    }

}

