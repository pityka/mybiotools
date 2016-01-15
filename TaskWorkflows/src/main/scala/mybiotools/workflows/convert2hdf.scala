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
import hdfdosage._
import akka.actor.{ ActorRefFactory }
import com.typesafe.config.{ Config, ConfigFactory, ConfigRenderOptions }

import hdfdosage.{ DosageFileFormat, HDFDosage, PDose, PGenotypeProbabilities, BIGSNPMajor, TPed }
import mybiotools.sharedfiletypes._
import mybiotools.sharedfiletypes.SharedFileSets._

case class ConvertToHDFParameters(
  cleanDuplicatedSNPs: Boolean,
  minimumMAF: Double,
  inputIsSortedByGenomicLocation: Boolean
) extends Result

case class ConvertToHDFInput(
  parameters: Option[ConvertToHDFParameters],
  inputGenotypes: Option[SharedFileSet]
) extends SimplePrerequisitive[ConvertToHDFInput]

case class ConvertToHDFOutput(
  hdffile: SharedHDFDosage,
  report: Import.ConvertPDosageReport
) extends ResultWithSharedFiles(hdffile.f)

object convert2hdftask {
  def apply(
    in: ConvertToHDFInput,
    update: UpdatePrerequisitive[ConvertToHDFInput] = identity[ConvertToHDFInput]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = 8000)
    ) {

      case (
        ConvertToHDFInput(
          Some(ConvertToHDFParameters(cleanDuplicatedSNPs, minimumMAF, inputIsSortedByGenomicLocation)),
          Some(inputshared)), ce) =>
        import ce._

        {

          val inputfile = inputshared.toFileSet
          val tmpfile = TempFile.createTempFile(".h5")

          val blockSize = if (inputIsSortedByGenomicLocation) 1000 else 1

          val report = FileSets.openFileSet(inputfile) { iterator =>
            Import.convertSNPMajorFromIterator(
              inputfiles = iterator,
              outputFile = tmpfile,
              cleanDuplicatedSNPs = cleanDuplicatedSNPs,
              blockSize = blockSize,
              minimumMAF = minimumMAF
            )
          }

          ConvertToHDFOutput(SharedHDFDosage(SharedFile(
            tmpfile, name = inputfile.name + ".h5", canMoveAway = true
          )), report)
        }
    }

}

