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

package gwasapp.tasks

import mybiotools._
import mybiotools.gwascommons._
import mybiotools.gwascommons.associationresults._
import gwascommons.genotypedata._
import gwascommons.gwas._
import gwascommons.gwas.GWAS._
import mybiotools.config.Config.configInstance
import mybiotools.stat.LinearRegression.readPlinkCovarFile
import collection.JavaConversions._
import java.io.File
import _root_.ch.systemsx.cisd.hdf5._
import hdfdosage.HDFDosageIterator
import mybiotools.stringstore._
import scala.concurrent.Future
import akka.actor.{ ActorRef, Actor, ActorRefFactory, Props }
import akka.actor.Actor._
import mybiotools.tasks._
import java.io.File
import scala.concurrent.duration._
import mybiotools.eq._
import mybiotools.iterator._
import mybiotools.sharedfiletypes._
import mybiotools.sharedfiletypes.SharedFileSets._

case class CopyToPlinkInput(
  assocFile: Option[Set[SharedFile]],
  expectedFiles: Option[Int],
  interactionheaderincluded: Option[Boolean],
  outname: Option[String]
)
    extends Prerequisitive[CopyToPlinkInput] {
  def ready = expectedFiles.isDefined &&
    outname.isDefined &&
    assocFile.isDefined &&
    interactionheaderincluded.isDefined &&
    assocFile.get.size === expectedFiles.get
}
object CopyToPlinkInput {

  def apply(expectedFiles: Int, outname: String, interactionheaderincluded: Boolean, assoc: Seq[SharedAssocFile], writeCovariates: Boolean): CopyToPlinkInput = {

    CopyToPlinkInput(
      assocFile = Some((if (!writeCovariates) assoc.map(_.snponly).toSet else assoc.map(_.alllines).toSet)), outname = Some(outname), expectedFiles = Some(expectedFiles), interactionheaderincluded = Some(interactionheaderincluded)
    )
  }

  def update: UpdatePrerequisitive[CopyToPlinkInput] = {
    case (self, m: SharedAssocFile) => self.copy(assocFile = Some(self.assocFile.map(xs => xs + m.snponly).getOrElse(Set(m.snponly))))
  }
}
object copyToPlink {

  def apply(
    in: CopyToPlinkInput,
    update: UpdatePrerequisitive[CopyToPlinkInput] = CopyToPlinkInput.update orElse identity[CopyToPlinkInput]
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = 1000)
    ) {
      case (CopyToPlinkInput(Some(assocFile), _, Some(interactionheaderincluded), Some(outname)), ce) =>
        import ce._

        val interactionheader = if (interactionheaderincluded) " INTER_MARKER INTER_MODEL" else ""

        val headerstr = "CHR SNP BP A1 TEST NMISS BETA STAT P PHENO" + interactionheader + " SE"
        val tmp = TempFile.createTempFile(".plink.assoc")
        openZippedFileWriter(tmp) { writer =>
          writer.write(headerstr)
          writer.write('\n')
          assocFile.foreach { file =>
            openZippedFileInputStream(file.file) { is =>
              associationresults.FullAssociationResult.fromInputStream(is).foreach { line =>
                writer.write(line.toLine)
                writer.write('\n')

              }
            }
          }
        }
        PlinkAssocFile(SharedFile(tmp, name = outname, canMoveAway = true))

    }

}

