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

package mybiotools.sharedfiletypes

import mybiotools.tasks._
import hdfdosage.FileSets
import hdfdosage.FileSets._
import akka.actor._

case class PlinkAssocFile(file: SharedFile) extends ResultWithSharedFiles(file)

case class MaybeBIMFile(file: Option[SharedFile]) extends ResultWithSharedFiles(file.toList: _*)

object SharedFileSets {

  def fromFileSet(file: GenotypeFileSet)(implicit fs: FileServiceActor, context: ActorRefFactory, prefix: FileServicePrefix): SharedFileSet = {

    file match {
      case FileSets.PDose(f, missing, fam) => SharedPDose(SharedFile(f), fam.map(f => SharedFile(f)), missing)
      case FileSets.PGenotypeProbabilities(f, missing, fam) => SharedPGeno(SharedFile(f), fam.map(f => SharedFile(f)), missing)
      case FileSets.BGZippedPDose(f, missing, fam, index) => SharedBlockCompressedPDose(SharedFile(f), fam.map(f => SharedFile(f)), missing, SharedFile(index))
      case FileSets.BGZippedPGenotypeProbabilities(f, missing, fam, index) => SharedBlockCompressedPGenotypeProbabilities(SharedFile(f), fam.map(f => SharedFile(f)), missing, SharedFile(index))
      case FileSets.HDFDosage(f) => SharedHDFDosage(SharedFile(f))
      case FileSets.VCFFile(f) => SharedVCF(SharedFile(f))
      case FileSets.VCFFileWithIndex(f, i) => SharedVCFWithIndex(SharedFile(f), SharedFile(i))
      case FileSets.TPed(fam, tped, missing) => SharedTped(SharedFile(fam), SharedFile(tped), missing)
      case FileSets.BedFile(bed, bim, fam) => SharedBedSet(SharedFile(bed), SharedFile(bim), SharedFile(fam))
    }
  }

  trait SharedFileSet extends ResultWithSharedFiles {
    def toFileSet(implicit fs: FileServiceActor, context: ActorRefFactory): GenotypeFileSet
  }

  case class SharedPDose(f: SharedFile, fam: Option[SharedFile], missing: Float) extends ResultWithSharedFiles(f :: fam.toList: _*) with SharedFileSet {
    def toFileSet(implicit fs: FileServiceActor, context: ActorRefFactory) = FileSets.PDose(f.file, missing, fam.map(_.file))
  }
  case class SharedBlockCompressedPDose(f: SharedFile, fam: Option[SharedFile], missing: Float, index: SharedFile) extends ResultWithSharedFiles(f :: index :: fam.toList: _*) with SharedFileSet {
    def toFileSet(implicit fs: FileServiceActor, context: ActorRefFactory) = FileSets.BGZippedPDose(f.file, missing, fam.map(_.file), index.file)
  }
  case class SharedBlockCompressedPGenotypeProbabilities(f: SharedFile, fam: Option[SharedFile], missing: Float, index: SharedFile) extends ResultWithSharedFiles(f :: index :: fam.toList: _*) with SharedFileSet {
    def toFileSet(implicit fs: FileServiceActor, context: ActorRefFactory) = FileSets.BGZippedPGenotypeProbabilities(f.file, missing, fam.map(_.file), index.file)
  }

  case class SharedPGeno(f: SharedFile, fam: Option[SharedFile], missing: Float) extends ResultWithSharedFiles(f :: fam.toList: _*) with SharedFileSet {
    def toFileSet(implicit fs: FileServiceActor, context: ActorRefFactory) = FileSets.PGenotypeProbabilities(f.file, missing, fam.map(_.file))
  }

  case class SharedHDFDosage(f: SharedFile) extends ResultWithSharedFiles(f) with SharedFileSet {
    def toFileSet(implicit fs: FileServiceActor, context: ActorRefFactory) = FileSets.HDFDosage(f.file)
  }

  case class SharedVCF(f: SharedFile) extends ResultWithSharedFiles(f) with SharedFileSet {
    def toFileSet(implicit fs: FileServiceActor, context: ActorRefFactory) = FileSets.VCFFile(f.file)
  }

  case class SharedVCFWithIndex(f: SharedFile, index: SharedFile) extends ResultWithSharedFiles(f, index) with SharedFileSet {
    def toFileSet(implicit fs: FileServiceActor, context: ActorRefFactory) = FileSets.VCFFileWithIndex(f.file, index.file)
  }

  case class SharedTped(tfam: SharedFile, tped: SharedFile, missing: Char) extends ResultWithSharedFiles(tped, tfam) with SharedFileSet {
    def toFileSet(implicit fs: FileServiceActor, context: ActorRefFactory) = FileSets.TPed(tfam.file, tped.file, missing)
  }

  case class SharedBedSet(bed: SharedFile, bim: SharedFile, fam: SharedFile) extends ResultWithSharedFiles(bed, bim, fam) with SharedFileSet {
    def toFileSet(implicit fs: FileServiceActor, context: ActorRefFactory) = FileSets.BedFile(bed.file, bim.file, fam.file)
  }

}