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

import mybiotools.tasks._
import java.io.File
import mybiotools._
import htsjdk.samtools.BamIndexValidator
import htsjdk.samtools.{ SAMFileReader, SAMFileWriterFactory, SAMSequenceRecord, SAMSequenceDictionary, BAMIndexer }
import scala.collection.JavaConversions._

case class BamFile(file: SharedFile) extends ResultWithSharedFiles(file)
case class BamIndexFile(file: SharedFile) extends ResultWithSharedFiles(file)

case class IndexBamInput(
  bam: Option[SharedFile]
) extends SimplePrerequisitive[IndexBamInput]

object IndexBamInput {
  def empty = IndexBamInput(None)

  def apply(file: File)(implicit components: TaskSystemComponents): IndexBamInput = {

    IndexBamInput(
      bam = Some(SharedFile(file))
    )

  }

  def updateIndexBam: UpdatePrerequisitive[IndexBamInput] = {
    case (self, i: BamFile) => self.copy(bam = Some(i.file))
  }

}

object indexbam {
  def apply(
    in: IndexBamInput,
    memory: Int,
    update: UpdatePrerequisitive[IndexBamInput] = IndexBamInput.updateIndexBam orElse identity[IndexBamInput]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = memory)
    ) {

      case (
        IndexBamInput(
          Some(bamfile)), ce) =>
        import ce._

        val tmp = TempFile.createTempFile(".bai")

        val reader = new SAMFileReader(bamfile.file)
        reader.enableFileSource(true)
        reader.setValidationStringency(htsjdk.samtools.ValidationStringency.SILENT)
        val header = reader.getFileHeader
        val indexer = new BAMIndexer(tmp, header)
        reader.iterator.foreach { r =>
          indexer.processAlignment(r)
        }
        indexer.finish
        reader.close

        // validate
        val reader2 = new SAMFileReader(bamfile.file, tmp)
        reader2.setValidationStringency(htsjdk.samtools.ValidationStringency.SILENT)
        BamIndexValidator.exhaustivelyTestIndex(reader2)
        reader2.close

        BamIndexFile(SharedFile(tmp, name = bamfile.name + ".bai", canMoveAway = true))
    }

}

