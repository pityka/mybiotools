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

object AddChrToBamHelper {

  def isChr(i: String) = if (i == "1" || i == "2" || i == "3" || i == "4" || i == "5" || i == "6" || i == "7" ||
    i == "8" || i == "9" || i == "10" || i == "11" || i == "12" || i == "13" || i == "14" || i == "15" ||
    i == "16" || i == "17" || i == "18" || i == "19" || i == "20" || i == "21" || i == "22" || i == "X" || i == "Y" || i == "XY" || i == "M") true else false

  def hasChr(file: File): Boolean = {
    import htsjdk.samtools.{ SAMFileReader, SAMFileWriterFactory, SAMSequenceRecord, SAMSequenceDictionary }
    import scala.collection.JavaConversions._

    val reader = new SAMFileReader(file)
    reader.setValidationStringency(htsjdk.samtools.ValidationStringency.SILENT)
    val b = reader.getFileHeader.getSequenceDictionary.getSequences.forall { sq =>
      val name = sq.getSequenceName
      !isChr(name) || name.startsWith("chr")
    }
    reader.close
    b

  }
}

object addchrtobam {
  def apply(
    in: SortBamInput,
    memory: Int,
    update: UpdatePrerequisitive[SortBamInput] = SortBamInput.updateSortBamInput orElse identity[SortBamInput]
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = memory)
    ) {

      case (
        SortBamInput(
          Some(bamfile)), ce) =>
        import ce._

        def replace(oldname: String) = {
          if (oldname startsWith "chr") oldname
          else {
            if (oldname.toLowerCase startsWith "chr") "chr" + oldname.drop(3)
            else {
              if (AddChrToBamHelper.isChr(oldname)) "chr" + oldname
              else oldname
            }
          }
        }

        import htsjdk.samtools.{ SAMFileReader, SAMFileWriterFactory, SAMSequenceRecord, SAMSequenceDictionary }
        import scala.collection.JavaConversions._
        val reader = new SAMFileReader(bamfile.file)
        reader.setValidationStringency(htsjdk.samtools.ValidationStringency.SILENT)
        val oldheader = reader.getFileHeader
        val sqs = oldheader.getSequenceDictionary.getSequences.map { sq =>
          new SAMSequenceRecord(replace(sq.getSequenceName), sq.getSequenceLength)
        }
        val newheader = oldheader.clone
        newheader.setSequenceDictionary(new SAMSequenceDictionary(sqs))

        val (tmpout, _) = Reheader.reheader(bamfile.file, newheader)

        reader.close

        BamFile(SharedFile(tmpout, name = bamfile.name + ".addchr.bam", canMoveAway = true))
    }

}

