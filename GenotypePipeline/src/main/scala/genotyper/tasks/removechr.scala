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

object removechrfrombam {
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
          if (oldname.toLowerCase startsWith "chr") oldname.drop(3)
          else oldname
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

        BamFile(SharedFile(tmpout, name = bamfile.name + ".removechr.bam", canMoveAway = true))
    }

}

