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

// package genotyper.tasks

// import mybiotools.tasks._
// import java.io.File
// import mybiotools._
// import mybiotools.gwascommons._
// import rnaseqalign.ReadGroupContents
// import genotyper.Helpers
// import net.sf.samtools.util.Iso8601Date;

// case class AddReadGroupInput(
//     bam: Option[SharedFile],
//     readgroup: Option[ReadGroupContents]) extends Prerequisitive[AddReadGroupInput] {

//   def ready = {
//     bam.isDefined &&
//       readgroup.isDefined
//   }

// }

// object AddReadGroupInput {

//   def apply(rg: ReadGroupContents)(implicit components: TaskSystemComponents): AddReadGroupInput = {
//     implicit val fs = components.fs
//     implicit val context = components.actorsystem

//     AddReadGroupInput(
//       readgroup = Some(rg),
//       bam = None
//     )

//   }

//   def updateAddReadGroupInput: UpdatePrerequisitive[AddReadGroupInput] = {
//     case (self, i: BamWithBai) => self.copy(bam = Some(i.bam))
//     case (self, i: BamFile) => self.copy(bam = Some(i.file))
//   }

// }

// object addReadGroup {
//   def apply(in: AddReadGroupInput,
//     memory: Int,
//     cpu: Int,
//     update: UpdatePrerequisitive[AddReadGroupInput] = AddReadGroupInput.updateAddReadGroupInput orElse identity[AddReadGroupInput])(implicit components: TaskSystemComponents) =
//     newTask(
//       in, update, CPUMemoryRequest(cpu = cpu, memory = memory)) {

//         case (
//           AddReadGroupInput(
//             Some(bam),
//             Some(readgroup)), ce) =>
//           import ce._

//           import net.sf.samtools.{ SAMFileReader, SAMFileWriterFactory, SAMSequenceRecord, SAMSequenceDictionary }
//           import scala.collection.JavaConversions._
//           val reader = new SAMFileReader(bam.file)
//           reader.setValidationStringency(SAMFileReader.ValidationStringency.SILENT)
//           val oldheader = reader.getFileHeader

//           val newheader = oldheader.clone

//           val RGID = readgroup.toString

//           // create the read group we'll be using
//           val rg = new net.sf.samtools.SAMReadGroupRecord(RGID);
//           rg.setLibrary(readgroup.lb);
//           rg.setPlatform(readgroup.pl);
//           rg.setSample(readgroup.sm);
//           rg.setPlatformUnit(readgroup.pu);
//           rg.setSequencingCenter(readgroup.cn);
//           rg.setDescription(readgroup.ds);

//           newheader.setReadGroups(List(rg));

//           val tmpout = genotyper.Helpers.reheader(bam.file, newheader)

//           reader.close

//           BamFile(SharedFile(tmpout, name = bam.name + ".readgroup.bam", canMoveAway = true))

//       }

// }

