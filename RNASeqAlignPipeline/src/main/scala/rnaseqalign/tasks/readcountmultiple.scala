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

// package rnaseqalign.tasks

// import mybiotools.tasks._
// import java.io.File
// import mybiotools._
// import mybiotools.workflows._
// import scala.sys.process._
// import scala.concurrent._
// import scala.concurrent.duration._
// import java.util.concurrent.Executors
// import rnaseqalign.Helpers
// import scala.util.{ Try, Success, Failure }
// import rnaseqalign.{ ReadGroupContents, FlowCellInfo }
// import net.sf.samtools.SAMFileReader
// import net.sf.samtools.{ CigarOperator => SAMCO }
// import scala.collection.JavaConversions._
// import mybiotools.saddlehelpers._

// import rnaseqalign._
// import rnaseqalign.htseqcount._
// import org.saddle._
// import mybiotools.intervaltree._
// import mybiotools.gwascommons._

// case class ReadCountMultipleInput(
//     files: Option[Seq[(SharedFile, SharedFile)]],
//     regions: Option[List[(String, GenericRegion)]],
//     strandedness: Option[Strandedness],
//     outname: Option[String],
//     expectedFiles: Option[Int]) extends Prerequisitive[ReadCountMultipleInput] {
//   def ready = files.isDefined && regions.isDefined && strandedness.isDefined && outname.isDefined && expectedFiles.isDefined && expectedFiles.get == files.get.size
// }

// object ReadCountMultipleInput {

//   def apply(regions: List[(String, GenericRegion)], strandedness: Strandedness, outname: String, expectedFiles: Int): ReadCountMultipleInput = {
//     ReadCountMultipleInput(
//       None,
//       Some(regions),
//       Some(strandedness),
//       Some(outname),
//       Some(expectedFiles)
//     )
//   }

//   def updateInput: UpdatePrerequisitive[ReadCountMultipleInput] = {
//     case (self, i: BamWithBai) => self.copy(files = (if (self.files.isEmpty) Some(List((i.bam -> i.bai))) else Some((i.bam, i.bai) +: self.files.get)))
//   }

// }

// object readcountplotmultiple {
//   def apply(in: ReadCountMultipleInput,
//     update: UpdatePrerequisitive[ReadCountMultipleInput] = ReadCountMultipleInput.updateInput orElse identity[ReadCountMultipleInput])(implicit components: TaskSystemComponents) =
//     newTask(
//       in, update, CPUMemoryRequest(cpu = 1, memory = 2000)) {

//         case (
//           ReadCountMultipleInput(
//             Some(files),
//             Some(regions),
//             Some(strandedness),
//             Some(outname),
//             Some(_)), ce) =>
//           import ce._

//           ReadCountPlots(regions.map {
//             case (name, region) =>

//               val plot = rnaseqalign.Helpers.readCountPlotFromMultipleBamFiles(files.map(x => (x._1.file, x._2.file)), strandedness, region)
//               val file = TempFile.createTempFile(".pdf")
//               writeBinaryToFile(file.getAbsolutePath,
//                 mybiotools.plots.renderToByteArray(plot, "application/pdf", 1))
//               name -> SharedFile(file, name = outname + s".$name.pdf", canMoveAway = true)
//           })

//       }
// }

