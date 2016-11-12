// /*
// * The MIT License
// *
// * Copyright (c) 2015 ECOLE POLYTECHNIQUE FEDERALE DE LAUSANNE, Switzerland,
// * Group Fellay
// *
// * Permission is hereby granted, free of charge, to any person obtaining
// * a copy of this software and associated documentation files (the "Software"),
// * to deal in the Software without restriction, including without limitation
// * the rights to use, copy, modify, merge, publish, distribute, sublicense,
// * and/or sell copies of the Software, and to permit persons to whom the Software
// * is furnished to do so, subject to the following conditions:
// *
// * The above copyright notice and this permission notice shall be included in all
// * copies or substantial portions of the Software.
// *
// * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// * SOFTWARE.
// */
//
// package mybiotools.workflows
//
// import mybiotools.tasks._
// import java.io.File
// import mybiotools._
//
// trait BamWithBai extends Result {
//   def bam: SharedFile
//   def bai: SharedFile
// }
//
// case class BamWithBaiImpl(bam: SharedFile, bai: SharedFile) extends ResultWithSharedFiles(bam, bai) with BamWithBai
//
// case class BamBaiInput(
//   bam: Option[SharedFile],
//   index: Option[SharedFile]
// ) extends SimplePrerequisitive[BamBaiInput]
//
// object BamBaiInput {
//   def empty = BamBaiInput(None, None)
//
//   def apply(bam: File, index: File)(implicit components: TaskSystemComponents): BamBaiInput = {
//
//     BamBaiInput(
//       bam = Some(SharedFile(bam)),
//       index = Some(SharedFile(index))
//     )
//
//   }
//
//   def apply(bam: File)(implicit components: TaskSystemComponents): BamBaiInput = {
//
//     BamBaiInput(
//       bam = Some(SharedFile(bam)),
//       index = None
//     )
//
//   }
//
//   def updateBamBaiInput: UpdatePrerequisitive[BamBaiInput] = {
//     case (self, i: BamFile) => self.copy(bam = Some(i.file))
//     case (self, i: BamIndexFile) => self.copy(index = Some(i.file))
//
//   }
//
// }
//
// object bambaiaggregator {
//   def apply(
//     in: BamBaiInput,
//     update: UpdatePrerequisitive[BamBaiInput] = BamBaiInput.updateBamBaiInput orElse identity[BamBaiInput]
//   )(implicit components: TaskSystemComponents) =
//     newTask(
//       in, update, CPUMemoryRequest(cpu = 1, memory = 100)
//     ) {
//
//       case (
//         BamBaiInput(
//           Some(bamfile),
//           Some(indexfile)), components) =>
//         import components._
//
//         BamWithBaiImpl(bam = bamfile, bai = indexfile)
//     }
//
// }
//
