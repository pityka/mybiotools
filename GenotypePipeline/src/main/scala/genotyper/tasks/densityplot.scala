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
// import genotyper.Helpers
// import genotyper._
// import collection.JavaConversions._
// import htsjdk.tribble.index.IndexFactory
// import htsjdk.tribble.index.IndexFactory.IndexBalanceApproach._
// import vcfhelpers._
// import mybiotools.stringstore._

// case class DensityPlotInput(
//   vcf: Option[SharedFile],
//   vcfidx: Option[SharedFile],
//   interval: Option[Set[Region]],
//   windowSize: Option[Int],
//   stepSize : Option[Int]
// ) extends SimplePrerequisitive[DensityPlotInput]

// object DensityPlotInput {

//   def apply(interval: Set[Region],windowSize:Int,stepSize:Int): DensityPlotInput = {

//     DensityPlotInput(
//       vcf = None,
//       vcfidx = None,
//       interval = Some(interval),
//       windowSize = Some(windowSize),
//       stepSize = Some(stepSize)
//     )

//   }  

//   def updateDensityPlotInput: UpdatePrerequisitive[DensityPlotInput] = {
//     // case (self, i: VCFFile) => self.copy(vcf = Some(i.file))
//     case (self, i: HaplotypeCallerOutput) => self.copy(vcf = Some(i.vcf), vcfidx = Some(i.vcfidx))
//     // case (self, i: VQSROutput) => self.copy(vcf = Some(i.vcf))
//     // case (self, i: CombineVCFOutput) => self.copy(vcf = Some(i.vcf))
//     // case (self, i: DbsnpIDAnnotationOutput) => self.copy(vcf = Some(i.vcf))
//     case (self, i: VCFWithIndex) => self.copy(vcf = Some(i.vcf), vcfidx = Some(i.vcfidx))
//   }

// }

// object DensityPlot {
//   def apply(
//     in: DensityPlotInput,
//     memory: Int,
//     update: UpdatePrerequisitive[DensityPlotInput] = DensityPlotInput.updateDensityPlotInput orElse identity[DensityPlotInput]
//   )(implicit ce: TaskSystemComponents) =
//     newTask(
//       in, update, CPUMemoryRequest(cpu = 1, memory = memory)
//     ) {

//       case (
//         DensityPlotInput(
//           Some(vcf),
//           Some(vcfidx),
//           Some(interval),
//           Some(windowSize),
//           Some(stepSize)), ce) =>
//         import ce._

//         val localtmpvcf = TempFile.createTempFile(".vcf")
//         val localtmpvcfidx = new File(localtmpvcf.getAbsolutePath + ".idx")
//         com.google.common.io.Files.copy(vcf.file, localtmpvcf)
//         com.google.common.io.Files.copy(vcfidx.file, localtmpvcfidx)

//         val resultvcf = TempFile.createTempFile("subset.vcf")

//         val index = IndexFactory.loadIndex(localtmpvcfidx.getAbsolutePath());

//         val reader = htsjdk.tribble.AbstractFeatureReader.getFeatureReader(
//           localtmpvcf.getAbsolutePath,
//           new QuickTribbleVCFCodec,
//           index
//         )

//         val header = reader.getHeader

//         implicit val order = regionOrderingByStart[String8, GenomicLocation, Region]
//         val set = interval.toList.sorted.flatMap { iv =>
//           iterableAsScalaIterable(reader.query(iv.chromosome, iv.from + 1, iv.to))
//         }.sortBy(vc => GenomicLocation(vc.getStart, vc.getChr))
//         reader.close

//         openFileWriter(resultvcf) { writer =>

//           QuickTribbleFeature.write(
//             header.asInstanceOf[QuickVCFHeader],
//             set,
//             writer
//           )

//           writer.close

//         }

//         val idxfile = new File(resultvcf.getAbsolutePath + ".idx")
//         val idx = IndexFactory.createDynamicIndex(resultvcf, new htsjdk.variant.vcf.VCFCodec, FOR_SEEK_TIME)
//         IndexFactory.writeIndex(idx, idxfile)

//         localtmpvcf.delete
//         localtmpvcfidx.delete

//         VCFWithIndex(
//           vcf = SharedFile(resultvcf, name = vcf.name + ".subset.vcf", canMoveAway = true),
//           vcfidx = SharedFile(idxfile, name = vcf.name + ".subset.vcf.idx", canMoveAway = true)
//         )

//     }

// }

