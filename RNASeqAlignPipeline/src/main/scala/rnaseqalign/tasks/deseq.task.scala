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

// import scala.sys.process._
// import scala.concurrent._
// import scala.concurrent.duration._
// import java.util.concurrent.Executors
// import rnaseqalign.Helpers
// import scala.util.{ Try, Success, Failure }
// import rnaseqalign.{ ReadGroupContents, FlowCellInfo }
// import mybiotools.saddlehelpers._
// import rnaseqalign._
// import rnaseqalign.analysis._
// import org.saddle._

// case class DESeqTaskInput(
//   counts: Option[FrameInSerializableEnvelope[String, String, Long]],
//   conditions: Option[Map[String, String]],
//   subjects: Option[Map[String, String]],
//   spikeInSizeFactors: Option[SeriesInSerializableEnvelope[String, Double]],
//   pairwiseDifferentialExpressionConditions: Option[List[(String, String)]],
//   hardRawCountThreshold: Option[Int],
//   thresholdDropIfBelowInAllSamplesPerBp: Option[Double],
//   gmts: Option[Seq[GeneSetDB]]) extends SimplePrerequisitive[DESeqTaskInput]

// object DESeqTaskInput {
//   def apply(counts: Frame[String, String, Long],
//     conditions: Map[String, String],
//     subjects: Map[String, String],
//     spikeInSizeFactors: Series[String, Double],
//     pairwiseDifferentialExpressionConditions: List[(String, String)],
//     hardRawCountThreshold: Int,
//     thresholdDropIfBelowInAllSamplesPerBp: Double,
//     gmts: Seq[GeneSetDB]): DESeqTaskInput =
//     DESeqTaskInput(
//       Some(frameToEnvelope(counts)),
//       Some(conditions),
//       Some(subjects),
//       Some(seriesToEnvelope(spikeInSizeFactors)),
//       Some(pairwiseDifferentialExpressionConditions),
//       Some(hardRawCountThreshold),
//       Some(thresholdDropIfBelowInAllSamplesPerBp),
//       Some(gmts))
// }

// case class DEResultSetShared(
//   fulldes: Map[String, DifferentialExpressionResultAsSharedFile],
//   pairwisedes: Map[String, List[PairWiseDEShared]]) extends ResultWithSharedFiles(fulldes.map(_._2.file).toSeq ++ pairwisedes.flatMap(_._2.map(_.des.file)): _*)

// object deseqtask {
//   def apply(in: DESeqTaskInput,
//     memory: Int,
//     update: UpdatePrerequisitive[DESeqTaskInput] = identity[DESeqTaskInput])(implicit components: TaskSystemComponents) =
//     newTask(
//       in, update, CPUMemoryRequest(cpu = 1, memory = memory)) {

//         case (
//           DESeqTaskInput(
//             Some(counts),
//             Some(conditions),
//             Some(subjects),
//             Some(spikeInSizeFactors),
//             Some(pairwiseDifferentialExpressionConditions),
//             Some(hardRawCountThreshold),
//             Some(thresholdDropIfBelowInAllSamplesPerBp),
//             Some(gmts)
//             ), ce) =>
//           import ce._

//           val ders = DEResultSet.fromCounts(envelopeToFrame(counts),
//             conditions,
//             subjects,
//             envelopeToSeries(spikeInSizeFactors),
//             pairwiseDifferentialExpressionConditions,
//             hardRawCountThreshold,
//             thresholdDropIfBelowInAllSamplesPerBp,
//             gmts)

//           DEResultSetShared(
//             ders.fulldes.map(x => x._1 -> x._2.toShared(s"deseq.${x._1}.csv")),
//             ders.pairwisedes.map(x => x._1 -> x._2.map(_.toShared(x._1))))

//       }

// }

