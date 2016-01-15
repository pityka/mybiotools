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

package viralshortread.tasks

import viralshortread._
import mybiotools.tasks._
import java.io.File
import mybiotools._
import mybiotools.gwascommons._

import mybiotools.workflows._

import scala.collection.JavaConversions._
import htsjdk.samtools.util.Iso8601Date
import htsjdk.samtools._

case class PerBaseMetrics(data: List[(viralshortread.ReferencePosition, (Char, scala.collection.immutable.Map[Char, Int], Option[Double]))], deliverables: List[SharedFile]) extends ResultWithSharedFiles(deliverables: _*)
case class NumberOfUniquelyMappedReadsResult(value: Int) extends Result

case class Sam2PerBaseMetricsInput(
  sam: Option[SharedFile],
  sample: Option[String],
  refname: Option[String]
) extends SimplePrerequisitive[Sam2PerBaseMetricsInput]

object Sam2PerBaseMetricsInput {

  def updateSam2PerBaseMetricsInput: UpdatePrerequisitive[Sam2PerBaseMetricsInput] = {
    case (self, i: BamFile) => self.copy(sam = Some(i.file))
  }

}

object sam2PerBaseMetrics {
  def apply(
    in: Sam2PerBaseMetricsInput,
    memory: Int,
    update: UpdatePrerequisitive[Sam2PerBaseMetricsInput] = Sam2PerBaseMetricsInput.updateSam2PerBaseMetricsInput orElse identity[Sam2PerBaseMetricsInput]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = memory)
    ) {
      case (
        Sam2PerBaseMetricsInput(
          Some(sam),
          Some(sample),
          Some(refname)), ce) =>
        import ce._

        val perBaseMetrics = sam2sequence(openAlignedStringIterator(sam.file))(compositeCallFunction(consensusCall(_, 5000), frequencyDistribution, entropy(_, 5000)))

        val cons = perBaseMetrics.map(_._2._1).filterNot(_ == '-').mkString
        val frequencies = perBaseMetrics.map(x => x._1 -> x._2._2).toIndexedSeq
        val entropies = perBaseMetrics.map(x => x._1 -> x._2._3).toIndexedSeq

        val files = SharedFile(writeToTempFile(">" + sample + "|" + refname + "\n" + cons), name = sample + ".consensus.fasta") ::
          SharedFile(writeToTempFile(entropies.map(x => x._1 + "\t" + x._2).mkString("\n")), name = sample + ".entropy.atconsensuspositions.txt") ::
          SharedFile(writeToTempFile(frequencies.map(x => x._1 + "\t" + x._2.map(x => x._1 + ":" + x._2).mkString("\t")).mkString("\n")), name = sample + ".frequencies.atconsensuspositions.txt") ::
          SharedFile(mybiotools.plots.pngToFile(viralshortread.Plots.plotFrequencyDistributions(frequencies.map(_._2))), name = sample + ".frequencies.png") ::
          SharedFile(mybiotools.plots.pngToFile(viralshortread.Plots.plot1DMetric(entropies.flatMap(_._2), false)), name = sample + ".entropy.png") ::
          Nil

        PerBaseMetrics(perBaseMetrics, files)

    }

}

case class NumberOfUniquelyMappedReadsInput(
  sam: Option[SharedFile]
) extends SimplePrerequisitive[NumberOfUniquelyMappedReadsInput]

object NumberOfUniquelyMappedReadsInput {

  def updateNumberOfUniquelyMappedReadsInput: UpdatePrerequisitive[NumberOfUniquelyMappedReadsInput] = {
    case (self, i: BamFile) => self.copy(sam = Some(i.file))
  }

}

object numberOfUniquelyMappedReads {
  def apply(
    in: NumberOfUniquelyMappedReadsInput,
    update: UpdatePrerequisitive[NumberOfUniquelyMappedReadsInput] = NumberOfUniquelyMappedReadsInput.updateNumberOfUniquelyMappedReadsInput orElse identity[NumberOfUniquelyMappedReadsInput]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = 500)
    ) {
      case (
        NumberOfUniquelyMappedReadsInput(
          Some(sam)), ce) =>
        import ce._

        NumberOfUniquelyMappedReadsResult(rnaseqalign.Helpers.openIteratorOnBam(sam.file, false).filter(x => x._1.unmappedFlag == false && x._1.NH.getOrElse(1) == 1 && x._1.mapQ >= 30).size)

    }

}

