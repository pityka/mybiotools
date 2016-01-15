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

import mybiotools._
import mybiotools.tasks._
import java.io.File
import akka.actor.{ ActorRefFactory }
import scala.util.Try
import mybiotools.stat.ZMetaCombination._
import mybiotools.gwascommons.Region

case class JoinHumanGenesOutput(tests: SharedFile) extends ResultWithSharedFiles(tests)

case class JoinHumanGenesInput(
  file: Option[SharedFile],
  bed: Option[SharedFile]
) extends SimplePrerequisitive[JoinHumanGenesInput]

object JoinHumanGenesInput {

  def apply(bed: File)(implicit components: TaskSystemComponents): JoinHumanGenesInput = {

    JoinHumanGenesInput(file = None, bed = Some(SharedFile(bed, name = bed.getName)))
  }

  def updateInput: UpdatePrerequisitive[JoinHumanGenesInput] = {
    case (self, i: TracksOutput) => self.copy(file = Some(i.tests))
  }
}

object joinHumanGenes {

  /** 4th field is gene name */
  private def readBedWithUniqueNames(lines: Iterator[String]): Map[String, Region] = lines.map { line =>
    val splitted = fastSplitSeparator(line, '\t')
    val chr = splitted(0)
    val start = splitted(1).toInt
    val end = splitted(2).toInt
    val name = if (splitted(3) != ".") new String(splitted(3)) else s"$chr:$start-$end"
    val region = Region(chr, start, end)
    name -> region
  }.toMap

  def apply(
    in: JoinHumanGenesInput,
    update: UpdatePrerequisitive[JoinHumanGenesInput] = JoinHumanGenesInput.updateInput orElse identity[JoinHumanGenesInput]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = 3000)
    ) {
      case (JoinHumanGenesInput(Some(file), Some(bed)), ce) =>
        import ce._
        val tmp = TempFile.createTempFile("joined")

        val genes: Map[String, Region] = NodeLocalCache.getItemBlocking("humangenes") {

          using(io.Source.fromFile(bed.file)) { source => readBedWithUniqueNames(source.getLines) }

        }

        val intervalregex = """([a-z0-9]+):(\d+)-(\d+)""".r

        openFileWriter(tmp) { fw =>
          openSource(file.file.getAbsolutePath)(_.getLines.foreach { line =>
            val spl = fastSplitSetSeparator(line, Set(' ', '\t'))
            val genename = spl.head
            val region = genes.get(genename)
            val regionstring = region.map(x => x.chromosome + " " + (x.from + 1) + " " + (x.to)).getOrElse {
              genename match {
                case intervalregex(chr, start, stop) => chr + " " + (start + 1) + " " + stop
                case _ => ""
              }
            }
            fw.write(line + " " + regionstring)
            fw.write('\n')
          })
        }

        JoinHumanGenesOutput(SharedFile(tmp, name = file.name + ".withGenes", canMoveAway = true))
    }
}
