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
import com.typesafe.config.{ Config, ConfigFactory, ConfigRenderOptions }
import mybiotools.gwascommons._
import mybiotools.stringstore._
import mybiotools.sharedfiletypes._
import mybiotools.sharedfiletypes.SharedFileSets._
import mybiotools.gwascommons.genotypedata._
import mybiotools.eq._

case class PreProcessSangerInput(
  input: Option[SharedPDose],
  info: Option[SharedFile],
  gmap: Option[SharedFile]
) extends SimplePrerequisitive[PreProcessSangerInput]

object preprocesssanger {
  def apply(
    in: PreProcessSangerInput,
    update: UpdatePrerequisitive[PreProcessSangerInput] = identity[PreProcessSangerInput]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = 8000)
    ) {

      case (
        PreProcessSangerInput(
          Some(SharedPDose(infile, fam, missing)),
          Some(info),
          Some(gmap)), ce) =>
        import ce._
        val outname = infile.name + ".preproc.gz"

        val genomicmapMemory = NodeLocalCache.getItemBlocking("genomicmap" + Some(gmap.name)) {
          getGenomicMapFromBimFile(gmap.file.getCanonicalPath)
        }

        val highinfoset: Set[String8] =
          NodeLocalCache.getItemBlocking("high_info_set:" + info.name) {
            openSource(info.file)(_.getLines8.drop(1).map { line =>
              val spl = line.fastSplit(Set(' ', '\t', ','))
              val rsid = StringStore(spl(1))
              val info = spl(4)
              (rsid, info.value.toDouble)
            }.filter(_._2 > 0.8).map(_._1).toSet)
          }

        val firstLine = if (fam.isEmpty) Some(openSource(infile.file.getAbsolutePath)(_.getLines.next)) else None

        var counter = 0L
        val bgzfunsorted = TempFile.createTempFile(".preproc.gz")
        openBlockedZippedFileWriter(bgzfunsorted) { writer =>
          openSource(infile.file.getAbsolutePath)(_.getLines.zipWithIndex.foreach {
            case (line, lineIdx) =>
              val spl = fastSplit1WideSeparatorIterator(line, ',')
              val id: String8 = StringStore(spl.next)
              val a1 = spl.next
              val a2 = spl.next

              if (highinfoset.contains(id)) {
                if (id === s8".") {
                  // writer.write(prefix + "_" + lineIdx + "," + spl.drop(1).mkString(","))
                  // writer.write("\n")
                  counter += 1
                } else {
                  writer.write(line)
                  writer.write("\n")
                }
              }

          })
        }
        val indexunsorted = BGZippedGenotypeHelper.makeIndex(bgzfunsorted)

        val sorted: Seq[String8] =
          indexunsorted.keySet
            .toSeq
            .map { x =>
              (x,
                (genomicmapMemory.get(x) match {
                  case Some(x) => Some(x)
                  case None => scala.util.Try(GenomicLocation(x)).toOption
                }))
            }
            .filter(_._2.isDefined)
            .map(x => x._1 -> x._2.get)
            .sortBy(_._2)
            .map(x => x._1)

        val bgzfsorted = TempFile.createTempFile(".preproc.gz")
        openBlockedZippedFileWriter(bgzfsorted) { writer =>

          firstLine.foreach(x => writer.write(x + "\n"))

          openBlockedZippedFileInputStream(bgzfunsorted) { is =>

            val query: String8 => String = flatindex.lookupAscii(is, indexunsorted)(x => x)

            sorted.foreach { (snp: String8) =>
              writer.write(query(snp) + "\n")
            }
          }
        }

        bgzfunsorted.delete

        val indexsorted = BGZippedGenotypeHelper.makeIndex(bgzfsorted)

        val indexFile = TempFile.createTempFile(".pidx")
        openZippedFileWriter(indexFile)(writer => flatindex.writeIndex(indexsorted, writer, (s: mybiotools.stringstore.String8) => s.value))

        log.info(s"Prepocessing Sanger file ${infile.name}, dropped $counter snps because id was missing.")
        SharedBlockCompressedPDose(
          SharedFile(bgzfsorted, name = outname, canMoveAway = true),
          fam,
          missing,
          SharedFile(indexFile, name = outname + ".pidx", canMoveAway = true)
        )

    }

}

