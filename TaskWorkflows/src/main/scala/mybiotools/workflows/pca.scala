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
import mybiotools.gwascommons._
import mybiotools.stringstore._
import scala.io.Source
import akka.actor.{ ActorRefFactory, ActorContext }
import hdfdosage._
import mybiotools.sharedfiletypes._
import mybiotools.sharedfiletypes.SharedFileSets._
import hdfdosage.FileSets._
import mybiotools.saddlehelpers._
import mybiotools.pcanew._

import hdfdosage.{ DosageFileFormat, HDFDosage, PDose, PGenotypeProbabilities, BIGSNPMajor, TPed }

case class SharedPCAResult[T](
  pcaresult: PCAResult[T]
) extends Result

case class PCATaskInput(
  parameters: Option[PCAParameters],
  inputlist: Option[Set[SharedFileSet]],
  genomicMap: Option[SharedFile]
) extends SimplePrerequisitive[PCATaskInput]

case class PCAParameters(
  ldPruningThreshold: Double,
  ldPruningWindowInBP: Int,
  numberOfAxes: Int
)

object pcaTask {

  def apply(
    in: PCATaskInput,
    cpu: Int,
    memory: Int,
    update: UpdatePrerequisitive[PCATaskInput] = identity[PCATaskInput]
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = (1, cpu), memory = memory)
    ) {
      case (PCATaskInput(
        Some(parameters),
        Some(inputs),
        Some(gmap)), ce) =>
        import ce._

        import parameters._

        val genomicmap = NodeLocalCache.getItemBlocking("genomicmap" + Some(gmap.name)) {
          getGenomicMapFromBimFile(gmap.file.getCanonicalPath)
        }

        val genotypefilelist: Seq[GenotypeFileSet] = {
          val f = inputs.map(x => x -> x.toFileSet).toList
          val (nonempty, empty) = f.partition(f => openFileSet(f._2)(_.snpIterator.hasNext))
          if (empty.size > 0) {
            log.warning(s"The following files are empty: ${empty.map(_._1)}")
          }
          nonempty.map(_._2).sortBy(f => openFileSet(f)(_.toLocusIteratorWithGenomicMap(genomicmap).loci.next._1.genomicLocation.getOrElse(GenomicLocation(0, "0"))))
        }

        val glordering = genomicLocationOrdering

        val ordered = {
          var last: Option[GenomicLocation] = None
          FileSets.openFileSets(genotypefilelist, 0.05, 1.0, genotypedata.Full, Set()) { reader =>
            reader.toLocusIteratorWithGenomicMap(genomicmap).loci.flatMap(_._1.genomicLocation).forall { gl =>
              if (last.isEmpty) {
                last = Some(gl)
                true
              } else {
                glordering.lteq(last.get, gl)
              }
            }
          }
        }

        log.info(s"Genotype files are ordered: $ordered. If no, using random access for pruning.")

        val result = if (ordered) {
          FileSets.openFileSets(
            genotypefilelist,
            minimumMaf = 0.05,
            maximumMaf = 1.0,
            subset = genotypedata.Full,
            Set()
          ) { reader =>

              genotypedata.GRM.getPCA(
                reader.prune(ldPruningThreshold, ldPruningWindowInBP, genomicmap),
                numberOfAxes,
                batchSize = 5000,
                threads = resourceAllocated.cpu
              )

            }
        } else {

          FileSets.openRandomAccessFileSets(
            genotypefilelist
          ) { query =>

            genotypedata.GRM.getPCA(
              query
                .sortedIterator(genomicmap)
                .filter(pd => pd.maf >= 0.05)
                .prune(ldPruningThreshold, ldPruningWindowInBP, genomicmap),
              numberOfAxes,
              batchSize = 5000,
              threads = resourceAllocated.cpu
            )

          }

        }

        SharedPCAResult(result)

    }

}