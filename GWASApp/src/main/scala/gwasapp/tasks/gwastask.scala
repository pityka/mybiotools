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

package gwasapp.tasks

import gwasapp._
import mybiotools._
import mybiotools.gwascommons._
import gwascommons.genotypedata._
import gwascommons.gwas._
import gwascommons.gwas.GWAS._
import mybiotools.config.Config.configInstance
import mybiotools.stat.LinearRegression.readPlinkCovarFile
import collection.JavaConversions._
import java.io.File
import _root_.ch.systemsx.cisd.hdf5._
import hdfdosage.HDFDosageIterator
import mybiotools.stringstore._
import org.saddle._
import scala.concurrent.Future
import akka.actor.{ ActorRef, Actor, ActorRefFactory, Props }
import akka.actor.Actor._
import mybiotools.tasks._
import java.io.File
import scala.concurrent.duration._
import vcfhelpers.VCFHelpers
import scala.util.{ Success, Failure }
import hdfdosage.FileSets
import hdfdosage.FileSets._
import mybiotools.saddlehelpers._
import mybiotools.sharedfiletypes._
import mybiotools.sharedfiletypes.SharedFileSets._
import akka.pattern.ask
import scala.concurrent.ExecutionContext.Implicits.global
import akka.agent.Agent

case class SharedAssocFile(snponly: SharedFile, alllines: SharedFile, testsDone: Long) extends ResultWithSharedFiles(snponly, alllines)
case class CovarData(value: SharedFile) extends Result
case class OutNamePart(value: String) extends Result
case class PhenotypeInfos(ph: Set[(String, PhenotypeScale, Set[String])]) extends Result
case class Model(value: GeneticModel) extends Result
case class MinimumMAF(value: Double) extends Result
case class SubsetRegion(value: Option[RegionSet[String8, GenomicLocation, Region]]) extends Result
case class KeepPValueThreshold(value: Double) extends Result
case class FileSubsetWrapper(value: SNPMajorSubset) extends Result
case class InteractionNames(value: Seq[String]) extends Result
case class InteractionModels(value: Seq[InteractionModel]) extends Result
case class WholeGenomeEpistasisScreen(value: Boolean) extends Result
case class WriteFullResults(value: Boolean) extends Result
case class PreFilterWithScoreTest(value: Boolean) extends Result
case class LDPruningParameters(ldPruningThreshold: Double, ldPruningWindow: Int) extends Result

object GWASTask {

  case class MyResultSet(
    covariates: Option[SharedFile],
    outnamePart: Option[String],
    phenotypeInfos: Option[Set[(String, PhenotypeScale, Set[String])]],
    mapfile: Option[Option[SharedFile]],
    sharedgeno: Option[SharedFileSet],
    model: Option[GeneticModel],
    subsetregions: Option[Option[RegionSet[String8, GenomicLocation, Region]]],
    filesubset: Option[SNPMajorSubset],
    maf: Option[Double],
    keepPValueThreshold: Option[Double],
    interactionNames: Option[Seq[String]],
    interactionModels: Option[Seq[InteractionModel]],
    wholeGenomeEpistasisScreen: Option[Boolean],
    ldPruningParameters: Option[LDPruningParameters],
    writeFullResults: Option[Boolean],
    preFilterWithScoreTest: Option[Boolean]
  ) extends SimplePrerequisitive[MyResultSet]

  class RunTask extends Computation[MyResultSet, SharedAssocFile] {

    def apply(rs: MyResultSet, env: ComputationEnvironment) = {
      import env._
      import rs._

      val t1 = System.nanoTime

      val unpackedcovariates = openSource(covariates.get.file)(s => readPlinkCovarFile(s, "-9"))

      val file: GenotypeFileSet = sharedgeno.get.toFileSet

      val subsetname = filesubset.get match {
        case Full => "ALL"
        case FileSubSet(from, to) => s"$from-$to"
        case RandomSubset(n, _) => throw new RuntimeException("RandomSubset not supported in gwas")
      }

      val outname = {
        val fname = file.name
        s"$fname.$subsetname.${model.get}.${outnamePart.get}.gwas.assoc"
      }

      val genomicmap = NodeLocalCache.getItemBlocking("genomicmap" + mapfile.get.map(_.name)) {
        file match {
          case VCFFile(file) => {
            val r = VCFHelpers.openVCF(file, None)
            val x = VCFHelpers.getGenomicMap(r)
            r.close
            x
          }
          case _ => mapfile.get.map(x => getGenomicMapFromBimFile(x.file.getCanonicalPath)).getOrElse(collection.Map[String8, GenomicLocation]())
        }

      }

      val (outzippedfull, outzippedsnponly) = {
        val filename = file.name

        mybiotools.TempFile.createTempFile(outname + ".full.bin.gz") -> mybiotools.TempFile.createTempFile(outname + ".snps.bin.gz")
      }

      val filterMarkerNames = subsetregions.get.map { regionset =>
        genomicmap.filter(x => regionset.contains(x._2)).toSeq.sortBy(_._2).map(_._1.value).toSeq
      }.getOrElse(Seq())

      val LDPruningParameters(ldPruningThreshold, ldPruningWindow) = ldPruningParameters.get

      val numberOfTestsDone: Long = openZippedFileOutputStream(outzippedfull) { fullwriter =>
        openZippedFileOutputStream(outzippedsnponly) { snponlywriter =>
          FileSets.openFileSet(file, maf.get, filesubset.get, filterMarkerNames.toSet) { snpmajoriterator =>

            val locusiter =
              (if (genomicmap.size > 0 && ldPruningThreshold < 1.0) snpmajoriterator.prune(ldPruningThreshold, ldPruningWindow, genomicmap)
              else snpmajoriterator).toLocusIteratorWithGenomicMap(genomicmap)

            val collector = actorsystem.actorOf(Props(new CollectorActor(snponlywriter, fullwriter, keepPValueThreshold.get, phenotypeInfos.get.size.toLong, interactionNames.get.size.toLong, interactionModels.get.size.toLong, !wholeGenomeEpistasisScreen.get && writeFullResults.get)).withDispatcher("my-pinned-dispatcher"))

            val summingAgent = Agent(0L)

            val r = singleMarkerTestOverBatch(
              iter = locusiter,
              covariates = unpackedcovariates,
              phenotypes = phenotypeInfos.get.toList.map(x => x.copy(_3 = x._3.toList)),
              model = model.get,
              threads = resourceAllocated.cpu,
              interactionnames = interactionNames.get,
              interactionmodels = interactionModels.get,
              individualSubset = None,
              wholeGenomeEpistasisScreen = wholeGenomeEpistasisScreen.get,
              prefilterWithScoreTest = preFilterWithScoreTest.get
            ) { (x1, x2) =>
                val numTests = (x1 ++ x2).map { (result: InMemoryBatchResult) =>

                  implicit val to = akka.util.Timeout(168, java.util.concurrent.TimeUnit.HOURS)
                  scala.concurrent.Await.ready(collector ? AskCollector, scala.concurrent.duration.Duration.Inf)
                  scala.concurrent.Await.result((collector ? result).asInstanceOf[Future[NumberOfTestsDone]], scala.concurrent.duration.Duration.Inf).n

                }.sum

                summingAgent send (_ + numTests)

                if (summingAgent.get % 10000 == 0) {
                  log.debug(summingAgent.get + " tests done so far.")
                }

                Nil
              }

            val testsDoneInThisBatch = scala.concurrent.Await.result((summingAgent.future).asInstanceOf[Future[Long]], scala.concurrent.duration.Duration.Inf)

            val elapsed = (System.nanoTime - t1) / 1E9
            log.info(testsDoneInThisBatch + s" tests done in a batch in ${elapsed} seconds (${testsDoneInThisBatch.toDouble / elapsed.toDouble} test/s).")

            testsDoneInThisBatch

          }
        }

      }

      if (ldPruningThreshold == 1.0 && maf.get == 0.0 && filterMarkerNames.isEmpty) {
        filesubset.get match {
          case filesubset: FileSubSet => {
            assert(numberOfTestsDone == (filesubset.toIdx - filesubset.fromIdx) * phenotypeInfos.get.size, s"expected # tests :${(filesubset.toIdx - filesubset.fromIdx) * phenotypeInfos.get.size}, got: $numberOfTestsDone")
          }
          case _ => {}
        }
      }

      SharedAssocFile(
        SharedFile(outzippedsnponly, name = outname + ".bin.gz", canMoveAway = true),
        SharedFile(outzippedfull, name = outname + ".full.bin.gz", canMoveAway = true), numberOfTestsDone
      )

    }
  }

  def startNew(cpu: Int)(implicit context: ActorRefFactory, queue: QueueActor, fileService: FileServiceActor, prefix: FileServicePrefix, cache: CacheActor): ActorRef = {
    context.actorOf(Props(new GWASTask(cpu)).withDispatcher("proxytask-dispatcher"))
  }

}
class GWASTask(cpu: Int)(implicit queue: QueueActor, fileService: FileServiceActor, prefix: FileServicePrefix, cache: CacheActor) extends ProxyTask(queue.actor, fileService.actor, prefix, cache.actor) {
  import GWASTask._

  type MyPrerequisitive = MyResultSet

  type MyResult = SharedAssocFile

  val runTaskClass = classOf[RunTask]

  override def resourceConsumed = CPUMemoryRequest(cpu = cpu, memory = 1500)

  def updatePrerequisitive: PartialFunction[(MyPrerequisitive, Result), MyPrerequisitive] = {
    case (self, m: SharedFileSet) => self.copy(sharedgeno = Some(m))
    case (self, m: MaybeBIMFile) => self.copy(mapfile = Some(m.file))
    case (self, m: PhenotypeInfos) => self.copy(phenotypeInfos = Some(m.ph))
    case (self, m: CovarData) => self.copy(covariates = Some(m.value))
    case (self, m: Model) => self.copy(model = Some(m.value))
    case (self, m: MinimumMAF) => self.copy(maf = Some(m.value))
    case (self, m: SubsetRegion) => self.copy(subsetregions = Some(m.value))
    case (self, m: KeepPValueThreshold) => self.copy(keepPValueThreshold = Some(m.
      value))
    case (self, m: FileSubsetWrapper) => self.copy(filesubset = Some(m.value))
    case (self, m: InteractionModels) => self.copy(interactionModels = Some(m.value))
    case (self, m: InteractionNames) => self.copy(interactionNames = Some(m.value))
    case (self, m: OutNamePart) => self.copy(outnamePart = Some(m.value))
    case (self, m: WholeGenomeEpistasisScreen) => self.copy(wholeGenomeEpistasisScreen = Some(m.value))
    case (self, m: PreFilterWithScoreTest) => self.copy(preFilterWithScoreTest = Some(m.value))
    case (self, m: WriteFullResults) => self.copy(writeFullResults = Some(m.value))
    case (self, m: LDPruningParameters) => self.copy(ldPruningParameters = Some(m))

  }

  override def emptyResultSet = MyResultSet(None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)

}

