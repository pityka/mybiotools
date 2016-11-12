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
import com.typesafe.config.{ Config, ConfigFactory }
import collection.JavaConversions._
import mybiotools.gwascommons.gcta._
import mybiotools.gwascommons._
import mybiotools.gwascommons.genotypedata.GRM
import mybiotools.stat.Resampling._
import scala.concurrent._
import scala.concurrent.duration._

// import hdfdosage.{ DosageFileFormat, HDFDosage, PDose, PGenotypeProbabilities, BIGSNPMajor, TPed }
import org.saddle._

case class GCTAResampleInput(
    outname: Option[String],
    extraArgs: Option[String],
    grms: Option[Set[GRMGZPair]],
    covarFile: Option[Option[SharedFile]],
    qCovarFile: Option[Option[SharedFile]],
    phenoFile: Option[SharedFile],
    expectedGRMs: Option[Int],
    randomizations: Option[Int],
    totallyRandomPhenotypes: Option[Int],
    bootstraps: Option[Int],
    resamples: Option[Seq[Int]]
) extends Prerequisitive[GCTAResampleInput] {

  def ready = outname.isDefined &&
    extraArgs.isDefined &&
    grms.isDefined &&
    covarFile.isDefined &&
    qCovarFile.isDefined &&
    phenoFile.isDefined &&
    expectedGRMs.isDefined &&
    expectedGRMs.get == grms.get.size &&
    randomizations.isDefined &&
    bootstraps.isDefined &&
    resamples.isDefined &&
    totallyRandomPhenotypes.isDefined

}

case class GCTAResampleOutput(
  main: GCTAOutput,
  bootstraps: Set[GCTAOutput],
  randomizations: Set[GCTAOutput],
  randomPhenotypes: Set[GCTAOutput],
  resamples: Set[GCTAOutput],
  bootstrapsSummary: SummarizedGCTAResult,
  randomizationsSummary: SummarizedGCTAResult,
  randomPhenotypesSummary: SummarizedGCTAResult,
  resamplesSummary: Seq[(Int, SummarizedGCTAResult)]
) extends ResultWithSharedFiles(((bootstraps ++ randomizations ++ randomPhenotypes) + main).flatMap(_.files).toSeq: _*)

object GCTAResampleInput {

  def apply(
    grms: Set[(File, File, String)],
    extraArgs: String,
    outname: String,
    covarFile: Option[File],
    qCovarFile: Option[File],
    phenoFile: File,
    randomizations: Int,
    totallyRandomPhenotypes: Int,
    bootstraps: Int,
    resamples: Seq[Int]
  )(implicit components: TaskSystemComponents): GCTAResampleInput = {

    GCTAResampleInput(
      outname = Some(outname),
      extraArgs = Some(extraArgs),
      grms = Some(grms.map(f => GRMGZPair(SharedFile(f._1), SharedFile(f._2), f._3))),
      covarFile = Some(covarFile.map(x => SharedFile(x))),
      qCovarFile = Some(qCovarFile.map(x => SharedFile(x))),
      phenoFile = Some(SharedFile(phenoFile)),
      expectedGRMs = Some(grms.size),
      randomizations = Some(randomizations),
      totallyRandomPhenotypes = Some(totallyRandomPhenotypes),
      bootstraps = Some(bootstraps),
      resamples = Some(resamples)
    )
  }

  def apply(
    extraArgs: String,
    outname: String,
    covarFile: Option[File],
    qCovarFile: Option[File],
    phenoFile: File,
    expectedGRMs: Int,
    randomizations: Int,
    totallyRandomPhenotypes: Int,
    bootstraps: Int,
    resamples: Seq[Int]
  )(implicit components: TaskSystemComponents): GCTAResampleInput = {

    GCTAResampleInput(
      outname = Some(outname),
      extraArgs = Some(extraArgs),
      covarFile = Some(covarFile.map(x => SharedFile(x))),
      qCovarFile = Some(qCovarFile.map(x => SharedFile(x))),
      phenoFile = Some(SharedFile(phenoFile)),
      expectedGRMs = Some(expectedGRMs),
      grms = None,
      randomizations = Some(randomizations),
      totallyRandomPhenotypes = Some(totallyRandomPhenotypes),
      bootstraps = Some(bootstraps),
      resamples = Some(resamples)
    )
  }

  private def updateGRMs(s: GCTAResampleInput, x: GRMFile) = s.grms match {
    case None => s.copy(grms = Some(Set(GRMGZPair(x.gctagz, x.gctaid, x.gctagz.name))))
    case Some(set) => s.copy(grms = Some(set + GRMGZPair(x.gctagz, x.gctaid, x.gctagz.name)))
  }

  def update: UpdatePrerequisitive[GCTAResampleInput] = {
    case (self, grm: GRMFile) => updateGRMs(self, grm)
  }

}

object gctaresampletask {

  def bootstrapGRM(
    original: Frame[Individual, Individual, Double],
    replica: Seq[Individual]
  ): Frame[Individual, Individual, Double] = {
    val inmemory: Frame[Individual, Individual, Double] = original.sortedRIx.sortedCIx
    val reversemap: Map[Int, Individual] = replica.zipWithIndex.map(x => x._2 -> x._1).toMap

    val ids = 0 until replica.size filter (x => inmemory.rowIx.contains(reversemap(x)))

    Frame(
      ids map { i =>
        val indi = Individual(i.toString, i.toString)
        val originali = reversemap(i)

        (indi, Series(
          ids map { j =>

            val indj = Individual(j.toString, j.toString)

            val originalj = reversemap(j)
            (indj, inmemory(originali, originalj).at(0, 0).get)
          }: _*
        ))
      }: _*
    )

  }

  def apply(
    in: GCTAResampleInput,
    update: UpdatePrerequisitive[GCTAResampleInput] = GCTAResampleInput.update orElse identity[GCTAResampleInput]
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 1, memory = 1000)
    ) {

      case (
        GCTAResampleInput(
          Some(outname),
          Some(args),
          Some(grms),
          Some(covarFile),
          Some(qCovarFile),
          Some(phenoFile),
          _,
          Some(randomizations),
          Some(totallyRandomPhenotypes),
          Some(bootstraps),
          Some(resamples)
          ), ce) =>
        import ce._

        val rnd = new org.apache.commons.math3.random.MersenneTwister
        val cxt = actorsystem
        import cxt.dispatcher

        def rungcta(idx: Int, tag: String, replica2: Seq[(Individual, Double)]): Future[GCTAOutput] = {

          // GCTA failed with duplicated samples, but with this it is not really a bootstrap
          val replica = replica2.distinct

          val reversemap: Map[Int, Individual] = replica.zipWithIndex.map(x => x._2 -> x._1._1).toMap
          val individualIndexMap: Map[Individual, Set[Int]] = replica.zipWithIndex.map(x => x._1._1 -> x._2).groupBy(_._1).map(x => x._1 -> x._2.map(_._2).toSet).toMap

          val replicaPhenotypeFile = {
            val f = TempFile.createTempFile(s".phenotypefile.$tag.$idx")
            writeToFile(f.getAbsolutePath, replica.zipWithIndex.map(x => s"${x._2} ${x._2} ${x._1._2}").mkString("\n"))
            SharedFile(f, name = outname + s".phenotypefile.$tag.$idx", canMoveAway = true)
          }

          def transform(f: File): File = writeToTempFile(
            openSource(f)(_
              .getLines
              .map(_.fastSplit(Set('\t', ' ')))
              .map(x => Individual(x(0), x(1)) -> x.drop(2))
              .filter(x => individualIndexMap.contains(x._1))
              .flatMap(x => individualIndexMap(x._1).map(i => i + " " + i + " " + x._2.mkString(" ")))
              .mkString("\n"))
          )

          val transformedCovarFile = covarFile.map { c =>
            val file = transform(c.file)
            SharedFile(file, name = outname + s".covarFile.$tag.$idx", canMoveAway = true)
          }
          val transformedQCovarFile = qCovarFile.map { q =>
            val file = transform(q.file)
            SharedFile(file, name = outname + s".qCovarFile.$tag.$idx", canMoveAway = true)
          }

          def transformGRM(g: GRMGZPair): GRMGZPair = {
            val inmemory: Frame[Individual, Individual, Double] = openSource(g.grmgz.file)(gz =>
              openSource(g.grmid.file)(id =>
                GRM.read(gz, id)))

            val transformed = bootstrapGRM(inmemory, replica.map(_._1))

            val tmpgz = TempFile.createTempFile(".grm.gz")
            val tmpid = TempFile.createTempFile(".grm.id")
            openZippedFileWriter(tmpgz) { grmwriter =>
              openFileWriter(tmpid) { idwriter =>
                GRM.write(transformed, grmwriter, idwriter)
              }
            }
            GRMGZPair(SharedFile(tmpgz, outname + s".grmgz.$tag.$idx.grm.gz", canMoveAway = true), SharedFile(tmpid, outname + s".grmid.$tag.$idx.grm.id", canMoveAway = true), g.name)
          }

          val input = GCTAInput(
            outname = Some(outname + s".$tag.$idx"),
            extraArgs = Some(args),
            grms = Some(grms.map(transformGRM)),
            covarFile = Some(transformedCovarFile),
            qCovarFile = Some(transformedQCovarFile),
            phenoFile = Some(replicaPhenotypeFile),
            expectedGRMs = Some(grms.size)
          )

          gctatask(input).?[GCTAOutput]
        }

        val mainGCTAOutput: Future[GCTAOutput] = {
          val input = GCTAInput(
            outname = Some(outname + s".main"),
            extraArgs = Some(args),
            grms = Some(grms),
            covarFile = Some(covarFile),
            qCovarFile = Some(qCovarFile),
            phenoFile = Some(phenoFile),
            expectedGRMs = Some(grms.size)
          )

          gctatask(input).?[GCTAOutput]
        }

        val commonindividuals: Set[Individual] = {

          def getIndividuals(f: File): Set[Individual] = openSource(f)(_
            .getLines
            .map(_.fastSplit(Set('\t', ' ')))
            .map(x => Individual(x(0), x(1))).toSet)

          val phenoInds: Set[Individual] = getIndividuals(phenoFile.file)
          val covarInds: Option[Set[Individual]] = covarFile.map(x => getIndividuals(x.file))
          val qCovarInds: Option[Set[Individual]] = qCovarFile.map(x => getIndividuals(x.file))
          val grminds: Set[Individual] = grms.map {
            case GRMGZPair(_, idfile, _) =>
              getIndividuals(idfile.file)
          }.toList.reduce(_ & _)

          val s1 = phenoInds & grminds
          val s2 = covarInds map (_ & s1) getOrElse (s1)
          qCovarInds map (_ & s2) getOrElse (s2)
        }

        val phenotypeFileLines: IndexedSeq[(Individual, Double)] =
          openSource(phenoFile.file)(_.getLines.toIndexedSeq.map(_.fastSplit(Set(' ', '\t'))).map(x => Individual(x(0), x(1)) -> x(2).toDouble)).filter(x => commonindividuals.contains(x._1))

        def randomizedPhenotype: IndexedSeq[(Individual, Double)] = {
          scala.util.Random.shuffle((0 until phenotypeFileLines.size).toVector).zipWithIndex map {
            case (shuff, idx) =>
              phenotypeFileLines(idx)._1 -> phenotypeFileLines(shuff)._2
          } toIndexedSeq
        }

        def totallyRandomPhenotype: IndexedSeq[(Individual, Double)] = {
          def random = rnd.nextGaussian
          phenotypeFileLines.map {
            case (ind, _) =>
              ind -> random
          } toIndexedSeq
        }

        val bootstrappedGCTAOutputs: Future[Seq[GCTAOutput]] = Future.sequence {
          bootstrap(bootstraps, phenotypeFileLines) {
            case (replica, idx) =>
              rungcta(idx, "bootstrap", replica)
          }
        }

        val smallsampleGCTAOutputs: Future[Seq[GCTAOutput]] = Future.sequence {
          resamples.zipWithIndex map {
            case (resamplecount, idx) =>
              val replica = resampledReplica(phenotypeFileLines, resamplecount, rnd)
              rungcta(idx, s"resampling_$resamplecount", replica)
          }
        }

        val randomizedGCTAOutputs: Future[Seq[GCTAOutput]] = Future.sequence {
          1 to randomizations map { idx =>
            val replica = randomizedPhenotype
            rungcta(idx, s"randomization", replica)
          }
        }

        val totallyRandomPhenotypeGCTAOutputs: Future[Seq[GCTAOutput]] = Future.sequence {
          1 to totallyRandomPhenotypes map { idx =>
            val replica = totallyRandomPhenotype
            rungcta(idx, s"randomphenotype", replica)
          }
        }

        val summarizedBootstraps: Future[SummarizedGCTAResult] = bootstrappedGCTAOutputs.map(seq => SummarizedGCTAResult(seq.flatMap(_.result)))
        val summarizedRandomizations: Future[SummarizedGCTAResult] = randomizedGCTAOutputs.map(seq => SummarizedGCTAResult(seq.flatMap(_.result)))
        val summarizedTotallyRandomPhenotypes: Future[SummarizedGCTAResult] = totallyRandomPhenotypeGCTAOutputs.map(seq => SummarizedGCTAResult(seq.flatMap(_.result)))
        val summarizedSmallsampleGCTAOutputs: Future[Seq[(Int, SummarizedGCTAResult)]] = smallsampleGCTAOutputs.map { seq =>
          SummarizedGCTAResult.createResampleBins(seq.flatMap(_.result))
        }

        val bigfuture = for {
          sumsmall <- summarizedSmallsampleGCTAOutputs
          sumrand <- summarizedRandomizations
          sumtotrand <- summarizedTotallyRandomPhenotypes
          sumboot <- summarizedBootstraps
          randomized <- randomizedGCTAOutputs
          randomphenotypes <- totallyRandomPhenotypeGCTAOutputs
          small <- smallsampleGCTAOutputs
          boot <- bootstrappedGCTAOutputs
          m <- mainGCTAOutput
        } yield (m, boot, small, randomized, randomphenotypes, sumboot, sumrand, sumtotrand, sumsmall)

        val (m, boot, small, randomized, randphen, sumboot, sumrand, sumrandphen, sumsmall) = LauncherActor.block(CPUMemoryRequest(resourceAllocated.cpu, resourceAllocated.memory)) {
          Await.result(bigfuture, atMost = Duration.Inf)

        }

        GCTAResampleOutput(
          m,
          boot.toSet,
          randomized.toSet,
          randphen.toSet,
          small.toSet,
          sumboot,
          sumrand,
          sumrandphen,
          sumsmall
        )

    }

}
