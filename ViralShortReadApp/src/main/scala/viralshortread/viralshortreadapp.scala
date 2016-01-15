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

package viralshortread

import mybiotools._
import mybiotools.workflows._
import mybiotools.tasks._
import com.typesafe.config.{ Config, ConfigFactory }
import mybiotools.config.Config.configInstance
import genotyper._
import java.io.File
import scala.concurrent.{ Future, Await }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.collection.JavaConversions._
import mybiotools.sequence.alignment._
import org.saddle._
import scala.util._

case class ViralShortReadDeliverables(
  entropyPlink: File,
  entropyDichotomizedPlink: File,
  entropyDichotomizedPlink5: File,
  entropyBlomPlink: File,
  binaryVariantCallsConsensusPlink: File,
  binaryVariantCallsLowFrequencyPlink: File,
  binaryVariantCallsLowFrequencyMinorityPlink: File,
  viralgenotypeDummiesPlink: File
)

object DefaultViralShortReadConfig extends ViralShortReadConfigImpl(configInstance.getConfig("viralshortread"))

class ViralShortReadConfigImpl(configInstance: Config) extends ViralShortReadConfig {

  def references: Map[String, java.io.File] = configInstance.getStringList("references").grouped(2).map(x => x(0) -> new File(x(1))).toMap

  def samples: List[InputFastQGroup] = {
    val listfile = configInstance.getString("fastqtable")
    val fromtable = if (listfile != "") openSource(listfile)(_.getLines.map(InputFastQGroup.fromLine).toList) else Nil
    val fromfileslist = configInstance.getString("fastqs")
    val regexp = configInstance.getString("fastqregexp")

    val fromfiles = if (fromfileslist != "") InputFastQGroup.fromFiles(openSource(fromfileslist)(_.getLines.toList).map(l => new File(l)).toSet, regexp) else Nil
    fromtable ++ fromfiles
  }

  val indexCPU = configInstance.getInt("bwa.index.CPU")
  val indexMemory = configInstance.getInt("bwa.index.RAM")
  val alignCPU = configInstance.getInt("bwa.map.CPU")
  val alignMemory = configInstance.getInt("bwa.map.RAM")
  val extraArgsAlign = configInstance.getString("bwa.map.extraArgs")
  val extraArgsIndex = configInstance.getString("bwa.index.extraArgs")
}

trait ViralShortReadConfig {

  def references: Map[String, File]
  def samples: List[InputFastQGroup]

  val indexCPU: Int
  val indexMemory: Int
  val alignCPU: Int
  val alignMemory: Int
  val extraArgsAlign: String
  val extraArgsIndex: String

}

class ViralShortReadRunner(ts: TaskSystemComponents, config: ViralShortReadConfig) {

  import config._
  import ts._
  implicit val components = ts
  implicit val fs = components.fs
  implicit val actorsystem = components.actorsystem

  val log = ts.getLogger(this)

  def run: ViralShortReadDeliverables = {

    log.info(samples.mkString("\n"))

    log.info(references.mkString("\n"))

    log.info(references.map(x => openSource(x._2)(readFasta).mapValues(_.size)).toString)

    val bwaindextasks = references.map {
      case (name, file) =>

        name -> bwaIndexGeneration(BWAIndexGenerationInput(file, extraArgsIndex), cpu = indexCPU, memory = indexMemory)
    }

    val alignedreferenceCigars = Future {
      val joined = references.map(x => openSource(x._2)(readFasta).map(y => x._1 -> y._2.map(_.toUpper))).reduce(_ ++ _)

      val msa = GlobalPairwiseProfileAlignment.makeProfileAlignmentMultiStage(joined.toSeq, GlobalPairwiseProfileAlignment.pdotp, 6.0, 1.0, 5)

      SharedFile(writeToTempFile(makeFastaString(msa)), name = "references.aligned.fasta")

      msa.map(x => x._1 -> cigarFromAlignedSequence(x._2, '-')).toMap
    }

    val mappedsamples = samples.map {
      case InputFastQGroup(files, readgroup) =>

        val aligned: Iterable[Future[(BamFile, String, Int)]] = bwaindextasks.map {
          case (refname, indextask) =>
            val aligntask = bwaAlign(BWAAlignInput(files, extraArgsAlign, readgroup.sm + "@" + refname, readgroup), cpu = alignCPU, memory = alignMemory)

            val sorttask = sortbam(SortBamInput.empty, memory = alignMemory)

            val counttask = tasks.numberOfUniquelyMappedReads(tasks.NumberOfUniquelyMappedReadsInput(None))

            indextask ~> aligntask ~> sorttask ~> counttask

            sorttask.?[BamFile].flatMap { sortedbam =>
              counttask.?[tasks.NumberOfUniquelyMappedReadsResult].map { output =>
                val percentageOfUniqueReads: Int = output.value
                log.info(s"${readgroup.sm} - $refname - $percentageOfUniqueReads")

                (sortedbam, refname, percentageOfUniqueReads)
              }
            }

        }

        val bestreference: Future[(BamFile, String, Int)] = Future.sequence(aligned).map(_.maxBy(_._3))

        bestreference.flatMap {
          case (bamfile, refname, percentageOfUniqueReads) =>

            tasks.sam2PerBaseMetrics(tasks.Sam2PerBaseMetricsInput(Some(bamfile.file), Some(readgroup.sm), Some(refname)), 3000).?[tasks.PerBaseMetrics].map(_.data).map { perBaseMetrics =>

              val cons = perBaseMetrics.map(_._2._1).filterNot(_ == '-').mkString
              val frequencies = perBaseMetrics.map(x => x._1 -> x._2._2).toIndexedSeq
              val entropies = perBaseMetrics.map(x => x._1 -> x._2._3).toIndexedSeq

              (cons, refname, perBaseMetrics, readgroup.sm)

            }
        }

    }

    val viralgenotypeDummiesPlink = Future.sequence(mappedsamples).map { samples =>

      val str = samples map {
        case (cons, refname, perBaseMetrics, samplename) =>

          s"samplename samplename refname"
      } mkString ("\n")

      SharedFile(writeToTempFile(str), name = "bestreference.txt")

      val dummies = {
        val (keys, values) = samples.map {
          case (cons, refname, perBaseMetrics, samplename) =>
            (mybiotools.gwascommons.Individual(samplename, samplename) -> refname)
        }.toSeq.unzip

        val dummies: Map[String, Seq[Boolean]] = dummifyAll(values)
        Frame(dummies.toSeq.map(x => x._1 -> Series(keys zip x._2: _*)): _*).mapValues(x => if (x) 1.0 else 0.0)
      }

      SharedFile(writeToTempFile(FrameToPlink.frameToPlink(dummies, "-9")), name = "viralgenotypes.plink.txt")

    }

    val joinedConsensus = Future.sequence(mappedsamples).flatMap { joinedsamples =>
      viralgenotypeDummiesPlink.flatMap { viralgenotypeDummiesPlink =>
        alignedreferenceCigars.map { referenceAlignments =>

          SharedFile(writeToTempFile(makeFastaString(joinedsamples.map(x => x._4 -> x._1), 80)), name = "samples.consensus.fasta")

          // val msa = GlobalPairwiseProfileAlignment.makeProfileAlignmentMultiStage(joinedsamples.map(x => x._4 -> x._1), GlobalPairwiseProfileAlignment.pdotp, 6.0, 1.0, 5)

          // SharedFile(writeToTempFile(makeFastaString(msa, 80)), name = "samples.consensus.aligned.fasta")

          val unifiedmetrics = {
            val unifiedreferencecoordinates: Set[ReferencePosition] = {
              0 until lengthOfCigar(referenceAlignments.head._2) map (i => PositionOnReference(i))
            } toSet

            joinedsamples.map {
              case (_, refname, perBaseMetrics, samplename) =>
                val refcigar = referenceAlignments(refname)
                val mappedmetrics: List[(viralshortread.ReferencePosition, Option[(Char, scala.collection.immutable.Map[Char, Int], Option[Double])])] = {
                  val mapped = Try(perBaseMetrics.map(x => mapcoordinates(refcigar, x._1) -> Some(x._2))) match {
                    case Success(r) => r
                    case Failure(e) => throw new RuntimeException(s"Error in mapcoordinates $refname $samplename", e)
                  }
                  val missingpositionsOnUnifiedReference = unifiedreferencecoordinates &~ mapped.map(_._1).toSet

                  (mapped ++ missingpositionsOnUnifiedReference.toSeq.map(x => x -> None)).sortBy(_._1)
                }

                SharedFile(writeToTempFile(mappedmetrics.map(x => x._1 + "\t" + x._2.map(x => x._2.map(x => x._1 + ":" + x._2).mkString("\t")).getOrElse("")).mkString("\n")), name = "unifiedcoordinates." + samplename + ".frequencies.atconsensuspositions.txt")

                (refname, mappedmetrics, samplename)
            }
          }

          val binaryVariantCallsConsensusPlink = {
            val consensusCalls: Frame[String, BinaryVariantKey, Boolean] = callVariants(
              unifiedmetrics.map(x => x._3 -> x._2.map(y => y._1 -> y._2.map(_._1)).filter(_._2.isDefined).map(x => x._1 -> x._2.get).toMap)
            )(callBinaryVariantsFromConsensus(_))

            val tmp = TempFile.createTempFile(".txt")
            openFileWriter(tmp) { w =>
              FrameToPlink.writeFrameToPlink(consensusCalls.mapColIndex(_.toString).mapRowIndex(x => mybiotools.gwascommons.Individual(x, x)).mapValues(if (_: Boolean) 1.0 else 0.0).filter(x => x.toVec.count / x.length.toDouble > 0.8).filter(x => x.toVec.sum > 10 && (x.count - x.toVec.sum) > 10), "-9", w)
            }
            SharedFile(tmp, name = "unifiedcoordinates.binaryvariantcalls.consensus.plink.txt")
          }

          val binaryVariantCallsLowFrequencyPlink = {
            val presenceCallsFromFrequency: Frame[String, BinaryVariantKey, Option[Boolean]] = callVariants(
              unifiedmetrics.map(x => x._3 -> x._2.map(y => y._1 -> y._2.map(_._2)).filter(_._2.isDefined).map(x => x._1 -> x._2.get).toMap)
            )(callBinaryVariantsFromFrequencyDistribution(_, 0.01, 5000))

            val tmp = TempFile.createTempFile(".txt")
            openFileWriter(tmp) { w =>
              FrameToPlink.writeFrameToPlink(presenceCallsFromFrequency.mapColIndex(_.toString).mapRowIndex(x => mybiotools.gwascommons.Individual(x, x)).mapValues(_.map((x: Boolean) => if (x) 1.0 else 0.0).getOrElse(Double.NaN)).filter(x => x.toVec.count / x.length.toDouble > 0.8).filter(x => x.toVec.sum > 10 && (x.count - x.toVec.sum) > 10), "-9", w)
            }
            SharedFile(tmp, name = "unifiedcoordinates.binaryvariantcalls.lowfrequency.plink.txt")
          }

          val binaryVariantCallsLowFrequencyMinorityPlink = {
            val presenceCallsFromFrequency: Frame[String, BinaryVariantKey, Option[Boolean]] = callVariants(
              unifiedmetrics.map(x => x._3 -> x._2.map(y => y._1 -> y._2.map(_._2)).filter(_._2.isDefined).map(x => x._1 -> x._2.get).toMap)
            )(callBinaryVariantsFromFrequencyDistributionMinority(_, 0.01, 5000))

            val tmp = TempFile.createTempFile(".txt")
            openFileWriter(tmp) { w =>
              FrameToPlink.writeFrameToPlink(presenceCallsFromFrequency.mapColIndex(_.toString).mapRowIndex(x => mybiotools.gwascommons.Individual(x, x)).mapValues(_.map((x: Boolean) => if (x) 1.0 else 0.0).getOrElse(Double.NaN)).filter(x => x.toVec.count / x.length.toDouble > 0.8).filter(x => x.toVec.sum > 10 && (x.count - x.toVec.sum) > 10), "-9", w)
            }
            SharedFile(tmp, name = "unifiedcoordinates.binaryvariantcalls.lowfrequency.minority.plink.txt")
          }

          val entropyframe: Frame[String, BinaryVariantKey, Double] = callVariants(
            unifiedmetrics.map(x => x._3 -> x._2.map(y => y._1 -> y._2.map(_._3)).filter(_._2.isDefined).map(x => x._1 -> x._2.get.getOrElse(Double.NaN)).toMap)
          )(x => List('E' -> x.map(_.getOrElse(Double.NaN))))

          val entropyDichotomizedPlink = {

            val tmp = TempFile.createTempFile(".txt")
            openFileWriter(tmp) { w =>
              FrameToPlink.writeFrameToPlink(entropyframe.mapColIndex(_.toString).mapRowIndex(x => mybiotools.gwascommons.Individual(x, x)).filter(x => x.toVec.count / x.length.toDouble > 0.8).mapValues(x => if (x > 0.2) 1.0 else 0.0).filter(x => x.toVec.sum > 10 && (x.count - x.toVec.sum) > 10), "-9", w)
            }
            SharedFile(tmp, name = "unifiedcoordinates.entropy_dichotomizedat0.2.plink.txt")

          }

          val entropyDichotomizedPlink5 = {

            val tmp = TempFile.createTempFile(".txt")
            openFileWriter(tmp) { w =>
              FrameToPlink.writeFrameToPlink(entropyframe.mapColIndex(_.toString).mapRowIndex(x => mybiotools.gwascommons.Individual(x, x)).filter(x => x.toVec.count / x.length.toDouble > 0.8).mapValues(x => if (x > 0.5) 1.0 else 0.0).filter(x => x.toVec.sum > 10 && (x.count - x.toVec.sum) > 10), "-9", w)
            }
            SharedFile(tmp, name = "unifiedcoordinates.entropy_dichotomizedat0.5.plink.txt")

          }

          val entropyPlink = {

            val tmp = TempFile.createTempFile(".txt")
            openFileWriter(tmp) { w =>
              FrameToPlink.writeFrameToPlink(entropyframe.mapColIndex(_.toString).mapRowIndex(x => mybiotools.gwascommons.Individual(x, x)).filter(x => x.toVec.count / x.length.toDouble > 0.8), "-9", w)
            }
            SharedFile(tmp, name = "unifiedcoordinates.entropy.plink.txt")

          }

          val entropyBlomPlink = {

            val tmp = TempFile.createTempFile(".txt")
            openFileWriter(tmp) { w =>
              FrameToPlink.writeFrameToPlink(entropyframe.mapColIndex(_.toString).mapRowIndex(x => mybiotools.gwascommons.Individual(x, x)).filter(x => x.toVec.count / x.length.toDouble > 0.8).mapVec(v => Vec(mybiotools.stat.BlomTransformation(v.toSeq): _*)), "-9", w)
            }
            SharedFile(tmp, name = "unifiedcoordinates.entropy_blom.plink.txt")

          }

          val unifiedMetricsWithoutIndels = unifiedmetrics.map(x => (x._1, x._2.filter(_._1.isInstanceOf[PositionOnReference]), x._3))

          SharedFile(writeToTempFile(makeFastaString(unifiedMetricsWithoutIndels.map(x => x._3 -> x._2.map(_._2.map(_._1).getOrElse('-')).mkString), 80)), name = "unifiedcoordinates.consensus.fasta")

          {
            val entropyframe: Frame[String, Int, Double] = Frame(unifiedMetricsWithoutIndels.map {
              case (_, metrics, sample) =>
                sample -> Series(metrics.zipWithIndex.map(x => x._2 -> x._1._2.map(_._3.getOrElse(Double.NaN)).getOrElse(0.0)): _*)
            }: _*).T
            val rowOrder = Index(mybiotools.clustering.traverse(clustering.clusterFrameByRows(entropyframe)(clustering.euclideanDistance)): _*)

            val reorderedFrame = entropyframe.reindex(rix = rowOrder, cix = entropyframe.colIx)
            SharedFile(mybiotools.plots.pngToFile(mybiotools.plots.Raster.createArrayBackedRasterPlot(reorderedFrame)), name = "unifiedcoordinates.entropy.png")

            SharedFile(
              mybiotools.plots.pngToFile(
                mybiotools.plots.HistogramPlot.createHistogramPlot(
                  data = entropyframe.toMat.contents,
                  main = "Distribution of all entropy values pooled together"
                )
              ),
              name = "unifiedcoordinates.entropy.hist.png"
            )

            SharedFile(
              mybiotools.plots.pngToFile(
                mybiotools.plots.HistogramPlot.createHistogramPlot(
                  data = entropyframe.toMat.contents,
                  main = "Distribution of all entropy values pooled together (ylim=100)",
                  ylim = Some(100)
                )
              ),
              name = "unifiedcoordinates.entropy.zoom.hist.png"
            )

          }

          ViralShortReadDeliverables(entropyPlink.file, entropyDichotomizedPlink.file, entropyDichotomizedPlink5.file, entropyBlomPlink.file, binaryVariantCallsConsensusPlink.file, binaryVariantCallsLowFrequencyPlink.file, binaryVariantCallsLowFrequencyMinorityPlink.file, viralgenotypeDummiesPlink.file)

        }
      }
    }

    Await.result(joinedConsensus, 24 hours)

  }
}

object ViralShortReadApp extends App {

  val ts = defaultTaskSystem
  if (ts.hostConfig.myRole == MASTER) {
    val config = DefaultViralShortReadConfig
    try {
      new ViralShortReadRunner(ts.components, config).run
    } finally {
      ts.shutdown
    }

  }

}
