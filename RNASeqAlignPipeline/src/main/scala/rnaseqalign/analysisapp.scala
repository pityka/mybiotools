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

package rnaseqalign

import com.typesafe.config.{ Config, ConfigFactory }
import mybiotools.tasks._
import rnaseqalign.tasks._
import java.io.File
import mybiotools.config.Config.configInstance
import scala.collection.JavaConversions._
import scala.concurrent.{ Future, Await }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import org.saddle.io._
import org.saddle._
import mybiotools._
import analysis._
import scala.util.Try

object CountAnalysisApp extends App {

  new CountAnalysisAppTrunk(DefaultCountAnalysisConfig).run
}

trait CountAnalysisConfig {
  def countFiles: List[File]
  def lengthFile: File
  def outTrunk: File
  def spikeInData: Set[SpikeIn]
  def spikeInPerSample: Map[String, SpikeInMix]
  def doPCA: Boolean
  def pseudoCoverage: Double
  def thresholdDropIfCoverageBelowInAllSamples: Double
  def genestoplot: Seq[DetailedPlotDescription]
  def conditions: Map[String, String]
  def secondaryDimension: Map[String, String]
  def columnToSample: Map[String, String]
  def pairwiseDifferentialExpressionConditions1: List[(String, String)]
  def pairwiseDifferentialExpressionConditions2: List[(String, String)]
  def translateHGNC: Boolean
  def gmtFiles: List[File]
  def houseKeepingGenes: Set[String]
  def normalizations: Seq[NormalizationMethod]
  def readLength: Int
  def conditionOrderOnPlots: Seq[String]

  lazy val conditionsReverse: Seq[(String, Set[String])] = reverseMap(conditions).toSeq.sortBy(x => conditionOrderOnPlots.indexOf(x._1))
  lazy val secondaryDimensionReverse: Seq[(String, Set[String])] = reverseMap(secondaryDimension).toSeq.sortBy(x => conditionOrderOnPlots.indexOf(x._1))

}

object DefaultCountAnalysisConfig extends CountAnalysisConfig {
  val gmtFiles: List[File] = configInstance.getStringList("countanalysis.gmtFiles").map(x => new File(x)).toList
  val translateHGNC = configInstance.getBoolean("countanalysis.translateHGNC")
  val thresholdDropIfCoverageBelowInAllSamples = configInstance.getDouble("countanalysis.thresholdDropIfCoverageBelowInAllSamples")
  val pseudoCoverage = configInstance.getDouble("countanalysis.pseudoCoverage")
  val doPCA = configInstance.getBoolean("countanalysis.pca")
  val list = configInstance.getStringList("countanalysis.counts").toList
  val file1 = configInstance.getString("countanalysis.count")
  val countFiles = (file1 :: list).filterNot(_ == "-").map(x => new File(x)).toList
  val lengthFile = new File(configInstance.getString("countanalysis.lengths"))
  val outTrunk = new File(configInstance.getString("countanalysis.outTrunk"))
  val readLength = configInstance.getInt("countanalysis.readLength")

  val columnToSample = configInstance.getStringList("countanalysis.columnToSample").toList.grouped(2).map(x => x(0) -> x(1)).toMap

  val conditions = configInstance.getStringList("countanalysis.conditions").toList.grouped(2).map(x => x(0) -> x(1)).toMap.filter(x => columnToSample.values.contains(x._1))
  val secondaryDimension = Try(configInstance.getStringList("countanalysis.secondaryDimension").toList.grouped(2).map(x => x(0) -> x(1)).toMap).toOption.getOrElse(conditions.map(x => x._1 -> "?")).filter(x => columnToSample.values.contains(x._1))

  val pairwiseDifferentialExpressionConditions1: List[(String, String)] = configInstance.getStringList("countanalysis.pairwiseDE1").toList.grouped(2).map(x => x(0) -> x(1)).filter(x => conditions.values.contains(x._1) && conditions.values.contains(x._2)).toList
  val pairwiseDifferentialExpressionConditions2: List[(String, String)] = configInstance.getStringList("countanalysis.pairwiseDE2").toList.grouped(2).map(x => x(0) -> x(1)).filter(x => secondaryDimension.values.contains(x._1) && secondaryDimension.values.contains(x._2)).toList

  val genestoplot = configInstance.getConfigList("countanalysis.genesToPlot").toList.map { c =>
    val axis = c.getInt("xaxis")
    val genes = c.getStringList("genes").toList
    val title = c.getString("main")
    DetailedPlotDescription(genes, axis, title)
  }

  val spikeInPerSample: Map[String, SpikeInMix] = {
    val f = configInstance.getString("countanalysis.spikeInPerSample")
    if (f != "") openSource(f)(_.getLines.map { line =>

      val spl = fastSplitSetSeparator(line, Set(' ', '\t', ',', ';'))
      val mix = spl(1).toLowerCase match {
        case "mix1" | "mixi" => Mix1
        case "mix2" | "mixii" => Mix2
      }
      spl(0) -> mix
    }.toMap)
    else Map()
  }

  val spikeInData: Set[SpikeIn] = {
    val f = configInstance.getString("countanalysis.spikeInData")
    if (f != "") openSource(f)(_.getLines.drop(1).map { line =>
      val spl = fastSplitSetSeparator(line, Set(','))
      val id = spl(1)
      val conc1 = spl(3).toDouble
      val conc2 = spl(4).toDouble
      SpikeIn(id, conc1, conc2)
    }.toSet)
    else Set()
  }

  val houseKeepingGenes: Set[String] = {
    val f = configInstance.getString("countanalysis.houseKeepingGenes")
    if (f == "-") Set[String]()
    else openSource(f)(_.getLines.toSet)
  }
  val normalizations: Seq[NormalizationMethod] = configInstance.getStringList("countanalysis.normalizationMethods").map(_.toLowerCase match {
    case "deseq" => Some(DeSeqNormalization)
    case "spikein" if !spikeInPerSample.isEmpty => Some(SpikeInNormalization)
    case "housekeepinggenes" if !houseKeepingGenes.isEmpty => Some(HouseKeepingNormalization(houseKeepingGenes))
    case "ruvghousekeepinggenes" if !houseKeepingGenes.isEmpty => Some(RuvGNormalization(houseKeepingGenes))
    case "ruvgspikeins" => Some(RuvGNormalizationWithSpikeIns)
    case "no" => Some(NoDepthNormalization)
    case "librarysize" => Some(LibrarySizeNormalization)
    case _ => None
  }).filter(_.isDefined).map(_.get)

  val conditionOrderOnPlots = configInstance.getStringList("countanalysis.conditionOrderOnPlots").toList

}

class CountAnalysisAppTrunk(conf: CountAnalysisConfig) {
  import conf._

  def run: Unit = {

    openUnbufferedFileWriter(new File(outTrunk + s"/logfile.txt")) { logwriter =>
      def log(s: String): Unit = {

        logwriter.write(s + "\n")
        println(s)
      }
      log("Current date and time: " + java.text.DateFormat.getDateTimeInstance().format(new java.util.Date()))

      log((mybiotools.config.Config.prettyPrintVersion(rnaseqalign.Reflected.name, rnaseqalign.Reflected.version)))
      log(mybiotools.config.Config.prettyPrint("countanalysis"))

      log("\nCount files:" + countFiles.map(x => "\t" + x.getAbsolutePath).mkString("\n", "\n", ""))
      log("\nLength file:" + lengthFile.getAbsolutePath)

      val (ensembletable, ensemblereversetable) = if (translateHGNC) Helpers.readEnsembleHGNCTable else (Map[String, String](), Map[String, Int]())

      val pooledCounts = {
        val nonpooled = countFiles.map { file =>
          if (openSource(file.getAbsolutePath)(_.getLines.next).filter(_ == '\t').size == 1) {
            val map = openSource(file.getAbsolutePath)(_.getLines.map { lines =>
              val spl = mybiotools.fastSplitSeparator(lines, '\t')
              spl(0) -> spl(1).toLong
            }.toMap)

            val changedname = {
              val origname = file.getName
              if (origname.endsWith(".aligned.merged.bam.counts.tsv")) origname.dropRight(".aligned.merged.bam.counts.tsv".size)
              else if (origname.endsWith(".aligned.merged.bam.counts")) origname.dropRight(".aligned.merged.bam.counts".size)
              else origname
            }

            Frame((changedname, Series(map.toSeq: _*)))
          } else {
            val csvfile = CsvFile(file.getAbsolutePath)
            val frame = CsvParser.parse()(csvfile).withRowIndex(0).withColIndex(0)
            frame.mapValues(CsvParser.parseLong).mapColIndex { origname =>
              if (origname.endsWith(".aligned.merged.bam.counts.tsv")) origname.dropRight(".aligned.merged.bam.counts.tsv".size)
              else if (origname.endsWith(".aligned.merged.bam.counts")) origname.dropRight(".aligned.merged.bam.counts".size)
              else origname
            }
          }

        }.reduce(_ rconcat _).mapRowIndex { rix => Helpers.translateHGNC(rix, ensembletable, ensemblereversetable) }

        val pooling: Map[String, Seq[(String, Series[String, Long])]] = nonpooled.toColSeq.groupBy(x => columnToSample(x._1))

        log(s"\nColumns in files (${nonpooled.numCols}):\n\t" + nonpooled.colIx.toSeq.sorted.mkString("\n\t"))

        log("\nSumming up counts into samples:")
        pooling.foreach {
          case (sample, columns) =>
            log("\t" + sample + ":\n" + columns.map(_._1).mkString("\t\t", "\n\t\t", "\n"))

        }
        log("")

        Frame(pooling.toSeq.map(x => x._1 -> (x._2.map(_._2).reduce(_ + _))).toSeq: _*)

      }

      val gmts: Seq[GeneSetDB] = gmtFiles.map(x => GeneSetDB(x.getName, openSource(x)(s => mybiotools.readGMT(s).toSeq.map(x => GeneSet(x._1.value, x._2.toSet.map((y: mybiotools.stringstore.String8) => y.value))))))

      log("\nConditions:\n" + conditions.toSeq.groupBy(_._2).map(x => "\t" + x._1 + "\n" + x._2.map(_._1).mkString("\t\t", "\n\t\t", "")).mkString("\n"))
      log("\nSecondary dimension (e.g. individual, cell type, etc):\n" + secondaryDimension.toSeq.groupBy(_._2).map(x => "\t" + x._1 + "\n" + x._2.map(_._1).mkString("\t\t", "\n\t\t", "")).mkString("\n"))
      log("\nSpike-in mix per sample: \n" + pooledCounts.colIx.toSeq.map(x => "\t" + x + "\t" + spikeInPerSample.get(x)).mkString("\n"))

      val lengths: Map[String, Int] = openSource(lengthFile.getAbsolutePath) {
        _.getLines.map { line =>
          val spl = fastSplitSeparator(line, '\t')
          spl(0) -> spl(1).toInt
        }.toMap
      }.map(x => Helpers.translateHGNC(x._1, ensembletable, ensemblereversetable) -> x._2)

      def report(counts: Frame[String, String, Long], normalization: NormalizationMethod) = {

        val name = normalization.name

        {
          val underscore = counts.rfilterIx(_.take(2) == "__").filterIx(x => conditions.contains(x))
          log("Underscore rows:" + underscore.stringify(underscore.numRows, underscore.numCols))
        }
        val qr = QuickReport.fromCountTable(counts.rfilterIx(_.take(2) != "__").filterIx(x => conditions.contains(x)), lengths, spikeInPerSample, spikeInData, doPCA, pseudoCoverage, thresholdDropIfCoverageBelowInAllSamples, genestoplot, conditionsReverse, secondaryDimensionReverse, normalization, gmts, readLength)(log)

        val qif = qr.toFiles

        com.google.common.io.Files.move(qif.normalizedOnLibrary, new File(outTrunk + s"/normalizedOnLibrary.$name.csv"))
        com.google.common.io.Files.move(qif.rpkm, new File(outTrunk + s"/rpkm.$name.csv"))
        com.google.common.io.Files.move(qif.counts, new File(outTrunk + s"/counts.$name.csv"))
        com.google.common.io.Files.move(qif.normalizedOnLibraryAndTranscriptLength, new File(outTrunk + s"/normalizedOnLibraryAndTranscriptLength.$name.csv"))
        com.google.common.io.Files.move(qif.varianceStabilizedByLogNormalizedOnLibrary, new File(outTrunk + s"/varianceStabilizedByLogNormalizedOnLibrary.$name.csv"))
        com.google.common.io.Files.move(qif.totalReadsPerSample, new File(outTrunk + s"/totalReadsPerSample.$name.csv"))
        com.google.common.io.Files.move(qif.detectedGenesNormalizedOnLibrarySize, new File(outTrunk + s"/detectedGenesNormalizedOnLibrarySize.$name.txt"))

        com.google.common.io.Files.move(qif.meanVarPlotAfterVSTPerBP, new File(outTrunk + s"/meanVarPlotAfterVSTPerBP.$name.png"))
        com.google.common.io.Files.move(qif.meanVarPlotAfterVST, new File(outTrunk + s"/meanVarPlotAfterVST.$name.png"))
        com.google.common.io.Files.move(qif.readCountPerBPHistogramNormalizedOnLibraryDepth, new File(outTrunk + s"/readCountPerBPHistogramNormalizedOnLibraryDepthAndTranscriptLength.$name.png"))
        com.google.common.io.Files.move(qif.readCountPerBPHistogramExpressedNormalizedOnLibraryDepth, new File(outTrunk + s"/readCountPerBPHistogramExpressedNormalizedOnLibraryDepthAndTranscriptLength.$name.png"))

        com.google.common.io.Files.move(qif.sizeFactors, new File(outTrunk + s"/sizeFactors.$name.txt"))

        com.google.common.io.Files.move(qif.readCountRaw, new File(outTrunk + s"/readCountRaw.$name.png"))

        qif.detailedPlot.zipWithIndex.foreach { x =>
          com.google.common.io.Files.move(x._1, new File(outTrunk + s"/detailedPlot.$name.${x._2}.pdf"))
        }

        qif.geneClusters.foreach {
          case (title, plotf, gmt) =>
            plotf.zipWithIndex.foreach {
              case (pf, i) =>
                com.google.common.io.Files.move(pf, new File(outTrunk + s"/clusters.$title.$i.png"))
            }

            com.google.common.io.Files.move(gmt, new File(outTrunk + s"/clusters.$title.gmt"))
        }

        com.google.common.io.Files.move(qif.genewisepca, new File(outTrunk + s"/genewisepca.png"))

        qif.pcaplots.foreach {
          case (idx, (pcaplot, loadings, enrichments, projections, details)) =>
            com.google.common.io.Files.move(pcaplot, new File(outTrunk + s"/pcaplots.$name.$idx.pdf"))
            com.google.common.io.Files.move(loadings, new File(outTrunk + s"/pcaloadings.$name.$idx.csv"))
            com.google.common.io.Files.move(projections, new File(outTrunk + s"/pcaprojections.$name.$idx.csv"))
            enrichments.foreach {
              case (idx2, er, list, erplot) =>
                com.google.common.io.Files.move(er, new File(outTrunk + s"/pcaloadings.enrichment.$name.$idx.$idx2.txt"))
                com.google.common.io.Files.move(erplot, new File(outTrunk + s"/pcaloadings.enrichment.$name.$idx.$idx2.pdf"))
                com.google.common.io.Files.move(list, new File(outTrunk + s"/pcaloadings.top500Loadings.$name.$idx.$idx2.txt"))

            }
            details.foreach {
              case (idx2, f) =>
                com.google.common.io.Files.move(f, new File(outTrunk + s"/pca.top500loading.detail.$name.$idx.$idx2.pdf"))
            }
        }
        qif.versusPlotsNormalizedOnLibraryDepth.zipWithIndex.foreach {
          case (plot, idx) =>
            com.google.common.io.Files.move(plot, new File(outTrunk + s"/correlationPlotsNormalizedOnLibraryDepth.$name.$idx.png"))
        }

        qif.spikeInDiagnosticPlots.foreach {
          case (title, plots) =>
            plots.zipWithIndex.foreach {
              case (plot, idx) =>
                com.google.common.io.Files.move(plot, new File(outTrunk + s"/diagnostic.$name.$title.$idx.png"))

            }

        }

        qr

      }

      val rep = normalizations.foreach { n =>
        log(s"\nGenerating a quick report with PCA using normalization method $n")
        report(pooledCounts, n)
      }

      def rundeseq(grouping: Map[String, String], title: String, secondaryDimension: Map[String, String], pairwise: List[(String, String)]): Unit = {
        log(s"\nRunning $title DESEQ, then enrichment analysis. To calculate size factors and expressed genes: no pseudo count is added and genes with all samples below $thresholdDropIfCoverageBelowInAllSamples/$readLength per bp will be excluded. Only genes passing the former filter will be tested for DE. ")
        val deseqs = DEResultSet.fromCounts(pooledCounts, grouping, normalizations, pairwise, thresholdDropIfCoverageBelowInAllSamples, gmts, lengths, spikeInData, spikeInPerSample, readLength, secondaryDimension, conditionOrderOnPlots)

        deseqs.fulldes.foreach {
          case (name, de: DifferentialExpressionResultAsFile) =>
            log(s"DE using all factors with LRT: ${de.interpreted.significantGenes.size} genes with < 0.001 and log2 fold change >=2 .")
            de.copyFile(new File(outTrunk + s"/deseq.$title.$name.csv"))
        }
        deseqs.pairwisedes.foreach {
          case (name, list) =>
            list.foreach {
              case PairWiseDE(cond1, cond2, des, upregulated, downregulated, enrichments) =>
                log(s"Pairwise DE $cond1 vs $cond2 ${upregulated.genes.size} upregulated and ${downregulated.genes.size} downregulated genes (BH p < 0.001 and log2 fold change >= 2).")
                des.copyFile(new File(outTrunk + s"/deseq.$title.$name.$cond1.$cond2.csv"))
                com.google.common.io.Files.copy(upregulated.toFile, new File(outTrunk + s"/deseq.genelist.$title.$name.${upregulated.name}.txt"))
                com.google.common.io.Files.copy(downregulated.toFile, new File(outTrunk + s"/deseq.genelist.$title.$name.${downregulated.name}.txt"))
                enrichments.foreach {
                  case (erup, erdown) =>
                    com.google.common.io.Files.copy(erup.toFile, new File(outTrunk + s"/deseq.enrichment.$title.$name.${erup.name}.txt"))
                    com.google.common.io.Files.copy(erdown.toFile, new File(outTrunk + s"/deseq.enrichment.$title.$name.${erdown.name}.txt"))

                    com.google.common.io.Files.copy(EnrichmentResult.plotToFile(erup, erdown), new File(outTrunk + s"/deseq.enrichment.$title.$name.${erup.name}.pdf"))
                }
            }
        }
      }

      rundeseq(conditions, "primary", secondaryDimension, pairwiseDifferentialExpressionConditions1)
      rundeseq(secondaryDimension, "secondary", conditions, pairwiseDifferentialExpressionConditions2)

    }
  }
}
