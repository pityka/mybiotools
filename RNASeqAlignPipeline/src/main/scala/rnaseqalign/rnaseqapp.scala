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
import htseqcount._
import java.io.File
import mybiotools.config.Config.configInstance
import scala.collection.JavaConversions._
import org.saddle.Frame
import scala.concurrent.{ Future, Await }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import mybiotools.saddlehelpers._
import rnaseqalign.analysis._
import org.saddle._
import mybiotools._
import mybiotools.gwascommons.Region
import mybiotools.workflows._
import mybiotools.stringstore._

case class InputFileGroup(
    fastqs: List[File],
    referenceIndexFolder: List[File],
    adapter: String,
    starparameters: String,
    readgroup: ReadGroupContents,
    countGTF: File,
    countExonFeatureName: String8,
    countGene_idAttribute: String8,
    countTranscript_idAttribute: String8,
    countResolveLongestTranscript: Boolean,
    countMinQual: Int,
    countAllowMultiMapInSameGene: Boolean,
    countStrandedness: Strandedness,
    name: String,
    conditionGroup: String,
    secondaryDimension: String
) {

  override def toString = s"""InputFileGroup $name:
 |name: $name
 |condition: $conditionGroup
 |secondaryDimension: $secondaryDimension
 |fastq #: ${fastqs.size}
 |read group: $readgroup
 |mapping: 
 |  adapter: $adapter
 |  starparameters: $starparameters
 |  referenceIndexFolder: ${referenceIndexFolder.head.getParent}
 |counting:
 |  gtf: $countGTF
 |  countLongestTranscript: $countResolveLongestTranscript
 |  allowMultiMapInSameGene: $countAllowMultiMapInSameGene
 |  strandedness: $countStrandedness
 |  minQual: $countMinQual
 |fastqs: ${fastqs.map(_.getAbsolutePath).mkString(",")}
 |""".stripMargin

}

object InputFileGroupHelper {
  val starRefFolder =
    "chrLength.txt" ::
      "chrName.txt" ::
      "chrNameLength.txt" ::
      "chrStart.txt" ::
      "Genome" ::
      "genomeParameters.txt" ::
      "SA" ::
      "SAindex" ::
      "sjdbInfo.txt" ::
      "sjdbList.out.tab" :: Nil
}

object InputFileGroup {

  def fromConfigBatch(c: Config): List[InputFileGroup] = {
    import java.util.regex.Pattern

    val base = fromConfig(c)

    val regexpstring = c.getString("regex")
    val pattern = Pattern.compile(regexpstring)

    val fastqs: List[File] = openSource(c.getString("fastqlist"))(_.getLines.toList).map(x => new File(x))

    fastqs.map { f =>
      val matcher = pattern.matcher(f.getName)
      if (matcher.matches) {
        val condition = matcher.group("condition")
        val sample = matcher.group("sample")
        (f, condition, sample)
      } else {
        println(s"""Regular expression failed on ${f.getName} .
          |Input regex: $regexpstring .
          |You need to have 2 capture groups named 'condition', 'sample'.
          |Please use named capture groups. An example is "^(?<sample>\\d+)_?(?<condition>.*)_[^_]{12}_.*fastq.gz$$" 
          |Or consult the java.util.regex.Pattern javadoc.""".stripMargin)
        System.exit(1)
        (f, "", "")
      }

    }.groupBy(x => (x._2, x._3))
      .toList.map(x => x._1 -> x._2.map(_._1))
      .map {
        case ((condition, sample), list) =>

          val newconditiongroup = base.conditionGroup + condition
          val newsecondarydimension = base.secondaryDimension + sample
          val newName = base.name + (if (sample == "") newconditiongroup else newconditiongroup + "_" + newsecondarydimension)

          val newreadgroup = base.readgroup.copy(sm = (if (base.readgroup.sm == "") newName else base.readgroup.sm + "." + newName))

          base.copy(
            name = newName,
            conditionGroup = newconditiongroup,
            secondaryDimension = base.secondaryDimension + sample,
            readgroup = newreadgroup,
            fastqs = list
          )
      }

  }

  def fromConfig(c: Config): InputFileGroup = {
    val fastqs = c.getStringList("fastqs").map(x => new File(x)).toList
    val referenceIndexFolder = {
      val p = new File(c.getString("alignment.referenceFolder"))
      p.listFiles.filter(x => InputFileGroupHelper.starRefFolder.contains(x.getName)).map(x => new File(p, x.getName)).toList
    }

    val adapter = c.getString("adapterSequence")
    val starparameters = c.getString("alignment.extraStarParameters")
    val readgroup = {
      val cc = c.getConfig("readGroup")
      val cn = cc.getString("CN")
      val ds = cc.getString("DS")
      val dt = cc.getString("DT")
      val lb = cc.getString("LB")
      val pl = cc.getString("PL")
      val pu = ""
      val sm = cc.getString("SM")
      ReadGroupContents(cn, ds, dt, lb, pl, pu, sm)
    }

    val countGTF = new File(c.getString("htseqcount.GTF"))

    val countExonFeatureName = StringStore(c.getString("htseqcount.exonFeatureName"))
    val countGene_idAttribute = StringStore(c.getString("htseqcount.gene_idAttribute"))
    val countTranscript_idAttribute = StringStore(c.getString("htseqcount.transcript_idAttribute"))
    val countResolveLongestTranscript: Boolean = c.getBoolean("htseqcount.countLongestTranscript")
    val countMinQual = c.getInt("htseqcount.minQual")
    val countAllowMultiMapInSameGene = c.getBoolean("htseqcount.countAllowMultiMapInSameGene")
    val countStrandedness = c.getString("htseqcount.strandedness") match {
      case "notstranded" => NotStranded
      case "stranded" => Stranded
      case "reversestranded" => ReverseStranded
    }
    val name = c.getString("name")
    val conditionGroup = c.getString("conditionGroup")
    val secondaryDimension = c.getString("secondaryDimension")

    InputFileGroup(
      fastqs = fastqs,
      referenceIndexFolder = referenceIndexFolder,
      adapter = adapter,
      starparameters = starparameters,
      readgroup = readgroup,
      countGTF = countGTF,
      countExonFeatureName = countExonFeatureName,
      countGene_idAttribute = countGene_idAttribute,
      countTranscript_idAttribute = countTranscript_idAttribute,
      countResolveLongestTranscript = countResolveLongestTranscript,
      countMinQual = countMinQual,
      countAllowMultiMapInSameGene = countAllowMultiMapInSameGene,
      name = name,
      conditionGroup = conditionGroup,
      secondaryDimension = secondaryDimension,
      countStrandedness = countStrandedness
    )
  }
}

object RNASeqAlignApp extends App {
  val config = DefaultRNASeqConfig
  if (!config.dryRun) {
    new RNASeqAlign(defaultTaskSystem, config).run
  } else {
    println("RNASeq pipeline dry run. This will print the input data and quit.\n")
    println(config.inputgroups.mkString("\n"))
    println("Samples: " + config.inputgroups.map(_.name).distinct.sorted.mkString(", "))
    println("Conditions: " + config.inputgroups.map(_.conditionGroup).distinct.sorted.mkString(", "))
    println("Samples with spike-ins: \n" + config.inputgroups.map(_.name).map(x => x + "\t" + config.spikeInPerSample.get(x)).mkString("\n"))
  }
}

trait RNASeqConfig {
  def inputgroups: List[InputFileGroup]
  def spikeInData: Set[SpikeIn]
  def spikeInPerSample: Map[String, SpikeInMix]
  def dryRun: Boolean
  def starCPU: Int
  def starMemory: Int
  def mergeMemory: Int
  def countMemory: Int
  def pseudoCoverage: Double
  def thresholdDropIfCoverageBelowInAllSamples: Double
  def genestoplot: Seq[DetailedPlotDescription]
  def regionsToPlot: Seq[(String, Region)]
  def pairwiseDifferentialExpressionConditions1: List[(String, String)]
  def pairwiseDifferentialExpressionConditions2: List[(String, String)]
  def translateHGNC: Boolean
  def gmtFiles: List[File]
  def houseKeepingGenes: Set[String]
  def normalizations: Seq[NormalizationMethod]
  def readLength: Int
  def conditionOrderOnPlots: Seq[String]

}

object DefaultRNASeqConfig extends RNASeqConfig {
  private val defaultInputGroupConfig = {
    val str = """
    |#alignment.referenceFolder = ""
    |#adapterSequence = ""
    |alignment.extraStarParameters = ""
    |readGroup {
    |  CN = "UHTS-LGTF"
    |  #DS = ""
    |  #DT = ""
    |  #LB = ""
    |  PL = Illumina
    |  #SM = ""      
    |}
    |#htseqcount.GTF = ""
    |htseqcount.gene_idAttribute = "gene_id"
    |htseqcount.transcript_idAttribute = "transcript_id"
    |htseqcount.exonFeatureName = exon
    |htseqcount.countLongestTranscript = false
    |htseqcount.minQual = 10
    |htseqcount.allowMultiMapInSameGene = false
    |htseqcount.strandedness = notstranded
    |#name = ""
    |#conditionGroup = ""
    |secondaryDimension = ""
    |regex = "^(?<sample>\\d+)_?(?<condition>.*)_[^_]{12}_(?<lane>L\\d+).*fastq.gz$"
    |fastqs = []
    """.stripMargin
    ConfigFactory.parseString(str)
  }
  val inputgroups: List[InputFileGroup] = {

    val list2 = configInstance.getConfigList("rnaseq.inputgroups").zipWithIndex.map {
      case ((c: Config, idx: Int)) =>
        val str = s"""
      |conditionGroup = $idx
      """.stripMargin
        InputFileGroup.fromConfig(c.withFallback(defaultInputGroupConfig.withFallback(ConfigFactory.parseString(str))))
    }

    val list3 = configInstance.getConfigList("rnaseq.inputgroupsBatch").zipWithIndex.flatMap {
      case ((c: Config, idx: Int)) =>

        InputFileGroup.fromConfigBatch(c.withFallback(defaultInputGroupConfig.withFallback(ConfigFactory.parseString("""
          |readGroup.SM=""
          |conditionGroup = ""
          |name = "" """.stripMargin))))
    }

    list2.toList ++ list3.toList

  }
  assert(inputgroups.map(_.name).toSet.size == inputgroups.size, "Non unique names of inputfilegroups (libraries). " + inputgroups.map(_.name))

  val spikeInPerSample: Map[String, SpikeInMix] = {
    val f = configInstance.getString("rnaseq.spikeInPerSample")
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
    val f = configInstance.getString("rnaseq.spikeInData")
    if (f != "") openSource(f)(_.getLines.drop(1).map { line =>
      val spl = fastSplitSetSeparator(line, Set(','))
      val id = spl(1)
      val conc1 = spl(3).toDouble
      val conc2 = spl(4).toDouble
      SpikeIn(id, conc1, conc2)
    }.toSet)
    else Set()
  }

  val pairwiseDifferentialExpressionConditions1: List[(String, String)] = configInstance.getStringList("rnaseq.pairwiseDE1").toList.grouped(2).map(x => x(0) -> x(1)).filter(x => inputgroups.map(_.conditionGroup).contains(x._1) && inputgroups.map(_.conditionGroup).contains(x._2)).toList
  val pairwiseDifferentialExpressionConditions2: List[(String, String)] = configInstance.getStringList("rnaseq.pairwiseDE2").toList.grouped(2).map(x => x(0) -> x(1)).filter(x => inputgroups.map(_.secondaryDimension).contains(x._1) && inputgroups.map(_.secondaryDimension).contains(x._2)).toList

  val starCPU = configInstance.getInt("rnaseq.star.CPU")
  val starMemory = configInstance.getInt("rnaseq.star.RAM")
  val mergeMemory = configInstance.getInt("rnaseq.mergebam.RAM")
  val countMemory = configInstance.getInt("rnaseq.htseqcount.RAM")
  val dryRun = configInstance.getBoolean("rnaseq.dryrun")
  val pseudoCoverage = configInstance.getDouble("rnaseq.pseudoCoverage")
  val thresholdDropIfCoverageBelowInAllSamples = configInstance.getDouble("rnaseq.thresholdDropIfCoverageBelowInAllSamples")
  val genestoplot = configInstance.getConfigList("rnaseq.genesToPlot").toList.map { c =>
    val axis = c.getInt("xaxis")
    val genes = c.getStringList("genes").toList
    val title = c.getString("main")
    DetailedPlotDescription(genes, axis, title)
  }
  val regionsToPlot = configInstance.getStringList("rnaseq.regionsToPlot").toList.grouped(4).map { g =>
    val name = g(0)
    val chr = g(1)
    val from = g(2).toInt
    val to = g(3).toInt
    name -> Region(chr, from - 1, to)
  }.toList
  val translateHGNC = configInstance.getBoolean("rnaseq.translateHGNC")
  val gmtFiles: List[File] = configInstance.getStringList("rnaseq.gmtFiles").map(x => new File(x)).toList
  val houseKeepingGenes: Set[String] = {
    val f = configInstance.getString("rnaseq.houseKeepingGenes")
    if (f == "-" || f == "") Set[String]()
    else openSource(f)(_.getLines.toSet)
  }
  val normalizations: Seq[NormalizationMethod] = configInstance.getStringList("rnaseq.normalizationMethods").map(_.toLowerCase match {
    case "deseq" => Some(DeSeqNormalization)
    case "spikein" if !spikeInPerSample.isEmpty => Some(SpikeInNormalization)
    case "housekeepinggenes" if !houseKeepingGenes.isEmpty => Some(HouseKeepingNormalization(houseKeepingGenes))
    case "no" => Some(NoDepthNormalization)
    case "librarysize" => Some(LibrarySizeNormalization)
    case "ruvghousekeepinggenes" if !houseKeepingGenes.isEmpty => Some(RuvGNormalization(houseKeepingGenes))
    case "ruvgspikeins" => Some(RuvGNormalizationWithSpikeIns)
    case _ => None
  }).filter(_.isDefined).map(_.get)
  val readLength = configInstance.getInt("rnaseq.readLength")
  val conditionOrderOnPlots = configInstance.getStringList("rnaseq.conditionOrderOnPlots").toList

}

class RNASeqAlign(ts: TaskSystem, rnaseqConfig: RNASeqConfig) {

  def run: Unit = {
    import ts._
    import rnaseqConfig._
    implicit val fs = components.fs
    implicit val actorsystem = components.actorsystem
    if (ts.hostConfig.myRole == MASTER) {
      val log = ts.getLogger(this)

      def quickreportToSharedFiles(data: Frame[String, String, Long], lengths: Map[String, Int], normalization: NormalizationMethod, spikeIns: Map[String, SpikeInMix], gmts: Seq[GeneSetDB], conditionsReverse: Seq[(String, Set[String])], secondaryDimensionReverse: Seq[(String, Set[String])]) = {
        import ts._
        implicit val fs = components.fs
        implicit val actorsystem = components.actorsystem
        import rnaseqConfig._

        val prefix = normalization.name

        val qr = analysis.QuickReport.fromCountTable(data, lengths, spikeIns, spikeInData, true, pseudoCoverage, thresholdDropIfCoverageBelowInAllSamples, genestoplot, conditionsReverse, secondaryDimensionReverse, normalization, gmts, readLength)(log.info)

        val qif = qr.toFiles

        val files = (
          SharedFile(qif.normalizedOnLibrary, name = s"analysis.normalizedOnLibrary.$prefix.csv", canMoveAway = true) ::
          SharedFile(qif.rpkm, name = s"analysis.rpkm.$prefix.csv", canMoveAway = true) ::
          SharedFile(qif.counts, name = s"analysis.counts.$prefix.csv", canMoveAway = true) ::
          SharedFile(qif.normalizedOnLibraryAndTranscriptLength, name = s"analysis.normalizedOnLibraryAndTranscriptLength.$prefix.csv", canMoveAway = true) ::
          SharedFile(qif.varianceStabilizedByLogNormalizedOnLibrary, name = s"analysis.varianceStabilizedByLogNormalizedOnLibrary.$prefix.csv", canMoveAway = true) ::
          SharedFile(qif.totalReadsPerSample, name = s"analysis.totalReadsPerSample.$prefix.csv", canMoveAway = true) ::
          SharedFile(qif.detectedGenesNormalizedOnLibrarySize, name = s"analysis.detectedGenesNormalizedOnLibrarySize.$prefix.txt", canMoveAway = true) ::
          SharedFile(qif.meanVarPlotAfterVSTPerBP, name = s"analysis.meanVarPlotAfterVSTPerBP.$prefix.png", canMoveAway = true) ::
          SharedFile(qif.meanVarPlotAfterVST, name = s"analysis.meanVarPlotAfterVST.$prefix.png", canMoveAway = true) ::
          SharedFile(qif.readCountPerBPHistogramNormalizedOnLibraryDepth, name = s"analysis.readCountPerBPHistogramNormalizedOnLibraryDepthAndTranscriptLength.$prefix.png", canMoveAway = true) ::
          SharedFile(qif.readCountPerBPHistogramExpressedNormalizedOnLibraryDepth, name = s"analysis.readCountPerBPHistogramExpressedNormalizedOnLibraryDepthAndTranscriptLength.$prefix.png", canMoveAway = true) ::
          SharedFile(qif.sizeFactors, name = s"analysis.sizeFactors.$prefix.txt", canMoveAway = true) ::
          SharedFile(qif.readCountRaw, name = s"analysis.readCountRaw.$prefix.png", canMoveAway = true) ::
          SharedFile(qif.houseKeepingGenes, name = s"analysis.housekeepingenes.$prefix.txt", canMoveAway = true) ::
          SharedFile(qif.genewisepca, name = s"analysis.genewisepca.png", canMoveAway = true) ::
          qif.spikeInDiagnosticPlots.flatMap {
            case (name, plots) =>
              plots.zipWithIndex.map {
                case (plot, idx) =>
                  SharedFile(plot, name = s"analysis.diagnostic.$prefix.$name.$idx.pdf", canMoveAway = true)
              }
          }.toList :::
          qif.geneClusters.map {
            case (title, plotf, gmt) =>

              SharedFile(gmt, name = s"analysis.clusters.$title.gmt", canMoveAway = true) ::
                plotf.zipWithIndex.toList.map {
                  case (pf, i) =>
                    SharedFile(pf, name = s"analysis.clusters.$title.$i.png", canMoveAway = true)
                }
          }
          :::
          qif.pcaplots.flatMap {
            case ((idx, (pcaplot, loadings, enrichments, projections, detailedplots))) =>
              SharedFile(pcaplot, name = s"analysis.pcaplot.$prefix.$idx.pdf", canMoveAway = true) ::
                SharedFile(projections, name = s"analysis.pcaprojections.$prefix.$idx.pdf", canMoveAway = true) ::
                SharedFile(loadings, name = s"analysis.pcaloadings.$prefix.$idx.csv", canMoveAway = true) ::
                (enrichments.flatMap {
                  case (idx2, erf, list, erplot) =>
                    SharedFile(erf, name = s"analysis.pcaloadings.enrichments.$prefix.$idx.$idx2.csv", canMoveAway = true) ::
                      SharedFile(erplot, name = s"analysis.pcaloadings.enrichments.$prefix.$idx.$idx2.pdf", canMoveAway = true) ::
                      SharedFile(list, name = s"analysis.pcaloadings.top500Loadings.$prefix.$idx.$idx2.csv", canMoveAway = true) :: Nil
                }.toList) :::
                (detailedplots.map {
                  case (idx2, plot) =>
                    SharedFile(plot, name = s"analysis.pca.top500loading.detail.$prefix.$idx.$idx2.pdf", canMoveAway = true)
                }.toList)
          }.toList ::: qif.versusPlotsNormalizedOnLibraryDepth.zipWithIndex.map {
            case (plot, idx) =>
              SharedFile(plot, name = s"analysis.correlationPlotsNormalizedOnLibraryDepth.$prefix.$idx.png", canMoveAway = true)
          }
        ) ::: qif.detailedPlot.toList.zipWithIndex.map { f =>
            SharedFile(f._1, name = s"analysis.detailedPlot.$prefix.${f._2}.pdf", canMoveAway = true)
          }

        (files, qr)

      }

      ts.components.registerApplicationFileLogger(new File(configInstance.getString("tasks.fileServiceBaseFolder") + "/logfile"))

      log.info("Badge:\n" + (mybiotools.config.Config.prettyPrintVersion(rnaseqalign.Reflected.name, rnaseqalign.Reflected.version)))
      log.info("Number of input groups: " + inputgroups.size)
      log.info("List of input files:\n" + inputgroups.mkString("\n"))
      log.info("Samples with spike-ins: \n" + inputgroups.map(_.name).map(x => x + "\t" + spikeInPerSample.get(x)).mkString("\n"))

      log.info("Detailed plots: " + genestoplot.mkString("\n"))

      val gmts: Seq[GeneSetDB] = gmtFiles.map(x => GeneSetDB(x.getName, openSource(x)(s => mybiotools.readGMT(s).toSeq.map(x => GeneSet(x._1.value, x._2.toSet.map((y: mybiotools.stringstore.String8) => y.value))))))

      // val readcountplotmultipletask = readcountplotmultiple(ReadCountMultipleInput(regionsToPlot.toList, inputgroups.head.countStrandedness, "allreadcounts", inputgroups.size))

      val groupwithtasks = inputgroups.map { group =>

        val mergetask = mergeUnsortedBams(
          in = MergeBamInput(group.fastqs.size, outname = group.name + ".aligned"),
          memory = mergeMemory
        )

        val sorttask = sortbam(SortBamInput.empty, memory = mergeMemory)
        val indextask = indexbam(IndexBamInput.empty, memory = mergeMemory)
        val bambaiaggregatortask = bambaiaggregator(BamBaiInput.empty)
        val readcountplottask = readcountplot(ReadCountInput(regionsToPlot.toList, group.countStrandedness))

        mergetask ~> sorttask ~> indextask
        sorttask ~> bambaiaggregatortask
        indextask ~> bambaiaggregatortask
        bambaiaggregatortask ~> readcountplottask

        val countTask = htseqcountTask(
          in = HTSeqInput(
            gtf = group.countGTF,
            gtfparam = GTFFileParameters(
              Transcript_id = group.countTranscript_idAttribute,
              Gene_id = group.countGene_idAttribute,
              Exon = group.countExonFeatureName,
              resolveLongestTranscript = group.countResolveLongestTranscript
            ),
            strandedness = group.countStrandedness,
            minQual = group.countMinQual,
            allowMultiMapInSameGene = group.countAllowMultiMapInSameGene,
            columnName = group.name
          ),
          memory = countMemory
        )

        val aligntask = group.fastqs.map { fastq =>

          val input = AlignRNASeqInput(
            fastq = fastq,
            refIndex = group.referenceIndexFolder,
            adapter = group.adapter,
            readgroup = group.readgroup,
            starparameters = group.starparameters
          )

          val t = fastq2bam(input, cpu = starCPU, memory = starMemory)

          t ~> mergetask

          t
        }

        mergetask ~> countTask

        (group, countTask, readcountplottask, bambaiaggregatortask)

      }

      val htseqcounts: Future[Seq[(InputFileGroup, HTSeqCounts, BamWithBai)]] = Future.sequence(groupwithtasks.map {
        case (group, countTask, _, bambaiaggregatortask) =>
          countTask.?[HTSeqCounts].flatMap(x => bambaiaggregatortask.?[BamWithBai].map(y => (group, x, y)))
      })

      val readcountplots: Future[Seq[ReadCountPlots]] = Future.sequence(groupwithtasks.map(_._3.?[ReadCountPlots]))

      val joined = htseqcounts.map {
        // (ConditionGroup,sampleGroup,counts)
        (countsWithGroups: Seq[(InputFileGroup, HTSeqCounts, BamWithBai)]) =>

          import org.saddle.io.CsvImplicits._

          val conditions = countsWithGroups.map(x => x._1.name -> x._1.conditionGroup).toMap.filter(_._2 != "")
          val secondaryDimension = countsWithGroups.map(x => x._1.name -> x._1.secondaryDimension).toMap.filter(_._2 != "")
          val conditionsReverse = reverseMap(conditions).toSeq.sortBy(x => conditionOrderOnPlots.indexOf(x._1))
          val secondaryDimensionReverse = reverseMap(secondaryDimension).toSeq.sortBy(x => conditionOrderOnPlots.indexOf(x._1))

          val (ensembletable, ensemblereversetable) = if (translateHGNC) Helpers.readEnsembleHGNCTable else (Map[String, String](), Map[String, Int]())

          val geneLengths = countsWithGroups.map(_._2).head.lengths.map(x => Helpers.translateHGNC(x._1, ensembletable, ensemblereversetable) -> x._2)

          val joined: Frame[String, String, Long] = {
            val raw = countsWithGroups.map(_._2).map(x => envelopeToFrame(x.counts)).reduce((x, y) => x.rconcat(y, org.saddle.index.OuterJoin)).map {
              case (r, c, v) =>
                (r, c, if (org.saddle.scalar.ScalarTagLong.isMissing(v)) 0L else v)
            }.mapRowIndex(rix => Helpers.translateHGNC(rix, ensembletable, ensemblereversetable))

            val allcountsFile = mybiotools.TempFile.createTempFile(".csv")
            raw.writeCsvFile(allcountsFile.getAbsolutePath, withColIx = true, withRowIx = true)
            SharedFile(allcountsFile, name = "allcounts.txt")
            log.info(s"""Written joined raw counts file with ${raw.numRows} rows and ${raw.numCols} columns. Columns: ${raw.colIx.toSeq.mkString(",")}""")
            raw.rfilterIx(_.take(2) != "__").filterIx(x => conditions.contains(x))

          }

          log.info(s"\nColumns in the allcounts.txt file:\n\t" + joined.colIx.toSeq.sorted.mkString("\n\t"))
          log.info("\nConditions:\n" + conditions.toSeq.groupBy(_._2).map(x => "\t" + x._1 + "\n" + x._2.map(_._1).mkString("\t\t", "\n\t\t", "")).mkString("\n"))
          log.info("\nSecondary dimension (e.g. individual, cell type, etc):\n" + secondaryDimension.toSeq.groupBy(_._2).map(x => "\t" + x._1 + "\n" + x._2.map(_._1).mkString("\t\t", "\n\t\t", "")).mkString("\n"))

          val (quickReportNogroupFiles, quickReportsNogroup) = {
            val (files, reports) = normalizations.map { n =>
              quickreportToSharedFiles(joined, geneLengths, n, spikeInPerSample, gmts, conditionsReverse, secondaryDimensionReverse)
            }.unzip
            (files.flatten, reports)
          }
          log.info(s"""Written 'quick' report files.""")

          {
            val deseqSizeFactors = getDESeqSizeFactors(joined).toSeq.toMap
            val bambaifiles = countsWithGroups.map(x => (x._1.conditionGroup, x._1.secondaryDimension, x._3.bam.file, x._3.bai.file, deseqSizeFactors(x._1.name)))
            regionsToPlot.map {
              case (name, region) =>

                {
                  val plot = Helpers.readCountPlotFromMultipleBamFiles(bambaifiles.map(x => (x._3, x._4, x._5)), countsWithGroups.head._1.countStrandedness, region)
                  val file = TempFile.createTempFile(".pdf")
                  writeBinaryToFile(
                    file.getAbsolutePath,
                    mybiotools.plots.renderToByteArray(plot, "application/pdf", 1)
                  )
                  SharedFile(file, name = s"analysis.allsamplereadcounts.$name.pdf", canMoveAway = true)
                }

                {
                  conditionsReverse.map {
                    case (condition, _) =>
                      val plot = Helpers.readCountPlotFromMultipleBamFiles(bambaifiles.filter(_._1 == condition).map(x => (x._3, x._4, x._5)), countsWithGroups.head._1.countStrandedness, region)
                      val file = TempFile.createTempFile(".pdf")
                      writeBinaryToFile(
                        file.getAbsolutePath,
                        mybiotools.plots.renderToByteArray(plot, "application/pdf", 1)
                      )
                      SharedFile(file, name = s"analysis.readcounts.primary$condition.$name.pdf", canMoveAway = true)
                  }
                }

                {
                  secondaryDimensionReverse.map {
                    case (secondaryDimension, _) =>
                      val plot = Helpers.readCountPlotFromMultipleBamFiles(bambaifiles.filter(_._2 == secondaryDimension).map(x => (x._3, x._4, x._5)), countsWithGroups.head._1.countStrandedness, region)
                      val file = TempFile.createTempFile(".pdf")
                      writeBinaryToFile(
                        file.getAbsolutePath,
                        mybiotools.plots.renderToByteArray(plot, "application/pdf", 1)
                      )
                      SharedFile(file, name = s"analysis.readcounts.secondary$secondaryDimension.$name.pdf", canMoveAway = true)
                  }
                }
            }
            log.info("Written read count plot.")
          }

          def rundeseq(grouping: Map[String, String], title: String, secondaryDimension: Map[String, String], pairwise: List[(String, String)]) = {

            val deseqs = DEResultSet.fromCounts(joined, grouping, normalizations, pairwise, thresholdDropIfCoverageBelowInAllSamples, gmts, geneLengths, spikeInData, spikeInPerSample, readLength, secondaryDimension, conditionOrderOnPlots)

            val s1 = deseqs.fulldes.flatMap {
              case (name, de) =>
                log.info(s"DE $title using all factors with LRT: ${de.interpreted.pValues.countif(_ < 0.001)} genes with < 0.001 .")
                SharedFile(de.file, name = s"analysis.deseq.$title.$name.csv", canMoveAway = true) ::
                  de.plots.map {
                    case (gene, file) =>
                      SharedFile(file, name = s"analysis.deseq.$title.detailedplot.$name.$gene.pdf", canMoveAway = true)
                  }.toList
            }
            val s2 = deseqs.pairwisedes.flatMap {
              case (name, list) =>
                list.map {
                  case PairWiseDE(cond1, cond2, des, upregulated, downregulated, enrichments) =>
                    log.info(s"Pairwise DE $cond1 vs $cond2 ${upregulated.genes.size} upregulated and ${downregulated.genes.size} downregulated genes (BH p < 0.001 and log2 fold change >= 2).")
                    SharedFile(des.file, name = s"analysis.deseq.$title.$name.$cond1.$cond2.csv", canMoveAway = true) ::
                      SharedFile(upregulated.toFile, name = s"analysis.deseq.$title.$name.genelist.${upregulated.name}.txt") ::
                      SharedFile(downregulated.toFile, name = s"analysis.deseq.$title.$name.genelist.${downregulated.name}.txt") ::
                      enrichments.toList.flatMap {
                        case (erup, erdown) =>
                          SharedFile(erup.toFile, name = s"analysis.enrichment.$title.$name.${erup.name}.txt", canMoveAway = true) ::
                            SharedFile(erdown.toFile, name = s"analysis.enrichment.$title.$name.${erdown.name}.txt", canMoveAway = true) ::
                            SharedFile(EnrichmentResult.plotToFile(erup, erdown), name = s"analysis.enrichment.$title.$name.${erdown.name}.pdf", canMoveAway = true) ::
                            Nil
                      }
                }

            }

            s1 ++ s2
          }
          val deseq = rundeseq(conditions, "primary", secondaryDimension, pairwiseDifferentialExpressionConditions1) ++ rundeseq(secondaryDimension, "secondary", conditions, pairwiseDifferentialExpressionConditions2)

          (joined +: List()) ++ quickReportNogroupFiles ++ deseq

      }

      Await.result(Future.sequence(joined :: readcountplots :: Nil), atMost = 168 hours)
      ts.shutdown

    }

  }
}
