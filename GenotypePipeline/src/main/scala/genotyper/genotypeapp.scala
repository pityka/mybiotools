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

package genotyper

import genotyper.tasks._
import mybiotools.config.Config.configInstance
import mybiotools.tasks._
import mybiotools._
import mybiotools.workflows._
import mybiotools.gwascommons._
import com.typesafe.config._
import scala.util.Try
import java.io.File
import scala.collection.JavaConversions._
import scala.concurrent.{ Future, Await }
import scala.concurrent.duration._
import mybiotools.workflows.{ MakeBedOut, BimFile, BedFile, FamFile }
import htsjdk.tribble.index.IndexFactory
import java.util.regex.Pattern
import htsjdk.tribble.index.IndexFactory.IndexBalanceApproach._
import mybiotools.stringstore._

case class InputFastQGroup(
    fastqs: List[File],
    readgroup: ReadGroupContents
) {

  override def toString = s"""InputFileGroup ${readgroup.sm}:
 |sample: ${readgroup.sm} 
 |fastq #: ${fastqs.size}
 |read group: $readgroup 
 |fastqs: ${fastqs.map(_.getAbsolutePath).mkString(",")}
 |""".stripMargin

}

object InputFastQGroup {
  def fromLine(l: String): InputFastQGroup = {
    val spl = fastSplitSetSeparatorIterator(l, Set(' ', '\t'))
    val sample = spl.next
    val library = spl.next
    val platform = spl.next
    val center = spl.next
    val files = spl.toList.map(f => new File(f))
    val flowcellinfos = files.map { file =>
      val flowcellinfos = openFileReader(file) { br =>
        rnaseqalign.Helpers.getFlowCellInfosFromFastQHeader(br)
      }
      assert(flowcellinfos.map(x => (x.flowcell, x.lane)).toSet.size == 1, "Different flowcells or lanes in 1 fastq. " + flowcellinfos)
      flowcellinfos
    }
    assert(flowcellinfos.flatMap(_.map(x => (x.flowcell, x.lane))).toSet.size == 1)
    val fc = flowcellinfos.head.head.flowcell
    val lane = flowcellinfos.head.head.lane

    val pu = fc + "." + lane
    val rg = ReadGroupContents(
      cn = center,
      ds = "",
      dt = "",
      lb = library,
      pl = platform,
      pu = pu,
      sm = sample
    )
    InputFastQGroup(files, rg)
  }

  def fromFiles(files: Set[File], extractorRegexp: String): List[InputFastQGroup] = {
    val byname = files.map(x => x.getName -> x).toMap
    val pattern = Pattern.compile(extractorRegexp)
    byname.toList.map {
      case (name, file) =>
        val matcher = pattern.matcher(name)
        if (matcher.matches) {
          val sample = matcher.group("sample")
          val lane = matcher.group("lane")
          val readpair = matcher.group("readpair")
          val chunk = matcher.group("chunk")
          Some((sample, lane, readpair, chunk, file))
        } else None
    }.filter(_.isDefined).map(_.get).groupBy(x => (x._1)).map(x => x._2).toList.map { filesOfSample =>
      // val flowcellinfos = filesOfSample.map {
      //   case (_, _, _, _, file) =>
      //     val flowcellinfos = openFileReader(file) { br =>
      //       rnaseqalign.Helpers.getFlowCellInfosFromFastQHeader(br)
      //     }
      //     println(flowcellinfos)
      //     assert(flowcellinfos.map(x => (x.flowcell, x.lane)).toSet.size == 1, "Different flowcells or lanes in 1 fastq. " + flowcellinfos)
      //     flowcellinfos
      // }
      // assert(flowcellinfos.flatMap(_.map(x => (x.flowcell, x.lane))).toSet.size == 1)
      // val fc = flowcellinfos.head.head.flowcell
      // val lane = flowcellinfos.head.head.lane

      val sample = filesOfSample.head._1

      val readgroup = ReadGroupContents(
        cn = "",
        ds = "",
        dt = "",
        lb = sample,
        pl = "",
        pu = "", //fc + "." + lane,
        sm = sample
      )

      val filesInOrder: List[File] = filesOfSample.groupBy(_._4).map(_._2.toSeq.sortBy(_._3)).toSeq.flatten.map(_._5).toList

      InputFastQGroup(filesInOrder, readgroup)

    }

  }
}

case class BamFileConfig(file: File, index: Option[File], sorted: Boolean, hasChr: Boolean, needsDeduplication: Boolean)
object BamFileConfig {
  def fromConfig(config: Config): BamFileConfig = {
    val file = config.getString("bam")
    val hasChr = Try(config.getBoolean("haschr")).toOption.getOrElse { AddChrToBamHelper.hasChr(new File(file)) }
    val sorted = Try(config.getBoolean("sorted")).toOption.getOrElse { SortHelper.isSorted(new File(file)) }
    val index = if (!sorted) None else Try { config.getString("bai") }.toOption
    val needsDeduplication = config.getBoolean("needsDeduplication")
    BamFileConfig(new File(file), index.map(x => new File(x)), sorted, hasChr, needsDeduplication)
  }

  // bam [bai] [sorted] [haschr] [needsdedup]
  def fromLine(line: String): BamFileConfig = {
    val spl = fastSplitSetSeparator(line, Set(' ', '\t'))
    val file = spl(0)
    val hasChr = if (spl.size >= 4) Try(spl(3).toBoolean).toOption.getOrElse(AddChrToBamHelper.hasChr(new File(file))) else AddChrToBamHelper.hasChr(new File(file))
    val sorted = if (spl.size >= 3) Try(spl(2).toBoolean).toOption.getOrElse(SortHelper.isSorted(new File(file))) else true

    val needsDeduplication = if (spl.size >= 5) Try(spl(4).toBoolean).toOption.getOrElse(true) else true

    val bai = if (!sorted || spl.size == 1) None else Try { new File(spl(1)) }.toOption.filter(_.canRead)
    BamFileConfig(new File(file), bai, sorted, hasChr, needsDeduplication)
  }
}

object GenotypingApp extends App {

  val ts = defaultTaskSystem
  import ts._
  implicit val fs = components.fs
  implicit val actorsystem = components.actorsystem

  if (ts.hostConfig.myRole == MASTER) {
    val log = ts.getLogger(this)
    ts.registerApplicationFileLogger(new File(configInstance.getString("tasks.fileServiceBaseFolder") + "/logfile"))

    log.info("Badge:\n" + (mybiotools.config.Config.prettyPrintVersion("GenotypePipeline", genotyper.Reflected.version)))
    log.info(mybiotools.config.Config.prettyPrint(Some("genotyper")))

    implicit val ec = mybiotools.concurrent.newExecutionContext("GenotypingApp", 20)

    val bamfiles: List[BamFileConfig] = {
      val listfile = configInstance.getString("genotyper.bamListFile")
      val list1 = if (listfile == "") Nil
      else openSource(listfile)(_.getLines.map(l => BamFileConfig.fromLine(l)).toList)

      val list2 = configInstance.getConfigList("genotyper.bams").map(BamFileConfig.fromConfig)

      list1.toList ++ list2.toList

    }

    val bamfilesForRemapping: List[File] = {
      val listfile = configInstance.getString("genotyper.remapbamListFile")
      val list1 = if (listfile == "") Nil
      else openSource(listfile)(_.getLines.map(l => new File(l)).toList)

      val list2 = configInstance.getStringList("genotyper.remapbams").map(l => new File(l))

      list1.toList ++ list2.toList
    }

    val fastqs: List[InputFastQGroup] = {
      val listfile = configInstance.getString("genotyper.fastqtable")
      val fromtable = if (listfile != "") openSource(listfile)(_.getLines.map(InputFastQGroup.fromLine).toList) else Nil
      val fromfileslist = configInstance.getString("genotyper.fastqs")
      val regexp = configInstance.getString("genotyper.fastqregexp")
      log.info("Get flow cell information from fastq files for read group..")

      val fromfiles = if (fromfileslist != "") InputFastQGroup.fromFiles(openSource(fromfileslist)(_.getLines.toList).map(l => new File(l)).toSet, regexp) else Nil
      fromtable ++ fromfiles
    }

    log.info("List of input bam files (no mapping):\n" + bamfiles.mkString("\n"))
    log.info("List of input bam files (mapping):\n" + bamfilesForRemapping.mkString("\n"))
    log.info("List of input fastq files (mapping):\n" + fastqs.mkString("\n"))

    val haplotypercallermaxintervals: Int = configInstance.getInt("genotyper.maxChunkSize")

    val bedfile = configInstance.getString("genotyper.bedFile")

    val preVQSRBedFile = configInstance.getString("genotyper.variantFiltration.bed")

    val prevqsrextraarg = configInstance.getString("genotyper.variantFiltration.extraArg")

    val prevqsrmemory = configInstance.getInt("genotyper.variantFiltration.RAM")

    val intervals: Iterable[List[Region]] = {
      if (bedfile == "-") List(Nil) else {
        val regions = io.Source.fromFile(bedfile).getLines.dropWhile { l =>
          val spl = mybiotools.fastSplitSeparator(l, '\t')
          spl.size < 2 || Try(spl(1).toInt).toOption.isEmpty
        }.map(x => Region.fromLine(x)).toList
        if (haplotypercallermaxintervals > 0) RegionSet[String8, GenomicLocation, Region](regions).chunkToMaxSize(haplotypercallermaxintervals)
        else List(regions)
      }
    }

    val preVQSRIntervals: List[Region] =
      Region.collapse[String8, GenomicLocation, Region](
        io.Source.fromFile(preVQSRBedFile).getLines.dropWhile { l =>
        val spl = mybiotools.fastSplitSeparator(l, '\t')
        spl.size < 2 || Try(spl(1).toInt).toOption.isEmpty
      }.map(x => Region.fromLine(x)).toList,
        0
      ).toList

    log.info("Chunked to " + intervals.size + " intervals.")

    if (bedfile != "-") {
      val intervalsize = intervals.map(_.map(_.size).sum).sum
      val bedsize = Region.collapse[String8, GenomicLocation, Region](
        io.Source.fromFile(bedfile).getLines.dropWhile { l =>
        val spl = mybiotools.fastSplitSeparator(l, '\t')
        spl.size < 2 || Try(spl(1).toInt).toOption.isEmpty
      }.map(x => Region.fromLine(x)).toList, 0
      ).map(_.size).sum
      assert(intervalsize == bedsize, s" $intervalsize != $bedsize ")
    }

    val referenceFasta = new File(configInstance.getString("genotyper.referenceFasta"))
    val referenceFai = new File(configInstance.getString("genotyper.referenceFai"))
    val referenceDict = new File(configInstance.getString("genotyper.referenceDict"))
    val referenceHasChrPrefix = configInstance.getBoolean("genotyper.referenceHasChrPrefix")

    val resourcesConfigsForSNPRecalibration = configInstance.getConfigList("genotyper.vqsr.snpresources").map(c => VQSRResource.fromConfig(c))

    val resourcesConfigsForIndelRecalibration = configInstance.getConfigList("genotyper.vqsr.indelresources").map(c => VQSRResource.fromConfig(c))

    val dedupjar = new File(configInstance.getString("genotyper.picardDeduplicateJar"))

    val gatkjar = new File(configInstance.getString("genotyper.gatkjar"))

    val dbsnpvcfBefore1kg = new File(configInstance.getString("genotyper.dbsnpvcfBefore1kg"))

    val dbsnpvcfRecent = new File(configInstance.getString("genotyper.dbsnpannotation.vcf"))

    val dbsnpannotationmemory = configInstance.getInt("genotyper.dbsnpannotation.RAM")

    val deduplicatememory = configInstance.getInt("genotyper.dedup.RAM")

    val addchrtobammemory = configInstance.getInt("genotyper.addchr.RAM")

    val sortmemory = configInstance.getInt("genotyper.sortbam.RAM")

    val indexmemory = configInstance.getInt("genotyper.indexbam.RAM")

    val hcmemory = configInstance.getInt("genotyper.haplotypecaller.RAM")

    val hccpu = configInstance.getInt("genotyper.haplotypecaller.CPU")

    val hcextraargs = configInstance.getString("genotyper.haplotypecaller.extraArgs")

    val genotypeGVCFMemory = configInstance.getInt("genotyper.genotypeGVCF.RAM")

    val genotypeGVCFCPU = configInstance.getInt("genotyper.genotypeGVCF.CPU")

    val minBadVariantsIndel = configInstance.getInt("genotyper.vqsr.minBadVariantsIndel")
    val minBadVariantsSNP = configInstance.getInt("genotyper.vqsr.minBadVariantsSNP")

    val maxGaussiansSNP = configInstance.getInt("genotyper.vqsr.maxGaussiansSNP")
    val maxGaussiansIndel = configInstance.getInt("genotyper.vqsr.maxGaussiansIndel")

    val evaluatememory = configInstance.getInt("genotyper.eval.RAM")
    val evaluatecpu = configInstance.getInt("genotyper.eval.CPU")

    val vqsrmemory = configInstance.getInt("genotyper.vqsr.RAM")
    val vqsrcpu = configInstance.getInt("genotyper.vqsr.CPU")

    val combinememory = configInstance.getInt("genotyper.combine.RAM")
    val combinecpu = configInstance.getInt("genotyper.combine.CPU")

    val evalExtraArgs = configInstance.getString("genotyper.eval.extraArgs")

    val mergedoutputname = configInstance.getString("genotyper.combine.outputname")

    val stopAfterPreprocessing = configInstance.getBoolean("genotyper.preprocessonly")

    val cutBeforeGenotypeGVCF = configInstance.getBoolean("genotyper.cutBeforeGenotypeGVCF")
    val subsetmemory = configInstance.getInt("genotyper.subset.RAM")

    val snapSeed = configInstance.getInt("genotyper.snap.index.seed")
    val snapIndexCPU = configInstance.getInt("genotyper.snap.index.CPU")
    val snapIndexMemory = configInstance.getInt("genotyper.snap.index.RAM")
    val snapCPUAlign = configInstance.getInt("genotyper.snap.map.CPU")
    val snapMemoryAlign = configInstance.getInt("genotyper.snap.map.RAM")
    val snapExtraArgsMap = configInstance.getString("genotyper.snap.map.extraArgs")
    val snapExtraArgsIndex = configInstance.getString("genotyper.snap.index.extraArgs")

    val runplatypus = configInstance.getBoolean("genotyper.platypus.run")
    val platypusFolder = new File(configInstance.getString("genotyper.platypus.folder"))
    val platypusMemory = configInstance.getInt("genotyper.platypus.RAM")
    val platypusCPU = configInstance.getInt("genotyper.platypus.CPU")

    val referenceFastaShared = SharedFile(referenceFasta)
    val referenceDictShared = SharedFile(referenceDict)
    val referenceFaiShared = SharedFile(referenceFai)
    val gatkjarShared = SharedFile(gatkjar)

    val alreadyGenotypedGVCFs: List[(File, File)] = {
      val listfile = configInstance.getString("genotyper.gvcfs")
      if (listfile != "-") openSource(listfile)(_.getLines.map { line =>
        if (line.split(" ").size == 1)
          new File(line) -> new File(line + ".idx")
        else new File(line.split(" ")(0)) -> new File(line.split(" ")(1))
      }.toList).map {
        case (vcf, idxfile) =>
          if (!idxfile.canRead) {
            Try {

              val index = IndexFactory.createDynamicIndex(vcf, new htsjdk.variant.vcf.VCFCodec, FOR_SEEK_TIME)
              IndexFactory.writeIndex(index, idxfile)
            } match {
              case scala.util.Failure(x) => throw new RuntimeException("Error during indexing of " + vcf.getAbsolutePath, x)
              case _ => {}
            }
          }
          (vcf, idxfile)
      }
      else Nil
    }

    log.info("List of input gvcf files:\n" + alreadyGenotypedGVCFs.mkString("\n"))

    if ((alreadyGenotypedGVCFs ++ bamfiles ++ bamfilesForRemapping ++ fastqs).isEmpty) {
      log.info("Nothing to do.")
      System.exit(0)
    }

    // Construct tasks

    val snapindextask = if (bamfilesForRemapping.size + fastqs.size > 0) Some(snapIndexGeneration(SnapIndexGenerationInput(referenceFasta, snapSeed, snapExtraArgsIndex), cpu = snapIndexCPU, memory = snapIndexMemory)) else None

    val preprocessed: List[Future[BamWithBai]] = bamfiles.map {
      case BamFileConfig(file, idx, sorted, hasChr, needsDeduplication) =>

        val bam: Future[BamFile] = {
          val sortedBam: Future[BamFile] = if (!sorted) {
            log.info(s"Scheduled sort of $file")
            val task = sortbam(SortBamInput(file = file), memory = sortmemory, SortBamInput.updateSortBamInput orElse SortBamHelper.updateSortBamInput orElse identity[SortBamInput])
            task.?[BamFile]
          } else Future.successful(BamFile(SharedFile(file)))

          val dedupped: Future[BamFile] = if (needsDeduplication) {
            log.info(s"Scheduled deduplication of $file")
            val task = deduplicate(DeduplicateInput(jar = dedupjar), memory = deduplicatememory)
            sortedBam.map(x => task <~ x)
            task.?[DeduplicateOutput].map(x => BamFile(x.bam))
          } else sortedBam
          val chrAddedOrRemoved = if (!hasChr && referenceHasChrPrefix) {
            log.info(s"Scheduled chradd of $file")
            val task = addchrtobam(SortBamInput.empty, memory = addchrtobammemory)
            dedupped.map(x => task <~ x)
            task.?[BamFile]
          } else if (!referenceHasChrPrefix && hasChr) {
            log.info(s"Scheduled chr-remove of $file")
            val task = removechrfrombam(SortBamInput.empty, memory = addchrtobammemory)
            dedupped.map(x => task <~ x)
            task.?[BamFile]
          } else dedupped

          chrAddedOrRemoved
        }
        val index: Future[BamIndexFile] =
          if (!needsDeduplication && !((hasChr && !referenceHasChrPrefix) || (!hasChr && referenceHasChrPrefix)) && sorted && idx.isDefined)
            Future.successful(BamIndexFile(SharedFile(idx.get)))
          else {
            log.info(s"Scheduled indexing of $file")
            val indextask = indexbam(IndexBamInput.empty, memory = indexmemory)
            bam.map(x => indextask <~ x)
            indextask.?[BamIndexFile]
          }

        index.flatMap { idx =>
          bam.map { bam =>
            BamWithBaiImpl(bam.file, idx.file)
          }
        }

    } ++ bamfilesForRemapping.map { remappablebam =>
      val snapaligntask = snapAlign(SnapAlignInput(List(remappablebam), snapExtraArgsMap, remappablebam.getName, None), cpu = snapCPUAlign, memory = snapMemoryAlign)

      snapindextask.foreach(_ ~> snapaligntask)

      snapaligntask.?[SnapOutput]
    } ++ fastqs.map {
      case InputFastQGroup(files, readgroup) =>
        val snapaligntask = snapAlign(SnapAlignInput(files, snapExtraArgsMap, readgroup.sm, Some(readgroup)), cpu = snapCPUAlign, memory = snapMemoryAlign)

        snapindextask.foreach(_ ~> snapaligntask)

        snapaligntask.?[SnapOutput]
    }

    if (stopAfterPreprocessing) {
      Await.result(Future.sequence(preprocessed), atMost = 168 hours)

    } else {

      val callers = intervals.map { regions =>
        val regionId = if (intervals.size == 1) "all" else {
          val first = regions.sortBy(_.from).head
          s"from.chr${first.chromosome}:${first.from}"
        }

        val rs = RegionSet[String8, GenomicLocation, Region](regions)
        val regionsForGVCF: Set[Region] = (preVQSRIntervals filter (rs intersects _)).toSet

        if (regionsForGVCF.map(_.size).sum == 0) None
        else {

          val genotypergvcf = genotypeGVCF(
            GenotypeGVCFInput(jar = gatkjar, referenceFasta = referenceFasta, referenceFai = referenceFai, referenceDict = referenceDict, interval = regionsForGVCF, outName = s"gvcf.$regionId", expectedMoreGvcfs = preprocessed.size + alreadyGenotypedGVCFs.size, extraArgs = "", dbsnp = dbsnpvcfBefore1kg, gvcfs = Nil),
            memory = genotypeGVCFMemory,
            cpu = genotypeGVCFCPU
          )

          val genotyperplatypus = if (runplatypus) Some(platypusGenotype(
            GenotypeWithPlatypusInput(platypusfolder = platypusFolder, referenceFasta = referenceFasta, referenceFai = referenceFai, referenceDict = referenceDict, interval = regionsForGVCF, outName = s"platypus.$regionId", expectedNumberOfBamFiles = preprocessed.size, extraArgs = "", bamfiles = List()),
            memory = platypusMemory,
            cpu = platypusCPU
          ))
          else None

          alreadyGenotypedGVCFs.foreach {
            case (vcf, idx) =>
              if (cutBeforeGenotypeGVCF) {
                val subsettask = subsetvcf(SubsetVCFInput(interval = regionsForGVCF, vcf = vcf, vcfidx = idx), memory = subsetmemory)
                subsettask ~> genotypergvcf
              } else {
                genotypergvcf <~ VCFWithIndex(
                  SharedFile(vcf),
                  SharedFile(idx)
                )
              }
          }

          preprocessed.foreach { b =>
            b.foreach { (x: BamWithBai) =>
              val sampleId = x.bam.name
              val haplotypecallertask = haplotypecaller(HaplotypeCallerInputFactory(gatkjarShared, referenceFastaShared, referenceFaiShared, referenceDictShared, regions, s"haplotypecaller.${regionId}.$sampleId", 1, hcextraargs + " -ERC GVCF -variant_index_type LINEAR -variant_index_parameter 128000 -pairHMM VECTOR_LOGLESS_CACHING "), memory = hcmemory, cpu = hccpu)
              log.debug("Send BamWithBai to HC.")
              haplotypecallertask <~ x

              genotyperplatypus.foreach(_ <~ x)

              if (cutBeforeGenotypeGVCF) {
                val subsettask = subsetvcf(SubsetVCFInput(regionsForGVCF), memory = subsetmemory)
                haplotypecallertask ~> subsettask ~> genotypergvcf

                subsettask <~ x

              } else {
                haplotypecallertask ~> genotypergvcf
              }

            }
          }

          Some(genotypergvcf -> genotyperplatypus)
        }

      }.filter(_.isDefined).map(_.get)

      val evaluation = {
        val combinetask = combinevcf(CombineVCFInput(gatkjar, referenceFasta, referenceFai, referenceDict, "", callers.size, mergedoutputname), memory = combinememory, cpu = combinecpu)

        val prevqsrInterval = variantFiltration(VariantFiltrationInput(gatkjar, referenceFasta, referenceFai, referenceDict, prevqsrextraarg, preVQSRIntervals), memory = prevqsrmemory)

        val dbsnpannotation = dbsnpAnnotation(DbsnpIDAnnotationInput(gatkjar, referenceFasta, referenceFai, referenceDict, dbsnpvcfRecent), memory = dbsnpannotationmemory)

        val vqsrsnptask = vqsr(VQSRInput(gatkjar, referenceFasta, referenceFai, referenceDict, s" --maxGaussians $maxGaussiansSNP ", "SNP", resourcesConfigsForSNPRecalibration.toList, minBadVariantsSNP), memory = vqsrmemory, cpu = vqsrcpu)

        val vqsrindeltask = vqsr(VQSRInput(gatkjar, referenceFasta, referenceFai, referenceDict, s" --maxGaussians $maxGaussiansIndel ", "INDEL", resourcesConfigsForIndelRecalibration.toList, minBadVariantsIndel), memory = vqsrmemory, cpu = vqsrcpu)

        val evaltask = varianteval(VariantEvalInput(gatkjar, referenceFasta, referenceFai, referenceDict, evalExtraArgs, dbsnpvcfBefore1kg), memory = evaluatememory, cpu = evaluatecpu)

        val combinetaskplatypus = if (runplatypus) Some(combinevcf(CombineVCFInput(gatkjar, referenceFasta, referenceFai, referenceDict, "", callers.size, mergedoutputname + ".platypus"), memory = combinememory, cpu = combinecpu)) else None

        // val toPlink = vcf2plink(VCF2PlinkInput(gatkjar, referenceFasta, referenceFai, referenceDict))

        callers.foreach(x => x._1 ~> combinetask)

        callers.foreach(x => x._2.foreach(_ ~> combinetaskplatypus.get))

        combinetask ~> prevqsrInterval ~> dbsnpannotation ~> vqsrsnptask ~> vqsrindeltask ~> evaltask

        // vqsrindeltask ~> toPlink

        Future.sequence(
          evaltask.?[VariantEvalOutput] ::
            // toPlink.?[MakeBedOut] ::
            Nil :::
            combinetaskplatypus.toList.map(_.?)
        )

      }

      Await.result(evaluation, atMost = 168 hours)

    }

    ts.shutdown

  }

}