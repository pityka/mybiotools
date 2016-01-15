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

package hdfdosage

import mybiotools.config.Config.configInstance

import hdfdosage.HDFDosageFile._
import hdfdosage._
import mybiotools.{ writeToFile, openFileWriter, getFileReader, fastSplitSetSeparatorIterator, fastSplitSetSeparator, openSource }
import mybiotools.gwascommons.{ getGenomicMapFromBimFile, getIndividualsFromFamFile }
import mybiotools.gwascommons.{ GenomicLocation, Individual }
import java.io.File
import scala.collection.JavaConversions._
import scala.io.Source
import mybiotools.stringstore._
import mybiotools.gwascommons.genotypedata._
import FileSets._
import mybiotools.mapreduce.MapReduceTraversal._

object Helper {
  def writeAsciiStuff(path: String) {
    openFileWriter(new File(path + ".indlist")) { writer =>
      readIndividualOrder(new File(path)).foreach { ind =>
        writer.write(ind.familyID + " " + ind.individualID)
        writer.write('\n')
      }
    }

    openFileWriter(new File(path + ".snplist")) { writer =>
      readSNPOrder(new File(path)).foreach { pdosesum =>
        writer.write(pdosesum.toLine)
        writer.write('\n')
      }
    }

  }
}

object HDF5DosageApplication extends App {
  import Helper._
  import scala.collection.JavaConversions._

  if (configInstance.getBoolean("recode.run")) {

    val threads = configInstance.getInt("recode.threads")

    val inputIsSortedByGenomicLocation = configInstance.getBoolean("recode.inputSorted")

    val recodeToRecessive = configInstance.getBoolean("recode.recodeToRecessive")

    val outputPaths = if (configInstance.getString("recode.output") != "") Some(openSource(configInstance.getString("recode.output"))(_.getLines.toIndexedSeq)) else None

    val blockSize = if (inputIsSortedByGenomicLocation) configInstance.getInt("recode.blockSize") else 1

    val includeIndividuals = if (configInstance.hasPath("recode.includeIndividuals")) Some(openSource(configInstance.getString("recode.includeIndividuals"))(_.getLines.map { x =>
      val spl = fastSplitSetSeparatorIterator(x, Set(' ', '\t'))
      val fid = spl.next
      val iid = spl.next
      Individual(fid, iid)
    }.toSet))
    else None

    val genomicMap: mybiotools.gwascommons.GenomicMap = {
      val x = configInstance.getString("recode.map")
      if (x == "") collection.Map()
      else getGenomicMapFromBimFile(x)
    }

    val outputFormat = try {
      configInstance.getString("recode.outputFormat") match {
        case x if x == "hdfdosage" => HDFDosage
        case x if x == "pdose" => PDose
        case x if x == "bigind" => BIGIndividualMajor
        case x if x == "big" || x == "bigsnp" => BIGSNPMajor
        case x if x == "pgenotypeprobabilities" => PGenotypeProbabilities
        case x if x == "impute" => IMPUTEFile
        case x if x == "missing" => Missingness

        case x if x == "tped" => {
          val hardcall = configInstance.getDouble("recode.hardcall.hardCallThreshold")
          val missing = configInstance.getString("recode.tpedMissingChar")
          TPed(hardcall, missing(0))
        }
        case x if x == "grm" => GRMMatrix
      }
    } catch {
      case x: Throwable => { println("recode.outputFormat should be tped pdose bigind big bigsnp grm missing or pgenotypeprobabilities"); throw x }
    }

    println(mybiotools.config.Config.prettyPrintVersion("dosagetool", hdfdosage.Reflected.version))
    println(mybiotools.config.Config.prettyPrint(Some("recode")))

    if (outputFormat == HDFDosage) {
      println("WARNING: the recommended way to produce a hdf dosage file is via merge or import.")
    }

    val genotypefilelist: List[GenotypeFileSet] = FileSets.parseConfig(configInstance.getConfig("recode"))

    val ldprune = configInstance.getBoolean("recode.prune")
    val prunewindow = configInstance.getInt("recode.pruneWindow")
    val prunethreshold = configInstance.getDouble("recode.pruneThreshold")
    val pruneKeepSnps = {
      val f = configInstance.getString("recode.pruneKeepSnps")
      if (f == "") Set[String]()
      else openSource(f)(_.getLines.toSet)
    }
    val pruneAwaySnps = {
      val f = configInstance.getString("recode.pruneAwaySnps")
      if (f == "") Set[String]()
      else openSource(f)(_.getLines.toSet)
    }
    val minimumMAF = configInstance.getDouble("recode.minimumMAF")
    val maximumMAF = configInstance.getDouble("recode.maximumMAF")
    val snpIncludeFilter = scala.util.Try {
      openSource(configInstance.getString("recode.includeSNPs"))(_.getLines.toSet)
    }.toOption
    val grmBatchSize = configInstance.getInt("recode.grmBatchSize")
    val grmThreads = configInstance.getInt("recode.grmThreads")
    val maximumMissingnessRate = configInstance.getInt("recode.maximumMissingnessRate")

    genotypefilelist.zipWithIndex.map {
      case (genotypefile, idx) =>

        val inputPath = genotypefile match {
          case FileSets.PDose(file, _, _) => file
          case FileSets.PGenotypeProbabilities(file, _, _) => file
          case FileSets.TPed(_, file, _) => file
          case FileSets.HDFDosage(file) => file
          case FileSets.VCFFile(file) => file
          case FileSets.BedFile(file, _, _) => file
        }

        val out = new File(if (outputPaths.isDefined) outputPaths.get(idx) else inputPath + ".recode")

        val counter = Recode.run(
          genotypefile,
          out,
          ldprune,
          prunethreshold,
          prunewindow,
          pruneKeepSnps,
          pruneAwaySnps,
          genomicMap,
          snpIncludeFilter,
          outputFormat,
          recodeToRecessive,
          blockSize,
          minimumMAF,
          maximumMAF,
          grmBatchSize,
          grmThreads,
          maximumMissingnessRate,
          includeIndividuals
        )

        println(s"$counter snps written.")

    }

  }

  if (configInstance.getBoolean("import.run")) {

    val threads = configInstance.getInt("import.threads")

    val inputPaths: List[GenotypeFileSet] = FileSets.parseConfig(configInstance.getConfig("import"))
    val outputPaths = if (configInstance.getString("import.output") != "") Some(openSource(configInstance.getString("import.output"))(_.getLines.toIndexedSeq)) else None

    if (outputPaths.isDefined) {
      assert(outputPaths.get.size == inputPaths.size, "outputpaths should have " + inputPaths.size + " lines. " + configInstance.getString("import.output"))
    }

    val missingValue = configInstance.getString("import.missingValue")
    val cleanDuplicates = configInstance.getBoolean("import.cleanDuplicates")

    val inputIsSortedByGenomicLocation = configInstance.getBoolean("import.inputSorted")

    val blockSize = if (inputIsSortedByGenomicLocation) configInstance.getInt("import.blockSize") else 1

    val minimumMAF = configInstance.getDouble("import.minimumMAF")

    mybiotools.redirectStdStreams(new File(configInstance.getString("import.genotypefiles") + ".log")) {

      println(mybiotools.config.Config.prettyPrintVersion("dosagetool", hdfdosage.Reflected.version))
      println(mybiotools.config.Config.prettyPrint(Some("import")))

      inputPaths.zipWithIndex.map {
        case (genotypefileset, idx) =>

          val outputPath = if (outputPaths.isDefined) outputPaths.get(idx) else genotypefileset.path.getAbsolutePath + ".h5"

          val report = FileSets.openFileSet(genotypefileset, minimumMAF, Full) { snpmajoriterator =>
            Import.convertSNPMajorFromIterator(snpmajoriterator, new File(outputPath), cleanDuplicates, blockSize = blockSize, minimumMAF = minimumMAF)
          }

          synchronized {
            if (!report.writeSuccessful) {
              println("Duplicates found. Writing these to .duplicatedIndividuals and .duplicatedSNPs files")
              writeToFile(outputPath + ".duplicatedIndividuals", report.duplicatedIndividuals.map(x => x.familyID + " " + x.individualID).mkString("\n"))

              openFileWriter(new File(outputPath + ".duplicatedSNPs")) { writer =>
                report.duplicatedSNPs.foreach { snp =>
                  writer.write(snp)
                  writer.write("\n")
                }
              }

            } else {
              println("Conversion to HDF5 dosage format finished:" + genotypefileset.path.getAbsolutePath)
              println("Duplicated SNPs removed: \n" + report.duplicatedSNPs.mkString("\n"))
              println("Duplicated individuals removed: \n" + report.duplicatedIndividuals.mkString("\n"))
              writeAsciiStuff(outputPath)
              println("Written snp and individual info to ascii files.")
            }
          }
      }
    }
  }

  if (configInstance.getBoolean("merge.run")) {

    val outputPath = configInstance.getString("merge.output")

    mybiotools.redirectStdStreams(new File(outputPath + ".log")) {

      println(mybiotools.config.Config.prettyPrintVersion("dosagetool", hdfdosage.Reflected.version))

      println(mybiotools.config.Config.prettyPrint(Some("merge")))

      val inputPaths: List[GenotypeFileSet] = FileSets.parseConfig(configInstance.getConfig("merge"))

      val sortByCHR = configInstance.getBoolean("merge.sortOutput")

      val cacheSize = configInstance.getInt("merge.cacheSize")

      val minimumSNPCoverage = configInstance.getDouble("merge.minimumSNPCoverage")

      val minimumMAF = configInstance.getDouble("merge.minimumMAF")

      import hdfdosage._
      val format = try {
        configInstance.getString("merge.outputFormat") match {
          case x if x == "hdfdosage" => HDFDosage
          case x if x == "pdose" => PDose
          case x if x == "bigind" => BIGIndividualMajor
          case x if x == "big" || x == "bigsnp" => BIGSNPMajor
          case x if x == "pgenotypeprobabilities" => PGenotypeProbabilities
          case x if x == "impute" => IMPUTEFile
          case x if x == "missing" => Missingness

          case x if x == "tped" => {
            val hardcall = configInstance.getDouble("merge.tped.hardCallThreshold")
            val missing = configInstance.getString("merge.tped.missingChar")
            TPed(hardcall, missing(0))
          }
          case x if x == "grm" => GRMMatrix
        }
      } catch {
        case x: Throwable => { println("merge.outputFormat should be hdfdosage pdose bigind big bigsnp grm or pgenotypeprobabilities"); throw x }
      }

      val recodeToRecessive = format match {
        case HDFDosage | BIGIndividualMajor | PGenotypeProbabilities | IMPUTEFile => false
        case _ => configInstance.getBoolean("merge.recodeToRecessive")
      }

      if (recodeToRecessive) {
        println("Recoding dosage genotypes to recessive with max(0, dosage - 1) .")
      }

      val genomicmap = configInstance.getString("merge.genomicMap") match {
        case x if x == "" => {
          if (format == BIGSNPMajor || format == BIGIndividualMajor || format.isInstanceOf[TPed]) {
            println("!! Error: merge.genomicMap is empty. If this is intentional, set it to EMPTY. In this case, dots (.) will be written to the .bim file instead of coordinates.")
            System.exit(1)
          }
          collection.Map[String8, GenomicLocation]()
        }
        case x if x == "EMPTY" => collection.Map[String8, GenomicLocation]()
        case x => getGenomicMapFromBimFile(x)
      }

      val blockSize = if (sortByCHR && genomicmap.size > 0) configInstance.getInt("merge.blockSize") else 1

      println("Files to be written:")
      format match {
        case Missingness => println("\t1. Missingness file")
        case HDFDosage => {
          println("\t1. HDF5 dosage file: " + new File(outputPath).getCanonicalPath)
          println(s"\t\tBlocksize $blockSize")
          println("\t2. List of individuals: " + new File(outputPath).getCanonicalPath + ".indlist .")
          println("\t3. List of SNPs: " + new File(outputPath).getCanonicalPath + ".snplist .")

        }
        case PDose => {
          println("\t1. pdose file: " + new File(outputPath).getCanonicalPath)
        }
        case IMPUTEFile => {
          println("\t1. IMPUTE genotype file: " + new File(outputPath).getCanonicalPath)
        }
        case BIGIndividualMajor => {
          println("\t1. BIGIndividualMajor file: " + new File(outputPath).getCanonicalPath + ".big .")
          println("\t2. List of individuals: " + new File(outputPath).getCanonicalPath + ".indlist .")
          println("\t3. List of SNPs: " + new File(outputPath).getCanonicalPath + ".bim .")
        }
        case BIGSNPMajor => {
          println("\t1. BIGSNPMajor file: " + new File(outputPath).getCanonicalPath + ".big .")
          println("\t2. List of individuals: " + new File(outputPath).getCanonicalPath + ".indlist .")
          println("\t3. List of SNPs: " + new File(outputPath).getCanonicalPath + ".bim .")
        }
        case TPed(_, _) => {
          println("\t1. TPed file: " + new File(outputPath).getCanonicalPath + ".tped .")
          println("\t2. List of individuals: " + new File(outputPath).getCanonicalPath + ".indlist .")
          println("\t3. List of SNPs: " + new File(outputPath).getCanonicalPath + ".bim .")
        }
        case PGenotypeProbabilities => {
          println("\t1. plink genotype file: " + new File(outputPath).getCanonicalPath)
        }
        case GRMMatrix => {
          println("\t1. GCTA gzipped GRM matrix: " + new File(outputPath + ".grm.gz").getCanonicalPath)
          println("\t2. GCTA gzipped GRM matrix companion file: " + new File(outputPath + ".grm.id").getCanonicalPath)
          println("\t3. Fastlmm formatted GRM file: " + new File(outputPath + ".fastlmmsim").getCanonicalPath)

        }
      }

      val includeIndividuals = if (configInstance.hasPath("merge.includeIndividuals")) scala.io.Source.fromFile(configInstance.getString("merge.includeIndividuals")).getLines.map { x =>
        val spl = fastSplitSetSeparatorIterator(x, Set(' ', '\t'))
        val fid = spl.next
        val iid = spl.next
        Individual(fid, iid)
      }.toSet
      else Set[Individual]()

      val excludeIndividuals = if (configInstance.hasPath("merge.excludeIndividuals")) scala.io.Source.fromFile(configInstance.getString("merge.excludeIndividuals")).getLines.map { x =>
        val spl = fastSplitSetSeparatorIterator(x, Set(' ', '\t'))
        val fid = spl.next
        val iid = spl.next
        Individual(fid, iid)
      }.toSet
      else Set[Individual]()

      val includeSNPs = if (configInstance.hasPath("merge.includeSNPs")) scala.io.Source.fromFile(configInstance.getString("merge.includeSNPs")).getLines.map(x => StringStore(x)).toSet
      else Set[String8]()

      val excludeSNPs = if (configInstance.hasPath("merge.excludeSNPs")) scala.io.Source.fromFile(configInstance.getString("merge.excludeSNPs")).getLines.map(x => StringStore(x)).toSet
      else Set[String8]()

      println("Listing inclusion/exclusion filter lengths (0 means no filter):")
      println("IncludeIndividuals: " + includeIndividuals.size)
      println("ExcludeIndividuals: " + excludeIndividuals.size)
      println("IncludeSNPs: " + includeSNPs.size)
      println("ExcludeSNPs: " + excludeSNPs.size)
      println("Sort output file by genomic map (if present):" + sortByCHR)

      // End input

      val report = mergeSNPMajors(
        inputPaths,
        new File(outputPath),
        minimumSNPCoverage,
        includeSNPs,
        excludeSNPs,
        includeIndividuals,
        excludeIndividuals,
        cacheSize,
        format,
        minimumMAF,
        genomicmap,
        sortByCHR = sortByCHR,
        blockSize = blockSize,
        recodeToRecessive = recodeToRecessive
      )
      if (!report.writeSuccessful) {
        println("Mismatched SNPs found")

        openFileWriter(new File(outputPath + ".mismatchedSNPs")) { writer =>
          report.mismatchedSNPs.foreach {
            _.foreach { snp =>
              writer.write(snp.snpName.toString)
              writer.write("\n")
            }
          }
        }

      } else {
        println("Merging finished.")
        if (format == HDFDosage) {
          writeAsciiStuff(outputPath)
          println("Written snp and individual info to ascii files.")
        }

      }
    }
  }

}