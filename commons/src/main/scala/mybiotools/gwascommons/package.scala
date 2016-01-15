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

package mybiotools

import mybiotools.stringstore._
import java.io.File
import mybiotools.gwascommons.genotypedata._
import mybiotools.eq._

/** Commons methods for manipulating GWAS/Plink files */
package object gwascommons {

  type GenomicMap = collection.Map[String8, GenomicLocation]

  type GenericRegionSet = RegionSet[String8, GenomicLocation, Region]

  implicit object RegionBuilder1 extends RegionBuilder[String8, GenomicLocation, Region] {
    def build(chr: String8, from: Int, to: Int) = Region(chr, from, to)
  }

  implicit object GenomicLocationBuilder1 extends GenomicLocationBuilder[String8, GenomicLocation] {
    def build(chr: String8, bp: Int) = GenomicLocation(bp, chr)
  }

  val ChromosomeLengths = Map(
    1 -> 248000000,
    2 -> 243000000,
    3 -> 200000000,
    4 -> 192000000,
    5 -> 181000000,
    6 -> 171000000,
    7 -> 159000000,
    8 -> 147000000,
    9 -> 141000000,
    10 -> 136000000,
    11 -> 135000000,
    12 -> 133000000,
    13 -> 115000000,
    14 -> 107000000,
    15 -> 101000000,
    16 -> 89000000,
    17 -> 79000000,
    18 -> 76000000,
    19 -> 64000000,
    20 -> 63000000,
    21 -> 47000000,
    22 -> 50000000,
    23 -> 155000000, // X
    24 -> 58000000 // Y
  )

  val Centromeres = Map(
    11 -> 53700000,
    12 -> 35800000,
    13 -> 17900000,
    14 -> 17600000,
    15 -> 19000000,
    16 -> 36600000,
    17 -> 24000000,
    18 -> 17200000,
    1 -> 125000000,
    10 -> 40200000,
    24 -> 12500000,
    23 -> 60600000,
    9 -> 49000000,
    8 -> 45600000,
    7 -> 59900000,
    6 -> 61000000,
    5 -> 48400000,
    4 -> 50400000,
    3 -> 91000000,
    22 -> 14700000,
    21 -> 13200000,
    20 -> 27500000,
    2 -> 93300000,
    19 -> 26500000,
    25 -> 0,
    26 -> 0
  )

  val ChromosomeRegions: Map[Int, Region] = ChromosomeLengths.map { case (chr, max) => chr -> Region(StringStore(chr.toString), 0, max) }

  // chromosomes is ordered in genomic order
  def getNearbyChromosome[T <: HasName with HasGenomicLocation](
    snp: T,
    chromosome: Vector[T],
    snpsOnChromosomeOrder: Map[T, Int],
    window: Int
  ): Vector[T] = {
    val gl = snp.genomicLocation

    val center = snpsOnChromosomeOrder(snp)

    val upperChromosome = chromosome.slice(from = center, until = chromosome.size)
    val lowerChromosome = chromosome.slice(from = 0, until = center + 1)

    val upperEnd = if (upperChromosome.size == 0) 0
    else mybiotools.jumpDirectionalSearch(
      upperChromosome,
      f = ((x: T) => (x.genomicLocation.basePairPosition - window) >= gl.basePairPosition),
      forward = true
    ) match {
      case Some((pos, value)) => pos
      case None => upperChromosome.size - 1
    }

    val lowerEnd = (if (lowerChromosome.size == 0) 0
    else mybiotools.jumpDirectionalSearch(
      lowerChromosome,
      f = (x: T) => x.genomicLocation.basePairPosition + window < gl.basePairPosition,
      forward = false
    ) match {
      case Some((pos, value)) => pos
      case None => 0
    })

    chromosome.slice(lowerEnd, center + upperEnd + 1)

  }

  import mybiotools.{ chromosomeStringFromNumber, chromosomeNumberFromString }

  implicit def indorder = scala.math.Ordering.by[Individual, (String, String)](Individual.unapply(_).get)

  implicit def simpleAlelleOrdering = scala.math.Ordering.by[SimpleAllele, String](_.nucleotides)

  implicit def genomicLocationOrdering(implicit o: Ordering[String8]) = new Ordering[GenomicLocation] {
    /**
     * Lexicographical ordering in the order Chr Bp
     * @return -1 if a < b; + 1 if a > b ; 0 if a == b
     */
    def compare(a: GenomicLocation, b: GenomicLocation): Int = (a, b) match {
      case (a, b) if o.lt(a.chromosomeAsT, b.chromosomeAsT) => -1
      case (a, b) if a.chromosomeAsT == b.chromosomeAsT && a.basePairPosition < b.basePairPosition => -1
      case (a, b) if a == b => 0
      case (a, b) => 1
    }
  }

  val genericRegionOrderingByStart = regionOrderingByStart[String8, GenomicLocation, Region]

  def regionOrderingByStart[T, GL <: GenomicLocationStub[T], K <: RegionStub[T, GL, K]](implicit o: Ordering[T]) = new Ordering[K] {
    /**
     * Lexicographical ordering in the order Chr Bp
     * @return -1 if a < b; + 1 if a > b ; 0 if a == b
     */
    def compare(a: K, b: K): Int = (a, b) match {
      case (a, b) if o.lt(a.chromosomeAsT, b.chromosomeAsT) => -1
      case (a, b) if a.chromosomeAsT === b.chromosomeAsT && a.from < b.from => -1
      case (a, b) if a.chromosomeAsT === b.chromosomeAsT && a.from == b.from => 0
      case _ => 1
    }
  }

  import scala.collection.immutable.HashMap

  /** Reads plink's regular .map or .bim file */
  def getGenomicMapFromBimFile(bimfile: String): GenomicMap = {
    val mmap: collection.mutable.Map[String8, GenomicLocation] = collection.mutable.AnyRefMap[String8, GenomicLocation]()
    using(io.Source.fromFile(bimfile))(_.getLines.foreach { line =>
      val splitted = fastSplitSetSeparatorIterator(line, Set(' ', '\t'))
      val chr = new String(splitted.next)
      val name = new String(splitted.next)
      val empt = splitted.next
      val bp = splitted.next
      mmap += (StringStore(name) -> GenomicLocation(bp.toInt, chr))
    })
    mmap
  }

  /** Reads plink's regular .map or .bim file */
  def getGenomicMapFromBimFileReverse(bimfile: String): Map[GenomicLocation, String8] = {
    // val map = scala.collection.mutable.ArrayBuffer[(String, GenomicLocation)]()
    using(io.Source.fromFile(bimfile))(_.getLines.map { line =>
      val splitted = fastSplitSetSeparatorIterator(line, Set(' ', '\t'))
      val chr = new String(splitted.next)
      val name = new String(splitted.next)
      val empt = splitted.next
      val bp = splitted.next
      (GenomicLocation(bp.toInt, chr) -> StringStore(name))
    }.toMap)
    // map.toMap
  }
  def getBimEntries(bimfile: String): Vector[(String8, GenomicLocation)] = openSource(bimfile) { s => getBimEntries(s) }

  def getBimEntries(source: scala.io.Source): Vector[(String8, GenomicLocation)] = source.getLines.map { line =>
    val splitted = fastSplitSetSeparatorIterator(line, Set(' ', '\t'))
    val chr = new String(splitted.next)
    val name = new String(splitted.next)
    val empt = splitted.next
    val bp = splitted.next
    (StringStore(name) -> GenomicLocation(bp.toInt, chr))
  }.toVector

  def getBimEntriesWithAllelesWithoutLocation(source: scala.io.Source): Iterator[(String8, String8, String8)] = source.getLines.map { line =>
    val splitted = fastSplitSetSeparatorIterator(line, Set(' ', '\t'))
    val chr = new String(splitted.next)
    val name = new String(splitted.next)
    val empt = splitted.next
    val bp = splitted.next
    val a1 = splitted.next
    val a2 = splitted.next
    (StringStore(name), StringStore(a1), StringStore(a2))
  }

  def getChromosomesFromBimFile(bimFile: String): Vector[String] =
    using(io.Source.fromFile(bimFile))(_.getLines.map { line =>
      val splitted = fastSplitSetSeparatorIterator(line, Set(' ', '\t'))
      splitted.next
    }.toVector.distinct)

  /** Reads plink's 3column .map file */
  def getGenomicMapFromMap3File(bimfile: String): GenomicMap = {
    val map = scala.collection.mutable.AnyRefMap[String8, GenomicLocation]()
    openSource(bimfile)(_.getLines.foreach { line =>
      val splitted = fastSplitSetSeparator(line, Set(' ', '\t'))
      map += (StringStore(splitted(1)) -> GenomicLocation(splitted(2).toInt, splitted(0)))
    })
    map
  }

  def getIndividualsFromFamFile(famFile: scala.io.Source): Vector[Individual] = {
    famFile.getLines.map { line =>
      val spl = fastSplitSetSeparator(line, Set(' ', '\t'))
      val fid = spl(0)
      val iid = spl(1)
      Individual(fid, iid)
    }.toVector
  }

  def getIndividualsFromPDoseHeader(header: String): Vector[Individual] = {
    val spl = fastSplitSetSeparator(header, Set(' ', '\t'))
    spl.drop(3).grouped(2).map { g =>
      Individual(g(0), g(1))
    }.toVector
  }

  /** 4th field is gene name */
  def readBedInterval(source: io.Source): Iterator[Region] = source.getLines.map { line =>
    val splitted = fastSplit1WideSeparator(line, '\t')
    val chr = splitted(0)
    val start = splitted(1).toInt
    val end = splitted(2).toInt
    Region(chr, start, end)
  }

  /** 4th field is gene name */
  def readBedWithNames(source: io.Source): Iterator[(String8, Region)] = source.getLines.map { line =>
    val splitted = fastSplitSeparator(line, '\t')
    val chr = splitted(0)
    val start = splitted(1).toInt
    val end = splitted(2).toInt
    val name = if (splitted(3) != ".") StringStore(new String(splitted(3))) else StringStore(s"$chr:$start-$end")
    val region = Region(chr, start, end)
    name -> region
  }

  def readPlinkFrequencyFile(file: File, separator: Char): Iterable[LocusData] =
    readPlinkFrequencyFile(io.Source.fromFile(file), separator)

  def readPlinkFrequencyFile(source: io.Source, separator: Char): Iterable[LocusData] = {

    source.getLines.drop(1).map { line =>
      val splitted = fastSplitSeparator(line, separator)
      if (splitted.size > 0) {
        val id = StringStore(splitted(1))
        val allele1 = SimpleAllele.makeSingleton(splitted(2)(0))
        val allele2 = SimpleAllele.makeSingleton(splitted(3)(0))
        val numObs = splitted(5).toInt
        val maf = splitted(4) match {
          case x if x == "NA" => None
          case x => Some(x.toDouble)
        }

        maf.map { m =>
          LocusData(
            name = id,
            genomicLocation = None,
            alleles = List(allele1, allele2),
            alleleFrequencies = List((allele1, m), (allele2, 1.0 - m))
          )
        }

      } else None
    }.filter(_.isDefined).map(_.get).toList
  }

  def runFastlmm(pdose: File, pheno: File, covar: Option[File], phenoIndex: Int, simFile: File): (File, File) = {

    val tmpout = TempFile.createTempFile(".fastlmmout")
    val tmplog = TempFile.createTempFile(".fastlmmoutlog")

    val famfile = {
      val tmpfam = TempFile.createTempFile(".fam")
      writeToFile(
        tmpfam.getCanonicalPath,
        getIndividualsFromPDoseHeader(openSource(pdose.getCanonicalPath)(_.getLines.next)).map(_.toFamLine).mkString("\n")
      )
      tmpfam
    }

    val common = putBesides((pdose -> ".dat" :: famfile -> ".fam" :: Nil): _*)

    val covarstr = covar.map { c =>
      " -covar " + c.getAbsolutePath
    }.getOrElse(" ")

    val cmdfastlmm = "fastlmmc -dfile1 " + common.getAbsolutePath + " -sim " + simFile.getAbsolutePath + " -runGwasType RUN -pheno " + pheno.getAbsolutePath + " -mpheno " + phenoIndex + covarstr + " -verbose -verboseOut -out " + tmpout

    val (stdout2, stderr2, success2) = execGetStreamsAndCode(cmdfastlmm, unsuccessfulOnErrorStream = false)

    if (!success2) throw new RuntimeException("error in fastlmm" + stdout2.mkString("\n") + stderr2.mkString("\n"))

    mybiotools.writeToFile(tmplog.getAbsolutePath, (stdout2 ++ stderr2).mkString("\n"))

    (tmpout, tmplog)

  }

  // def runFastlmm(bed: File, bim: File, fam: File, pheno: File, covar: File, phenoIndex: Int, simFile: File): (File, File) = {

  //   def putBesides(bed: File, bim: File, fam: File): File = {

  //     val tmp = mybiotools.TempFile.createTempFile("bedbimfam")

  //     val bedfilepath = java.nio.file.Paths.get(bed.getCanonicalPath)
  //     val bedfilelinkpath = java.nio.file.Paths.get(tmp.getCanonicalPath + (".bed"))
  //     java.nio.file.Files.createSymbolicLink(bedfilelinkpath, bedfilepath)

  //     val bimfilepath = java.nio.file.Paths.get(bim.getCanonicalPath)
  //     val bimfilelinkpath = java.nio.file.Paths.get(tmp.getCanonicalPath + (".bim"))
  //     java.nio.file.Files.createSymbolicLink(bimfilelinkpath, bimfilepath)

  //     val famfilepath = java.nio.file.Paths.get(fam.getCanonicalPath)
  //     val famfilelinkpath = java.nio.file.Paths.get(tmp.getCanonicalPath + (".fam"))
  //     java.nio.file.Files.createSymbolicLink(famfilelinkpath, famfilepath)

  //     tmp

  //   }

  //   val trunk = putBesides(bed, bim, fam)

  //   val tmpout = TempFile.createTempFile(".fastlmmout")
  //   val tmplog = TempFile.createTempFile(".fastlmmoutlog")

  //   val cmdfastlmm = "fastlmmc -bfile " + trunk.getCanonicalPath + " -sim " + simFile.getAbsolutePath + " -runGwasType RUN -pheno " + pheno.getAbsolutePath + " -mpheno " + phenoIndex + " -covar " + covar.getAbsolutePath + " -verbose -verboseOut -out " + tmpout

  //   val (stdout2, stderr2, success2) = execGetStreamsAndCode(cmdfastlmm,unsuccessfulOnErrorStream=false)

  //   if (!success2) throw new RuntimeException("error in fastlmm" + stdout2.mkString("\n") + stderr2.mkString("\n"))

  //   mybiotools.writeToFile(tmplog.getAbsolutePath, (stdout2 ++ stderr2).mkString("\n"))

  //   (tmpout, tmplog)

  // }

  def runSmartPCA(
    bedFile: String,
    bimFile: String,
    famFile: String,
    evecoutFilePath: String,
    workingDirectory: String,
    populationFile: Option[String] = None
  ): Boolean = {
    import scala.sys.process._

    execGetStreamsAndCode(
      ("plink --noweb --bed " + bedFile + " --bim " + bimFile + " --fam " + famFile + "  --out " + workingDirectory + "/" + "bad_chr8_list --write-snplist --from-bp 8000000 --to-bp 12000000 --chr 8") ###
        ("plink --noweb --bed " + bedFile + " --bim " + bimFile + " --fam " + famFile + " --out " + workingDirectory + "/" + "bad_chr6_list --write-snplist --from-bp 20000000 --to-bp 40000000 --chr 6 ") ###
        ("plink --noweb --bed " + bedFile + " --bim " + bimFile + " --fam " + famFile + " --out " + workingDirectory + "/" + "bad_chr11_list --write-snplist --from-bp 45000000 --to-bp 57000000 --chr 11") ###
        ("plink --noweb --bed " + bedFile + " --bim " + bimFile + " --fam " + famFile + " --out " + workingDirectory + "/" + "bad_chr5_list --write-snplist --from-bp 44000000 --to-bp 51500000 --chr 5")
    )

    val badLists = List(
      new File(workingDirectory + "/bad_chr8_list.snplist"),
      new File(workingDirectory + "/bad_chr6_list.snplist"),
      new File(workingDirectory + "/bad_chr11_list.snplist"),
      new File(workingDirectory + "/bad_chr5_list.snplist")
    ).filter(_.canRead)

    mybiotools.cat(badLists, new File(workingDirectory + "/badSNPs.txt"))

    val (stdout, stderr, finished) = execGetStreamsAndCode(("plink --maf 0.05 --geno 0.00 --noweb  --bed " + bedFile + " --bim " + bimFile + " --fam " + famFile + " --out " + workingDirectory + "/badsnpcleared  --exclude " + workingDirectory + "/badSNPs.txt --make-bed"))

    if (!finished) throw new RuntimeException("error in plink" + stdout.mkString("\n") + stderr.mkString("\n"))

    val fam = openSource(workingDirectory + "/badsnpcleared.fam") { source =>
      readTable(source, sep = " ", header = false)
    }
    val pop = populationFile match {
      case Some(file) => openSource(file)(s => Some(readTable(s, sep = " ", header = false)))
      case None => None
    }
    val famMap = fam.zipWithIndex.map {
      case (entry, idx) =>
        val fid = entry('V1)
        val iid = entry('V2)
        idx -> (fid, iid)
    }.toMap
    val famstring = fam.zipWithIndex.map {
      case (entry, idx) =>
        val p = pop match {
          case Some(t) => t.find(x => x('V1) == entry('V1) && x('V2) == entry('V2)).orElse(Some(Map('V3 -> "???"))).get('V3)
          case None => entry('V6)
        }
        idx.toString :: idx.toString :: entry('V3) :: entry('V4) :: entry('V5) :: p :: Nil
    }.map(_.mkString(" ")).mkString("", "\n", "\n")
    writeToFile(workingDirectory + "/badsnpcleared.pedind", famstring)

    if (finished) {
      writeToFile(workingDirectory + "/par.txt", "genotypename: " + workingDirectory + "/badsnpcleared.bed")
      appendToFile(workingDirectory + "/par.txt", "\nsnpname: " + workingDirectory + "/badsnpcleared.bim")
      appendToFile(workingDirectory + "/par.txt", "\nindivname: " + workingDirectory + "/badsnpcleared.pedind")
      appendToFile(workingDirectory + "/par.txt", "\nevecoutname: " + evecoutFilePath)
      appendToFile(workingDirectory + "/par.txt", "\nevaloutname: " + workingDirectory + "/eval.txt")
      appendToFile(workingDirectory + "/par.txt", "\nsnpweightoutname: " + workingDirectory + "/snpweights.txt")
      appendToFile(workingDirectory + "/par.txt", "\nkillr2: YES \nr2thresh: 0.2\nnumoutlieriter: 0")
      val (stdout, stderr, finished) = execGetStreamsAndCode("smartpca -p " + workingDirectory + "/par.txt")

      writeToFile(workingDirectory + "/smartpcalog.txt", (stdout ++ stderr).mkString("\n"))

      if (finished) {
        {
          val listb = scala.collection.mutable.ListBuffer[String]()
          var first = true

          openSource(evecoutFilePath)(_.getLines.foreach { line =>
            if (first) {
              first = false
              listb append line
            } else {
              val splitted = line.split("\\s+")
              val idx = splitted(1).split(":").head.toInt
              val (fid, iid) = famMap(idx)
              val x = fid + ":" + iid :: splitted.toList.drop(2)
              listb append x.mkString("\t")
            }
          })
          writeToFile(evecoutFilePath, listb.toList.mkString("\n"))
        }

        pop.foreach { popg =>
          var first = true
          val listb = scala.collection.mutable.ListBuffer[String]()

          openSource(evecoutFilePath)(_.getLines.foreach { line =>

            if (first) {
              first = false
              listb append line
            } else {
              val splitted = line.split("\\s+")
              val fid = splitted(1).split(":").head
              val iid = splitted(1).split(":")(1)
              val population = popg.find(x => x('V1) == fid && x('V2) == iid).orElse(Some(Map('V3 -> "???"))).get('V3);
              val x = splitted.dropRight(1).toList :+ population
              listb append x.mkString("\t")
            }
          })
          writeToFile(workingDirectory + "/evecwithpop.txt", listb.toList.mkString("\n"))
        }
        true
      } else false
    } else {
      false
    }

  }

}