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

package mybiotools.gwascommons.genotypedata

import mybiotools._
import mybiotools.stringstore._
import mybiotools.gwascommons._
import mybiotools.gwascommons.gwas._
import scala.io.Source
import org.saddle._
import java.io.RandomAccessFile
import mybiotools.eq._
import java.io.File

sealed trait SNPMajorDosageInputFileSet
case class PDoseFiles(source: scala.io.Source, missingValue: Float, fam: Option[Seq[Individual]] = None) extends SNPMajorDosageInputFileSet
case class PGenotypeProbabilitiesFiles(source: scala.io.Source, missingValue: Float, fam: Option[Seq[Individual]] = None) extends SNPMajorDosageInputFileSet
case class TPedFiles(fam: scala.io.Source, tped: scala.io.Source, missingValue: Char) extends SNPMajorDosageInputFileSet

sealed trait SNPMajorDosageInputFileSetBGZipped {
  def index: collection.Map[String8, Long]
  def file: File
  def missingValue: Float
  def fam: Option[Seq[Individual]]
}
case class BGZPDose(file: File, missingValue: Float, fam: Option[Seq[Individual]], index: collection.Map[String8, Long]) extends SNPMajorDosageInputFileSetBGZipped
case class BGZPGeno(file: File, missingValue: Float, fam: Option[Seq[Individual]], index: collection.Map[String8, Long]) extends SNPMajorDosageInputFileSetBGZipped

sealed trait SNPMajorSubset {
  def contains(idx: Int): Boolean
}
/** left inclusive right exlusive [a,b) */
case class FileSubSet(fromIdx: Int, toIdx: Int) extends SNPMajorSubset {
  assert(fromIdx >= 0 && toIdx >= 0 && toIdx >= fromIdx)
  def contains(idx: Int) = fromIdx <= idx && toIdx > idx
}
case object Full extends SNPMajorSubset {
  def contains(idx: Int) = true
}
case class RandomSubset(rate: Double, rng: org.apache.commons.math3.random.RandomGenerator = new org.apache.commons.math3.random.Well19937c(1)) extends SNPMajorSubset {
  assert(rate > 0 && rate <= 1)
  def contains(idx: Int) = rng.nextDouble <= rate
}

trait SNPMajorList extends SNPMajorIterators {
  def list: List[Tuple2[PDosageFileRowSummary, Array[Float]]]
  override def snpIterator = list.iterator
}

trait SNPMajorIterators {

  def individuals: Seq[Individual]
  def snpIterator: Iterator[Tuple2[PDosageFileRowSummary, Array[Float]]]
  def duplicatedSNPs: Set[String8]
  def duplicatedIndividuals: Set[Individual]
  def missingValue: Float

  def filterIndividuals(in: Set[Individual]): SNPMajorIterators = {
    val filtered = individuals.filter(in.contains)
    val filteredIdx = individuals.zipWithIndex.filter(x => in.contains(x._1)).map(_._2).toArray
    val snpiter = snpIterator.map(x => (x._1 -> filteredIdx.map(x._2)))
    SNPMajorIterators(filtered, snpiter, duplicatedSNPs, duplicatedIndividuals & in, missingValue)
  }

  def toLocusIteratorWithGenomicMap(map: GenomicMap) = {
    val iter = snpIterator.map {
      case (pdsum, genotypes) =>
        SNPMajorIterators.convertToLocusData(pdsum, genotypes, map, missingValue)
    }
    SNPMajorLocusIterator(individuals, iter)
  }

  def filter(p: (PDosageFileRowSummary => Boolean)) = SNPMajorIterators(
    individuals,
    snpIterator.filter(x => p(x._1)),
    duplicatedSNPs,
    duplicatedIndividuals,
    missingValue
  )

  def filterByIndex(f: FileSubSet) = SNPMajorIterators(
    individuals,
    snpIterator.zipWithIndex.filter(x => f.contains(x._2)).map(_._1),
    duplicatedSNPs,
    duplicatedIndividuals,
    missingValue
  )

  def prune(threshold: Double, window: Int, keepSnps: Set[String8], remove: Seq[Series[Individual, Float]], map: GenomicMap) = SNPMajorIterators(
    individuals,
    LDPruning.pruneIterator(snpIterator, map, missingValue, window, threshold, keepSnps, remove.map(_(individuals: _*).toVec.toSeq.toArray)),
    duplicatedSNPs,
    duplicatedIndividuals,
    missingValue
  )

  def prune(threshold: Double, window: Int, map: GenomicMap) = SNPMajorIterators(
    individuals,
    LDPruning.pruneIterator(snpIterator, map, missingValue, window, threshold, Set(), Nil),
    duplicatedSNPs,
    duplicatedIndividuals,
    missingValue
  )

  def prune(threshold: Double, window: Int) = SNPMajorIterators(
    individuals,
    LDPruning.pruneIteratorWithoutGenomicMap(snpIterator, missingValue, window, threshold, Set(), Nil),
    duplicatedSNPs,
    duplicatedIndividuals,
    missingValue
  )

  def toList = SNPMajorListImpl(individuals, snpIterator.toList, duplicatedSNPs, duplicatedIndividuals, missingValue)

}

object SNPMajorIterators {

  def recodeToGeneticModel(geno: Vector[Option[WithVariantDosage]], geneticModel: GeneticModel, indIdx: Index[Individual]) = {
    Series(Vec(geno.map(x => x.map(y => GWAS.recodeGenotype(y.variantAlleleDosage, geneticModel)).getOrElse(Double.NaN)): _*), indIdx)
  }

  def convertToLocusData(
    pdsum: PDosageFileRowSummary,
    genotypes: Array[Float],
    map: GenomicMap,
    missingValue: Float
  ): (LocusData, Vector[Option[WithVariantDosage]]) = {

    val locusdata = pdsum.toLocusDataWithGenomicLocation(map.get(pdsum.snpName))

    val d = if (locusdata.variant.isEmpty || StringStore(locusdata.variant.get.nucleotides) === pdsum.al1)
      genotypes.map(dosageOfFirstAllele =>
        if (dosageOfFirstAllele === missingValue || dosageOfFirstAllele.isNaN) None
        else Some(DosageGenotype(dosageOfFirstAllele.toDouble))).toVector
    else genotypes.map(dosageOfFirstAllele =>
      if (dosageOfFirstAllele === missingValue || dosageOfFirstAllele.isNaN) None
      else Some(DosageGenotype(2.0 - dosageOfFirstAllele.toDouble))).toVector

    (locusdata, d)
  }

  def unapply(x: SNPMajorIterators) = Some(x.individuals, x.snpIterator, x.duplicatedSNPs, x.duplicatedIndividuals, x.missingValue)

  def apply(
    individuals: Seq[Individual],
    snpIterator: Iterator[Tuple2[PDosageFileRowSummary, Array[Float]]],
    duplicatedSNPs: Set[String8],
    duplicatedIndividuals: Set[Individual],
    missingValue: Float
  ) = SNPMajorIteratorsImpl(individuals, snpIterator, duplicatedSNPs, duplicatedIndividuals, missingValue)
}

case class SNPMajorIteratorsImpl(
  individuals: Seq[Individual],
  snpIterator: Iterator[Tuple2[PDosageFileRowSummary, Array[Float]]],
  duplicatedSNPs: Set[String8],
  duplicatedIndividuals: Set[Individual],
  missingValue: Float
) extends SNPMajorIterators

case class SNPMajorListImpl(
  individuals: Seq[Individual],
  list: List[Tuple2[PDosageFileRowSummary, Array[Float]]],
  duplicatedSNPs: Set[String8],
  duplicatedIndividuals: Set[Individual],
  missingValue: Float
) extends SNPMajorList

trait SNPMajorLocusIterator {

  def individuals: Seq[Individual]

  def loci: Iterator[(LocusData, Vector[Option[WithVariantDosage]])]

  def recodedToGeneticModel(geneticModel: GeneticModel): Iterator[(LocusData, Series[Individual, Double])] = {

    val indIdx = Index(individuals: _*)
    loci.map {
      case (ld, geno) =>
        ld -> SNPMajorIterators.recodeToGeneticModel(geno, geneticModel, indIdx)
    }

  }

  def maskToRegion(r: Region) = SNPMajorLocusIterator(individuals, loci = loci.filter(_._1.genomicLocation.map(gl => r.contains(gl)).getOrElse(true)))
  def filter(p: (LocusData => Boolean)) = SNPMajorLocusIterator(individuals, loci.filter(x => p(x._1)))

  def toFrame: Frame[Individual, String8, Double] = {
    Frame(loci.map {
      case (locusData, dosages) =>
        locusData.name -> Series((individuals zip dosages.map(_.map(_.variantAlleleDosage).getOrElse(Double.NaN))): _*)
    }.toList: _*)
  }

  def toList = SNPMajorLocusList(individuals, loci.toList)

}

object SNPMajorLocusIterator {
  def apply(
    individuals: Seq[Individual],
    loci: Iterator[(LocusData, Vector[Option[WithVariantDosage]])]
  ) = SNPMajorLocusIteratorsImpl(individuals, loci)
}

case class SNPMajorLocusIteratorsImpl(
  val individuals: Seq[Individual],
  val loci: Iterator[(LocusData, Vector[Option[WithVariantDosage]])]
) extends SNPMajorLocusIterator

case class SNPMajorLocusList(
    val individuals: Seq[Individual],
    list: List[(LocusData, Vector[Option[WithVariantDosage]])]
) extends SNPMajorLocusIterator {

  def loci = list.iterator

}

object SNPMajorReaders {

  def concatenate(iters1: Iterator[(SNPMajorIterators, { def close(): Unit })]): Option[SNPMajorIterators] = {
    var missingValue: Option[Float] = None
    var inds: Option[Seq[Individual]] = None

    val iters = iters1.filter(_._1.snpIterator.hasNext)

    if (!iters.hasNext) None
    else {
      var current = iters.next
      if (missingValue.isEmpty) missingValue = Some(current._1.missingValue)
      if (inds.isEmpty) inds = Some(current._1.individuals)

      val snpiter = new Iterator[(PDosageFileRowSummary, Array[Float])] {
        def hasNext = current._1.snpIterator.hasNext

        def next = {
          val r = current._1.snpIterator.next
          if (!current._1.snpIterator.hasNext && iters.hasNext) {
            current._2.close
            current = iters.next
            assert(current._1.missingValue == missingValue.get || (missingValue.get.isNaN && current._1.missingValue.isNaN), "missing ")
            assert(current._1.individuals == inds.get, "inds: " + current._1.individuals + "  vs " + inds.get)
          }
          r
        }
      }

      Some(SNPMajorIterators(inds.get, snpiter, Set(), Set(), missingValue.get))

    }

  }

  val MissingValue = -9f

  def getSNPMajorIterator(inputfiles: SNPMajorDosageInputFileSet): SNPMajorIterators = getSNPMajorIterator(inputfiles, Full, Set(), false)

  def getSNPMajorIterator(inputfiles: SNPMajorDosageInputFileSet, subset: SNPMajorSubset): SNPMajorIterators = getSNPMajorIterator(inputfiles, subset, Set(), false)

  def getSNPMajorIterator(inputfiles: SNPMajorDosageInputFileSet, subset: SNPMajorSubset, filterMarkerNames: Set[String]): SNPMajorIterators = getSNPMajorIterator(inputfiles, subset, filterMarkerNames, false)

  def getSNPMajorIterator(bed: java.io.InputStream, fam: scala.io.Source, bim: scala.io.Source, subset: SNPMajorSubset, filterMarkerNames: Set[String]): SNPMajorIterators = {

    val individuals = getIndividualsFromFamFile(fam)
    val numIndividuals = individuals.size
    val linelength = individuals.size / 4 + (if (individuals.size % 4 != 0) 1 else 0)
    val array = Array.ofDim[Byte](linelength)
    val snps = getBimEntriesWithAllelesWithoutLocation(bim)
    val magicnumber1 = bed.read
    val magicnumber2 = bed.read
    val snpmajor = bed.read
    assert(snpmajor == 1 && magicnumber1 == 108 && magicnumber2 == 27, "This is not an SNP major plink bed file. ")

    val iter = snps.zipWithIndex.flatMap {
      case ((name, a1, a2), idx1) =>

        if ((filterMarkerNames.isEmpty || filterMarkerNames.contains(name.value)) && subset.contains(idx1)) {

          var nr = linelength
          while (nr != 0) {
            val n = bed.read(array, array.size - nr, nr)
            assert(n >= 0, s"EOF reached in bed reader read returned: $n, missing bytes: $nr, snp: $name / $idx1")
            nr -= n
          }

          val dosagesOfA1 = Array.ofDim[Float](individuals.size)
          java.util.Arrays.fill(dosagesOfA1, 0.0f)

          BinaryPlinkFileHelper.decodeBinaryBedLine(array, linelength, numIndividuals, dosagesOfA1)

          val pdrs = PDosageFileRowSummary(name, a1, a2, dosagesOfA1, Float.NaN)
          Some((pdrs, dosagesOfA1))
        } else {
          var sumskipped = 0
          while (sumskipped != linelength) {
            val skipped = bed.skip(linelength - sumskipped)
            if (skipped >= 0) {
              sumskipped += skipped.toInt
            }
          }

          None
        }
    }

    SNPMajorIterators(
      individuals = individuals,
      snpIterator = iter,
      duplicatedSNPs = Set(),
      duplicatedIndividuals = Set(),
      missingValue = Float.NaN
    )

  }

  def getIndividualsFromSingleDosage(pdosage: scala.io.Source) = {
    val iter = pdosage.getLines
    fastSplitSetSeparator(iter.next, Set(' ', '\t', ',')).drop(3).grouped(2).map { group =>
      Individual(group(0), group(1))
    }.toIndexedSeq
  }

  def getSNPMajorIterator(inputfiles: SNPMajorDosageInputFileSet, subset: SNPMajorSubset, filterMarkerNames: Set[String], checkForDuplicatedSNPs: Boolean): SNPMajorIterators = {

    val individuals: IndexedSeq[Individual] = inputfiles match {
      case PDoseFiles(pdosage, _, None) => getIndividualsFromSingleDosage(pdosage)
      case PGenotypeProbabilitiesFiles(pdosage, _, None) => getIndividualsFromSingleDosage(pdosage)
      case PDoseFiles(_, _, Some(inds)) => inds.toIndexedSeq
      case PGenotypeProbabilitiesFiles(_, _, Some(inds)) => inds.toIndexedSeq
      case TPedFiles(fam, _, _) => {
        fam.getLines.map { line =>
          val spl = fastSplitSetSeparator(line, Set(' ', '\t'))
          Individual(spl(0), spl(1))
        }.toIndexedSeq
      }
    }
    val wsSep = Set(' ', '\t', ',')

    // Search for duplicate individuals
    val duplicatedIndividuals = (individuals.foldLeft(Map[Individual, Int]()) { (aggr, x) =>
      if (aggr.contains(x)) aggr.updated(x, aggr(x) + 1)
      else aggr + (x -> 1)
    }).filter(_._2 > 1).keys.toSet

    val (iter, duplicatedSNPs) = if (checkForDuplicatedSNPs) {

      val snpNameIter = inputfiles match {
        case PDoseFiles(pdosage, _, _) => pdosage.getLines.map { line =>
          StringStore(new java.lang.String(fastSplitSetSeparatorIterator(line, wsSep).next))
        }
        case PGenotypeProbabilitiesFiles(pdosage, _, _) => pdosage.getLines.map { line =>
          StringStore(new java.lang.String(fastSplitSetSeparatorIterator(line, wsSep).next))
        }
        case TPedFiles(_, tped, _) => tped.getLines.map { line =>
          val it = fastSplitSetSeparatorIterator(line, wsSep)
          it.next
          StringStore(new java.lang.String(it.next))
        }
      }

      val tup = {
        val snpCountsWithPositions = scala.collection.mutable.Map[String8, Tuple2[Int, Int]]()
        val allLines = collection.mutable.Set[Int]()
        var counter = 0

        snpNameIter.zipWithIndex.filter(snpNameWithIdx => filterMarkerNames.isEmpty || filterMarkerNames.contains(snpNameWithIdx._1)).foreach {
          case (snpName, idx) =>
            // val snpName = new java.lang.String(fastSplitSetSeparatorIterator(snpLine, wsSep).next)
            counter += 1

            if (!filterMarkerNames.isEmpty) { allLines += idx }

            snpCountsWithPositions.get(snpName) match {
              case None => {
                snpCountsWithPositions.update(snpName, (1, idx))

              }
              case Some(tup) => snpCountsWithPositions.update(snpName, (tup._1 + 1, tup._2))
            }

        }

        val duplicates = snpCountsWithPositions.filter(_._2._1 > 1)

        (counter + 1, duplicates.keys.toSet, duplicates.map(_._2._2).toSet, allLines.toSet &~ duplicates.map(_._2._2).toSet)
      }
      val duplicatedSNPs = tup._2
      val invalidLines = tup._3
      val validLines = tup._4

      val iter = inputfiles match {
        case PDoseFiles(pdosage, _, None) => pdosage.reset.getLines.drop(1)
        case PDoseFiles(pdosage, _, Some(_)) => pdosage.reset.getLines
        case PGenotypeProbabilitiesFiles(pdosage, _, None) => pdosage.reset.getLines.drop(1)
        case PGenotypeProbabilitiesFiles(pdosage, _, Some(_)) => pdosage.reset.getLines
        case TPedFiles(_, tped, _) => tped.reset.getLines
      }

      iter.zipWithIndex.filterNot { lineWithIndex =>
        invalidLines.contains(lineWithIndex._2) ||
          !subset.contains(lineWithIndex._2) ||
          !(filterMarkerNames.isEmpty || validLines.contains(lineWithIndex._2))
      }.map(_._1) -> duplicatedSNPs

    } else {

      val iter = inputfiles match {
        case PDoseFiles(pdosage, _, None) => pdosage.reset.getLines.drop(1)
        case PDoseFiles(pdosage, _, Some(_)) => pdosage.reset.getLines
        case PGenotypeProbabilitiesFiles(pdosage, _, None) => pdosage.reset.getLines.drop(1)
        case PGenotypeProbabilitiesFiles(pdosage, _, Some(_)) => pdosage.reset.getLines
        case TPedFiles(_, tped, _) => tped.reset.getLines
      }

      iter.zipWithIndex.filter(x => subset.contains(x._2)).map(_._1) -> Set[String8]()

    }

    val iterToHDF = iter.map { line =>

      val arrayOfDosages: Array[Float] = Array.fill(individuals.size)(-18f)

      val (snpName: String8, a1: String8, a2: String8) = {
        val spl = fastSplitSetSeparatorIterator(line, wsSep)

        inputfiles match {
          case PGenotypeProbabilitiesFiles(_, _, _) | PDoseFiles(_, _, _) => {
            val snpName = StringStore(new java.lang.String(spl.next))
            val a1 = StringStore(new java.lang.String(spl.next))
            val a2 = StringStore(new java.lang.String(spl.next))
            (snpName, a1, a2)
          }
          case TPedFiles(_, _, missingValue) => {
            val chr = spl.next
            val snpName = StringStore(new java.lang.String(spl.next))
            val junk1 = spl.next
            val junk2 = spl.next
            val a1 = StringStore(new java.lang.String(spl.find(_(0) != missingValue).getOrElse("?")))
            val a2 = StringStore(new java.lang.String(spl.find(x => x(0) != a1(0) && x(0) != missingValue).getOrElse(missingValue.toString)))
            (snpName, a1, a2)
          }

        }
      }

      if (filterMarkerNames.isEmpty || filterMarkerNames.contains(snpName)) {

        val spl = fastSplitSetSeparatorIterator(line, wsSep)

        inputfiles match {
          case PDoseFiles(_, missingValue, _) => {
            spl.drop(3).zipWithIndex.foreach {
              case (x, idx) =>
                val value = x.toFloat
                if (!((value >= 0.0f && value <= 2.0f) || value == missingValue)) throw new RuntimeException("invalid value: " + value)
                val y = if (value == missingValue) MissingValue else value
                arrayOfDosages(idx) = y
            }
          }
          case PGenotypeProbabilitiesFiles(_, missingValue, _) => {
            spl.drop(3).grouped(2).zipWithIndex.foreach {
              case (group, idx) =>
                val probOfHomozygote1 = group(0)
                val probOfHeterozygote = group(1)
                val value = (probOfHomozygote1.toDouble * 2.0 + probOfHeterozygote.toDouble).toFloat
                if (!((value >= 0.0f && value <= 2.0f) || probOfHeterozygote.toFloat == missingValue || probOfHomozygote1.toFloat == missingValue)) throw new RuntimeException("invalid value: " + value)
                val y = if (probOfHomozygote1.toFloat == missingValue || probOfHeterozygote.toFloat == missingValue) MissingValue else value
                arrayOfDosages(idx) = y
            }
          }
          case TPedFiles(_, _, missingValue) => {
            spl.drop(4).grouped(2).zipWithIndex.foreach {
              case (group, idx) =>

                val y = group.toList match {
                  case List(x, y) if x(0) == missingValue || y(0) == missingValue => MissingValue
                  case List(x, y) if x(0) == a1(0) && y(0) == a1(0) => 2.0f
                  case List(x, y) if (x(0) == a1(0) && y(0) == a2(0)) || (x(0) == a2(0) && y(0) == a1(0)) => 1.0f
                  case List(x, y) if x(0) == a2(0) && y(0) == a2(0) => 0.0f
                  case List(_, _) => MissingValue
                }

                arrayOfDosages(idx) = y
            }
          }
        }

        val snpSummary = PDosageFileRowSummary(snpName, a1, a2, arrayOfDosages, MissingValue)
        Some((snpSummary, arrayOfDosages))
      } else None
    } filter (_.isDefined) map (_.get)

    SNPMajorIterators(
      individuals = individuals,
      snpIterator = iterToHDF,
      duplicatedSNPs = duplicatedSNPs,
      duplicatedIndividuals = duplicatedIndividuals,
      missingValue = MissingValue
    )

  }

}

trait SNPMajorQuery {

  def individuals: Seq[Individual]
  def query(name: String8): Option[(PDosageFileRowSummary, Array[Float])]
  def allSNPs: Vector[String8]
  def missingValue: Float
  def indIdx = Index(individuals: _*)

  def query[T <: HasName](name: T): Option[(PDosageFileRowSummary, Array[Float])] = query(name.name)

  def queryLocus[T <: HasName](name: T, map: GenomicMap): Option[(LocusData, Vector[Option[mybiotools.gwascommons.WithVariantDosage]])] =
    query(name) map {
      case (pd, geno) =>
        SNPMajorIterators.convertToLocusData(pd, geno, map, missingValue)
    }

  def queryLocusRecodedToGeneticModel[T <: HasName](name: T, geneticModel: GeneticModel, map: GenomicMap): Option[(LocusData, Series[Individual, Double])] =
    query(name) map {
      case (pd, geno) =>
        val (locusdata, withvariantdosage) = SNPMajorIterators.convertToLocusData(pd, geno, map, missingValue)
        (locusdata, SNPMajorIterators.recodeToGeneticModel(withvariantdosage, geneticModel, indIdx))
    }

  def queryLocusRecodedToAdditive[T <: HasName](name: T, map: GenomicMap): Option[(LocusData, Series[Individual, Double])] = queryLocusRecodedToGeneticModel(name, Additive, map)

  def sortedIterator(map: GenomicMap): SNPMajorIterators = {
    val iter = allSNPs.filter(map.contains).sortBy(map).iterator.map { snpName =>
      query(snpName).get
    }

    SNPMajorIteratorsImpl(
      individuals,
      iter,
      Set(),
      Set(),
      missingValue
    )
  }

}

class CompositeSNPMajorQuery(members: Seq[SNPMajorQuery]) extends SNPMajorQuery {
  assert(members.map(_.individuals).distinct.size == 1, "member readers do not have the same layout")
  assert(members.map(_.missingValue).distinct.size == 1, "member readers do not have the same missing value")
  val missingValue = members.head.missingValue
  val individuals = members.head.individuals
  def query(name: String8) = members.find(_.query(name).isDefined).flatMap(_.query(name))
  val allSNPs = members.flatMap(_.allSNPs).distinct.toVector
}

class RandomAccessBedReader(bed: RandomAccessFile, bim: Source, fam: Source) extends SNPMajorQuery {

  val individuals = getIndividualsFromFamFile(fam)
  private val numIndividuals = individuals.size

  private val linelength = individuals.size / 4 + (if (individuals.size % 4 != 0) 1 else 0)

  private val array = Array.ofDim[Byte](linelength)

  val snps: Map[String8, (Int, String8, String8, String8)] = getBimEntriesWithAllelesWithoutLocation(bim).zipWithIndex.map(x => x._1._1 -> (x._2, x._1._1, x._1._2, x._1._3)).toMap

  val allSNPs = getBimEntriesWithAllelesWithoutLocation(bim).map(_._1).toVector

  private val magicnumber1 = bed.read
  private val magicnumber2 = bed.read
  private val snpmajor = bed.read
  assert(snpmajor == 1 && magicnumber1 == 108 && magicnumber2 == 27, "This is not an SNP major plink bed file. ")

  private val startOfData: Long = bed.getFilePointer

  val missingValue = Float.NaN

  def query(snp: String8) = snps.get(snp).map {
    case (idx1, name, a1, a2) =>

      bed.seek(idx1 * linelength + startOfData)

      var nr = linelength
      while (nr != 0) {

        val n = bed.read(array, array.size - nr, nr)
        assert(n >= 0, s"EOF reached in bed reader read returned: $n, missing bytes: $nr, snp: $name / $idx1")
        nr -= n
      }

      val dosagesOfA1 = Array.ofDim[Float](individuals.size)
      java.util.Arrays.fill(dosagesOfA1, 0.0f)

      BinaryPlinkFileHelper.decodeBinaryBedLine(array, linelength, numIndividuals, dosagesOfA1)

      val pdrs = PDosageFileRowSummary(name, a1, a2, dosagesOfA1, Float.NaN)
      (pdrs, dosagesOfA1)

  }

}

object BinaryPlinkFileHelper {
  @inline
  def getBits(b: Byte, pos: Int) = (b >> pos) & 3

  @inline
  def getDosageOfFirstBimAllele(bits: Int): Float =
    if (bits == 1) Float.NaN
    else if (bits == 0) 2.0f
    else if (bits == 2) 1.0f
    else 0.0f

  def decodeBinaryBedLine(array: Array[Byte], linelength: Int, individuals: Int, dosagesOfA1: Array[Float]) = {

    var idx = 0
    while (idx < array.size) {
      val b = array(idx)
      val indsInThisByte = if (idx < linelength - 1) 4 else individuals - (idx * 4)
      var j = 0
      while (j < indsInThisByte) {
        val dosage = getDosageOfFirstBimAllele(getBits(b, j * 2))
        if (dosage != 0.0f) {
          dosagesOfA1(idx * 4 + j) = dosage
        }

        j += 1
      }
      idx += 1
    }
  }
}

object BGZippedGenotypeHelper {
  import mybiotools.flatindex._

  def makeIndex(file: File): collection.Map[String8, Long] = openBlockedZippedFileInputStream(file) { bcis =>
    indexAscii(bcis) { line =>
      StringStore(line.fastSplitIterator(Set(' ', '\t', ',')).next)
    }
  }
}

class DelimitedSNPMajorQuery(file: SNPMajorDosageInputFileSetBGZipped) extends SNPMajorQuery {
  import htsjdk.samtools.util.{ BlockCompressedInputStream }
  import mybiotools.flatindex._

  def getIndividualsFromSingleDosage(pdosage: scala.io.Source) = {
    val iter = pdosage.getLines
    fastSplitSetSeparator(iter.next, Set(' ', '\t', ',')).drop(3).grouped(2).map { group =>
      Individual(group(0), group(1))
    }.toIndexedSeq
  }

  val individuals: IndexedSeq[Individual] = file match {
    case BGZPGeno(f, _, None, _) => openSource(f)(getIndividualsFromSingleDosage)
    case BGZPDose(f, _, None, _) => openSource(f)(getIndividualsFromSingleDosage)
    case BGZPGeno(_, _, Some(inds), _) => inds.toIndexedSeq
    case BGZPDose(_, _, Some(inds), _) => inds.toIndexedSeq
  }

  val missingValue = file.missingValue

  val bcis = new BlockCompressedInputStream(file.file)

  val allSNPs = file.index.toSeq.sortBy(_._2).map(_._1).toVector.filterNot(_ === s8"SNP")

  val queryFun = lookupAscii(bcis, file.index) { line =>

    val arrayOfDosages: Array[Float] = Array.fill(individuals.size)(-18f)

    val spl = fastSplitSetSeparatorIterator(line, Set(' ', '\t', ','))

    val (snpName: String8, a1: String8, a2: String8) = {

      {
        val snpName = StringStore(new java.lang.String(spl.next))
        val a1 = StringStore(new java.lang.String(spl.next))
        val a2 = StringStore(new java.lang.String(spl.next))
        (snpName, a1, a2)
      }

    }

    file match {
      case BGZPDose(_, missingValue, _, _) => {
        spl.zipWithIndex.foreach {
          case (x, idx) =>
            val value = x.toFloat
            if (!((value >= 0.0f && value <= 2.0f) || value == missingValue)) throw new RuntimeException("invalid value: " + value)
            val y = if (value == missingValue) file.missingValue else value
            arrayOfDosages(idx) = y
        }
      }
      case BGZPGeno(_, missingValue, _, _) => {
        spl.grouped(2).zipWithIndex.foreach {
          case (group, idx) =>
            val probOfHomozygote1 = group(0)
            val probOfHeterozygote = group(1)
            val value = (probOfHomozygote1.toDouble * 2.0 + probOfHeterozygote.toDouble).toFloat
            if (!((value >= 0.0f && value <= 2.0f) || probOfHeterozygote.toFloat == missingValue || probOfHomozygote1.toFloat == missingValue)) throw new RuntimeException("invalid value: " + value)
            val y = if (probOfHomozygote1.toFloat == missingValue || probOfHeterozygote.toFloat == missingValue) file.missingValue else value
            arrayOfDosages(idx) = y
        }
      }

    }

    val snpSummary = PDosageFileRowSummary(snpName, a1, a2, arrayOfDosages, file.missingValue)
    (snpSummary, arrayOfDosages)
  }

  def query(name: String8): Option[(PDosageFileRowSummary, Array[Float])] = {
    if (file.index.contains(name)) Some(queryFun(name)) else None
  }

  def close(): Unit = bcis.close

}
