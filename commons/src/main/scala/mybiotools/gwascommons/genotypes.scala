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

package mybiotools.gwascommons
import mybiotools._

import mybiotools.stringstore._

case class GenomicLocation(basePairPosition: Int, chromosomeAsT: String8) extends GenomicLocationStub[String8] {
  def chromosome = chromosomeAsT.value
}

trait GenomicLocationStub[@specialized(Int) +T] {
  def basePairPosition: Int
  def chromosomeAsT: T
  def chromosome: String
  def basePairPosition0based = basePairPosition - 1
}

object GenomicLocation {
  def apply(bp: Int, chr: String): GenomicLocation = GenomicLocation(bp, StringStore(chr))
  def apply(bp: Int, chr: Int): GenomicLocation = GenomicLocation(bp, StringStore(chr.toString))
  def apply(chrbp: String): GenomicLocation = try {
    val spl = fastSplit1WideSeparator(chrbp, ':')

    val chr = StringStore(spl(0))
    val bp = spl(1).toInt
    GenomicLocation(bp, chr)
  } catch {
    case x: Throwable => throw new RuntimeException(s"$chrbp can't be converted to GenomicLocation")
  }
}

trait HasGenomicLocation {
  def genomicLocation: GenomicLocation
}

trait MayHaveGenomicLocation {
  def genomicLocation: Option[GenomicLocation]
}

trait HasName {
  def name: String8
}

trait HasMaf {
  def maf: Double
}

case class OnlyName(name: String8) extends HasName

case class Locus(name: String8, genomicLocation: GenomicLocation) extends HasName with HasGenomicLocation {
  def toLine = genomicLocation.chromosomeAsT + " " + name.value + " " + genomicLocation.basePairPosition
}

// locus
trait AllelicContext[A <: Allele] {
  val alleles: Seq[A]

  /** Majority/reference allele at this locus */
  def reference: A

  /** Minority/variant allele at this locus */
  def variant: Option[A]

  lazy val indel = alleles.exists(_.nucleotides == "-")
  lazy val snp = !indel && alleles.forall(_.snp)

}

trait AllelicContextWithAF extends AllelicContext[SimpleAllele] with HasMaf {

  val alleleFrequencies: List[(SimpleAllele, Double)]

  lazy val reference = alleleFrequencies.reverse.sortBy(x => -1 * x._2).head._1

  lazy val variant = alleles.filterNot(_ == reference).headOption

  lazy val maf = variant.map(x => alleleFrequencies.toMap.apply(x)).getOrElse(0.0)

}

trait Allele {

  val nucleotides: String

  override def toString = nucleotides

  lazy val snp = nucleotides.size == 1

  lazy val purine = snp && (nucleotides.toUpperCase == "A" || nucleotides.toUpperCase == "G")
  lazy val pirimidine = snp && (nucleotides.toUpperCase == "T" || nucleotides.toUpperCase == "C")

  // is the mutation This to That a transition?
  def transition(that: Allele) = this != that && this.nucleotides != that.nucleotides && this.snp && that.snp && ((this.purine && that.purine) || (this.pirimidine && that.pirimidine))

  // is the mutation This to That a transversion?
  def transversion(that: Allele) = this != that && this.nucleotides != that.nucleotides && this.snp && that.snp && ((this.purine && that.pirimidine) || (that.purine && this.pirimidine))

}

trait WithVariantDosage {
  def variantAlleleDosage: Double
}

case class DosageGenotype(variantAlleleDosage: Double) extends WithVariantDosage

sealed trait GenotypePhase
case class Phased(phaseIDs: List[Int]) extends GenotypePhase {
  assert(phaseIDs.size <= 2)
}
case object Unphased extends GenotypePhase

trait DiscreteGenotypeProto extends WithVariantDosage {

  def noCall: Boolean
  def isHomRef: Boolean
  def isHomVar: Boolean
  def isHet: Boolean

  /**
   * Number of variant (mutant) alleles in this genotype
   *
   * Possible values are 0 1 2
   * NoCall alleles count as exception
   */
  lazy val variantAlleleDosage = if (this.noCall) throw new RuntimeException("genotype is missing/nocall")
  else if (this.isHomRef) {
    0.0
  } else {
    if (this.isHet) { 1.0 }
    else { 2.0 }
  }

}
object DiscreteGenotypes {
  case object Heterozygous extends DiscreteGenotypeProto {
    def noCall = false
    def isHomRef = false
    def isHomVar = false
    def isHet = true
  }

  case object HomozygousRef extends DiscreteGenotypeProto {
    def noCall = false
    def isHomRef = true
    def isHomVar = false
    def isHet = false
  }

  case object HomozygousVar extends DiscreteGenotypeProto {
    def noCall = false
    def isHomRef = false
    def isHomVar = true
    def isHet = false
  }

  case object Missing extends DiscreteGenotypeProto {
    def noCall = true
    def isHomRef = false
    def isHomVar = false
    def isHet = false
  }
  val Het = Heterozygous
  val HomRef = HomozygousRef
  val HomVar = HomozygousVar
}

trait BiAllelicLocus[A <: Allele] {

  def allele1: A

  def allele2: A

}

// To interpret plink files
case class SimpleAllele(nucleotides: String) extends Allele

object SimpleAllele {
  def makeSingleton(str: String): SimpleAllele = str.toUpperCase match {
    case "A" => Allele_A
    case "T" => Allele_T
    case "G" => Allele_G
    case "C" => Allele_C
    case "." => Allele_Missing
    case "0" => Allele_Zero
    case "I" => Allele_Insert
    case "D" => Allele_Delete
    case "1" => Allele_One
    case "2" => Allele_Two
    case x => SimpleAllele(x)
  }

  def makeSingleton(ch: Char): SimpleAllele = apply(ch.toString)

  def opposite(a: SimpleAllele) = a match {
    case Allele_A => Allele_T
    case Allele_T => Allele_A
    case Allele_G => Allele_C
    case Allele_C => Allele_G
    case Allele_Insert => Allele_Delete
    case Allele_Delete => Allele_Insert
    case _ => throw new RuntimeException("opposite of " + a)
  }

}

object Allele_One extends SimpleAllele("1")
object Allele_Zero extends SimpleAllele("0")
object Allele_Two extends SimpleAllele("2")
object Allele_A extends SimpleAllele("A")
object Allele_T extends SimpleAllele("T")
object Allele_G extends SimpleAllele("G")
object Allele_C extends SimpleAllele("C")
object Allele_Insert extends SimpleAllele("I")
object Allele_Delete extends SimpleAllele("D")
object Allele_Missing extends SimpleAllele(".")

case class LocusData(
    name: String8,
    genomicLocation: Option[GenomicLocation],
    alleles: Seq[SimpleAllele],
    alleleFrequencies: List[(SimpleAllele, Double)]
) extends AllelicContextWithAF with HasName with MayHaveGenomicLocation {

  /** write plink map line */
  def toMapLine = genomicLocation.map(_.chromosomeAsT).getOrElse(".") + " " + name + " 0 " + genomicLocation.map(_.basePairPosition).getOrElse(".")

  def toLocus: Option[Locus] = genomicLocation.map(gl => Locus(name, gl))

}

object LocusData {
  def apply(
    name: String8,
    genomicLocation: GenomicLocation,
    alleles: Seq[SimpleAllele],
    alleleFrequencies: List[(SimpleAllele, Double)]
  ): LocusData = LocusData(name, Some(genomicLocation), alleles, alleleFrequencies)
}

// These are mainly for interpreting genotypes in VCF files.

trait AllelicContextWithReference extends AllelicContext[AlleleWithReference] {

  lazy val reference: AlleleWithReference = alleles.filter(_.reference).head

  lazy val variant: Option[AlleleWithReference] = alleles.filterNot(_.reference).headOption

}

trait AlleleWithReference extends Allele {

  /** Majority/reference allele? */
  def reference: Boolean

}

object Allele {
  def applyWithSingletons(nucleotides: String, reference: Boolean): AlleleWithReference = {
    nucleotides.toUpperCase match {
      case "A" => if (reference) Allele_A_REF else Allele_A_VAR
      case "T" => if (reference) Allele_T_REF else Allele_T_VAR
      case "G" => if (reference) Allele_G_REF else Allele_G_VAR
      case "C" => if (reference) Allele_C_REF else Allele_C_VAR
      case "-" => if (reference) Allele_INSERT_REF else Allele_INSERT_VAR
      case "N" => if (reference) Allele_N_REF else Allele_N_VAR
      case "." => Allele_NoCall
      case _ => Allele1(nucleotides, reference)
    }
  }
}

case class Allele1(nucleotides: String, reference: Boolean) extends AlleleWithReference

object Allele_NoCall extends Allele1(".", false)
object Allele_A_REF extends Allele1("A", true)
object Allele_A_VAR extends Allele1("A", false)
object Allele_T_REF extends Allele1("T", true)
object Allele_T_VAR extends Allele1("T", false)
object Allele_G_REF extends Allele1("G", true)
object Allele_G_VAR extends Allele1("G", false)
object Allele_C_REF extends Allele1("C", true)
object Allele_C_VAR extends Allele1("C", false)
object Allele_INSERT_REF extends Allele1("-", true) { override lazy val snp = false }
object Allele_INSERT_VAR extends Allele1("-", false) { override lazy val snp = false }
object Allele_N_REF extends Allele1("N", true)
object Allele_N_VAR extends Allele1("N", false)

trait GenotypeLikeWithReference extends BiAllelicLocus[AlleleWithReference] with DiscreteGenotypeProto {

  def noCall = allele1 == Allele_NoCall || allele2 == Allele_NoCall
  def isHomRef = !noCall && allele1 == allele2 && allele1.reference
  def isHomVar = !noCall && allele1 == allele2 && !allele1.reference
  def isHet = !noCall && allele1 != allele2

}

trait GenotypeLikeInAllelicContextWithReference extends GenotypeLikeWithReference {

  def variant: HasName with HasGenomicLocation with AllelicContextWithReference

  assert((variant.alleles.contains(allele1) || allele1 == Allele_NoCall) && (variant.alleles.contains(allele2) || allele2 == Allele_NoCall), variant.alleles.toString + " vs " + allele1 + allele2)
}
