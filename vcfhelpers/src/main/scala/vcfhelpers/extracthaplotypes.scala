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

package vcfhelpers
import mybiotools._
import mybiotools.gwascommons._
import mybiotools.stringstore._
import scala.collection.JavaConversions._
import mybiotools.gwascommons.DiscreteGenotypes._

object ExtractHaplotypes {

  case class VariantWithPhase(phase: GenotypePhase, genotype: DiscreteGenotypeProto, location: GenomicLocation, reference: String8, alternate: String8, attribute: String)

  case class Event(individual: Individual, variants: Set[VariantWithPhase]) {
    /** This is only allowed for Events from the same vcf (because phaseIDs) */
    private[vcfhelpers] def +(that: Event) = if (this.individual == that.individual) Event(individual, this.variants ++ that.variants) else throw new IllegalArgumentException(s"$this + $that")
  }

  def extractVariantOnSamePhase(source: scala.io.Source, targetVariant: GenomicLocation): Seq[Event] = {

    val (vcfheader, vcfiterator) = vcfhelpers.VCFHelpers.readVCF(source)

    val phaseIDs = collection.mutable.AnyRefMap[Individual, Int]()

    val individualsWithPhasedData: Seq[Event] = vcfiterator.flatMap { vcontext =>
      val chr = vcontext.getChr
      val bp = vcontext.getStart
      val ref = vcontext.getReference.getBaseString
      val alts = vcontext.getAlternateAlleles.toIndexedSeq.map(_.getBaseString)
      val gloc = GenomicLocation(bp, chr)

      val attribute = vcontext.getAttributeAsString("EFF", "")

      vcontext.getGenotypes.iterator.filter(_.isCalled).map { gt =>
        val phased = gt.isPhased
        val firstAllele = gt.getAllele(0).getBaseString
        val secondAllele = gt.getAllele(1).getBaseString
        val samplename = gt.getSampleName
        val individual = Individual(samplename)

        if (!phased) {
          phaseIDs.get(individual) match {
            case None => phaseIDs.update(individual, 0)
            case Some(x) => phaseIDs.update(individual, x + 2)
          }
        } else {
          phaseIDs.get(individual) match {
            case None => phaseIDs.update(individual, 0)
            case _ => {}
          }
        }

        val currentPhaseID = phaseIDs(individual)

        val (genotype, phase) = (phased, firstAllele, secondAllele) match {
          case (true, a1, a2) if a1 == a2 && a1 == ref => (HomRef, Phased(currentPhaseID :: currentPhaseID + 1 :: Nil))
          case (true, a1, a2) if a1 == a2 => (HomVar, Phased(currentPhaseID :: currentPhaseID + 1 :: Nil))
          case (true, a1, a2) if a1 == ref => (Het, Phased(currentPhaseID :: Nil))
          case (true, a1, a2) if (a2 == ref) => (Het, Phased((currentPhaseID + 1) :: Nil))
          case (false, a1, a2) if a1 == a2 && a1 == ref => (HomRef, Unphased)
          case (false, a1, a2) if a1 == a2 => (HomVar, Unphased)
          case (false, a1, a2) if a1 != a2 => (Het, Unphased)
        }

        val altallele: String = if (firstAllele == ref) secondAllele else firstAllele

        Event(individual, Set(VariantWithPhase(phase, genotype, gloc, StringStore(ref), StringStore(altallele), attribute)))

      }
    }.toList.groupBy(_.individual).map(x => x._2.reduce(_ + _)).toList

    individualsWithPhasedData
      .filter(_.variants.exists(v => v.location == targetVariant && v.phase.isInstanceOf[Phased]))
      .map { indEvent =>
        val targetPhases = indEvent.variants.filter(v => v.location == targetVariant).head.phase.asInstanceOf[Phased].phaseIDs.toSet

        indEvent.copy(variants = indEvent.variants.filter(_.phase match {
          case Unphased => false
          case Phased(ids) if !(ids.toSet & targetPhases).isEmpty => true
          case _ => false
        }).filter(_.genotype.variantAlleleDosage > 0.0))
      }

  }

  // val map = mmap.map { x => x._1 -> x._2.groupBy(_.individual).map(x => x._2.reduce(_ + _)).toList }

}