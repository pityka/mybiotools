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

package dispensability
import mybiotools.gwascommons._
import mybiotools.gwascommons.DiscreteGenotypes._
import scala.math._
import mybiotools.stringstore._

case class GeneSetKey(name: String8)

object GeneSetKey {
  def apply(name: String): GeneSetKey = GeneSetKey(StringStore(name))
}

trait WithID {
  def name: String8
}

case class Gene(hgnc: String8, probFS: Double, probStop: Double, probSplice: Double, probSynonym: Double) extends WithID {
  override def toString = s"Gene($hgnc,0.0,0.0,0.0,0.0,0)"
  def name = hgnc
}
object Gene {
  def apply(name: String8): Gene = Gene(name, 0.0, 0.0, 0.0, 0.0)
}

sealed trait ScoreOrdering
case object MoreDamagingIsLower extends ScoreOrdering
case object MoreDamagingIsHigher extends ScoreOrdering

trait AnnotationScore[T <: Annotation[T]] extends (Variant[T] => Option[Double]) {
  def apply(t: Variant[T]): Option[Double]

  def orderingType: ScoreOrdering

  def nonDamagingValue: Double

  lazy val scoreOrdering: Ordering[Double] = orderingType match {
    case MoreDamagingIsLower => math.Ordering.Double
    case MoreDamagingIsHigher => math.Ordering.by((t: Double) => -1 * t)
  }

  /* Most damaging is first */
  lazy val ordering: Ordering[Variant[T]] = orderingType match {
    case MoreDamagingIsLower => math.Ordering.by((t: Variant[T]) => apply(t).getOrElse(Double.NaN))
    case MoreDamagingIsHigher => math.Ordering.by((t: Variant[T]) => -1 * apply(t).getOrElse(Double.NaN))
  }

}

trait Annotation[T <: Annotation[T]] { self: T =>
  // def score(implicit as: AnnotationScore[T]) = as(self)
}

trait AnnotationWithAAPosition[T <: AnnotationWithAAPosition[T]] extends Annotation[T] { self: T =>
  def aminoAcidPosition: Int
}

trait AnnotationWithGene[T <: AnnotationWithGene[T]] extends Annotation[T] { self: T =>
  def gene: Gene
}

sealed trait VariantWithoutAnnotation {
  def location: GenomicLocation
  def reference: String8
  def alternate: String8
  def homRefCount: Int
  def homVarCount: Int
  def hetCount: Int
  def variantAlleleCount = hetCount + 2 * homVarCount
  def alleleCount = variantAlleleCount
  def refAlleleCount = hetCount + 2 * homRefCount
  def unAffectedIndividualCount = homRefCount
  def affectIndividualCount = hetCount + homVarCount
  def variantkey = VariantKey(location, reference, alternate)
  def variantCount = affectIndividualCount
  def callCount = homRefCount + homVarCount + hetCount
  def alleleFrequency = alleleCount / (2 * callCount.toDouble)
  def isFrameshift = ((reference.size - alternate.size) % 3) != 0
}
case class VariantWithoutAnnotationImpl(location: GenomicLocation, reference: String8, alternate: String8, homRefCount: Int, hetCount: Int, homVarCount: Int) extends VariantWithoutAnnotation

sealed trait Variant[T <: Annotation[T]] extends VariantWithoutAnnotation { self: Variant[T] =>
  def score(implicit as: AnnotationScore[T]) = as(self)
  def annotation: T

}

case class VariantWithSumGenotype[T <: Annotation[T]](annotation: T, homRefCount: Int, hetCount: Int, homVarCount: Int, location: GenomicLocation, reference: String8, alternate: String8) extends Variant[T] {
  def maxHaplotypes = (hetCount + homVarCount + homRefCount) * 2
}
object VariantWithSumGenotype {
  def apply[T <: Annotation[T]](annotation: T, homrefCount: Int, hetCount: Int, homvarCount: Int, location: GenomicLocation, reference: String, alternate: String): VariantWithSumGenotype[T] = VariantWithSumGenotype(annotation, homrefCount, hetCount, homvarCount, location, StringStore(reference), StringStore(alternate))
}

