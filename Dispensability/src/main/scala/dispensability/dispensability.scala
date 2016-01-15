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

import mybiotools.stringstore._
import mybiotools.gwascommons._

case class SnpEffAnnotation(location: GenomicLocation, gene: Gene, effect: String, aminoAcidPosition: Int) extends AnnotationWithAAPosition[SnpEffAnnotation] with AnnotationWithGene[SnpEffAnnotation]

case class NutVarAnnotation(location: GenomicLocation, gene: Gene,
    MaCArthur2012: Option[Double],
    NUTvar_ProbabilityPathogenic: Option[Double],
    NUTvar_ProbabilityPathogenicxMaCArthur2012: Option[Double],
    RVIS: Option[Double],
    NUTvar_ProbabilityPathogenicxRVIS: Option[Double],
    NUTvar_ProbabilityPathogenic_RankPercentile: Option[Double],
    MaCArthur2012_RankPercentile: Option[Double],
    NUTvar_ProbabilityPathogenicxMaCArthur2012_RankPercentile: Option[Double],
    RVIS_RankPercentile: Option[Double],
    NUTvar_ProbabilityPathogenicxRVIS_RankPercentile: Option[Double],
    LongestCCDSLength: Double,
    PercentageLongestCCDSAffected: Double,
    GEUVADIS_1000G_AverageGeneZscoreAll: Option[Double]) extends AnnotationWithAAPosition[NutVarAnnotation] with AnnotationWithGene[NutVarAnnotation] {
  def aminoAcidPosition = ((LongestCCDSLength * (100.0 - PercentageLongestCCDSAffected)) / 3).toInt

  def toLine = s"${location.chromosome} ${location.basePairPosition} ${gene.name} $MaCArthur2012 $NUTvar_ProbabilityPathogenic $NUTvar_ProbabilityPathogenicxMaCArthur2012 $NUTvar_ProbabilityPathogenicxRVIS $RVIS $NUTvar_ProbabilityPathogenic_RankPercentile $NUTvar_ProbabilityPathogenicxMaCArthur2012_RankPercentile $RVIS_RankPercentile $NUTvar_ProbabilityPathogenicxRVIS_RankPercentile $LongestCCDSLength $PercentageLongestCCDSAffected $GEUVADIS_1000G_AverageGeneZscoreAll"

}
object NutVarAnnotation {
  // val avg = (t1: NutVarAnnotation, t2: NutVarAnnotation) => t1.copy(NUTvar_ProbabilityPathogenic = (t1.NUTvar_ProbabilityPathogenic + t2.NUTvar_ProbabilityPathogenic) / 2)
  def worst(implicit ord: Ordering[Variant[NutVarAnnotation]]) = (t1: Variant[NutVarAnnotation], t2: Variant[NutVarAnnotation]) => if (ord.gt(t1, t2)) t1 else t2
}

object NutVarAnnotationScores {
  val SnpEffAnnotationScore = new AnnotationScore[SnpEffAnnotation] {
    def apply(v: Variant[SnpEffAnnotation]) = v.annotation.effect.toUpperCase match {
      case "FRAME_SHIFT" | "SPLICE_SITE_ACCEPTOR" | "SPLICE_SITE_DONOR" | "STOP_GAINED" => Some(1.0)
      case _ => Some(0.0)
    }
    def orderingType = MoreDamagingIsHigher
    def nonDamagingValue = 0.0
  }

  val NUTvar_ProbabilityPathogenic_RankPercentile = new AnnotationScore[NutVarAnnotation] {
    def apply(v: Variant[NutVarAnnotation]) = v.annotation.NUTvar_ProbabilityPathogenic_RankPercentile
    def orderingType = MoreDamagingIsLower
    def nonDamagingValue = 100.0
  }
  val ReverseNUTvar_ProbabilityPathogenic_RankPercentile = new AnnotationScore[NutVarAnnotation] {
    def apply(v: Variant[NutVarAnnotation]) = v.annotation.NUTvar_ProbabilityPathogenic_RankPercentile.map(100.0 - _)
    def orderingType = MoreDamagingIsHigher
    def nonDamagingValue = 0.0
  }
  val NUTvar_ProbabilityPathogenic = new AnnotationScore[NutVarAnnotation] {
    def apply(v: Variant[NutVarAnnotation]) = v.annotation.NUTvar_ProbabilityPathogenic
    def orderingType = MoreDamagingIsHigher
    def nonDamagingValue = 0.0
  }
  val NUTvar_ProbabilityNonPathogenic = new AnnotationScore[NutVarAnnotation] {
    def apply(v: Variant[NutVarAnnotation]) = v.annotation.NUTvar_ProbabilityPathogenic.map(1.0 - _)
    def orderingType = MoreDamagingIsLower
    def nonDamagingValue = 1.0
  }

  val NUTvar_ProbabilityPathogenicxRVIS = new AnnotationScore[NutVarAnnotation] {
    def apply(v: Variant[NutVarAnnotation]) = v.annotation.NUTvar_ProbabilityPathogenicxRVIS
    def orderingType = MoreDamagingIsHigher
    def nonDamagingValue = 0.0
  }

  val GEUVADIS_1000G_AverageGeneZscoreAll = new AnnotationScore[NutVarAnnotation] {
    def apply(v: Variant[NutVarAnnotation]) = v.annotation.GEUVADIS_1000G_AverageGeneZscoreAll
    def orderingType = MoreDamagingIsLower
    def nonDamagingValue = 0.0
  }
  val NUTvar_ProbabilityPathogenicxMaCArthur2012 = new AnnotationScore[NutVarAnnotation] {
    def apply(v: Variant[NutVarAnnotation]) = v.annotation.NUTvar_ProbabilityPathogenicxMaCArthur2012
    def orderingType = MoreDamagingIsHigher
    def nonDamagingValue = 0.0
  }
  val NUTvar_ProbabilityPathogenicxApriori = new AnnotationScore[NutVarAnnotation] {
    def apply(v: Variant[NutVarAnnotation]) = {
      val d = math.log10(if (v.isFrameshift) v.annotation.NUTvar_ProbabilityPathogenic.get * v.annotation.gene.probFS else v.annotation.NUTvar_ProbabilityPathogenic.get * v.annotation.gene.probStop)
      if (d.isNaN || d.isInfinite) None else Some(d)
    }
    def orderingType = MoreDamagingIsHigher
    def nonDamagingValue = -8
  }
  val length = new AnnotationScore[NutVarAnnotation] {
    def apply(v: Variant[NutVarAnnotation]) = Some(v.annotation.LongestCCDSLength)
    def orderingType = MoreDamagingIsHigher
    def nonDamagingValue = 0.0
  }
  val expectedPerLength = new AnnotationScore[NutVarAnnotation] {
    def apply(v: Variant[NutVarAnnotation]) = Some(math.log10((v.annotation.gene.probStop + v.annotation.gene.probFS) / v.annotation.LongestCCDSLength.toDouble))
    def orderingType = MoreDamagingIsHigher
    def nonDamagingValue = 0.0
  }

}
