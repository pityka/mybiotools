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

sealed trait HLAGene
case object HLA_A extends HLAGene {
  override def toString = "HLA_A"
}
case object HLA_B extends HLAGene {
  override def toString = "HLA_B"
}
case object HLA_C extends HLAGene {
  override def toString = "HLA_C"
}

case class HLAReportCalled(
    A1: Option[String],
    A2: Option[String],
    B1: Option[String],
    B2: Option[String],
    C1: Option[String],
    C2: Option[String]
) {
  def extractGene(gene: HLAGene): (Option[String], Option[String]) = gene match {
    case HLA_A => (A1, A2)
    case HLA_B => (B1, B2)
    case HLA_C => (C1, C2)
  }

  def isHomozygoteOn(haplotype: String, gene: HLAGene) = gene match {
    case HLA_A => (A1.isDefined && A2.isDefined && A1.get == haplotype && A2.get == haplotype)
    case HLA_B => (B1.isDefined && B2.isDefined && B1.get == haplotype && B2.get == haplotype)
    case HLA_C => (C1.isDefined && C2.isDefined && C1.get == haplotype && C2.get == haplotype)
  }

  def isHeterozygoteOn(haplotype: String, gene: HLAGene) = gene match {
    case HLA_A => (A1.isDefined && A2.isDefined && ((A1.get == haplotype && A2.get != haplotype) || (A1.get != haplotype && A2.get == haplotype)))
    case HLA_B => (B1.isDefined && B2.isDefined && ((B1.get == haplotype && B2.get != haplotype) || (B1.get != haplotype && B2.get == haplotype)))
    case HLA_C => (C1.isDefined && C2.isDefined && ((C1.get == haplotype && C2.get != haplotype) || (C1.get != haplotype && C2.get == haplotype)))
  }

  def hasNoAllele(haplotype: String, gene: HLAGene) = gene match {
    case HLA_A => (A1.isDefined && A2.isDefined && A1.get != haplotype && A2.get != haplotype)
    case HLA_B => (B1.isDefined && B2.isDefined && B1.get != haplotype && B2.get != haplotype)
    case HLA_C => (C1.isDefined && C2.isDefined && C1.get != haplotype && C2.get != haplotype)
  }

}

case class HLAReport(
    A1: (String, Double),
    A2: (String, Double),
    B1: (String, Double),
    B2: (String, Double),
    C1: (String, Double),
    C2: (String, Double),

    DRB1_1: (String, Double),
    DRB1_2: (String, Double),
    DQA1_1: (String, Double),
    DQA1_2: (String, Double),
    DQB1_1: (String, Double),
    DQB1_2: (String, Double),
    DPA1_1: (String, Double),
    DPA1_2: (String, Double),
    DPB1_1: (String, Double),
    DPB1_2: (String, Double)
) {
  def called = HLAReportCalled(
    A1 = if (A1._2 > 0.98) Some(A1._1) else None,
    A2 = if (A2._2 > 0.98) Some(A2._1) else None,
    B1 = if (B1._2 > 0.98) Some(B1._1) else None,
    B2 = if (B2._2 > 0.98) Some(B2._1) else None,
    C1 = if (C1._2 > 0.98) Some(C1._1) else None,
    C2 = if (C2._2 > 0.98) Some(C2._1) else None
  )
}

