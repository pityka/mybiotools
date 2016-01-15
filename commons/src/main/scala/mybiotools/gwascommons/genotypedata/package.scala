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
import java.io.File
import mybiotools.stringstore._
import org.saddle._

package object genotypedata {

  def readBeagleGprobsToDosage(gprobs: scala.io.Source, fam: scala.io.Source): Frame[Individual, String, Double] = {
    val individuals = getIndividualsFromFamFile(fam)

    import mybiotools.tabular._
    import TabSerialization._
    val protocol = new RProtocolWithCustomSeparator(' ')
    import protocol._

    val data: Map[String, Map[Individual, Double]] =
      fromTab[Seq[(String, String, String, Seq[Double])]](SimpleWhiteSpaceTableParser.parse(gprobs, false)).map {
        case (allele, _, _, states) =>
          val grouped: Seq[(Double, Double, Double)] = states.toSeq.grouped(3).map(x => (x(0), x(1), x(2))).toSeq
          val withInd: Map[Individual, (Double, Double, Double)] = (individuals zip grouped).toMap

          allele -> withInd.map(x => x._1 -> (x._2._2 + x._2._3 * 2))
      }.toMap

    Frame(data.toSeq.map(x => x._1 -> Series(x._2.toSeq: _*)): _*)
  }

  def callHLAFromSNPMajorLocusIterator(iter: SNPMajorLocusIterator): Map[Individual, HLAReportCalled] = {

    val hlaLoci = iter.loci.filter(_._1.name.value.startsWith("HLA")).toVector

    iter.individuals.zipWithIndex.map {
      case (ind, indidx) =>

        val individualDosagePerLocus: Seq[(String, Double)] = hlaLoci.map(x => x._1.name.value -> x._2(indidx).map(_.variantAlleleDosage)).filter(s => s._1.size == 10 && s._2.isDefined && s._2.get >= 0.66).map(x => x._1 -> x._2.get)

        def callAllele(prefix: String): (Option[String], Option[String]) = {
          val best2 = individualDosagePerLocus.filter(_._1.take(5) == prefix).sortBy {
            case (allelename, probs) =>
              probs * -1.0
          }.take(2)
          if (best2.size == 0) (None, None)
          else if (best2.head._2 >= 1.33) (Some(best2.head._1), Some(best2.head._1))
          else if (best2.size == 1) (Some(best2.head._1), None)
          else (Some(best2.head._1), Some(best2(1)._1))
        }

        val a = callAllele("HLA_A")
        val b = callAllele("HLA_B")
        val c = callAllele("HLA_C")

        ind -> HLAReportCalled(
          A1 = a._1,
          A2 = a._2,
          B1 = b._1,
          B2 = b._2,
          C1 = c._1,
          C2 = c._2
        )

    }.toMap

  }

  def readBeagleGprobs(gprobs: scala.io.Source, fam: scala.io.Source): Map[Individual, HLAReport] = {
    val individuals = getIndividualsFromFamFile(fam)

    import mybiotools.tabular._
    import TabSerialization._
    val protocol = new RProtocolWithCustomSeparator(' ')
    import protocol._

    val data: Map[String, Map[Individual, (Double, Double, Double)]] =
      fromTab[Seq[(String, String, String, Seq[Double])]](SimpleWhiteSpaceTableParser.parse(gprobs, false)).map {
        case (allele, _, _, states) =>
          val grouped: Seq[(Double, Double, Double)] = states.toSeq.grouped(3).map(x => (x(0), x(1), x(2))).toSeq
          val withInd: Map[Individual, (Double, Double, Double)] = (individuals zip grouped).toMap

          allele -> withInd
      }.toMap

    individuals.map { ind =>

      val indData: Map[String, (Double, Double, Double)] = data.map(x => x._1 -> x._2.apply(ind))

      def getBestAllelesFor(prefix: String): Seq[(String, (Double, Double, Double))] = indData.filterKeys(s => (s.size >= prefix.size + 5) && s.startsWith(prefix)).toSeq.sortBy {
        case (allelename, probs) =>
          List(probs._1, probs._2).max * -1.0
      }.take(2)

      val allelesA1 = getBestAllelesFor("HLA_A")(0)
      val allelesA2 = getBestAllelesFor("HLA_A")(1)

      val allelesB1 = getBestAllelesFor("HLA_B")(0)
      val allelesB2 = getBestAllelesFor("HLA_B")(1)

      val allelesC1 = getBestAllelesFor("HLA_C")(0)
      val allelesC2 = getBestAllelesFor("HLA_C")(1)

      val allelesDRB1_1 = getBestAllelesFor("HLA_DRB1")(0)
      val allelesDRB1_2 = getBestAllelesFor("HLA_DRB1")(1)

      val allelesDQA1_1 = getBestAllelesFor("HLA_DQA1")(0)
      val allelesDQA1_2 = getBestAllelesFor("HLA_DQA1")(1)

      val allelesDQB1_1 = getBestAllelesFor("HLA_DQB1")(0)
      val allelesDQB1_2 = getBestAllelesFor("HLA_DQB1")(1)

      val allelesDPA1_1 = getBestAllelesFor("HLA_DPA1")(0)
      val allelesDPA1_2 = getBestAllelesFor("HLA_DPA1")(1)

      val allelesDPB1_1 = getBestAllelesFor("HLA_DPB1")(0)
      val allelesDPB1_2 = getBestAllelesFor("HLA_DPB1")(1)

      def getReportedAlles(
        a1: (String, (Double, Double, Double)),
        a2: (String, (Double, Double, Double))
      ): ((String, Double), (String, Double)) =
        (a1, a2) match {
          case ((name1, (pHomPres1, pHet1, pHomAbs1)), (name2, (pHomPres2, pHet2, pHomAbs2))) => {
            if (pHomPres1 > pHet1) ((name1, pHomPres1), (name1, pHomPres1))
            else if (pHomPres2 > pHet1) throw new RuntimeException(a1.toString + a2)
            else ((name1, pHet1), (name2, pHet2))
          }
        }

      val repA = getReportedAlles(allelesA1, allelesA2)
      val repB = getReportedAlles(allelesB1, allelesB2)
      val repC = getReportedAlles(allelesC1, allelesC2)

      val repDRB1 = getReportedAlles(allelesDRB1_1, allelesDRB1_2)
      val repDQA1 = getReportedAlles(allelesDQA1_1, allelesDQA1_2)
      val repDQB1 = getReportedAlles(allelesDQB1_1, allelesDQB1_2)
      val repDPA1 = getReportedAlles(allelesDPA1_1, allelesDPA1_2)
      val repDPB1 = getReportedAlles(allelesDPB1_1, allelesDPB1_2)

      ind -> HLAReport(
        A1 = repA._1,
        A2 = repA._2,
        B1 = repB._1,
        B2 = repB._2,
        C1 = repC._1,
        C2 = repC._2,
        DRB1_1 = repDRB1._1,
        DRB1_2 = repDRB1._2,
        DQA1_1 = repDQA1._1,
        DQA1_2 = repDQA1._2,
        DQB1_1 = repDQB1._1,
        DQB1_2 = repDQB1._2,
        DPA1_1 = repDPA1._1,
        DPA1_2 = repDPA1._2,
        DPB1_1 = repDPB1._1,
        DPB1_2 = repDPB1._2
      )

    }.toMap

  }

  def extractSNPsFromBed[B](
    bed: File,
    bim: File,
    fam: File,
    snpList: Traversable[String]
  )(implicit factory: String => B): Map[B, Map[Symbol, Option[Double]]] = {
    import mybiotools.gwascommons.genotypedata.DiscreteGenotypeData
    import mybiotools.gwascommons.genotypedata.GenotypeStates._

    val listFile = File.createTempFile("bio", ".snpList")
    writeToFile(listFile.getAbsolutePath, snpList.mkString("\n"))

    val tmpFile = File.createTempFile("bio", "")
    execIsFinishedWithoutErrorStream("plink --bed " + bed.getAbsolutePath + " --bim " + bim.getAbsolutePath + " --fam  " + fam.getAbsolutePath + " --recode --allow-no-sex --noweb --out " + tmpFile.getAbsolutePath + " --extract " + listFile.getAbsolutePath)

    val extractedGD = DiscreteGenotypeData.fromPedMap(new File(tmpFile.getAbsolutePath + ".ped"), new File(tmpFile.getAbsolutePath + ".map"))
    val dosageMap = Map[B, Map[Symbol, Option[Double]]](extractedGD.getPatients.map { patient =>
      factory(patient.FID) -> Map[Symbol, Option[Double]](snpList.map { snpName16 =>
        val snpName = StringStore(snpName16)
        val snpInfo = extractedGD.getSNP(snpName).get
        val snpState: Option[GenotypeState] = extractedGD.getState(patient.id, snpName)

        val dosageOfMinorAllele = extractedGD.getStateWithRespectToMinor(patient.id, snpName).map(_.variantAlleleDosage)
        Symbol(snpName) -> dosageOfMinorAllele
      }.toSeq: _*)
    }.toSeq: _*)
    dosageMap
  }

  def extractSNPsFromDosage[B](
    pdosage: File,
    map: File,
    fam: File,
    snpList: Traversable[String]
  )(implicit factory: String => B): Map[B, Map[Symbol, Option[Double]]] = {
    import mybiotools.gwascommons.genotypedata.DosageGenotypeData
    import mybiotools.gwascommons.genotypedata.GenotypeStates._

    val listFile = File.createTempFile("bio", ".snpList")
    writeToFile(listFile.getAbsolutePath, snpList.mkString("\n"))

    val tmpFile = File.createTempFile("bio", "")
    execIsFinishedWithoutErrorStream("plink --dosage " + pdosage.getAbsolutePath + " format=1 --map " + map.getAbsolutePath + " --silent --fam  " + fam.getAbsolutePath + " --write-dosage --allow-no-sex --missing-phenotype -13 --noweb --out " + tmpFile.getAbsolutePath + " --extract " + listFile.getAbsolutePath)

    val extractedGD = DosageGenotypeData.fromPlinkDosage(new File(tmpFile.getAbsolutePath + ".out.dosage"), map, fam)

    val dosageMap = Map[B, Map[Symbol, Option[Double]]](extractedGD.getPatients.map { patient =>
      factory(patient.FID) -> Map[Symbol, Option[Double]](snpList.map { snpName16 =>
        val snpName = StringStore(snpName16)
        val snpInfo = extractedGD.getSNP(snpName).get
        val dosageOfAllele1 = extractedGD.getState(patient.FID, snpName)
        val dosageOfMinorAllele = extractedGD.getStateWithRespectToMinor(patient.FID, snpName).map(_.variantAlleleDosage)

        Symbol(snpName) -> dosageOfMinorAllele
      }.toSeq: _*)
    }.toSeq: _*)

    dosageMap
  }

  def recodeTransposeBed(bed: File, bim: File, fam: File): (File, File) = {
    val tmp = TempFile.createTempFile("plink")
    val b = execIsFinishedWithoutErrorStream("plink --bed " + bed.getAbsolutePath + " --bim " + bim.getAbsolutePath + " --fam " + fam.getAbsolutePath + " --noweb --recode --transpose --out " + tmp.getAbsolutePath)
    if (!b) throw new RuntimeException("error in plink")
    (new File(tmp.getAbsolutePath + ".tped"), new File(tmp.getAbsolutePath + ".tfam"))
  }
}