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

import org.scalatest.FunSuite

import mybiotools._
import mybiotools.gwascommons._
import mybiotools.gwascommons.genotypedata._
import GenotypeStates._
import scala.collection.mutable.Map
import mybiotools.stringstore._

class DiscreteGenotypeDataTestSuite extends FunSuite {

  implicit def string2string8(s: String) = StringStore(s)

  test("trivial 0") {
    val ped = new java.io.File(getClass.getResource("kicsi.ped").getPath)
    val map = new java.io.File(getClass.getResource("kicsi.map").getPath)
    val m = DiscreteGenotypeData.fromPedMap(ped, map)
    val pat = Set(
      PatientInformation("f1", None, None),
      PatientInformation("f2", None, None)
    )
    val snps = Set(
      LocusData("rs1", GenomicLocation(1, "1"), Seq(Allele_A, Allele_T), List(Allele_A -> 0.75, Allele_T -> 0.25))
    )
    assertResult(m.getPatients)(pat)
    assertResult(m.getSNPs)(snps)
    assertResult(Homozygous1)(m.getState("f1", "rs1").get)
    assertResult(Heterozygous)(m.getState("f2", "rs1").get)
  }

  test("2") {
    val ped = new java.io.File(getClass.getResource("genotypedata/1.ped").getPath)
    val map = new java.io.File(getClass.getResource("genotypedata/1.map").getPath)
    val m = DiscreteGenotypeData.fromPedMap(ped, map)
    val pat = Set(
      PatientInformation("f1", None, None),
      PatientInformation("f2", None, None)
    )
    val snps = Set(
      LocusData("rs1", GenomicLocation(1, "1"), Seq(Allele_A, Allele_T), List(Allele_A -> 0.5, Allele_T -> 0.5)),
      LocusData("rs2", GenomicLocation(2, "1"), Seq(Allele_A, Allele_T), List(Allele_A -> 0.5, Allele_T -> 0.5)),
      LocusData("rs3", GenomicLocation(3, "1"), Seq(Allele_A, Allele_T), List(Allele_A -> 0.5, Allele_T -> 0.5)),
      LocusData("rs4", GenomicLocation(4, "1"), Seq(Allele_A, Allele_T), List(Allele_A -> 0.5, Allele_T -> 0.5)),
      LocusData("rs5", GenomicLocation(5, "1"), Seq(Allele_A, Allele_T), List(Allele_A -> 0.25, Allele_T -> 0.75))
    )
    assertResult(m.getPatients)(pat)
    assertResult(m.getSNPs)(snps)
    assertResult(Homozygous1)(m.getState("f1", "rs1").get)
    assertResult(Homozygous2)(m.getState("f2", "rs1").get)
    assertResult(Homozygous1)(m.getState("f1", "rs2").get)
    assertResult(Homozygous2)(m.getState("f2", "rs2").get)

    assertResult(Homozygous1)(m.getState("f1", "rs3").get)
    assertResult(Homozygous2)(m.getState("f2", "rs3").get)

    assertResult(Homozygous1)(m.getState("f1", "rs4").get)
    assertResult(Homozygous2)(m.getState("f2", "rs4").get)

    assertResult(Heterozygous)(m.getState("f1", "rs5").get)
    assertResult(Homozygous2)(m.getState("f2", "rs5").get)

    assertResult(DiscreteGenotypes.Heterozygous)(m.getStateWithRespectToMinor("f1", "rs5").get)
    assertResult(DiscreteGenotypes.HomozygousRef)(m.getStateWithRespectToMinor("f2", "rs5").get)

  }

  test("3") {
    val ped = new java.io.File(getClass.getResource("genotypedata/2.ped").getPath)
    val map = new java.io.File(getClass.getResource("genotypedata/2.map").getPath)
    val m = DiscreteGenotypeData.fromPedMap(ped, map)
    val pat = Set(
      PatientInformation("f1", Some(Male), None),
      PatientInformation("f2", Some(Female), None),
      PatientInformation("f3", None, None)
    )
    val snps = Set(
      LocusData("rs1", GenomicLocation(1, "1"), Seq(Allele_A, Allele_T), List(Allele_A -> 0.5, Allele_T -> 0.5)),
      LocusData("rs2", GenomicLocation(2, "1"), Seq(Allele_A, Allele_T), List(Allele_A -> 0.5, Allele_T -> 0.5)),
      LocusData("rs3", GenomicLocation(3, "1"), Seq(Allele_A, Allele_T), List(Allele_A -> 0.5, Allele_T -> 0.5)),
      LocusData("rs4", GenomicLocation(4, "1"), Seq(Allele_A, Allele_T), List(Allele_A -> 0.5, Allele_T -> 0.5)),
      LocusData("rs5", GenomicLocation(5, "1"), Seq(Allele_A, Allele_T), List(Allele_A -> 0.5, Allele_T -> 0.5))
    )

    assertResult(m.getPatients)(pat)
    assertResult(m.getSNPs)(snps)
    assertResult(Homozygous1)(m.getState("f1", "rs1").get)
    assertResult(Homozygous2)(m.getState("f2", "rs1").get)
    assertResult(Homozygous1)(m.getState("f1", "rs2").get)
    assertResult(Homozygous2)(m.getState("f2", "rs2").get)

    assertResult(Homozygous1)(m.getState("f1", "rs3").get)
    assertResult(Homozygous2)(m.getState("f2", "rs3").get)

    assertResult(Homozygous1)(m.getState("f1", "rs4").get)
    assertResult(Homozygous2)(m.getState("f2", "rs4").get)

    assertResult(Homozygous1)(m.getState("f1", "rs5").get)
    assertResult(Homozygous2)(m.getState("f2", "rs5").get)

    assertResult(Heterozygous)(m.getState("f3", "rs1").get)
    assertResult(Heterozygous)(m.getState("f3", "rs2").get)
    assertResult(Heterozygous)(m.getState("f3", "rs3").get)
    assertResult(Heterozygous)(m.getState("f3", "rs4").get)
    assertResult(Heterozygous)(m.getState("f3", "rs5").get)

  }

  test(" monomorphic") {
    val ped = new java.io.File(getClass.getResource("genotypedata/3.ped").getPath)
    val map = new java.io.File(getClass.getResource("genotypedata/3.map").getPath)
    val m = DiscreteGenotypeData.fromPedMap(ped, map)
    val pat = Set(
      PatientInformation("f1", Some(Male), Some(1)),
      PatientInformation("f2", Some(Female), None),
      PatientInformation("f3", None, None)
    )

    val snps = Set(
      LocusData("rs1", GenomicLocation(1, "1"), Seq(Allele_A, Allele_T), List(Allele_A -> 0.5, Allele_T -> 0.5)),
      LocusData("rs2", GenomicLocation(2, "1"), Seq(Allele_A, Allele_T), List(Allele_A -> 0.5, Allele_T -> 0.5)),
      LocusData("rs3", GenomicLocation(3, "1"), Seq(Allele_A, Allele_T), List(Allele_A -> 0.5, Allele_T -> 0.5)),
      LocusData("rs4", GenomicLocation(4, "1"), Seq(Allele_A, Allele_T), List(Allele_A -> 0.5, Allele_T -> 0.5)),
      LocusData("rs5", GenomicLocation(5, "1"), Seq(Allele_A, Allele_T), List(Allele_A -> 0.5, Allele_T -> 0.5))
    )

    assertResult(m.getPatients)(pat)
    assertResult(m.getSNPs.filter(_.name == "rs1"))(snps.filter(_.name == "rs1"))
    assertResult(m.getSNPs)(snps)
    assertResult(Heterozygous)(m.getState("f1", "rs1").get)
    assertResult(Homozygous1)(m.getState("f2", "rs1").get)
    assertResult(Homozygous1)(m.getState("f1", "rs2").get)
    assertResult(Homozygous2)(m.getState("f2", "rs2").get)

    assertResult(Homozygous1)(m.getState("f1", "rs3").get)
    assertResult(Homozygous2)(m.getState("f2", "rs3").get)

    assertResult(Homozygous1)(m.getState("f1", "rs4").get)
    assertResult(Homozygous2)(m.getState("f2", "rs4").get)

    assertResult(Homozygous1)(m.getState("f1", "rs5").get)
    assertResult(Homozygous2)(m.getState("f2", "rs5").get)

    assertResult(Homozygous2)(m.getState("f3", "rs1").get)
    assertResult(Heterozygous)(m.getState("f3", "rs2").get)
    assertResult(Heterozygous)(m.getState("f3", "rs3").get)
    assertResult(Heterozygous)(m.getState("f3", "rs4").get)
    assertResult(Heterozygous)(m.getState("f3", "rs5").get)

    assertResult(DiscreteGenotypes.Heterozygous)(m.getStateWithRespectToMinor("f1", "rs1").get)
    assertResult(DiscreteGenotypes.HomozygousVar)(m.getStateWithRespectToMinor("f2", "rs1").get)
    assertResult(DiscreteGenotypes.HomozygousRef)(m.getStateWithRespectToMinor("f3", "rs1").get)

  }

  test("A0") {
    val ped = new java.io.File(getClass.getResource("genotypedata/4.ped").getPath)
    val map = new java.io.File(getClass.getResource("genotypedata/4.map").getPath)
    intercept[RuntimeException](DiscreteGenotypeData.fromPedMap(ped, map))
  }

  test("ACT") {
    val ped = new java.io.File(getClass.getResource("genotypedata/5.ped").getPath)
    val map = new java.io.File(getClass.getResource("genotypedata/5.map").getPath)
    intercept[RuntimeException](DiscreteGenotypeData.fromPedMap(ped, map))
  }

  test("format phenotype") {
    val ped = new java.io.File(getClass.getResource("genotypedata/3.ped").getPath)
    val map = new java.io.File(getClass.getResource("genotypedata/3.map").getPath)
    val m = DiscreteGenotypeData.fromPedMap(ped, map)
    import mybiotools.formatters.Formatters._
    val formatted = (m.formatPhenotypes(formatters.PlinkFam))
    val exp = """|FID IID gender default
                 |f1 1 1 1.0
                 |f2 1 2 -9
                 |f3 1 -9 -9""".stripMargin
    assertResult(exp)(formatted)
  }

  test("format map") {
    val ped = new java.io.File(getClass.getResource("genotypedata/3.ped").getPath)
    val map = new java.io.File(getClass.getResource("genotypedata/3.map").getPath)
    val m = DiscreteGenotypeData.fromPedMap(ped, map)
    val formatted = (m.formatMap)
    val exp = "1 rs1 0 1\n1 rs2 0 2\n1 rs3 0 3\n1 rs4 0 4\n1 rs5 0 5\n"
    assertResult(exp)(formatted)
  }

  test("format ped") {
    val ped = new java.io.File(getClass.getResource("genotypedata/3.ped").getPath)
    val map = new java.io.File(getClass.getResource("genotypedata/3.map").getPath)
    val m = DiscreteGenotypeData.fromPedMap(ped, map)
    val formatted = (m.formatPed)
    val exp = """|f1 1 0 0 1 1.0 A T A A A A A A A A
         |f2 1 0 0 2 -9 A A T T T T T T T T
         |f3 1 0 0 -9 -9 T T A T A T A T A T
         |""".stripMargin
    assertResult(exp)(formatted)
  }

  test(" 6, read, without expect") {
    val ped = new java.io.File(getClass.getResource("genotypedata/6.ped").getPath)
    val map = new java.io.File(getClass.getResource("genotypedata/6.map").getPath)
    val m = DiscreteGenotypeData.fromPedMap(ped, map)

    // println(m)

  }

  test("read and write medium ped") {
    val ped = new java.io.File(getClass.getResource("genotypedata/medium.recoded.ped").getPath)
    val map = new java.io.File(getClass.getResource("genotypedata/medium.recoded.map").getPath)
    val outped = new java.io.File(getClass.getResource("/").getPath + "/medium.ped")
    val outmap = new java.io.File(getClass.getResource("/").getPath + "/medium.map")
    val outped2 = new java.io.File(getClass.getResource("/").getPath + "/medium2.ped")
    val outmap2 = new java.io.File(getClass.getResource("/").getPath + "/medium2.map")
    val m = DiscreteGenotypeData.fromPedMap(ped, map)

    writeToFile(outmap.getAbsolutePath, m.formatMap)
    openFileWriter(outped) { writer =>
      m.writePed(writer)
    }
    val m2 = DiscreteGenotypeData.fromPedMap(outped, outmap)
    // m.getSNPs.foreach { m1s =>
    //   val m2s = m2.getSNP(m1s.name).get
    //   assertResult(m2s)(m1s)
    // }

    val reformat = "plink --file " + getClass.getResource("/").getPath + "/medium --recode --noweb --out " + getClass.getResource("/").getPath + "/medium2 "
    reformat!

    assertResult(getCheckSum(outped2.getAbsolutePath))(getCheckSum(ped.getAbsolutePath))
    assertResult(getCheckSum(outmap2.getAbsolutePath))(getCheckSum(map.getAbsolutePath))

  }
}