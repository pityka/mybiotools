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

class DosageGenotypeDataTestSuite extends FunSuite {
  implicit def string2string8(s: String) = StringStore(s)

  test("1.pdosage") {
    val pdosage = new java.io.File(getClass.getResource("genotypedata/2.pdosage").getPath)
    val map = new java.io.File(getClass.getResource("genotypedata/2.pdosage.map").getPath)
    val fam = new java.io.File(getClass.getResource("genotypedata/1.fam").getPath)
    val m = DosageGenotypeData.fromPlinkDosage(pdosage, map, fam)
    val pat = Set(
      PatientInformation("f1", None, None),
      PatientInformation("f2", None, None)
    )
    val snps = Set(
      LocusData(
        genomicLocation = GenomicLocation(1, "1"),
        alleles = List(Allele_A, Allele_T),
        name = "rs1",
        alleleFrequencies = List(Allele_A -> 0.24999999441206455, Allele_T -> (1.0 - 0.24999999441206455))
      ),
      LocusData(
        genomicLocation = GenomicLocation(2, "1"),
        alleles = List(Allele_A, Allele_T),
        name = "rs2",
        alleleFrequencies = List(Allele_A -> 0.15000000037252903, Allele_T -> (1.0 - 0.15000000037252903))
      ),
      LocusData(
        genomicLocation = GenomicLocation(3, "1"),
        alleles = List(Allele_A, Allele_T),
        name = "rs3",
        alleleFrequencies = List(Allele_A -> 0.44999998807907104, Allele_T -> (1.0 - 0.44999998807907104))
      ),
      LocusData(
        genomicLocation = GenomicLocation(4, "1"),
        alleles = List(Allele_A, Allele_T),
        name = "rs4",
        alleleFrequencies = List(Allele_A -> 0.44999998807907104, Allele_T -> (1.0 - 0.44999998807907104))
      ),
      LocusData(
        genomicLocation = GenomicLocation(5, "1"),
        alleles = List(Allele_A, Allele_T),
        name = "rs5",
        alleleFrequencies = List(Allele_A -> 0.4749999940395355, Allele_T -> (0.5250000059604645))
      ),
      LocusData(
        genomicLocation = GenomicLocation(6, "1"),
        alleles = List(Allele_A, Allele_T),
        name = "rs6",
        alleleFrequencies = List(Allele_A -> 0.75, Allele_T -> (0.25))
      )
    )
    assertResult(pat)(m.getPatients)
    assertResult(snps)(m.getSNPs)
    assertResult(0.1F)(m.getState("f1", "rs1").get)
    assertResult(0.9F)(m.getState("f2", "rs1").get)
    assertResult(0.1F)(m.getState("f1", "rs2").get)
    assertResult(0.5F)(m.getState("f2", "rs2").get)

    assertResult(0.0F)(m.getState("f1", "rs3").get)
    assertResult(1.8F)(m.getState("f2", "rs3").get)

    assertResult(1.8F)(m.getState("f1", "rs4").get)
    assertResult(0)(m.getState("f2", "rs4").get)

    assertResult(1.4F)(m.getState("f1", "rs5").get)
    assertResult(0.5f)(m.getState("f2", "rs5").get)

    val sw = new java.io.StringWriter
    val swfam = new java.io.StringWriter
    val swmap = new java.io.StringWriter
    m.writeDosage(swfam, swmap, sw, -9)
    val exp1 = """SNP A1 A2 f1 1 f2 1
rs1 A T 0.1 0.9
rs2 A T 0.1 0.5
rs3 A T 0.0 1.8
rs4 A T 1.8 0.0
rs5 A T 1.4 0.5
rs6 A T 1.5 1.5
"""
    val expfam = """f1 1 0 0 -9 -9
f2 1 0 0 -9 -9
"""

    assertResult("""1 rs1 0 1 A T
1 rs2 0 2 A T
1 rs3 0 3 A T
1 rs4 0 4 A T
1 rs5 0 5 A T
1 rs6 0 6 A T
""")(swmap.toString)
    assertResult(exp1)(sw.toString)
    assertResult(expfam)(swfam.toString)

  }

  test("1.pdosage extracttotable") {
    val pdosage = new java.io.File(getClass.getResource("genotypedata/2.pdosage").getPath)
    val map = new java.io.File(getClass.getResource("genotypedata/2.pdosage.map").getPath)
    val fam = new java.io.File(getClass.getResource("genotypedata/1.fam").getPath)
    val m = DosageGenotypeData.fromPlinkDosage(pdosage, map, fam)

    val table = extractSNPsFromDosage[String](pdosage, map, fam, List("rs1", "rs2", "rs3", "rs4", "rs5", "rs6"))

    val expectedtable = Map(
      "f1" -> Map(
        'rs1 -> Some(0.10000000149011612),
        'rs2 -> Some(0.10000000149011612),
        'rs3 -> Some(0.0),
        'rs4 -> Some(1.7999999523162842),
        'rs5 -> Some(1.399999976158142),
        'rs6 -> Some(0.5)
      ),
      "f2" -> Map(
        'rs1 -> Some(0.8999999761581421),
        'rs2 -> Some(0.5),
        'rs3 -> Some(1.7999999523162842),
        'rs4 -> Some(0.0),
        'rs5 -> Some(0.5),
        'rs6 -> Some(0.5)
      )
    )
    assertResult(table("f1")('rs1))(expectedtable("f1")('rs1))
    assertResult(table("f1")('rs2))(expectedtable("f1")('rs2))
    assertResult(table("f1")('rs3))(expectedtable("f1")('rs3))
    assertResult(table("f1")('rs4))(expectedtable("f1")('rs4))
    assertResult(table("f1")('rs5))(expectedtable("f1")('rs5))
    assertResult(table("f1")('rs6))(expectedtable("f1")('rs6))

    assertResult(table("f2")('rs1))(expectedtable("f2")('rs1))
    assertResult(table("f2")('rs2))(expectedtable("f2")('rs2))
    assertResult(table("f2")('rs3))(expectedtable("f2")('rs3))
    assertResult(table("f2")('rs4))(expectedtable("f2")('rs4))
    assertResult(table("f2")('rs5))(expectedtable("f2")('rs5))
    assertResult(table("f2")('rs6))(expectedtable("f2")('rs6))

    assertResult(expectedtable)(table)

  }

  test("extract 1.ped") {
    val ped = new java.io.File(getClass.getResource("genotypedata/1.bed").getPath)
    val map = new java.io.File(getClass.getResource("genotypedata/1.bim").getPath)
    val fam = new java.io.File(getClass.getResource("genotypedata/1.fam").getPath)

    val table = extractSNPsFromBed[String](ped, map, fam, List("rs1", "rs2", "rs3", "rs4", "rs5"))

    val expectedtable = Map(
      "f1" -> Map(
        'rs1 -> Some(2.0),
        'rs2 -> Some(2.0),
        'rs3 -> Some(2.0),
        'rs4 -> Some(2.0),
        'rs5 -> Some(1.0)
      ),
      "f2" -> Map(
        'rs1 -> Some(0.0),
        'rs2 -> Some(0.0),
        'rs3 -> Some(0.0),
        'rs4 -> Some(0.0),
        'rs5 -> Some(0.0)
      )
    )

    assertResult(table("f1")('rs1))(expectedtable("f1")('rs1))
    assertResult(table("f1")('rs2))(expectedtable("f1")('rs2))
    assertResult(table("f1")('rs3))(expectedtable("f1")('rs3))
    assertResult(table("f1")('rs4))(expectedtable("f1")('rs4))
    assertResult(table("f1")('rs5))(expectedtable("f1")('rs5))

    assertResult(table("f2")('rs1))(expectedtable("f2")('rs1))
    assertResult(table("f2")('rs2))(expectedtable("f2")('rs2))
    assertResult(table("f2")('rs3))(expectedtable("f2")('rs3))
    assertResult(table("f2")('rs4))(expectedtable("f2")('rs4))
    assertResult(table("f2")('rs5))(expectedtable("f2")('rs5))

  }

}