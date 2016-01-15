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

package mybiotools.gwascommons.gwas

import mybiotools.gwascommons._
import mybiotools.gwascommons.associationresults._
import mybiotools.gwascommons.genotypedata._
import mybiotools._
import java.io.File
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.saddle._

class GWASSpecLinear extends FunSpec with Matchers {

  def close(d1: Double, d2: Double): Boolean = math.abs(d1 - d2) < 0.01

  describe("vs plink") {

    val tpedfile = new File(getClass.getResource("/").getPath + "/genotypedata/plink3.tped")
    val tfamfile = new File(getClass.getResource("/").getPath + "/genotypedata/plink3.tfam")

    it("additive") {

      val plinkout = mybiotools.TempFile.createTempFile(".plink")

      val plinkcommand = "plink --tped " + tpedfile.getCanonicalPath + " --tfam " + tfamfile.getCanonicalPath + " --sex --missing-genotype -  --linear --out " + plinkout.getCanonicalPath

      val (stdout, stderr, success) = execGetStreamsAndCode(plinkcommand)

      val plinkresult = scala.io.Source.fromFile(plinkout + ".assoc.linear").getLines.toList
      // println(plinkresult.mkString("\n"))

      val covariates: Frame[Individual, String, Double] = Frame(scala.io.Source.fromFile(tfamfile).getLines.map { line =>
        val spl = fastSplitSeparator(line, ' ')
        val ind = Individual(spl(0), spl(1))
        val sex = spl(4).toDouble * -1.0
        val ph = spl(5).toDouble
        (ind, Series("SEX" -> (if (sex == 0.0) Double.NaN else sex), "PHENO" -> (if (ph == -9.0) Double.NaN else ph)))

      }.toSeq: _*).T

      val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfamfile), tped = scala.io.Source.fromFile(tpedfile), '-')

      val gmap = getGenomicMapFromBimFile(tpedfile.getCanonicalPath)

      val data = SNPMajorReaders.getSNPMajorIterator(tpedfiles).toLocusIteratorWithGenomicMap(gmap)

      val myresult = GWAS.singleMarkerTestOverBatch(
        iter = data,
        covariates = covariates,
        phenotypes = ("PHENO", Linear, List("SEX")) :: Nil,
        model = Additive,
        Seq(),
        Seq(),
        1,
        None,
        false, false
      ) { _ ++ _ }.get.head.results.head.regressionResult.get

      // val plinkread = readAssociationResultsFromPlinkFile(scala.io.Source.fromFile(plinkout + ".assoc.linear"), None)
      // println(plinkread.toList)
      val plinkresultAssocs = scala.io.Source.fromFile(plinkout + ".assoc.linear").getLines.drop(1).grouped(2).foreach { lines =>

        val addline = lines.head
        val genderline = lines(1)

        val splittedadd = mybiotools.fastSplitSetSeparator(addline, Set(' ', '\t'))
        val splittedgender = mybiotools.fastSplitSetSeparator(genderline, Set(' ', '\t'))

        val snpname = mybiotools.stringstore.StringStore(splittedadd(1))
        val nmiss = splittedadd(5).toInt
        val orADD = splittedadd(6).toDouble
        val seADD = splittedadd(7).toDouble

        val orSEX = splittedgender(6).toDouble
        val seSEX = splittedgender(7).toDouble

        val myORADD = myresult.covariates("SNP")._1.slope
        val mySDADD = myresult.covariates("SNP")._1.sd

        val myORSEX = myresult.covariates("SEX")._1.slope
        val mySDSEX = myresult.covariates("SEX")._1.sd

        myresult.numberOfSamples should equal(nmiss)
        close(myORADD, orADD) should be(true)
        close(myORSEX, orSEX) should be(true)

      }

    }

    it("dominant") {

      val plinkout = mybiotools.TempFile.createTempFile(".plink")

      val plinkcommand = "plink --tped " + tpedfile.getCanonicalPath + " --tfam " + tfamfile.getCanonicalPath + " --sex --missing-genotype -  --dominant --linear --out " + plinkout.getCanonicalPath

      val (stdout, stderr, success) = execGetStreamsAndCode(plinkcommand)

      val plinkresult = scala.io.Source.fromFile(plinkout + ".assoc.linear").getLines.toList

      val covariates: Frame[Individual, String, Double] = Frame(scala.io.Source.fromFile(tfamfile).getLines.map { line =>
        val spl = fastSplitSeparator(line, ' ')
        val ind = Individual(spl(0), spl(1))
        val sex = spl(4).toDouble * -1.0
        val ph = spl(5).toDouble
        (ind, Series("SEX" -> (if (sex == 0.0) Double.NaN else sex), "PHENO" -> (if (ph == -9.0) Double.NaN else ph)))

      }.toSeq: _*).T

      val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfamfile), tped = scala.io.Source.fromFile(tpedfile), '-')

      val gmap = getGenomicMapFromBimFile(tpedfile.getCanonicalPath)

      val data = SNPMajorReaders.getSNPMajorIterator(tpedfiles).toLocusIteratorWithGenomicMap(gmap)

      val myresult = GWAS.singleMarkerTestOverBatch(
        iter = data,
        covariates = covariates,
        phenotypes = ("PHENO", Linear, List("SEX")) :: Nil,
        model = Dominant,
        Seq(),
        Seq(),
        1,
        None, false, false
      ) { _ ++ _ }.get.head.results.head.regressionResult.get
      // val plinkread = readAssociationResultsFromPlinkFile(scala.io.Source.fromFile(plinkout + ".assoc.linear"), None)
      // println(plinkread.toList)
      val plinkresultAssocs = scala.io.Source.fromFile(plinkout + ".assoc.linear").getLines.drop(1).grouped(2).foreach { lines =>

        val addline = lines.head
        val genderline = lines(1)

        val splittedadd = mybiotools.fastSplitSetSeparator(addline, Set(' ', '\t'))
        val splittedgender = mybiotools.fastSplitSetSeparator(genderline, Set(' ', '\t'))

        val snpname = mybiotools.stringstore.StringStore(splittedadd(1))
        val nmiss = splittedadd(5).toInt
        val orADD = splittedadd(6).toDouble
        val seADD = splittedadd(7).toDouble

        val orSEX = splittedgender(6).toDouble
        val seSEX = splittedgender(7).toDouble

        val myORADD = myresult.covariates("SNP")._1.slope
        val mySDADD = myresult.covariates("SNP")._1.sd

        val myORSEX = myresult.covariates("SEX")._1.slope
        val mySDSEX = myresult.covariates("SEX")._1.sd

        myresult.numberOfSamples should equal(nmiss)
        close(myORADD, orADD) should be(true)
        close(myORSEX, orSEX) should be(true)

      }

    }

    it("recessive") {

      val plinkout = mybiotools.TempFile.createTempFile(".plink")

      val plinkcommand = "plink --tped " + tpedfile.getCanonicalPath + " --tfam " + tfamfile.getCanonicalPath + " --sex --missing-genotype -  --recessive --linear --out " + plinkout.getCanonicalPath

      val (stdout, stderr, success) = execGetStreamsAndCode(plinkcommand)

      val plinkresult = scala.io.Source.fromFile(plinkout + ".assoc.linear").getLines.toList

      val covariates: Frame[Individual, String, Double] = Frame(scala.io.Source.fromFile(tfamfile).getLines.map { line =>
        val spl = fastSplitSeparator(line, ' ')
        val ind = Individual(spl(0), spl(1))
        val sex = spl(4).toDouble * -1.0
        val ph = spl(5).toDouble
        (ind, Series("SEX" -> (if (sex == 0.0) Double.NaN else sex), "PHENO" -> (if (ph == -9.0) Double.NaN else ph)))

      }.toSeq: _*).T

      val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfamfile), tped = scala.io.Source.fromFile(tpedfile), '-')

      val gmap = getGenomicMapFromBimFile(tpedfile.getCanonicalPath)

      val data = SNPMajorReaders.getSNPMajorIterator(tpedfiles).toLocusIteratorWithGenomicMap(gmap)

      val myresult = GWAS.singleMarkerTestOverBatch(
        iter = data,
        covariates = covariates,
        phenotypes = ("PHENO", Linear, List("SEX")) :: Nil,
        model = Recessive,
        Seq(),
        Seq(),
        1,
        None, false, false
      ) { _ ++ _ }.get.head.results.head.regressionResult.get

      // val plinkread = readAssociationResultsFromPlinkFile(scala.io.Source.fromFile(plinkout + ".assoc.linear"), None)
      // println(plinkread.toList)
      val plinkresultAssocs = scala.io.Source.fromFile(plinkout + ".assoc.linear").getLines.drop(1).grouped(2).foreach { lines =>

        val addline = lines.head
        val genderline = lines(1)

        val splittedadd = mybiotools.fastSplitSetSeparator(addline, Set(' ', '\t'))
        val splittedgender = mybiotools.fastSplitSetSeparator(genderline, Set(' ', '\t'))

        val snpname = mybiotools.stringstore.StringStore(splittedadd(1))
        val nmiss = splittedadd(5).toInt
        val orADD = splittedadd(6).toDouble
        val seADD = splittedadd(7).toDouble

        val orSEX = splittedgender(6).toDouble
        val seSEX = splittedgender(7).toDouble

        val myORADD = myresult.covariates("SNP")._1.slope
        val mySDADD = myresult.covariates("SNP")._1.sd

        val myORSEX = myresult.covariates("SEX")._1.slope
        val mySDSEX = myresult.covariates("SEX")._1.sd

        myresult.numberOfSamples should equal(nmiss)
        close(myORADD, orADD) should be(true)
        close(myORSEX, orSEX) should be(true)

      }

    }

    it("additive interaction, test interaction term only") {

      val plinkout = mybiotools.TempFile.createTempFile(".plink")

      val plinkcommand = "plink --tped " + tpedfile.getCanonicalPath + " --tfam " + tfamfile.getCanonicalPath + " --missing-genotype - --sex --linear --interaction --out " + plinkout.getCanonicalPath

      val (stdout, stderr, success) = execGetStreamsAndCode(plinkcommand)

      val plinkresult = scala.io.Source.fromFile(plinkout + ".assoc.linear").getLines.toList
      // println(plinkresult.mkString("\n"))

      val covariates: Frame[Individual, String, Double] = Frame(scala.io.Source.fromFile(tfamfile).getLines.map { line =>
        val spl = fastSplitSeparator(line, ' ')
        val ind = Individual(spl(0), spl(1))
        val sex = spl(4).toDouble * -1.0
        val ph = spl(5).toDouble

        (ind, Series("SEX" -> (if (sex == 0.0) Double.NaN else sex), "PHENO" -> (if (ph == -9.0) Double.NaN else ph)))
      }.toSeq: _*).T

      val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfamfile), tped = scala.io.Source.fromFile(tpedfile), '-')

      val gmap = getGenomicMapFromBimFile(tpedfile.getCanonicalPath)

      val data = SNPMajorReaders.getSNPMajorIterator(tpedfiles).toLocusIteratorWithGenomicMap(gmap)

      val myresult = GWAS.singleMarkerTestOverBatch(
        iter = data,
        covariates = covariates,
        phenotypes = ("PHENO", Linear, List("SEX")) :: Nil,
        model = Additive,
        Seq("SEX"),
        Seq(ProductInteraction),
        threads = 1,
        None, false, false
      ) { _ ++ _ }.get.head.results.head.regressionResult.get

      // val plinkread = readAssociationResultsFromPlinkFile(scala.io.Source.fromFile(plinkout + ".assoc.logistic"), None)
      // println(plinkread.toList)
      val plinkresultAssocs = scala.io.Source.fromFile(plinkout + ".assoc.linear").getLines.drop(1).grouped(3).foreach { lines =>

        val addline = lines.head
        val genderline = lines(1)
        val interactionline = lines(2)

        val splittedadd = mybiotools.fastSplitSetSeparator(addline, Set(' ', '\t'))
        val splittedgender = mybiotools.fastSplitSetSeparator(genderline, Set(' ', '\t'))
        val splittedinteraction = mybiotools.fastSplitSetSeparator(interactionline, Set(' ', '\t'))

        val snpname = mybiotools.stringstore.StringStore(splittedadd(1))
        val nmiss = splittedadd(5).toInt
        val orADD = splittedadd(6).toDouble
        val seADD = splittedadd(7).toDouble

        val orSEX = splittedgender(6).toDouble
        val seSEX = splittedgender(7).toDouble

        val orINT = splittedinteraction(6).toDouble
        val seINT = splittedinteraction(7).toDouble

        val myORADD = myresult.covariates("SNP")._1.slope
        val mySDADD = myresult.covariates("SNP")._1.sd

        val myORINT = myresult.covariates("SNPxSEX")._1.slope
        val mySDINT = myresult.covariates("SNPxSEX")._1.sd

        val myORSEX = myresult.covariates("SEX")._1.slope
        val mySDSEX = myresult.covariates("SEX")._1.sd

        myresult.numberOfSamples should equal(nmiss)

        // close(myORADD, orADD) should be(true)
        // close(myORSEX, orSEX) should be(true)
        if (!close(myORINT, orINT)) {
          println(myORINT)
          println(orINT)
        }
        close(myORINT, orINT) should be(true)

      }

    }

  }
}