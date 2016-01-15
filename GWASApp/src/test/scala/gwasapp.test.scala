
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
package gwasapp

import org.scalatest.FunSpec
import org.scalatest.Matchers
import collection.immutable.HashMap
import com.typesafe.config._
import mybiotools.tasks._
import mybiotools._
import scala.util.Try
import mybiotools.gwascommons.associationresults._
import gwasapp.metagwasapp._

class GWASAppSpec extends FunSpec with Matchers {

  describe("meta") {
    it("1") {

      val testname = "metagwaslog"
      val bed = getClass.getResource("/").getPath + "/medium.bed"
      val bim = getClass.getResource("/").getPath + "/medium.bim"
      val fam = getClass.getResource("/").getPath + "/medium.fam"
      val tfam = getClass.getResource("/").getPath + "/medium.tfam"
      val tped = getClass.getResource("/").getPath + "/medium.tped"
      val pheno = getClass.getResource("/").getPath + "/medium.logistic.pheno.txt"
      val configstr = s"""
              bed = "$bed"
              bim = "$bim"
              fam = "$fam"
              #tped = "$tped"
              #tfam = "$tfam"
              map = "$bim"
              covar = "$pheno"
              pheno-name = "PHENO"
              minimumMAF = 0.01
              phenoscale = logistic
              gwasCPU = 1
              tasks.fileServiceBaseFolder = "${getClass.getResource("/").getPath}"/$testname/ 
              tasks.cacheEnabled=false  
              """
      new java.io.File(getClass.getResource("/").getPath).mkdir
      val c = ConfigFactory.parseString(configstr).withFallback(ConfigFactory.load())
      val ts = customTaskSystem(new LocalConfiguration(8, 5000), c)
      implicit val fs = ts.components
      val config = GWASAppConfig(c)
      val env = GWASEnvironmentConfig(c)
      val metaconfig = MetaGWASAppConfig(
        subconfigs = Map("a" -> config)
      )
      val metaenv = MetaEnvironmentConfig(
        gwasenv = env,
        metaGroupCount = 4,
        metaAnalyiseMemory = 1,
        gwasMemory = 1
      )
      val gwasrunner = new MetaGWASRunner(metaconfig, metaenv, ts.components)

      val myt1 = System.nanoTime
      gwasrunner.run
      println((System.nanoTime - myt1) / 1E9)

      ts.shutdown

      val expectedOut = getClass.getResource("/").getPath + s"/$testname/combinedP.gwas.concatenated"
      new java.io.File(expectedOut).canRead should be(true)

      val plinkcmd = s"""plink --maf 0.01 --bed $bed --bim $bim --fam $fam --logistic --allow-no-sex --pheno-name PHENO --pheno $pheno --out ${getClass.getResource("/").getPath}/plinklog.out"""
      val plinkt1 = System.nanoTime
      mybiotools.execGetStreamsAndCode(plinkcmd)
      println((System.nanoTime - plinkt1) / 1E9)

      val myresults = mybiotools.openSource(expectedOut)(s => mybiotools.gwascommons.associationresults.readAssociationResultsFromFileWithoutHeader(
        iter = s.getLines,
        None,
        snpcolumn = 1,
        pcolumn = 5,
        chrcolumn = Some(0),
        bpcolumn = Some(2),
        effectcolumn = Some(6),
        allelecolumn = Some(8),
        nonMissColumn = None,
        errorColumn = Some(7),
        testColumn = Some(3),
        frqColumn = None,
        phenoColumn = Some(4)
      ).asInstanceOf[Iterator[AssociationResult with HasEffectSize with HasTest]].toList).groupBy(_.name)

      val plinkoutput = getClass.getResource("/").getPath + "/plinklog.out.assoc.logistic"
      val plinkresults = mybiotools.openSource(plinkoutput)(s => mybiotools.gwascommons.associationresults.readAssociationResultsFromPlinkLogisticAssoc(s, None).toList).groupBy(_.name)

      (plinkresults.keySet &~ myresults.keySet).foreach(println)
      plinkresults.keySet &~ myresults.keySet should equal(Set())
      myresults.keySet &~ plinkresults.keySet should equal(Set())

      plinkresults.foreach {
        case (k, v) =>

          if (v.head.genomicLocation.chromosome.toInt < 23 && !v.head.effectSize.isNaN && math.abs(v.head.effectSize) < 1E8) {
            val effectSizeFlipped = if (v.head.allele == myresults(k).head.allele) myresults(k).head.effectSize else (-1 * myresults(k).head.effectSize)
            if (math.abs(math.exp(effectSizeFlipped) - v.head.effectSize) > 0.001) {
              println(v)
              println(k)
              println(myresults(k))
            }
            math.abs(math.exp(effectSizeFlipped) - v.head.effectSize) should be < 0.001
            // math.abs(myresults(k).head.pValue - v.head.pValue) should be < 0.0001
          }

      }

      myresults.foreach {
        case (k, v) =>

          if (v.head.genomicLocation.chromosome.toInt < 23 && !v.head.effectSize.isNaN && math.abs(plinkresults(k).head.effectSize) < 1E8) {
            val effectSizeFlipped = if (v.head.allele == plinkresults(k).head.allele) v.head.effectSize else (-1 * v.head.effectSize)

            if (math.abs(math.exp(effectSizeFlipped) - plinkresults(k).head.effectSize) > 0.001) {
              println(v)
              println(k)
              println(plinkresults(k))
            }
            math.abs(math.exp(effectSizeFlipped) - plinkresults(k).head.effectSize) should be < 0.001
            // math.abs(myresults(k).head.pValue - v.head.pValue) should be < 0.0001
          }

      }

      new java.io.File(expectedOut).delete
    }

  }

  describe("subset") {
    it("chunks") {
      val testname = "chunks"
      val bed = getClass.getResource("/").getPath + "/small.bed"
      val bim = getClass.getResource("/").getPath + "/small.bim"
      val fam = getClass.getResource("/").getPath + "/small.fam"
      val pheno = getClass.getResource("/").getPath + "/medium.linear.pheno.txt"
      // val covarfile = getClass.getResource("/").getPath + "/cov.txt"
      val configstr = s"""

      includePCAAxes = 10
      bed = "$bed"
      bim = "$bim"
      fam = "$fam"     
      map = "$bim"
      chr = 1
      from-bp = 2358171
      to-bp = 6690268
      chunks = 50
      covar = "$pheno"
      pheno-name = "PHENO"
      phenoscale = linear
      gwasCPU = 8
      tasks.fileServiceBaseFolder = "${getClass.getResource("/").getPath}"/$testname  
      tasks.cacheEnabled=false  
      wholeGenomeEpistasis = false
      keepPValueThreshold = 1.0
      ldPruningThreshold = 1.0
      """
      new java.io.File(getClass.getResource("/").getPath).mkdir
      val c = ConfigFactory.parseString(configstr).withFallback(ConfigFactory.load())
      val ts = customTaskSystem(new LocalConfiguration(8, 5000), c)
      implicit val fs = ts.components

      val config = GWASAppConfig(c)
      val env = GWASEnvironmentConfig(c)

      val gwasrunner = new GWASRunner(config, env, ts.components)

      val myt1 = System.nanoTime
      gwasrunner.run
      println((System.nanoTime - myt1) / 1E9)

      ts.shutdown

      val expectedOut = getClass.getResource("/").getPath + s"/$testname/assoc.gz"
      new java.io.File(expectedOut).canRead should be(true)
      // mybiotools.openSource(getClass.getResource("/").getPath + "/gwaslinintwg.out.testsDone")(_.getLines.next.toInt) should equal(5050)

      val myresults = mybiotools.openSource(expectedOut)(_.getLines.drop(1).map { line =>
        val spl = fastSplitSetSeparator(line, Set(' ', '\t'))
        val snp1 = spl(1)
        val stat = spl(6)
        Try(spl(10)).toOption.map { snp2 =>

          if (snp1 < snp2) (snp1, snp2) -> Try(stat.toDouble).toOption.getOrElse(Double.NaN) else (snp2, snp1) -> Try(stat.toDouble).toOption.getOrElse(Double.NaN)
        }
      }.filter(_.isDefined).map(_.get).filter(!_._2.isNaN).toMap)

      new java.io.File(expectedOut).delete
    }
  }

  describe("vs plink") {
    it("linear whole genome interaction") {
      val testname = "gwaslinintwg"
      val bed = getClass.getResource("/").getPath + "/small.bed"
      val bim = getClass.getResource("/").getPath + "/small.bim"
      val fam = getClass.getResource("/").getPath + "/small.fam"
      val tfam = getClass.getResource("/").getPath + "/small.tfam"
      val tped = getClass.getResource("/").getPath + "/small.tped"
      val pheno = getClass.getResource("/").getPath + "/medium.linear.pheno.txt"
      // val covarfile = getClass.getResource("/").getPath + "/cov.txt"
      val configstr = s"""
      bed = "$bed"
      bim = "$bim"
      fam = "$fam"
      #tped = "$tped"
      #tfam = "$tfam"
      map = "$bim"
      covar = "$pheno"
      pheno-name = "PHENO"
      phenoscale = linear
      gwasCPU = 8
      tasks.fileServiceBaseFolder = "${getClass.getResource("/").getPath}"/$testname 
      tasks.cacheEnabled=false  
      interactionmodels = PROD
      wholeGenomeEpistasis = true
      keepPValueThreshold = 1.0
      ldPruningThreshold = 1.0
      """
      new java.io.File(getClass.getResource("/").getPath).mkdir
      val c = ConfigFactory.parseString(configstr).withFallback(ConfigFactory.load())
      val ts = customTaskSystem(new LocalConfiguration(8, 5000), c)
      implicit val fs = ts.components
      val config = GWASAppConfig(c)
      val env = GWASEnvironmentConfig(c)
      val gwasrunner = new GWASRunner(config, env, ts.components)

      val myt1 = System.nanoTime
      gwasrunner.run
      println((System.nanoTime - myt1) / 1E9)

      ts.shutdown

      val expectedOut = getClass.getResource("/").getPath + s"/$testname/assoc.gz"
      new java.io.File(expectedOut).canRead should be(true)
      // mybiotools.openSource(getClass.getResource("/").getPath + "/gwaslinintwg.out.testsDone")(_.getLines.next.toInt) should equal(5050)

      val plinkcmd = s"""plink --bed $bed --bim $bim --fam $fam --linear --allow-no-sex --pheno-name PHENO --pheno $pheno --epistasis --epi1 1.0 --out ${getClass.getResource("/").getPath}/plink.linintggw.out"""
      val plinkt1 = System.nanoTime
      mybiotools.execGetStreamsAndCode(plinkcmd)
      println((System.nanoTime - plinkt1) / 1E9)

      val myresults = mybiotools.openSource(expectedOut)(_.getLines.drop(1).map { line =>
        val spl = fastSplitSetSeparator(line, Set(' ', '\t'))
        println(spl)
        val snp1 = spl(1)
        val stat = spl(6)
        Try(spl(10)).toOption.map { snp2 =>

          if (snp1 < snp2) (snp1, snp2) -> Try(stat.toDouble).toOption.getOrElse(Double.NaN) else (snp2, snp1) -> Try(stat.toDouble).toOption.getOrElse(Double.NaN)
        }
      }.filter(_.isDefined).map(_.get).filter(!_._2.isNaN).toMap)

      val plinkoutput = getClass.getResource("/").getPath + "/plink.linintggw.out.epi.qt"
      val plinkresults = mybiotools.openSource(plinkoutput)(_.getLines.drop(1).map { line =>
        val spl = fastSplitSetSeparator(line, Set(' ', '\t'))
        val snp1 = spl(1)
        val snp2 = spl(3)
        val stat = spl(4)
        if (snp1 < snp2) (snp1, snp2) -> Try(stat.toDouble).toOption.getOrElse(Double.NaN) else (snp2, snp1) -> Try(stat.toDouble).toOption.getOrElse(Double.NaN)
      }.filter(!_._2.isNaN).toMap)
      (myresults.keySet &~ plinkresults.keySet).foreach(println)
      plinkresults.keySet &~ myresults.keySet should equal(Set())

      // we report some STAT fields but no p-value fields where SE is high, and we don't check rsq between snpXsnp. ld should be run upstream
      // (myresults.keySet &~ plinkresults.keySet).size should equal(8)

      plinkresults.foreach {
        case (k, v) =>
          if (math.abs(myresults(k) - v) > 0.001) {
            println(k)
            println(v)
            println(myresults(k))
          }
          math.abs(myresults(k) - v) should be < 0.001
      }

      myresults.foreach {
        case (k, v) =>
          if (plinkresults.contains(k)) {
            if (math.abs(myresults(k) - v) > 0.001) {
              println(k)
              println(v)
              println(plinkresults(k))
            }
            math.abs(myresults(k) - v) should be < 0.001
          }
      }

      new java.io.File(expectedOut).delete
    }
    it("logistic interaction") {
      val testname = "gwaslogint"
      val bed = getClass.getResource("/").getPath + "/medium.bed"
      val bim = getClass.getResource("/").getPath + "/medium.bim"
      val fam = getClass.getResource("/").getPath + "/medium.fam"
      val tfam = getClass.getResource("/").getPath + "/medium.tfam"
      val tped = getClass.getResource("/").getPath + "/medium.tped"
      val pheno = getClass.getResource("/").getPath + "/medium.logistic.pheno.txt"
      val covarfile = getClass.getResource("/").getPath + "/cov.txt"
      val configstr = s"""
              bed = "$bed"
              bim = "$bim"
              fam = "$fam"
              #tped = "$tped"
              #tfam = "$tfam"
              map = "$bim"
              covar = "$pheno,$covarfile"
              pheno-name = "PHENO"
              phenoscale = logistic
              gwasCPU = 1
              tasks.fileServiceBaseFolder = "${getClass.getResource("/").getPath}"/$testname/  
              tasks.cacheEnabled=false  
              interactioncovariates-list = rs11918908_2
              interactionmodels = PROD
              """
      new java.io.File(getClass.getResource("/").getPath).mkdir
      val c = ConfigFactory.parseString(configstr).withFallback(ConfigFactory.load())
      val ts = customTaskSystem(new LocalConfiguration(8, 5000), c)
      implicit val fs = ts.components
      val config = GWASAppConfig(c)
      val env = GWASEnvironmentConfig(c)
      val gwasrunner = new GWASRunner(config, env, ts.components)

      val myt1 = System.nanoTime
      gwasrunner.run
      println((System.nanoTime - myt1) / 1E9)

      ts.shutdown

      val expectedOut = getClass.getResource("/").getPath + s"/$testname/assoc.gz"
      new java.io.File(expectedOut).canRead should be(true)
      mybiotools.openSource(getClass.getResource("/").getPath + s"/$testname/testDone")(_.getLines.next.toInt) should equal(24806)

      val plinkcmd = s"""plink --bed $bed --bim $bim --fam $fam --logistic --allow-no-sex --pheno-name PHENO --interaction --pheno $pheno --covar $covarfile --covar-name rs11918908_2 --out ${getClass.getResource("/").getPath}/plink.logint.out"""
      val plinkt1 = System.nanoTime
      mybiotools.execGetStreamsAndCode(plinkcmd)
      println((System.nanoTime - plinkt1) / 1E9)

      val myresults = mybiotools.openSource(expectedOut)(s => mybiotools.gwascommons.associationresults.readAssociationResultsFromPlinkLinearAssoc(s, None).toList).filter(_.test.value.endsWith("xrs11918908_2")).groupBy(_.name)

      val plinkoutput = getClass.getResource("/").getPath + "/plink.logint.out.assoc.logistic"
      val plinkresults = mybiotools.openSource(plinkoutput)(s => mybiotools.gwascommons.associationresults.readAssociationResultsFromPlinkLogisticAssoc(s, None).toList).filter(x => x.test.value.endsWith("xrs11918908_2")).groupBy(_.name)

      (plinkresults.keySet &~ myresults.keySet).foreach(println)
      plinkresults.keySet &~ myresults.keySet should equal(Set())
      myresults.keySet &~ plinkresults.keySet should equal(Set())

      plinkresults.foreach {
        case (k, v) =>

          if (v.head.genomicLocation.chromosome.toInt < 23 && !v.head.effectSize.isNaN && math.abs(v.head.effectSize) < 1E8) {
            val effectSizeFlipped = if (v.head.allele == myresults(k).head.allele) myresults(k).head.effectSize else (-1 * myresults(k).head.effectSize)

            if (math.abs(math.exp(effectSizeFlipped) - v.head.effectSize) > 0.01) {
              println(v)
              println(k)
              println(myresults(k))
            }
            math.abs(math.exp(effectSizeFlipped) - v.head.effectSize) should be < 0.01
            // math.abs(myresults(k).head.pValue - v.head.pValue) should be < 0.0001
          }

      }

      myresults.foreach {
        case (k, v) =>

          if (v.head.genomicLocation.chromosome.toInt < 23 && !v.head.effectSize.isNaN && math.abs(plinkresults(k).head.effectSize) < 1E8) {
            val effectSizeFlipped = if (v.head.allele == plinkresults(k).head.allele) v.head.effectSize else (-1 * v.head.effectSize)

            if (math.abs(math.exp(effectSizeFlipped) - plinkresults(k).head.effectSize) > 0.01) {
              println(v)
              println(k)
              println(plinkresults(k))
            }
            math.abs(math.exp(effectSizeFlipped) - plinkresults(k).head.effectSize) should be < 0.01
            // math.abs(myresults(k).head.pValue - v.head.pValue) should be < 0.0001
          }

      }

      new java.io.File(expectedOut).delete
    }
    it("linear interaction") {
      val testname = "gwaslinint"
      val bed = getClass.getResource("/").getPath + "/medium.bed"
      val bim = getClass.getResource("/").getPath + "/medium.bim"
      val fam = getClass.getResource("/").getPath + "/medium.fam"
      val tfam = getClass.getResource("/").getPath + "/medium.tfam"
      val tped = getClass.getResource("/").getPath + "/medium.tped"
      val pheno = getClass.getResource("/").getPath + "/medium.linear.pheno.txt"
      val covarfile = getClass.getResource("/").getPath + "/cov.txt"
      val configstr = s"""
              bed = "$bed"
              bim = "$bim"
              fam = "$fam"
              #tped = "$tped"
              #tfam = "$tfam"
              map = "$bim"
              covar = "$pheno,$covarfile"
              pheno-name = "PHENO"
              phenoscale = linear
              gwasCPU = 1
              tasks.fileServiceBaseFolder = "${getClass.getResource("/").getPath}"/$testname/  
              tasks.cacheEnabled=false  
              interactioncovariates-list = rs11918908_2
              interactionmodels = PROD
              """
      new java.io.File(getClass.getResource("/").getPath).mkdir
      val c = ConfigFactory.parseString(configstr).withFallback(ConfigFactory.load())
      val ts = customTaskSystem(new LocalConfiguration(8, 5000), c)
      implicit val fs = ts.components
      val config = GWASAppConfig(c)
      val env = GWASEnvironmentConfig(c)
      val gwasrunner = new GWASRunner(config, env, ts.components)

      val myt1 = System.nanoTime
      gwasrunner.run
      println((System.nanoTime - myt1) / 1E9)

      ts.shutdown

      val expectedOut = getClass.getResource("/").getPath + s"/$testname/assoc.gz"
      new java.io.File(expectedOut).canRead should be(true)
      mybiotools.openSource(getClass.getResource("/").getPath + s"/$testname/testDone")(_.getLines.next.toInt) should equal(24806)

      val plinkcmd = s"""plink --bed $bed --bim $bim --fam $fam --linear --allow-no-sex --pheno-name PHENO --interaction --pheno $pheno --covar $covarfile --covar-name rs11918908_2 --out ${getClass.getResource("/").getPath}/plink.linint.out"""
      val plinkt1 = System.nanoTime
      mybiotools.execGetStreamsAndCode(plinkcmd)
      println((System.nanoTime - plinkt1) / 1E9)

      val myresults = mybiotools.openSource(expectedOut)(s => mybiotools.gwascommons.associationresults.readAssociationResultsFromPlinkLinearAssoc(s, None).toList).filter(_.test.value.endsWith("xrs11918908_2")).groupBy(_.name)

      val plinkoutput = getClass.getResource("/").getPath + "/plink.linint.out.assoc.linear"
      val plinkresults = mybiotools.openSource(plinkoutput)(s => mybiotools.gwascommons.associationresults.readAssociationResultsFromPlinkLinearAssoc(s, None).toList).filter(x => x.test.value.endsWith("xrs11918908_2") && math.abs(x.effectSize) < 1E6).groupBy(_.name)

      (myresults.keySet &~ plinkresults.keySet).foreach(println)
      plinkresults.keySet &~ myresults.keySet should equal(Set())
      myresults.keySet &~ plinkresults.keySet should equal(Set())

      plinkresults.foreach {
        case (k, v) =>
          if (math.abs(myresults(k).head.pValue - v.head.pValue) > 0.0001) {
            println(k)
            println(v)
            println(myresults(k))
          }
          math.abs(myresults(k).head.pValue - v.head.pValue) should be < 0.0001
      }

      myresults.foreach {
        case (k, v) =>
          if (math.abs(plinkresults(k).head.pValue - v.head.pValue) > 0.0001) {
            println(k)
            println(v)
            println(plinkresults(k))
          }
          math.abs(plinkresults(k).head.pValue - v.head.pValue) should be < 0.0001
      }

      new java.io.File(expectedOut).delete
    }
    it("linear ") {
      val testname = "gwaslin"
      val bed = getClass.getResource("/").getPath + "/medium.bed"
      val bim = getClass.getResource("/").getPath + "/medium.bim"
      val fam = getClass.getResource("/").getPath + "/medium.fam"
      val tfam = getClass.getResource("/").getPath + "/medium.tfam"
      val tped = getClass.getResource("/").getPath + "/medium.tped"
      val pheno = getClass.getResource("/").getPath + "/medium.linear.pheno.txt"
      val configstr = s"""
            	bed = "$bed"
            	bim = "$bim"
            	fam = "$fam"
              #tped = "$tped"
              #tfam = "$tfam"
              map = "$bim"
            	covar = "$pheno"
            	pheno-name = "PHENO"
              phenoscale = linear
              gwasCPU = 1
            	tasks.fileServiceBaseFolder = "${getClass.getResource("/").getPath}"/$testname 
              tasks.cacheEnabled=false  
              plot = true
            	"""
      new java.io.File(getClass.getResource("/").getPath).mkdir
      val c = ConfigFactory.parseString(configstr).withFallback(ConfigFactory.load())
      val ts = customTaskSystem(new LocalConfiguration(8, 5000), c)
      implicit val fs = ts.components
      val config = GWASAppConfig(c)
      val env = GWASEnvironmentConfig(c)
      val gwasrunner = new GWASRunner(config, env, ts.components)

      val myt1 = System.nanoTime
      gwasrunner.run
      println((System.nanoTime - myt1) / 1E9)

      ts.shutdown

      val expectedOut = getClass.getResource("/").getPath + s"/$testname/assoc.gz"
      new java.io.File(expectedOut).canRead should be(true)
      mybiotools.openSource(getClass.getResource("/").getPath + s"/$testname/testDone")(_.getLines.next.toInt) should equal(24806)

      val plinkcmd = s"""plink --bed $bed --bim $bim --fam $fam --linear --allow-no-sex --pheno-name PHENO --pheno $pheno --out ${getClass.getResource("/").getPath}/plink.lin.out"""
      val plinkt1 = System.nanoTime
      mybiotools.execGetStreamsAndCode(plinkcmd)
      println((System.nanoTime - plinkt1) / 1E9)

      val myresults = mybiotools.openSource(expectedOut)(s => mybiotools.gwascommons.associationresults.readAssociationResultsFromPlinkLinearAssoc(s, None).toList).groupBy(_.name)

      val plinkoutput = getClass.getResource("/").getPath + "/plink.lin.out.assoc.linear"
      val plinkresults = mybiotools.openSource(plinkoutput)(s => mybiotools.gwascommons.associationresults.readAssociationResultsFromPlinkLinearAssoc(s, None).toList).groupBy(_.name)

      (plinkresults.keySet &~ myresults.keySet).foreach(println)
      plinkresults.keySet &~ myresults.keySet should equal(Set())
      myresults.keySet &~ plinkresults.keySet should equal(Set())

      plinkresults.foreach {
        case (k, v) =>
          if (math.abs(myresults(k).head.pValue - v.head.pValue) > 0.0001) {
            println(k)
            println(v)
            println(myresults(k))
          }
          math.abs(myresults(k).head.pValue - v.head.pValue) should be < 0.0001
      }

      myresults.foreach {
        case (k, v) =>
          if (math.abs(plinkresults(k).head.pValue - v.head.pValue) > 0.0001) {
            println(k)
            println(v)
            println(plinkresults(k))
          }
          math.abs(plinkresults(k).head.pValue - v.head.pValue) should be < 0.0001
      }

      new java.io.File(expectedOut).delete
    }

    it("logistic  ") {
      val testname = "gwaslog"
      val bed = getClass.getResource("/").getPath + "/medium.bed"
      val bim = getClass.getResource("/").getPath + "/medium.bim"
      val fam = getClass.getResource("/").getPath + "/medium.fam"
      val tfam = getClass.getResource("/").getPath + "/medium.tfam"
      val tped = getClass.getResource("/").getPath + "/medium.tped"
      val pheno = getClass.getResource("/").getPath + "/medium.logistic.pheno.txt"
      val configstr = s"""
              bed = "$bed"
              bim = "$bim"
              fam = "$fam"
              #tped = "$tped"
              #tfam = "$tfam"
              map = "$bim"
              covar = "$pheno"
              pheno-name = "PHENO"
              minimumMAF = 0.01
              phenoscale = logistic
              gwasCPU = 1
              tasks.fileServiceBaseFolder = "${getClass.getResource("/").getPath}"/$testname/ 
              tasks.cacheEnabled=false  
              """
      new java.io.File(getClass.getResource("/").getPath).mkdir
      val c = ConfigFactory.parseString(configstr).withFallback(ConfigFactory.load())
      val ts = customTaskSystem(new LocalConfiguration(8, 5000), c)
      implicit val fs = ts.components
      val config = GWASAppConfig(c)
      val env = GWASEnvironmentConfig(c)
      val gwasrunner = new GWASRunner(config, env, ts.components)

      val myt1 = System.nanoTime
      gwasrunner.run
      println((System.nanoTime - myt1) / 1E9)

      ts.shutdown

      val expectedOut = getClass.getResource("/").getPath + s"/$testname/assoc.gz"
      new java.io.File(expectedOut).canRead should be(true)
      mybiotools.openSource(getClass.getResource("/").getPath + s"/$testname/testDone")(_.getLines.next.toInt) should equal(24033)

      val plinkcmd = s"""plink --maf 0.01 --bed $bed --bim $bim --fam $fam --logistic --allow-no-sex --pheno-name PHENO --pheno $pheno --out ${getClass.getResource("/").getPath}/plinklog.out"""
      val plinkt1 = System.nanoTime
      mybiotools.execGetStreamsAndCode(plinkcmd)
      println((System.nanoTime - plinkt1) / 1E9)

      val myresults = mybiotools.openSource(expectedOut)(s => mybiotools.gwascommons.associationresults.readAssociationResultsFromPlinkLinearAssoc(s, None).toList).groupBy(_.name)

      val plinkoutput = getClass.getResource("/").getPath + "/plinklog.out.assoc.logistic"
      val plinkresults = mybiotools.openSource(plinkoutput)(s => mybiotools.gwascommons.associationresults.readAssociationResultsFromPlinkLogisticAssoc(s, None).toList).groupBy(_.name)

      (plinkresults.keySet &~ myresults.keySet).foreach(println)
      plinkresults.keySet &~ myresults.keySet should equal(Set())
      myresults.keySet &~ plinkresults.keySet should equal(Set())

      plinkresults.foreach {
        case (k, v) =>

          if (v.head.genomicLocation.chromosome.toInt < 23 && !v.head.effectSize.isNaN && math.abs(v.head.effectSize) < 1E8) {
            val effectSizeFlipped = if (v.head.allele == myresults(k).head.allele) myresults(k).head.effectSize else (-1 * myresults(k).head.effectSize)
            if (math.abs(math.exp(effectSizeFlipped) - v.head.effectSize) > 0.001) {
              println(v)
              println(k)
              println(myresults(k))
            }
            math.abs(math.exp(effectSizeFlipped) - v.head.effectSize) should be < 0.001
            // math.abs(myresults(k).head.pValue - v.head.pValue) should be < 0.0001
          }

      }

      myresults.foreach {
        case (k, v) =>

          if (v.head.genomicLocation.chromosome.toInt < 23 && !v.head.effectSize.isNaN && math.abs(plinkresults(k).head.effectSize) < 1E8) {
            val effectSizeFlipped = if (v.head.allele == plinkresults(k).head.allele) v.head.effectSize else (-1 * v.head.effectSize)

            if (math.abs(math.exp(effectSizeFlipped) - plinkresults(k).head.effectSize) > 0.001) {
              println(v)
              println(k)
              println(plinkresults(k))
            }
            math.abs(math.exp(effectSizeFlipped) - plinkresults(k).head.effectSize) should be < 0.001
            // math.abs(myresults(k).head.pValue - v.head.pValue) should be < 0.0001
          }

      }

      new java.io.File(expectedOut).delete
    }

    it("logistic score: if plink p < 0.01, then score test p should be <0.01 as well") {
      val testname = "gwaslogscore"
      val bed = getClass.getResource("/").getPath + "/medium.bed"
      val bim = getClass.getResource("/").getPath + "/medium.bim"
      val fam = getClass.getResource("/").getPath + "/medium.fam"
      val tfam = getClass.getResource("/").getPath + "/medium.tfam"
      val tped = getClass.getResource("/").getPath + "/medium.tped"
      val pheno = getClass.getResource("/").getPath + "/medium.logistic.pheno.txt"
      val configstr = s"""
              bed = "$bed"
              bim = "$bim"
              fam = "$fam"
              #tped = "$tped"
              #tfam = "$tfam"
              map = "$bim"
              covar = "$pheno"
              pheno-name = "PHENO"
              minimumMAF = 0.01
              phenoscale = logistic
              gwasCPU = 1
              tasks.fileServiceBaseFolder = "${getClass.getResource("/").getPath}"/$testname/ 
              tasks.cacheEnabled=false  
              prefilterWithScoreTest = true
              """
      new java.io.File(getClass.getResource("/").getPath).mkdir
      val c = ConfigFactory.parseString(configstr).withFallback(ConfigFactory.load())
      val ts = customTaskSystem(new LocalConfiguration(8, 5000), c)
      implicit val fs = ts.components
      val config = GWASAppConfig(c)
      val env = GWASEnvironmentConfig(c)
      val gwasrunner = new GWASRunner(config, env, ts.components)

      val myt1 = System.nanoTime
      gwasrunner.run
      println((System.nanoTime - myt1) / 1E9)

      ts.shutdown

      val expectedOut = getClass.getResource("/").getPath + s"/$testname/assoc.gz"
      new java.io.File(expectedOut).canRead should be(true)
      mybiotools.openSource(getClass.getResource("/").getPath + s"/$testname/testDone")(_.getLines.next.toInt) should equal(24033)

      val plinkcmd = s"""plink --maf 0.01 --bed $bed --bim $bim --fam $fam --logistic --allow-no-sex --pheno-name PHENO --pheno $pheno --out ${getClass.getResource("/").getPath}/plinklog.out"""
      val plinkt1 = System.nanoTime
      mybiotools.execGetStreamsAndCode(plinkcmd)
      println((System.nanoTime - plinkt1) / 1E9)

      val myresults = mybiotools.openSource(expectedOut)(s => mybiotools.gwascommons.associationresults.readAssociationResultsFromPlinkLinearAssoc(s, None).toList).groupBy(_.name)

      val plinkoutput = getClass.getResource("/").getPath + "/plinklog.out.assoc.logistic"
      val plinkresults = mybiotools.openSource(plinkoutput)(s => mybiotools.gwascommons.associationresults.readAssociationResultsFromPlinkLogisticAssoc(s, None).toList).groupBy(_.name)

      (plinkresults.keySet &~ myresults.keySet).foreach(println)
      plinkresults.keySet &~ myresults.keySet should equal(Set())
      myresults.keySet &~ plinkresults.keySet should equal(Set())

      plinkresults.foreach {
        case (k, v) =>

          if (v.head.genomicLocation.chromosome.toInt < 23 && !v.head.effectSize.isNaN && math.abs(v.head.effectSize) < 1E8) {
            val effectSizeFlipped = if (v.head.allele == myresults(k).head.allele) myresults(k).head.effectSize else (-1 * myresults(k).head.effectSize)
            if (math.abs(math.exp(effectSizeFlipped) - v.head.effectSize) > 0.001) {
              println(v)
              println(k)
              println(myresults(k))
            }
            if (!effectSizeFlipped.isNaN) {
              math.abs(math.exp(effectSizeFlipped) - v.head.effectSize) should be < 0.001
            } else {
              if (v.head.pValue < 0.01) myresults(k).head.pValue should be < 0.01

            }

          }

      }

      myresults.foreach {
        case (k, v) =>

          if (v.head.genomicLocation.chromosome.toInt < 23 && !v.head.effectSize.isNaN && math.abs(plinkresults(k).head.effectSize) < 1E8) {
            val effectSizeFlipped = if (v.head.allele == plinkresults(k).head.allele) v.head.effectSize else (-1 * v.head.effectSize)

            if (math.abs(math.exp(effectSizeFlipped) - plinkresults(k).head.effectSize) > 0.001) {
              println(v)
              println(k)
              println(plinkresults(k))
            }
            math.abs(math.exp(effectSizeFlipped) - plinkresults(k).head.effectSize) should be < 0.001
            // math.abs(myresults(k).head.pValue - v.head.pValue) should be < 0.0001
          }

      }

      new java.io.File(expectedOut).delete
    }
  }

}