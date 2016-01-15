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

import org.scalatest.FunSuite

import mybiotools._
import mybiotools.gwascommons._
import mybiotools.gwascommons.genotypedata._
import GenotypeStates._
import scala.collection.mutable.Map
import mybiotools.stringstore._
import org.scalatest.Matchers
import java.io.File

class GRMTestSuite extends FunSuite with Matchers {

  test("recursively remove samples") {
    import org.saddle._
    val f = Frame(
      'a' -> Series('a' -> 1.0, 'b' -> 2.0, 'c' -> 3.0),
      'b' -> Series('a' -> 2.0, 'b' -> 3.0, 'c' -> 4.0),
      'c' -> Series('a' -> 3.0, 'b' -> 4.0, 'c' -> 5.0)
    )
    GRM.recursivelyDropSamples(f, 0.0) should equal(f)
    GRM.recursivelyDropSamples(f, 1.0) should equal(f)
    GRM.recursivelyDropSamples(f, 1.1) should equal(Frame(
      'b' -> Series('b' -> 3.0, 'c' -> 4.0),
      'c' -> Series('b' -> 4.0, 'c' -> 5.0)
    ))
    GRM.recursivelyDropSamples(f, 2.1) should equal(Frame(
      'b' -> Series('b' -> 3.0, 'c' -> 4.0),
      'c' -> Series('b' -> 4.0, 'c' -> 5.0)
    ))
    GRM.recursivelyDropSamples(f, 4.1) should equal(Frame(
      'c' -> Series('c' -> 5.0)
    ))
  }

  def close(a: Double, b: Double) = math.abs(a - b) < 0.0001
  def close2(a: Double, b: Double) = math.abs(a - b) < 0.01
  def close3(a: Double, b: Double) = math.abs(a - b) < 0.05

  // gcta
  // 	1	1	5.000000e+00	8.799999e-01
  // 2	1	5.000000e+00	-7.200000e-01
  // 2	2	5.000000e+00	8.799999e-01
  // 3	1	5.000000e+00	-1.600000e-01
  // 3	2	5.000000e+00	-1.600000e-01
  // 3	3	5.000000e+00	3.200000e-01

  ignore("grm with singletons") {
    val genotypes = List(
      Vector(0, 0, 0, 2, 0, 0),
      Vector(0, 0, 2, 0, 0, 0),
      Vector(0, 2, 0, 0, 0, 0),
      Vector(2, 0, 0, 0, 0, 0)
    )
    val inds = Vector(Individual("a1", "a1"), Individual("a2", "a1"), Individual("a3", "a1"), Individual("a4", "a1"), Individual("a5", "a1"), Individual("a6", "a1"))
    val iter = genotypes.zipWithIndex.map {
      case (x, i) =>
        (PDosageFileRowSummary("s" + i, "A", "T", x.map(_.toFloat), Float.NaN), x.toArray.map(_.toFloat))
    }.iterator
    val (grm, snps) = GRM.getGRMFromAutosomes(
      dosages = iter,
      individuals = inds,
      missingValue = Float.NaN,
      batchSize = 1,
      threads = 1
    )
    println(grm)

    val tmpgz = TempFile.createTempFile(".grm.gz")
    val tmpid = new File(tmpgz.getAbsolutePath.dropRight(2) + "id")
    val pheno = TempFile.createTempFile(".pheno")
    openFileWriter(tmpgz) { gzwriter =>
      openFileWriter(tmpid) { idwriter =>
        GRM.write(grm, gzwriter, idwriter)
      }
    }
    val phenodata = Vector(Individual("a1", "a1") -> 1.0, Individual("a2", "a1") -> 1.0, Individual("a3", "a1") -> 1.0, Individual("a4", "a1") -> 1.0, Individual("a5", "a1") -> 0.0, Individual("a6", "a1") -> 0.0)
    writeToFile(pheno.getAbsolutePath, phenodata.map(x => x._1.toLine + " " + x._2).mkString("\n"))

    println(tmpgz)
    println(pheno)

  }

  test("grm tiny") {
    val tpedfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny.tped")
    val tfamfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny.tfam")
    val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfamfile), tped = scala.io.Source.fromFile(tpedfile), '-')
    val reader = SNPMajorReaders.getSNPMajorIterator(tpedfiles)
    val (grm1, snps) = GRM.getGRMFromAutosomes(reader, 3, 10)
    val grm = grm1.toMat
    grm.raw(0, 0) should equal(0.8799999999999997)
    grm.raw(1, 0) should equal(-0.7199999999999999)
    grm.raw(1, 1) should equal(0.8799999999999997)
    grm.raw(2, 0) should equal(-0.16)
    grm.raw(2, 1) should equal(-0.16)
    grm.raw(2, 2) should equal(0.3200000000000001)
    snps should equal(5)
  }

  test("grm tiny frame") {
    val tpedfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny.tped")
    val tfamfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/grmtest_tiny.tfam")
    val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfamfile), tped = scala.io.Source.fromFile(tpedfile), '-')
    val reader = SNPMajorReaders.getSNPMajorIterator(tpedfiles).toLocusIteratorWithGenomicMap(Map()).toFrame
    val (grm1) = GRM.fromFrame(reader)
    val grm = grm1.toMat
    grm.raw(0, 0) should equal(0.8799999999999997)
    grm.raw(1, 0) should equal(-0.7199999999999999)
    grm.raw(1, 1) should equal(0.8799999999999997)
    grm.raw(2, 0) should equal(-0.15999999999999998)
    grm.raw(2, 1) should equal(-0.15999999999999998)
    grm.raw(2, 2) should equal(0.32)
  }

  test("gcta short test file") {
    val tpedfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/testgcta.short.tped")
    val tfamfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/testgcta.short.tfam")
    val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfamfile), tped = scala.io.Source.fromFile(tpedfile), '0')
    val reader = SNPMajorReaders.getSNPMajorIterator(tpedfiles)
    val (grm, snpcount) = GRM.getGRMFromAutosomes(reader, 5, 10)
    val testfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/testgcta.short.grm.gz")
    mybiotools.openSource(testfile.getAbsolutePath)(_.getLines.foreach { line =>
      val spl = mybiotools.fastSplitSeparator(line, '\t')
      val idxi = spl(0).toInt - 1
      val idxj = spl(1).toInt - 1
      val v = spl(3).toDouble
      val x = close(grm.raw(idxi, idxj), v)
      if (!x) {
        println(grm.raw(idxi, idxj) + " vs " + v)
      }
      x should be(true)
    })
    snpcount should equal(1000)
    val sw1 = new java.io.StringWriter
    val sw2 = new java.io.StringWriter
    GRM.write(grm, sw1, sw2)
    val grm2 = GRM.read(io.Source.fromString(sw1.toString), io.Source.fromString(sw2.toString))
    //println(sw1.toString)
    //println(sw2.toString)
    grm should equal(grm2)

  }

  test("gcta short test file frame") {
    val tpedfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/testgcta.short.tped")
    val tfamfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/testgcta.short.tfam")
    val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfamfile), tped = scala.io.Source.fromFile(tpedfile), '0')
    val data = SNPMajorReaders.getSNPMajorIterator(tpedfiles).toLocusIteratorWithGenomicMap(Map()).toFrame
    val (grm) = GRM.fromFrame(data)
    val testfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/testgcta.short.grm.gz")
    mybiotools.openSource(testfile.getAbsolutePath)(_.getLines.foreach { line =>
      val spl = mybiotools.fastSplitSeparator(line, '\t')
      val idxi = spl(0).toInt - 1
      val idxj = spl(1).toInt - 1
      val v = spl(3).toDouble
      val x = close2(grm.raw(idxi, idxj), v)
      if (!x) {
        println(grm.raw(idxi, idxj) + " vs " + v)
      }
      x should be(true)
    })

    val sw1 = new java.io.StringWriter
    val sw2 = new java.io.StringWriter
    GRM.write(grm, sw1, sw2)
    val grm2 = GRM.read(io.Source.fromString(sw1.toString), io.Source.fromString(sw2.toString))
    //println(sw1.toString)
    //println(sw2.toString)
    grm should equal(grm2)

  }

  test("gcta  test file") {
    val tpedfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/testgcta.tped")
    val tfamfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/testgcta.tfam")
    val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfamfile), tped = scala.io.Source.fromFile(tpedfile), '0')
    val reader = SNPMajorReaders.getSNPMajorIterator(tpedfiles)
    val t1 = System.nanoTime
    val (grm1, snpcount) = GRM.getGRMFromAutosomes(reader, 100, 5)
    val grm = grm1.toMat
    println((System.nanoTime - t1) / 1E9)
    val testfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/testgcta.grm.gz")
    mybiotools.openSource(testfile.getAbsolutePath)(_.getLines.foreach { line =>
      val spl = mybiotools.fastSplitSeparator(line, '\t')
      val idxi = spl(0).toInt - 1
      val idxj = spl(1).toInt - 1
      val v = spl(3).toDouble
      close(grm.raw(idxi, idxj), v) should be(true)
    })
    snpcount should equal(1000)

  }

  test("gcta  test file frame") {
    val tpedfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/testgcta.tped")
    val tfamfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/testgcta.tfam")
    val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfamfile), tped = scala.io.Source.fromFile(tpedfile), '0')
    val reader = SNPMajorReaders.getSNPMajorIterator(tpedfiles).toLocusIteratorWithGenomicMap(Map()).toFrame
    val t1 = System.nanoTime
    val (grm1) = GRM.fromFrame(reader)
    val grm = grm1.toMat
    println((System.nanoTime - t1) / 1E9)
    val testfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/testgcta.grm.gz")
    mybiotools.openSource(testfile.getAbsolutePath)(_.getLines.foreach { line =>
      val spl = mybiotools.fastSplitSeparator(line, '\t')
      val idxi = spl(0).toInt - 1
      val idxj = spl(1).toInt - 1
      val v = spl(3).toDouble
      val x = close3(grm.raw(idxi, idxj), v)
      if (!x) {
        println(grm.raw(idxi, idxj) + " vs " + v)
      }
      x should be(true)
    })

  }

  ignore("pca") {
    val tpedfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/testgcta.tped")
    val tfamfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/testgcta.tfam")
    val bimfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/testgcta.bim")
    val bedfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/testgcta.bed")
    val famfile = new java.io.File(getClass.getResource("/").getPath + "genotypedata/testgcta.fam")
    val tpedfiles = TPedFiles(fam = scala.io.Source.fromFile(tfamfile), tped = scala.io.Source.fromFile(tpedfile), '0')
    val reader = SNPMajorReaders.getSNPMajorIterator(tpedfiles)
    val (grm, snpcount) = GRM.getGRMFromAutosomes(reader, 100, 10)
    val pca = mybiotools.pcanew.pcaFromGRM(grm)
    // mybiotools.plots.show(mybiotools.pcanew.plotPCAResult(pca, 1, 2))

    // println(pca.eigenValues)
  }

}