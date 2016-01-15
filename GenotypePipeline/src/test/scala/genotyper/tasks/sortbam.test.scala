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
package genotyper.tasks

import org.scalatest._

import mybiotools.tasks._

import org.scalatest.FunSpec
import org.scalatest.Matchers
import mybiotools.gwascommons._
import mybiotools.workflows._
import scala.concurrent.ExecutionContext.Implicits.global

object TestsSortCall {

  val ts = defaultTaskSystem("""
    tasks.cacheEnabled=false
    tasks.logFile="" 
    tasks.fileServiceBaseFolder = "./target/"
    """)
  import ts._

  val exampleBamFile = new java.io.File(getClass.getResource("/").getPath + "/example.chr1:1-100000.bam")

  val exampleFasta = new java.io.File(getClass.getResource("/").getPath + "/exampleFASTA.fasta")

  val exampleFastaDict = new java.io.File(getClass.getResource("/").getPath + "/exampleFASTA.fasta.dict")

  val exampleFastaFai = new java.io.File(getClass.getResource("/").getPath + "/exampleFASTA.fasta.fai")

  val gatk = new java.io.File(getClass.getResource("/").getPath + "/GenomeAnalysisTK.jar")

  val intervalFile = new java.io.File(getClass.getResource("/").getPath + "/example.interval")

  val sorttask = sortbam(SortBamInput.empty, 500)

  val addchrtask = addchrtobam(SortBamInput(exampleBamFile), 500)

  val indextask = indexbam(IndexBamInput.empty, 500)

  val secondindextask = indexbam(IndexBamInput.empty, 500)

  val validatetask = validatebam(BamBaiInput.empty, 500)

  val hapmap = new java.io.File(getClass.getResource("/").getPath + "/hapmapchr1.vcf.recode.vcf")

  val bambaiaggregatortask = bambaiaggregator(BamBaiInput.empty)

  val haplotypecallertask = haplotypecaller(HaplotypeCallerInput(gatk, exampleFasta, exampleFastaFai, exampleFastaDict, Set(Region("chr1", 0, 100000)), "examplename", 1, " --min_mapping_quality_score 20 -ERC GVCF -variant_index_type LINEAR -variant_index_parameter 128000 "), memory = 500, cpu = 1)

  val genotypegvcf = genotypeGVCF(
    GenotypeGVCFInput(jar = gatk, referenceFasta = exampleFasta, referenceFai = exampleFastaFai, referenceDict = exampleFastaDict, interval = Set(Region("chr1", 0, 100000)), outName = "examplename", expectedGvcfs = 1, extraArgs = "", dbsnp = hapmap),
    memory = 500,
    cpu = 1
  )

  val combinetask = combinevcf(CombineVCFInput(gatk, exampleFasta, exampleFastaFai, exampleFastaDict, "", 1, "merged.vcf"), memory = 500, cpu = 1)

  val subsettask = subsetvcf(SubsetVCFInput(Set(Region("chr1", 0, 100000))), memory = 500)

  addchrtask ~> sorttask

  sorttask ~> indextask

  indextask ~> validatetask

  sorttask ~> validatetask

  indextask ~> bambaiaggregatortask

  sorttask ~> bambaiaggregatortask

  bambaiaggregatortask ~> haplotypecallertask

  haplotypecallertask ~> subsettask

  subsettask ~> genotypegvcf

  genotypegvcf ~> combinetask

  val sharedsorted = sorttask.?![BamFile].file

  val sharedindex = indextask.?![BamIndexFile].file

  val valid = validatetask.?![ValidateBamOutput].success

  val validreportfile = validatetask.?![ValidateBamOutput].report.file

  val sortedFile = sharedsorted.file
  val sortedName = sharedsorted.name

  val indexFile = sharedindex.file
  val indexName = sharedindex.name

  val hcvcfshared = haplotypecallertask.?![HaplotypeCallerOutput].vcf
  val hcvcf = hcvcfshared.file
  val hcvcfname = hcvcfshared.name

  val combinedshared = combinetask.?![CombineVCFOutput].vcf
  val combined = combinedshared.file
  val combinedname = combinedshared.name

  ts.shutdown

}

class SortBamTestSuite extends FunSuite with Matchers {

  test("unsorted ..-> HaplotypeCallerOutput example + varianteval") {
    TestsSortCall.sortedFile.canRead should be(true)
    TestsSortCall.sortedName should equal("example.chr1:1-100000.bam.addchr.bam.sorted.bam")
    TestsSortCall.indexFile.canRead should be(true)
    TestsSortCall.indexName should equal("example.chr1:1-100000.bam.addchr.bam.sorted.bam.bai")
    TestsSortCall.valid should be(false)
    io.Source.fromFile(TestsSortCall.validreportfile).getLines.filter(_.size > 0).mkString("\n") should equal("|## HISTOGRAM\tjava.lang.String\nError Type\tCount\nERROR:INVALID_ALIGNMENT_START\t8\nERROR:INVALID_FLAG_MATE_UNMAPPED\t6\nERROR:INVALID_INDEX_FILE_POINTER\t1\nERROR:MATE_NOT_FOUND\t8".stripMargin)

    TestsSortCall.hcvcf.canRead should be(true)
    TestsSortCall.hcvcfname should equal("examplename.g.vcf")

    TestsSortCall.combined.canRead should be(true)
    TestsSortCall.combinedname should equal("merged.vcf.combined.vcf")

    // TestVariantEval.evaltaskout.canRead should be(true)

  }

}