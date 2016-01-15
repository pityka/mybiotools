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

object TestsSnap {

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

  val snapindextask = snapIndexGeneration(SnapIndexGenerationInput(exampleFasta, 23, ""), cpu = 1, memory = 100)

  val snapindex = snapindextask.?![SnapIndex].snapindexfiles.map(_._2.file)

  val snapaligntask = snapAlign(SnapAlignInput(List(exampleBamFile), "", "test", None), cpu = 1, memory = 100)

  val haplotypecallertask = haplotypecaller(HaplotypeCallerInput(gatk, exampleFasta, exampleFastaFai, exampleFastaDict, List(Region("chr1", 0, 100000)), "examplename", 1, " --min_mapping_quality_score 20 -ERC GVCF -variant_index_type LINEAR -variant_index_parameter 128000 "), memory = 500, cpu = 1)

  snapindextask ~> snapaligntask ~> haplotypecallertask

  val aligned = snapaligntask.?![BamWithBai]

  val hcout = haplotypecallertask.?![HaplotypeCallerOutput]

  val bam = aligned.bam.file
  val bai = aligned.bai.file
  val vcf = hcout.vcf.file

  ts.shutdown

}

class SnapTestSuite extends FunSuite with Matchers {

  test("unsorted ..-> HaplotypeCallerOutput example + varianteval") {
    TestsSnap.snapindex.foreach { f =>
      f.canRead should be(true)
      println(f.getAbsolutePath)
    }
    TestsSnap.bam.canRead should be(true)
    TestsSnap.bai.canRead should be(true)
    TestsSnap.vcf.canRead should be(true)
    println(TestsSnap.bam)
    println(TestsSnap.vcf)

    // TestVariantEval.evaltaskout.canRead should be(true)

  }

}