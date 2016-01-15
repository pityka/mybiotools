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
import scala.concurrent.ExecutionContext.Implicits.global

object TestVariantEval {

  val ts = defaultTaskSystem("""
    tasks.cacheEnabled=false
    tasks.logFile="" 
    tasks.fileServiceBaseFolder = "./target/"
    """)
  import ts._

  val exampleFasta = new java.io.File(getClass.getResource("/").getPath + "/chunk.hg19.fasta")

  val exampleFastaFai = new java.io.File(getClass.getResource("/").getPath + "/chunk.hg19.fasta.fai")

  val exampleFastaDict = new java.io.File(getClass.getResource("/").getPath + "/chunk.hg19.dict")

  val gatk = new java.io.File(getClass.getResource("/").getPath + "/GenomeAnalysisTK.jar")

  val uncalibrated = new java.io.File(getClass.getResource("/").getPath + "/uncalibrated.vcf")

  val hapmap = new java.io.File(getClass.getResource("/").getPath + "/hapmapchr1.vcf.recode.vcf")

  val evaltask = varianteval(VariantEvalInput(gatk, exampleFasta, exampleFastaFai, exampleFastaDict, "", hapmap), memory = 500, cpu = 1)

  evaltask <~ VCFFile(SharedFile(uncalibrated))

  val evaltaskout = evaltask.?![VariantEvalOutput].report.file

  ts.shutdown

}

