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

package genotyper

import genotyper.tasks._
import mybiotools.config.Config.configInstance
import mybiotools.tasks._
import mybiotools._
import mybiotools.gwascommons._
import com.typesafe.config._
import scala.util.Try
import java.io.File
import scala.collection.JavaConversions._
import scala.concurrent.{ Future, Await }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object VQSRApp extends App {

  val ts = defaultTaskSystem
  import ts._
  implicit val fs = components.fs
  implicit val actorsystem = components.actorsystem
  if (ts.hostConfig.myRole == MASTER) {
    val log = ts.getLogger(this)

    ts.registerFileLogger(new File(configInstance.getString("tasks.fileServiceBaseFolder") + "/logfile"))

    log.info("Badge:\n" + (mybiotools.config.Config.prettyPrintVersion("VQSRApp", genotyper.Reflected.version)))

    val gatkjar = new File(configInstance.getString("genotyper.gatkjar"))

    val referenceFasta = new File(configInstance.getString("genotyper.referenceFasta"))
    val referenceFai = new File(configInstance.getString("genotyper.referenceFai"))
    val referenceDict = new File(configInstance.getString("genotyper.referenceDict"))

    val resourcesConfigsForSNPRecalibration = configInstance.getConfigList("genotyper.vqsr.snpresources").map(c => VQSRResource.fromConfig(c))

    val resourcesConfigsForIndelRecalibration = configInstance.getConfigList("genotyper.vqsr.indelresources").map(c => VQSRResource.fromConfig(c))

    val dbsnpvcfBefore1kg = new File(configInstance.getString("genotyper.dbsnpvcfBefore1kg"))

    val minBadVariantsIndel = configInstance.getInt("genotyper.vqsr.minBadVariantsIndel")
    val minBadVariantsSNP = configInstance.getInt("genotyper.vqsr.minBadVariantsSNP")

    val maxGaussiansSNP = configInstance.getInt("genotyper.vqsr.maxGaussiansSNP")
    val maxGaussiansIndel = configInstance.getInt("genotyper.vqsr.maxGaussiansIndel")

    val evaluatememory = configInstance.getInt("genotyper.eval.RAM")
    val evaluatecpu = configInstance.getInt("genotyper.eval.CPU")

    val vqsrmemory = configInstance.getInt("genotyper.vqsr.RAM")
    val vqsrcpu = configInstance.getInt("genotyper.vqsr.CPU")

    val evalExtraArgs = configInstance.getString("genotyper.eval.extraArgs")

    val vcfinput = SharedFile(new File(configInstance.getString("genotyper.vqsr.vcfinput")))

    val evaluation: Future[VariantEvalOutput] = {

      val vqsrsnptask = vqsr(VQSRInput(gatkjar, referenceFasta, referenceFai, referenceDict, s" --maxGaussians $maxGaussiansSNP ", "SNP", resourcesConfigsForSNPRecalibration.toList, minBadVariantsSNP), memory = vqsrmemory, cpu = vqsrcpu)

      val vqsrindeltask = vqsr(VQSRInput(gatkjar, referenceFasta, referenceFai, referenceDict, s" --maxGaussians $maxGaussiansIndel ", "INDEL", resourcesConfigsForIndelRecalibration.toList, minBadVariantsIndel), memory = vqsrmemory, cpu = vqsrcpu)

      val evaltask = varianteval(VariantEvalInput(gatkjar, referenceFasta, referenceFai, referenceDict, evalExtraArgs, dbsnpvcfBefore1kg), memory = evaluatememory, cpu = evaluatecpu)

      vqsrsnptask <~ VCFFile(vcfinput)

      vqsrsnptask ~> vqsrindeltask ~> evaltask

      evaltask.?[VariantEvalOutput]

    }

    Await.result(evaluation, atMost = 168 hours)

    ts.shutdown

  }

}