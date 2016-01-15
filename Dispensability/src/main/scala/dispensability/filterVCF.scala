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

package dispensability

import mybiotools.config.Config.configInstance
import mybiotools._
import java.io.File

trait FilterVCFTNutvarConfig {
  def vcfs: List[File]
  def nutvars: List[File]
  def outputPrefix: File
  def snpeff: List[File]
}

class FilterVCFToNutvar(config: FilterVCFTNutvarConfig) {
  import config._

  def run: Unit = {

    val nutvarindex = {
      val fromnutvar = nutvars.map(x => openSource(x)(s => Input.readNutVarOutputKeys(s))).foldLeft(Set[VariantKey]())(_ ++ _)
      val fromsnpeff = snpeff.map(x => openSource(x)(s => Input.readVariantKeysFromSnpEffAnnotationFile(s))).foldLeft(Set[VariantKey]())(_ ++ _)
      fromnutvar ++ fromsnpeff
    }

    println("read nutvarindex")

    vcfs.par.foreach { vcf =>
      val outputfile = new File(outputPrefix.getAbsolutePath + vcf.getName + ".nutvarfiltered.vcf.gz")
      openZippedFileOutputStream(outputfile) { os =>
        openSource(vcf.getAbsolutePath) { source =>
          println(vcf.getAbsolutePath)
          Input.filterVCF(source, os, nutvarindex)

        }
      }

    }

  }
}

object FilterVCFToNutvarApp extends App {

  val annotationFormat = configInstance.getString("nutvardispensability.annotationFormat").toLowerCase match {
    case "nutvar" => NutVar
    case "snpeff" => SnpEffVCF
  }

  val conf = new DispensabilityScoringConfigurationFromConfig[SnpEffAnnotation](configInstance, annotationFormat, NutVarAnnotationScores.SnpEffAnnotationScore)

  new FilterVCFToNutvar(conf).run

}

