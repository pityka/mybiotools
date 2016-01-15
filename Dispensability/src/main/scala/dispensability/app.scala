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

import mybiotools._
import mybiotools.config.Config
import java.io.File
import scala.collection.JavaConversions._
import org.saddle._
import mybiotools.stringstore._
import java.awt.Color

trait DispensabilityScoringConfiguration[AT <: Annotation[AT]] extends FilterVCFTNutvarConfig {

  def compactAprioris: Boolean
  def maxAffected: Double
  def minAffected: Double
  def downSamplingReplicaCount: Int
  def minimumGeneSetSize: Int
  def vcfs: List[File]
  def hweThreshold: Double
  def nutvars: List[File]
  def outputPrefix: File

  def externalGeneAnnotations: Map[String, File]

  def gmtFiles: List[File]
  def removeFromGMT: File
  def samochaTable: File
  def pthreshold: Double
  def analize_apriorisets: Boolean
  def snpeff: List[File]
  def annotationFormat: AnnotationFormat
  def doDownSampling: Boolean
  def antonioOMIMFile: File

  implicit def score: AnnotationScore[AT]

}

class DispensabilityScoringConfigurationFromConfig[AT <: Annotation[AT]](configInstance: com.typesafe.config.Config, val annotationFormat: AnnotationFormat, val score: AnnotationScore[AT]) extends DispensabilityScoringConfiguration[AT] {

  val compactAprioris = configInstance.getBoolean("nutvardispensability.compactAprioris")

  val externalGeneAnnotations = configInstance.getStringList("nutvardispensability.external").grouped(2).map(x => x(0) -> new File(x(1))).toList.toMap

  val maxAffected = configInstance.getDouble("nutvardispensability.maxAffected")
  val minAffected = configInstance.getDouble("nutvardispensability.minAffected")

  val samochaTable = new File(configInstance.getString("nutvardispensability.samochaTable"))

  val downSamplingReplicaCount = configInstance.getInt("nutvardispensability.downSamplingReplicaCount")

  val minimumGeneSetSize = configInstance.getInt("nutvardispensability.minimumGeneSetSize")

  val analize_apriorisets = configInstance.getBoolean("nutvardispensability.aprioriSetAnalysis")

  val pthreshold = configInstance.getDouble("nutvardispensability.pthreshold")

  val hweThreshold = configInstance.getDouble("nutvardispensability.hwethreshold")

  val doDownSampling = configInstance.getBoolean("nutvardispensability.performDownSampling")

  val antonioOMIMFile = new File(configInstance.getString("nutvardispensability.antonioomim"))

  val removeFromGMT = new File(configInstance.getString("nutvardispensability.removeFromGMT"))

  val vcfs: List[File] = {
    val vcf = {
      val p = configInstance.getString("nutvardispensability.vcf")
      if (p != "") Some(new File(p)) else None
    }
    val vcfs = configInstance.getString("nutvardispensability.vcfs")
    if (vcfs != "") vcf.toList ::: openSource(vcfs)(_.getLines.toList.map(f => new File(f))) else vcf.toList
  }
  val nutvars: List[File] = {
    val file = {
      val p = configInstance.getString("nutvardispensability.nutvar")
      if (p != "") Some(new File(p)) else None
    }
    val files = configInstance.getString("nutvardispensability.nutvars")
    if (files != "") file.toList ::: openSource(files)(_.getLines.toList.map(f => new File(f))) else file.toList
  }

  val snpeff: List[File] = {
    val file = {
      val p = configInstance.getString("nutvardispensability.snpeff")
      if (p != "") Some(new File(p)) else None
    }
    val files = configInstance.getString("nutvardispensability.snpeffs")
    if (files != "") file.toList ::: openSource(files)(_.getLines.toList.map(f => new File(f))) else file.toList
  }
  val gmtFiles: List[File] = {
    val file = {
      val p = configInstance.getString("nutvardispensability.gmtfile")
      if (p != "") Some(new File(p)) else None
    }
    val files = configInstance.getStringList("nutvardispensability.gmtfiles").toList.map(f => new File(f))
    file.toList ::: files
  }
  val outputPrefix = new File(configInstance.getString("nutvardispensability.out"))

}
