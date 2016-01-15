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

package rnaseqalign

import com.typesafe.config.{ Config, ConfigFactory }
import mybiotools.tasks._
import rnaseqalign.tasks._
import java.io.File
import mybiotools.config.Config.configInstance
import scala.collection.JavaConversions._
import org.saddle.Frame
import scala.concurrent.{ Future, Await }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import Helpers.htseqCountFromBam
import htseqcount._
import mybiotools.stringstore._

object HTSeqCountApp extends App {

  new HTSeqCountAppTrunk(DefaultHTSeqCountConfig).run
}

trait HTSeqCountConfig {
  def bams: List[File]
  def gtf: File
  def strandedness: Strandedness
  def minQual: Int
  def exon: String8
  def gene_id: String8
  def transcript_id: String8
  def resolveLongestTranscript: Boolean
  def allowMultiMapInSameGene: Boolean
}

object DefaultHTSeqCountConfig extends HTSeqCountConfig {
  val bams: List[File] = {
    val p = configInstance.getString("htseqcount.bam")
    if (p == "-") Nil else List(new File(p))
  } ++ {
    val p = configInstance.getString("htseqcount.bams")
    if (p != "") mybiotools.openSource(p)(_.getLines.map(x => new File(x)).toList) else Nil
  }
  val gtf: File = new File(configInstance.getString("htseqcount.gtf"))
  val strandedness: Strandedness = configInstance.getString("htseqcount.strandedness") match {
    case x if x.toUpperCase == "STRANDED" => Stranded
    case x if x.toUpperCase == "NOTSTRANDED" => NotStranded
    case x if x.toUpperCase == "REVERSESTRANDED" => ReverseStranded
  }
  val minQual = configInstance.getInt("htseqcount.minQual")
  val exon = StringStore(configInstance.getString("htseqcount.exonFeatureName"))
  val gene_id = StringStore(configInstance.getString("htseqcount.gene_idAttribute"))
  val transcript_id = StringStore(configInstance.getString("htseqcount.transcript_idAttribute"))
  val resolveLongestTranscript = configInstance.getBoolean("htseqcount.countLongestTranscript")
  val allowMultiMapInSameGene = configInstance.getBoolean("htseqcount.allowMultiMapInSameGene")
}

class HTSeqCountAppTrunk(conf: HTSeqCountConfig) {
  import conf._

  def run: Unit = {

    val iss = bams.size match {
      case 0 => List("std" -> System.in)
      case 1 => List(bams.head.getAbsolutePath -> new java.io.BufferedInputStream(new java.io.FileInputStream(bams.head)))
      case _ => bams.map(x => x.getAbsolutePath -> new java.io.BufferedInputStream(new java.io.FileInputStream(x)))
    }

    val rawlist = mybiotools.openSource(gtf.getAbsolutePath) { s => HTSeqCount.readGTF(s, gene_id, transcript_id)(_ == exon) }

    val (list, lengths) = HTSeqCount.getLengths(
      rawlist = rawlist,
      Transcript_id = transcript_id,
      Gene_id = gene_id,
      Exon = exon,
      resolveLongestTranscript = resolveLongestTranscript
    )

    val trees = HTSeqCount.createIntervalTree(list)

    if (iss.size == 1) {
      val map = Helpers.htseqCountFromBam(
        in = iss.head._2,
        intervaltrees = trees,
        strandedness = strandedness,
        minQual = minQual,
        allowMultiMapInSameGene = allowMultiMapInSameGene
      )

      iss.head._2.close

      map.map {
        case (gene, count) =>
          println(gene + "\t" + count)
      }
    } else {
      iss.foreach { is =>
        val map = Helpers.htseqCountFromBam(
          in = is._2,
          intervaltrees = trees,
          strandedness = strandedness,
          minQual = minQual,
          allowMultiMapInSameGene = allowMultiMapInSameGene
        )

        is._2.close

        mybiotools.openFileWriter(new File(is._1 + ".counts.tsv")) { writer =>
          map.map {
            case (gene, count) =>
              writer.write(gene + "\t" + count + "\n")
          }
        }

      }

    }

    val tmpfileLengths = new File("genelengths")
    mybiotools.openFileWriter(tmpfileLengths) { writer =>
      lengths.foreach {
        case (gene, count) =>
          writer.write(gene + "\t" + count + "\n")
      }
    }
  }
}
