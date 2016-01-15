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

import mybiotools._

import genotyper.tasks._
import mybiotools.gwascommons._
import java.io.File
import mybiotools._
import htsjdk.samtools.{ SAMFileReader, SAMFileWriterFactory, SAMSequenceRecord, SAMSequenceDictionary, SAMFileHeader, SAMRecord, SAMTextHeaderCodec }
import scala.collection.JavaConversions._
import scala.util.Try
import mybiotools.stringstore._
import java.nio.{ ByteBuffer, ByteOrder }
import htsjdk.samtools.cram.ref.ReferenceSource
import htsjdk.samtools._
import mybiotools.tasks._
import java.io.File
import mybiotools._
import mybiotools.gwascommons._
import genotyper._
import collection.JavaConversions._
import htsjdk.tribble.index.IndexFactory
import htsjdk.tribble.index.IndexFactory.IndexBalanceApproach._
import vcfhelpers._
import mybiotools.stringstore._
import genotyper._
import mybiotools.eq._

object VCFDensity extends App {

  val vcf1 = new File(args(0))
  val vcf2 = new File(args(1))
  val bed = new File(args(2))
  val w = args(3).toInt
  val minaf = args(4).toDouble
  val maxaf = args(5).toDouble

  val intervals: List[Region] =
    Region.collapse[String8, GenomicLocation, Region](
      io.Source.fromFile(bed).getLines.dropWhile { l =>
      val spl = mybiotools.fastSplitSeparator(l, '\t')
      spl.size < 2 || Try(spl(1).toInt).toOption.isEmpty
    }.map(x => Region.fromLine(x)).toList,
      0
    ).toList

  val index1 = IndexFactory.createDynamicIndex(vcf1, new QuickTribbleVCFCodec, FOR_SEEK_TIME)
  val index2 = IndexFactory.createDynamicIndex(vcf2, new QuickTribbleVCFCodec, FOR_SEEK_TIME)

  val reader1 = htsjdk.tribble.AbstractFeatureReader.getFeatureReader(
    vcf1.getAbsolutePath,
    new QuickTribbleVCFCodec,
    index1
  )

  val reader2 = htsjdk.tribble.AbstractFeatureReader.getFeatureReader(
    vcf2.getAbsolutePath,
    new QuickTribbleVCFCodec,
    index2
  )

  val d1 = Helpers.slidingVariantDensity(reader1, intervals.toSet, w, minaf, maxaf)
  val d2 = Helpers.slidingVariantDensity(reader2, intervals.toSet, w, minaf, maxaf)

  println(vcf1 + " has " + d1.map(_._2).sum + s" variants having PASS or . filter column and AF in ($minaf,$maxaf)")
  println(vcf2 + " has " + d2.map(_._2).sum + s" variants having PASS or . filter column and AF in ($minaf,$maxaf)")

  println(plots.pngToFile(Helpers.plotDensity(d1, d2)))

  implicit val order = regionOrderingByStart[String8, GenomicLocation, Region]

  val diff = (iterableAsScalaIterable(reader2.iterator).filter(_.chr === "1").filter { line =>
    val spl = line.line.fastSplit('\t')

    val af = {
      spl(7).fastSplitIterator(';').find(_.startsWith("AF=")).map(_.drop(3).fastSplitIterator(',').next.toDouble).getOrElse(0.0)
    }
    af >= minaf && af <= maxaf
  }.map(x => (x.chr, x.start, x.end)).toSet &~ iterableAsScalaIterable(reader1.iterator).filter(_.chr === "1").map(x => (x.chr, x.start, x.end)).toSet).map(x => Region(x._1, x._2, x._3)).toSeq.sorted
  println(diff.take(1000))

}