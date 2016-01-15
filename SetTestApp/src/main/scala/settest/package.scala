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

import mybiotools.gwascommons._
import mybiotools.stringstore._
import mybiotools.eq._
import mybiotools._
import hdfdosage.FileSets._

package object settest {

  def buildPathWays[T <: HasGenomicLocation with HasName](
    loci: Vector[T],
    regionsets: Map[RegionSetName, GenericRegionSet]
  ): Map[RegionSetName, Vector[T]] = {

    val lociGroupedAndSorted: Map[String8, Vector[T]] = loci.groupBy(_.genomicLocation.chromosomeAsT).map(x => x._1 -> x._2.sortBy(_.genomicLocation).toVector)

    regionsets.map {
      case (pw, region) =>
        pw -> {
          region.maskTo(lociGroupedAndSorted).values.reduce(_ ++ _)
        }
    }.filter(_._2.size > 0).toMap

  }

  lazy val HumanGeneMapGRCh37p10: Map[RegionName, Region] = using(scala.io.Source.fromURL(getClass.getResource("/human.genes.GRCh37.p10.bed"))) { source => readBedWithNames(source).map(x => RegionName(x._1) -> x._2).toMap }

  def regionSetsFromGMT(
    gmt: Map[String8, Seq[String8]],
    regions: Map[RegionName, Region]
  ): Map[RegionSetName, GenericRegionSet] = gmt.toList.par.map { names =>
    RegionSetName(names._1) -> RegionSet[String8, GenomicLocation, Region](regions.filterKeys(x => names._2.has(x.value)).values)
  }.seq.toMap

  def readThurmanFile(lines: Iterator[String]): Vector[(RegionName, Region)] = lines.map { line =>
    val splitted = fastSplitSeparator(line, '\t')
    val name = RegionName(StringStore(splitted(3)))
    val chr = splitted(4)
    val start = splitted(5).toInt
    val end = splitted(6).toInt
    val region = Region(chr, start, end)
    (name, region)
  }.toVector

  def readNatarajansFiles(
    associations: io.Source,
    bed: io.Source,
    geneIDs: io.Source
  ): Map[RegionName, Vector[Region]] = {
    val ids: Map[Int, RegionName] = geneIDs.getLines.drop(1).map { line =>
      val spl = fastSplitSeparator(line, '\t')
      spl.head.toInt -> RegionName(StringStore(spl.last))
    }.toMap
    val regions: Map[Int, Region] = bed.getLines.zipWithIndex.map {
      case (line, idx) =>
        val spl = fastSplitSeparator(line, '\t')
        val chr = spl(0)
        val start = spl(1).toInt
        val end = spl(2).toInt
        val region = Region(StringStore(chr), start, end)
        (idx + 1) -> region

    }.toMap

    associations.getLines.map { line =>
      val spl = fastSplitSeparator(line, '\t')
      val genename = ids(spl(0).toInt)

      val regulators = fastSplitSeparator(spl(2).drop(1), ';').map(x => regions(x.toInt)).toVector
      genename -> regulators
    }.toMap
  }

  def pathWayWithLocationToLine(pw: RegionSetName, loci: Seq[GenomicLocation]): String =
    (pw.value +: loci.map(x => x.chromosome + " " + x.basePairPosition)).mkString(" ")

  def pathWayWithLocationFromLine(s: String): (RegionSetName, List[GenomicLocation]) = {
    val spl = fastSplitSeparatorIterator(s, ' ')
    val pw = RegionSetName(StringStore(spl.next))
    val loci = spl.grouped(2).map(x => GenomicLocation(x(1).toInt, x(0))).toList
    (pw, loci)
  }

}