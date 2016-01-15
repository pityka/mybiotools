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

package settest

import mybiotools.gwascommons._
import mybiotools._
import mybiotools.stringstore._

sealed trait TrackStrategy { self =>
  def regions: Map[RegionSetName, GenericRegionSet]

  def +(that: TrackStrategy): TrackStrategy = new TrackStrategy {
    val regions = mybiotools.addMaps(self.regions, that.regions)(_ + _)
  }

  def remove(mask: GenericRegionSet) = if (mask == EmptyRegionSet) this else new TrackStrategy {
    val regions = self.regions.map { case (pwn, rs) => pwn -> (rs - mask) }
  }

  def intersect(mask: GenericRegionSet) = if (mask == EverythingRegionSet) this else new TrackStrategy {
    val regions = self.regions.map { case (pwn, rs) => pwn -> (rs - (rs - mask)) }
  }
}

case class NatarajansExtendedGene(
    associations: io.Source,
    bed: io.Source,
    geneIDs: io.Source,
    includeCoding: Boolean
) extends TrackStrategy {
  val regions: Map[RegionSetName, GenericRegionSet] = readNatarajansFiles(associations, bed, geneIDs).map {
    case (gene, list) =>
      val listExtended = if (includeCoding) {
        list ++ HumanGeneMapGRCh37p10.get(gene).toList
      } else list
      RegionSetName(gene.value) -> GenericRegionSet(listExtended)
  }
}

/**
 * Define sets based on exact chr:bp positions.
 *
 * File format is: NAME chr1 bp1 chr2 bp2 chr1 bp3 . 1 line per set
 */
case class LocationList(locations: io.Source) extends TrackStrategy {
  val regions = locations.getLines.map { x =>
    val (pw, list) = pathWayWithLocationFromLine(x)
    pw -> RegionSet.fromGenomicLocations(list)
  }.toMap
}

/**
 * Define sets based on exact chr:bp positions.
 *
 * File format is: NAME chr1 bp1 and repeat. Multiple lines per NAME.
 */
case class LocationListMultiLine(locations: io.Source) extends TrackStrategy {
  import mybiotools.gwascommons.GenomicLocation
  val regions: Map[RegionSetName, GenericRegionSet] = locations.getLines.map { line =>
    val spl = mybiotools.fastSplitSetSeparator(line, Set(' ', '\t'))
    RegionSetName(StringStore(spl(0))) -> GenomicLocation(spl(2).toDouble.toInt, spl(1))
  }.toSeq.groupBy(_._1).map(x => x._1 -> RegionSet.fromGenomicLocations(x._2.map(_._2)))
}

/**
 * Define sets on a GMT file.
 *
 * The names in the gmt file refer to HumanGeneMapGRCh37p10
 */
case class UseGMT(gmt: io.Source, flankingBefore: Int, flankingAfter: Int) extends TrackStrategy {

  val regions = regionSetsFromGMT(readGMT(gmt), HumanGeneMapGRCh37p10.mapValues(region => region.extendBefore(flankingBefore).extendAfter(flankingAfter)).toMap)

}

/**
 * Define sets based on HumanGeneMapGRCh37p10
 *
 * HumanGeneMapGRCh37p10 holds the contents of a bed file on the classpath.
 * The bed file is downloaded from ensemble "human.genes.GRCh37.p10.bed"
 */
case class UseHumanGenes(flankingBefore: Int, flankingAfter: Int) extends TrackStrategy {
  val regions = HumanGeneMapGRCh37p10.map {
    case (name, region) => RegionSetName(name.value) -> GenericRegionSet(List(region.extendBefore(flankingBefore).extendAfter(flankingAfter)))
  }
}

case object UseHumanGenes extends TrackStrategy {
  val regions = HumanGeneMapGRCh37p10.map {
    case (name, region) => RegionSetName(name.value) -> GenericRegionSet(List(region))
  }
}

/** Define sets based on bed file. */
case class UseBed(source: scala.io.Source, flankingBefore: Int, flankingAfter: Int) extends TrackStrategy {
  val regions = readBedWithNames(source).map {
    case (name, region) => RegionSetName(name) -> GenericRegionSet(List(region.extendBefore(flankingBefore).extendAfter(flankingAfter)))
  } toMap
}

/** Define sets based on bed file. */
case class UseBedMergeNames(source: scala.io.Source, flankingBefore: Int, flankingAfter: Int) extends TrackStrategy {
  val regions = (readBedWithNames(source).map {
    case (name, region) => RegionSetName(name) -> region.extendBefore(flankingBefore).extendAfter(flankingAfter)
  }).toList.groupBy(_._1).map(x => x._1 -> GenericRegionSet(x._2.map(_._2)))
}

case class SlidingWindowBed(source: scala.io.Source, flankingBefore: Int, flankingAfter: Int, windowSize: Int, step: Int) extends TrackStrategy {
  val regions = readBedWithNames(source).map {
    case (name, region) => region.extendBefore(flankingBefore).extendAfter(flankingAfter)
  }.toSeq.groupBy(_.chromosome).flatMap {
    case (chr, bedsonchr) =>
      bedsonchr.sortBy(_.from).sliding(windowSize, step).map { list =>
        RegionSetName(s8"chr$chr:${list.head.from}-${list.last.to}") -> GenericRegionSet(list)
      }
  }

}

case class SlidingWindowMapDistance(windowSize: Int, overlap: Int) extends TrackStrategy {
  val regions = mybiotools.gwascommons.ChromosomeLengths.map {
    case (chr, maxLength) =>
      (1 to (((maxLength - windowSize) / overlap) + 1)).map { idx =>
        val start = (idx - 1) * overlap
        val end = math.min(start + windowSize, maxLength)
        RegionSetName(s8"$chr:$start-$end") -> GenericRegionSet(List(Region(StringStore(chr.toString), start, end)))
      }
  }.flatten.toMap
}

/**
 * Sets based on Thurman et al files
 *
 * If includeCoding is false, then define the sets solely based on the contents of thurmansFile
 * If includeCoding is true, then takes the human genes HumanGeneMapGRCh37p10 and augments them
 * with the "regulatory" regions in thurmansFile.
 * Genes in HumanGeneMapGRCh37p10 but not in the thurmansFile are included without modification.
 * Regions in thurmansFile but not in HumanGeneMapGRCh37p10 are also included without modification.
 */
case class ThurmanGeneRegulators(thurmansFile: io.Source, includeCoding: Boolean) extends TrackStrategy {

  val regions = {
    val withoutcoding = readThurmanFile(thurmansFile.getLines).groupBy(_._1).map(x => x._1 -> x._2.map(_._2))
    if (!includeCoding) withoutcoding.map {
      case (name, regions) => RegionSetName(name.value) -> GenericRegionSet(regions)
    }
    else {
      val regulatorsWithCoding = withoutcoding.map {
        case (name, regions) =>
          RegionSetName(name.value) -> GenericRegionSet(regions ++ HumanGeneMapGRCh37p10.get(name).toList)
      }
      val coding = UseHumanGenes.regions
      mybiotools.addMaps(regulatorsWithCoding, coding)((reg, cod) => reg)
    }
  }
}

case class ThurmanDHSBasedPathways(thurmansFile: io.Source) extends TrackStrategy {

  val regions = readThurmanFile(thurmansFile.getLines).groupBy(_._2).map(_._2.map(_._1)).zipWithIndex.map(x => RegionSetName(StringStore(x._2.toString)) -> GenericRegionSet(x._1.map(z => HumanGeneMapGRCh37p10.get(z)).filter(_.isDefined).map(_.get))).toMap
}

case class ThurmanSharedRegulatorsByGMT(thurmansFile: io.Source, gmt: io.Source, minimumShare: Int, includeCoding: Boolean) extends TrackStrategy {

  val regions = {

    val rawthurman = readThurmanFile(thurmansFile.getLines)

    val regulatorsByGene: Map[RegionName, Vector[Region]] = rawthurman.groupBy(_._1).map(x => x._1 -> x._2.map(_._2))

    val pathways: Map[RegionSetName, IndexedSeq[RegionName]] = readGMT(gmt).map(x => RegionSetName(x._1) -> x._2.map(y => RegionName(y)).toIndexedSeq)

    val regulatorsByPathways: Map[RegionSetName, IndexedSeq[Set[Region]]] = pathways.map {
      case (pw, genes) =>
        pw -> genes.map(g => regulatorsByGene.get(g)).filter(_.isDefined).map(_.get.toSet)
    }

    val sharedRegionsByPathway: Map[RegionSetName, Vector[Region]] = regulatorsByPathways.map { x =>
      x._1 -> mybiotools.selectSharedElements(x._2, minimumShare)
    }

    if (!includeCoding) {
      sharedRegionsByPathway.map(x => x._1 -> GenericRegionSet(x._2))
    } else {

      val genesByRegulators: Map[Region, Vector[RegionName]] = rawthurman.groupBy(_._2).map(x => x._1 -> x._2.map(_._1))

      sharedRegionsByPathway.map {
        case (pw, regulators) =>
          val codingAndRegulatorRegionsInPathway = regulators.map(regulator => genesByRegulators(regulator).map(gene => HumanGeneMapGRCh37p10.get(gene).toVector :+ regulator)).reduce(_ ++ _).flatten.distinct
          pw -> GenericRegionSet(codingAndRegulatorRegionsInPathway)
      }

    }
  }

}

