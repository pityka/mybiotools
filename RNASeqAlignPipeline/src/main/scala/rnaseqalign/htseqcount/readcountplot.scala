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

package rnaseqalign.htseqcount

import mybiotools._
import mybiotools.gwascommons._
import scala.collection.JavaConversions._
import mybiotools.intervaltree._
import scala.collection.immutable.ListMap
import mybiotools.plots._
import mybiotools.plots.ScatterPlot._
import java.awt.Color

object ReadCountPlot {

  def extractCoordinates(r: Interval): Seq[Int] = r.from until r.to map (x => x)

  def extractSplitCoordinates(rs: Seq[Interval]): Seq[Int] = {
    rs.sortBy(_.from).sliding(2, 1).flatMap { slide =>
      val first = slide.head
      val second = slide.last
      first.to until second.from map (x => x)
    }.toSeq
  }

  def extractCoordinatesFromIterator(
    iterator: Iterator[SAMRecordEssentials],
    strandedness: Strandedness
  ) = {

    val coordinatesAccumulatorFw = scala.collection.mutable.LongMap[Int]()
    val splitCoordinatesAccumulatorFw = scala.collection.mutable.LongMap[Int]()
    val coordinatesAccumulatorBackward = scala.collection.mutable.LongMap[Int]()
    val splitCoordinatesAccumulatorBackward = scala.collection.mutable.LongMap[Int]()

    def increase(i: Long, m: collection.mutable.LongMap[Int]): Unit = {
      m.get(i) match {
        case None => m.update(i, 1)
        case Some(c) => m.update(i, c + 1)
      }
    }

    iterator.foreach { samRecord =>
      val strand =
        if (strandedness == NotStranded) Forward
        else if (strandedness == ReverseStranded) (if (samRecord.negativeStrandFlag) Forward else Reverse)
        else (if (samRecord.negativeStrandFlag) Reverse else Forward)

      val overlapregions = HTSeqCount.parseCigar(samRecord.cigar, samRecord.chromosome,
        samRecord.alignmentStart, strand)

      // exons
      val coordinates: Seq[Int] = overlapregions.sortBy(_.region.from).flatMap(x => extractCoordinates(x.region))

      // introns
      val splitCoordinates: Seq[Int] = extractSplitCoordinates(overlapregions.map(_.region))

      if (strand == Forward) {
        coordinates.foreach { i =>
          increase(i, coordinatesAccumulatorFw)
        }
        splitCoordinates.foreach { i =>
          increase(i, splitCoordinatesAccumulatorFw)
        }
      } else {
        coordinates.foreach { i =>
          increase(i, coordinatesAccumulatorBackward)
        }
        splitCoordinates.foreach { i =>
          increase(i, splitCoordinatesAccumulatorBackward)
        }
      }

    }

    Map(
      Forward -> ((coordinatesAccumulatorFw, splitCoordinatesAccumulatorFw)),
      Reverse -> ((coordinatesAccumulatorBackward, splitCoordinatesAccumulatorBackward))
    )
  }

  def plot(
    iterator: Iterator[SAMRecordEssentials],
    strandedness: Strandedness
  ) = {

    val coordinates = extractCoordinatesFromIterator(iterator, strandedness).toSeq

    val colorCoveredPlus = mybiotools.plots.colorPick(0, 4)
    val colorCoveredNeg = mybiotools.plots.colorPick(1, 4)
    val colorSplitPlus = mybiotools.plots.colorPick(2, 4)
    val colorSplitNeg = mybiotools.plots.colorPick(3, 4)

    val histogramdataAsScatter =
      {

        coordinates.toSeq.flatMap {
          case (strand, (coveredbases, splitbases)) =>

            val coveredData =
              if (coveredbases.isEmpty) seq2(Vector[(Double, Double)]())
              else {
                val minimumX = coveredbases.keys.toSeq.min
                val maximumX = coveredbases.keys.toSeq.max
                val zeros = minimumX to maximumX filterNot (i => coveredbases.contains(i)) map (_.toDouble -> 0.0)
                seq2(coveredbases.toIndexedSeq.map(x => x._1.toDouble -> x._2.toDouble) ++ zeros.toIndexedSeq)
              }

            val splitData =
              if (splitbases.isEmpty)
                seq2(Vector[(Double, Double)]())
              else {
                val minimumX = splitbases.keys.toSeq.min
                val maximumX = splitbases.keys.toSeq.max
                val zeros = minimumX to maximumX filterNot (i => splitbases.contains(i)) map (_.toDouble -> 0.0)
                seq2(splitbases.toIndexedSeq.map(x => x._1.toDouble -> x._2.toDouble) ++ zeros.toIndexedSeq)
              }

            val (label1) = Label(strand.toString, colorCoveredPlus, new java.awt.geom.Ellipse2D.Double(-0.5, -0.5, 1.0, 1.0), new java.awt.BasicStroke(1f), lineGroup = 1)
            val (label2) = Label(strand.toString, colorSplitPlus, new java.awt.geom.Ellipse2D.Double(-0.5, -0.5, 1.0, 1.0), new java.awt.BasicStroke(1f), lineGroup = 0)

            val coveredscatterdataStranded = strand match {
              case Forward => (label1, coveredData)
              case Reverse => (label1.copy(color = Left(colorCoveredNeg)), coveredData.map(x => ScatterPlot.mapY(x)(y => -1 * y)))
            }
            val splitscatterdata2Stranded = strand match {
              case Forward => (label2, splitData)
              case Reverse => (label2.copy(color = Left(colorSplitNeg)), splitData.map(x => ScatterPlot.mapY(x)(y => -1 * y)))
            }

            coveredscatterdataStranded :: splitscatterdata2Stranded :: Nil

        }

      }.toSeq

    ScatterPlot.createScatterPlotFromMultiple(histogramdataAsScatter)

  }

  def plotMultiple(
    iterators: Seq[(String, Iterator[SAMRecordEssentials], Double)],
    strandedness: Strandedness
  ) = {

    val coordinates = iterators.map(x => (x._1, extractCoordinatesFromIterator(x._2, strandedness).toSeq, x._3))

    val histogramdataAsScatter =
      {

        coordinates.zipWithIndex.flatMap {
          case ((name, coordinates, sizeFactor), idx) =>
            coordinates.flatMap {
              case (strand, (coveredbases, splitbases)) =>

                val coveredData =
                  if (coveredbases.isEmpty) seq2(Vector[(Double, Double)]())
                  else {
                    val minimumX = coveredbases.keys.toSeq.min
                    val maximumX = coveredbases.keys.toSeq.max
                    val zeros = minimumX to maximumX filterNot (i => coveredbases.contains(i)) map (_.toDouble -> 0.0)
                    seq2(
                      coveredbases.toIndexedSeq.map(x => x._1.toDouble -> (x._2.toDouble * sizeFactor)) ++
                        zeros.toIndexedSeq
                    )
                  }

                val (label1) = Label(name, mybiotools.plots.colorPick(idx, iterators.size), new java.awt.geom.Ellipse2D.Double(-0.5, -0.5, 1.0, 1.0), new java.awt.BasicStroke(1f), lineGroup = idx)

                val coveredscatterdataStranded = strand match {
                  case Forward => (label1, coveredData)
                  case Reverse => (label1, coveredData.map(x => ScatterPlot.mapY(x)(y => -1 * y)))
                }

                coveredscatterdataStranded :: Nil

            }
        }
      }.toSeq

    ScatterPlot.createScatterPlotFromMultiple(histogramdataAsScatter)

  }

}