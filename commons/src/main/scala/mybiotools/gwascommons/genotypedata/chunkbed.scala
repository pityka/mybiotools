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

package mybiotools.gwascommons.genotypedata

import java.io.File
import mybiotools.stringstore._
import mybiotools.gwascommons._
import mybiotools._

object ChunkBed {
  // ____chunkstart|*********************************|chunkend______
  // ____startoverlap|*****|start|*******|end|*******|endoverlap____
  // these are coordinates
  case class BedChunk(
    chr: String,
    start: Int,
    end: Int,
    startOverlap: Int,
    endOverlap: Int
  )

  def chunkUpBedFile(
    bed: File,
    bim: File,
    fam: File,
    outFolder: File,
    chunkSize: Int,
    overlap: Int
  ): Set[Tuple2[BedChunk, String]] = {
    import mybiotools.gwascommons.getGenomicMapFromBimFile

    case class ChunkTMP(start: (String8, GenomicLocation), end: (String8, GenomicLocation), startoverlap: (String8, GenomicLocation), endoverlap: (String8, GenomicLocation))

    val genomicMapBySNPName: GenomicMap = getGenomicMapFromBimFile(bim.getCanonicalPath)

    val genomicMap: Map[String, Seq[(String8, GenomicLocation)]] = genomicMapBySNPName.toSeq.groupBy(_._2.chromosome)

    genomicMap.keys.toSeq.sorted.par.flatMap { chr =>

      val centromere = Centromeres(chromosomeNumberFromString(chr))

      val sortedchromosome: Vector[(String8, GenomicLocation)] = genomicMap(chr).sortBy(_._2.basePairPosition).toVector

      val (firsthalf, secondhalf) = sortedchromosome.span(_._2.basePairPosition <= centromere)

      def chunkChromosome(vec: Vector[(String8, GenomicLocation)]): Vector[ChunkTMP] = if (vec.size == 0) Vector() else {
        val chunks = (0 until vec.size by chunkSize).map { (startidx: Int) =>
          val start = vec(startidx)
          val startOverlap = vec(List(startidx - overlap, 0).max)

          val endOverlap = vec(List(startidx + overlap + chunkSize - 1, vec.size - 1).min)

          val end = vec(List(startidx + chunkSize - 1, vec.size - 1).min)

          ChunkTMP(start, end, startOverlap, endOverlap)

        }.toVector

        // merge the last into the penultimate
        if (chunks.size >= 2) {
          val last = chunks.last
          val penultimate = chunks.dropRight(1).last
          val merged = ChunkTMP(penultimate.start, last.end, penultimate.startoverlap, last.endoverlap)
          chunks.dropRight(2) :+ merged
        } else chunks
      }

      (chunkChromosome(firsthalf) ++ chunkChromosome(secondhalf)).map {
        case ChunkTMP(start, end, startoverlap, endoverlap) =>

          val out = new File(outFolder, "chr" + chr + "_from" + start._2.basePairPosition + "_to" + end._2.basePairPosition).getCanonicalPath

          val command = "plink --noweb --bed " + bed.getCanonicalPath + " --bim " + bim.getCanonicalPath + " --fam " + fam.getCanonicalPath + " --make-bed " + " --from " + startoverlap._1.value + " --to " + endoverlap._1.value + " --out " + out

          val (stdout, stderr, finished) = execGetStreamsAndCode(command)

          if (finished) {

            (BedChunk(

              chr = chr,
              start = start._2.basePairPosition,
              end = end._2.basePairPosition,
              startOverlap = startoverlap._2.basePairPosition,
              endOverlap = endoverlap._2.basePairPosition
            ), out)

          } else throw new RuntimeException("error in plink chunk" + stdout.mkString("\n" + stderr.mkString("\n")))

      }
    }.seq.toSet
  }
}