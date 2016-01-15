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

package hdfdosage

import FileSets._
import mybiotools.gwascommons._
import mybiotools.gwascommons.genotypedata._
import mybiotools._
import mybiotools.stringstore._
import java.io.File
import org.saddle._

object Recode {
  def run(
    genotypefile: GenotypeFileSet,
    outPath: File,
    ldprune: Boolean,
    prunethreshold: Double,
    prunewindow: Int,
    pruneKeepSNPs: Set[String],
    pruneAwaySNPs: Set[String],
    genomicMap: GenomicMap,
    snpIncludeFilter: Option[Set[String]],
    outputFormat: DosageFileFormat,
    recodeToRecessive: Boolean,
    blockSize: Int,
    minimumMAF: Double,
    maximumMAF: Double,
    grmBatchSize: Int,
    grmThreads: Int,
    maximumMissingnessRate: Double,
    includeIndividuals: Option[Set[Individual]]
  ): (Long, Option[GenotypeFileSet]) = {

    if (ldprune && prunethreshold < 1.0) {
      assert(genomicMap.size > 0, "Genomic Map is required for ld pruning. Additionally, the file needs to be sorted.")
    }

    val pruneAwayDosages: Seq[Series[Individual, Float]] = if (!pruneAwaySNPs.isEmpty && ldprune && prunethreshold < 1.0) {
      FileSets.openFileSet(genotypefile, 0.0, Full, pruneAwaySNPs) { snpmajoriterator =>
        snpmajoriterator.snpIterator.map(x => Series(snpmajoriterator.individuals zip x._2: _*)).toList
      }
    } else Nil

    var counter = 0L

    openFileWriter(new File(outPath + ".snps")) { writer =>

      FileSets.openFileSet(genotypefile, minimumMAF, Full, Set()) { snpmajoriterator =>

        val filtered = {
          val snpfiltered = {
            val pruned = if (ldprune && prunethreshold < 1.0) snpmajoriterator.prune(prunethreshold, prunewindow, pruneKeepSNPs.map(x => StringStore(x)), pruneAwayDosages, genomicMap) else snpmajoriterator
            if (snpIncludeFilter.isEmpty) pruned else pruned.filter(x => snpIncludeFilter.get.contains(x.snpName.value))
          }.filter(x => (x.maf <= maximumMAF && x.missingRate <= maximumMissingnessRate) || pruneKeepSNPs.contains(x.snpName.value))

          if (includeIndividuals.isEmpty) snpfiltered else snpfiltered.filterIndividuals(includeIndividuals.get)
        }

        val written = WriteData.write(filtered.individuals, filtered.snpIterator.map { x =>
          writer.write(x._1.toLine + "\n")
          counter += 1
          if (counter % 10000 == 0) {
            println(counter)
          }
          x
        }, filtered.missingValue, outputFormat, outPath, genomicMap, recodeToRecessive, blockSize, grmBatchSize, grmThreads)

        (counter, written)

      }
    }

  }
}