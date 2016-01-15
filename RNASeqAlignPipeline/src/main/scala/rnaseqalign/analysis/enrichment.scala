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

package rnaseqalign.analysis

import mybiotools._
import mybiotools.plots._
import java.io.File

case class GeneSet(name: String, genes: Set[String])
case class GeneSetDB(name: String, setsByName: Map[String, GeneSet], representativeCategories: Map[String, String])
object GeneSetDB {
  def apply(name: String, sets: Seq[GeneSet]): GeneSetDB = {
    val repr: Map[String, String] = RepresentativeCategories.createRepresentativeCategories(in = sets.map(s => s.name -> s.genes).toMap, misc = "Misc")
    GeneSetDB(
      name,
      sets.map(s => s.name -> s).toMap,
      repr
    )
  }
}
case class EnrichmentResult(db: GeneSetDB, enrichmentPValues: Map[String, Double], query: GeneList) {
  def name = db.name + "." + query.name
  def toFile = {
    val tmp = TempFile.createTempFile(s"enrichment.${name}")
    mybiotools.writeToFile(
      tmp,
      "GENESET P AVGDE REPRESENTATIVECATEGORY QUERY\n" +
        enrichmentPValues.toSeq.sortBy(_._2).map {
          case (setname, p) =>
            setname + "\t" + p + "\t" + SummaryStat(db.setsByName(setname).genes.flatMap(g => query.differentialExpressions.get(g))).mean + "\t" + db.representativeCategories(setname) + "\t" + query.name
        }.mkString("\n")
    )
    tmp
  }
}

case class EnrichmentResultPlain(db: GeneSetDB, enrichmentPValues: Map[String, Double], query: Set[String], weights: Map[String, Double]) {
  def toFile = {
    val tmp = TempFile.createTempFile(s"enrichment")
    mybiotools.writeToFile(
      tmp,
      "GENESET P AVGW REPRESENTATIVECATEGORY\n" +
        enrichmentPValues.toSeq.sortBy(_._2).map {
          case (setname, p) =>
            setname + "\t" + p + "\t" + SummaryStat(db.setsByName(setname).genes.flatMap(g => weights.get(g))).mean + "\t" + db.representativeCategories(setname) + "\t"
        }.mkString("\n")
    )
    tmp
  }

  def plotToFile = {
    val tmp = TempFile.createTempFile(s"enrichment.${this.db.name}")
    mybiotools.writeBinaryToFile(
      tmp,
      mybiotools.plots.renderToByteArray(EnrichmentPlot.createPlot(this), "application/pdf", 1.0)
    )
    tmp
  }
}

object EnrichmentResult {
  def plotToFile(up: EnrichmentResult, down: EnrichmentResult) = {
    val tmp = TempFile.createTempFile(s"enrichment.${up.db.name}")
    mybiotools.writeBinaryToFile(
      tmp,
      mybiotools.plots.renderToByteArray(EnrichmentPlot.createPlot(up, down), "application/pdf", 1.0)
    )
    tmp
  }
  def apply(aprioris: GeneSetDB, backgroundGenes: Set[String], query: Set[String], weights: Map[String, Double]): EnrichmentResultPlain = {
    val uncorrected = aprioris.setsByName.map {
      case (name, apriori) =>
        val aprioriInBackground = apriori.genes & backgroundGenes
        val pvalue = mybiotools.stat.OverRepresentation.geneSet(numberOfAllGenes = backgroundGenes.size, aprioriSet = aprioriInBackground, targetSet = query)
        apriori.name -> pvalue
    }.toIndexedSeq
    EnrichmentResultPlain(aprioris, mybiotools.adjustPValuesByFDRPairs(uncorrected).toMap.filter(_._2 < 5E-2), query, weights)
  }
  def apply(aprioris: GeneSetDB, backgroundGenes: Set[String], query: GeneList): EnrichmentResult = {
    val uncorrected = aprioris.setsByName.map {
      case (name, apriori) =>
        val aprioriInBackground = apriori.genes & backgroundGenes
        val queryset = query.genes
        val pvalue = mybiotools.stat.OverRepresentation.geneSet(numberOfAllGenes = backgroundGenes.size, aprioriSet = aprioriInBackground, targetSet = queryset)
        apriori.name -> pvalue
    }.toIndexedSeq
    EnrichmentResult(aprioris, mybiotools.adjustPValuesByFDRPairs(uncorrected).toMap.filter(_._2 < 5E-2), query)
  }
}