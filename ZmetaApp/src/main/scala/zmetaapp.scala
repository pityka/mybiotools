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
import mybiotools.gwascommons.associationresults._
import mybiotools._
import mybiotools.config.Config.configInstance
import scala.collection.JavaConversions._
import mybiotools.stringstore._

object ZmetaApp extends App {

  val Norm = new org.apache.commons.math3.distribution.NormalDistribution

  def convertpvalue(pval: Double, beta: Double) = {

    if (beta > 0) {
      Norm.inverseCumulativeProbability(pval / 2)
    } else {
      -1 * Norm.inverseCumulativeProbability(pval / 2)
    }
  }

  type A = AssociationResult with HasNonMissingCount with HasEffectSize

  val assocfilesWithHeader = configInstance.getStringList("assocfiles").toVector

  val outfile = configInstance.getString("out")

  val assocs: Vector[Map[String8, A]] = assocfilesWithHeader.map { file =>

    readAssociationResultsFromPlinkLinearAssoc(scala.io.Source.fromFile(file), None).toVector.map(x => x.name -> x).toMap
  }

  val commonsnps: Vector[String8] = assocs.map(_.keys.toSet).reduce(_ & _).toVector.distinct

  if (commonsnps.size == 0) {
    println("No common snps found.")
  } else {

    val meta: Map[String8, (Double, Double)] = commonsnps.map { snp =>

      val nonMissCounts: Vector[Int] = assocs.map(_.apply(snp).nonMiss)

      val nsum = nonMissCounts.sum

      val weights = nonMissCounts.map(x => math.sqrt(x) / math.sqrt(nsum))

      val zvalues = assocs.map { m =>
        val as = m.apply(snp)
        val p = as.pValue
        val beta = as.effectSize
        convertpvalue(p, beta)
      }

      val weightedZSum = zvalues zip weights map (x => x._1 * x._2) sum

      val pmeta = Norm.cumulativeProbability(-1 * (math.abs(weightedZSum))) * 2;

      snp -> (pmeta, weightedZSum)
    }.toMap

    openFileWriter(new java.io.File(outfile)) { writer =>
      writer.write("CHR SNP BP METAZ P\n")
      meta.foreach {
        case (snp, (p, z)) =>
          val chr = assocs.head(snp).genomicLocation.chromosome
          val bp = assocs.head(snp).genomicLocation.basePairPosition
          writer.write(chr + " " + snp + " " + bp + " " + z + " " + p + "\n")
      }
    }

    val pvalues = meta.values.iterator

    writeBinaryToFile(outfile + ".qq.png", mybiotools.plots.renderToByteArray(mybiotools.plots.QQPlot.plot(pvalues.map(_._1)), "image/png", 1000, 1000))
  }

}