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
import org.saddle._
import org.saddle.io.CsvImplicits._
import org.saddle.io._
import mybiotools.tasks._
import java.io.File
import org.apache.commons.math3.ml.distance._

case class PairWiseDE(query: String, baseLine: String, des: DifferentialExpressionResultAsFile, upregulated: GeneList, downregulated: GeneList, enrichments: Seq[(EnrichmentResult, EnrichmentResult)]) {
  def toShared(name: String)(implicit components: TaskSystemComponents) = {
    PairWiseDEShared(query, baseLine,
      des.toShared(s"deseq.$name.$query.vs.$baseLine.csv"),
      upregulated,
      downregulated,
      SharedFile(upregulated.toFile, name = s"deseq.$name.genelist.${upregulated.name}.txt"),
      SharedFile(downregulated.toFile, name = s"deseq.$name.genelist.${downregulated.name}.txt"),
      enrichments,
      enrichments.map {
        case (up, down) =>
          (
            SharedFile(up.toFile, s"enrichment.$name.${up.name}.txt"),
            SharedFile(down.toFile, s"enrichment.$name.${down.name}.txt")
          )
      })
  }
}
case class PairWiseDEShared(cond1: String, cond2: String, des: DifferentialExpressionResultAsSharedFile, upregulated: GeneList, downregulated: GeneList, upregulatedShared: SharedFile, downregulatedShared: SharedFile, enrichments: Seq[(EnrichmentResult, EnrichmentResult)], enrichmentsShared: Seq[(SharedFile, SharedFile)])

case class DEResultSet(
  fulldes: Map[String, DifferentialExpressionResultAsFile],
  pairwisedes: Map[String, List[PairWiseDE]]
)

object DEResultSet {
  def fromCounts(
    counts: Frame[String, String, Long],
    conditions: Map[String, String],
    normalizations: Seq[NormalizationMethod],
    pairwiseDifferentialExpressionConditions: List[(String, String)],
    thresholdDropIfCoverageBelowInAllSamples: Double,
    gmts: Seq[GeneSetDB],
    lengths: Map[String, Int],
    spikeInData: Set[SpikeIn],
    spikeInPerSample: Map[String, SpikeInMix],
    readLength: Int,
    secondaryDimension: Map[String, String],
    conditionOrderOnPlots: Seq[String]
  ): DEResultSet = {

    val fulldes = normalizations.map {
      norm =>
        ("full." + norm.name) -> DESeq.runDESeq(counts, conditions, thresholdDropIfCoverageBelowInAllSamples, norm, spikeInData, spikeInPerSample, lengths, readLength, secondaryDimension, conditionOrderOnPlots)
    }

    val pairwisedes = normalizations.map {
      norm =>
        ("pairwise." + norm.name) -> norm
    } map {
      case (name, norm) =>
        (name, pairwiseDifferentialExpressionConditions.filter {
          case (cond1, cond2) =>
            conditions.filter(x => x._2 == cond1 || x._2 == cond2).size >= 2
        }.map {
          case (cond1, cond2) =>
            val de = DESeq.runDESeq(counts, conditions.filter(x => x._2 == cond1 || x._2 == cond2), thresholdDropIfCoverageBelowInAllSamples, norm, spikeInData, spikeInPerSample, lengths, readLength, secondaryDimension, conditionOrderOnPlots)

            val interpreted = de.interpreted
            val glup = interpreted.upregulatedGenes(cond1, cond2, significanceLevel)
            val gldown = interpreted.downregulatedGenes(cond1, cond2, significanceLevel)

            val enrichments: Seq[(EnrichmentResult, EnrichmentResult)] = gmts.map { gmt =>
              EnrichmentResult(gmt, interpreted.pValues.index.toSeq.toSet, glup) ->
                EnrichmentResult(gmt, interpreted.pValues.index.toSeq.toSet, gldown)
            }
            PairWiseDE(
              cond2,
              cond1,
              de,
              glup,
              gldown,
              enrichments
            )

        })

    }

    DEResultSet(fulldes.toMap, pairwisedes.toMap)
  }
}

case class DifferentialExpressionResultAsSharedFile(file: SharedFile, conditions: Set[String])

case class DifferentialExpressionResultAsFile(file: File, conditions: Set[String], plots: Seq[(String, File)]) {

  def toShared(name: String)(implicit components: TaskSystemComponents) = {
    DifferentialExpressionResultAsSharedFile(SharedFile(file, name = name), conditions)
  }

  def copyFile(n: File) = {

    com.google.common.io.Files.copy(file, n)
    val nplots = plots.map {
      case (name, oldfile) =>
        val n2 = new File(n.getAbsolutePath + "." + name + ".pdf")
        com.google.common.io.Files.copy(oldfile, n2)
        name -> n2
    }
    DifferentialExpressionResultAsFile(n, conditions, nplots)
  }

  lazy val interpreted = DifferentialExpressionResultAsFile.readFile(file, conditions)

}

object DifferentialExpressionResultAsFile {
  def readFile(file: java.io.File, allConditions: Set[String]): DifferentialExpressionResult = {
    val csvfile = CsvFile(file.getAbsolutePath)
    val frame = CsvParser.parse()(csvfile).withRowIndex(0).withColIndex(0)
      .filterIx(colIx => colIx match {
        case "(Intercept)" => true
        case x if x.startsWith("condition") => true
        case "p" => true
        case _ => false
      }).mapValues(CsvParser.parseDouble)

    val otherConditions = frame.colIx.toSeq.filter(_.startsWith("condition")).map(_.stripPrefix("condition"))
    val interceptCondition = {
      val s = (allConditions &~ otherConditions.toSet)
      assert(s.size == 1, s + " " + file.getAbsolutePath + " " + allConditions.toString)
      s.head
    }
    val baselineExpressions = frame.firstCol("(Intercept)")
    val pvalues = frame.firstCol("p")
    val contrastToBaseline = frame.col(otherConditions.map(x => "condition" + x): _*).mapColIndex(_.stripPrefix("condition"))
    DifferentialExpressionResult(interceptCondition, otherConditions, baselineExpressions, contrastToBaseline, pvalues)

  }
}

sealed trait DEDirection {
  def stringRep: String
}
case object Upregulated extends DEDirection {
  def stringRep = "UP"
}
case object Downregulated extends DEDirection {
  def stringRep = "DOWN"
}

case class GeneList(baselineCondition: String, queryCondition: String, direction: DEDirection, genes: Set[String], differentialExpressions: Map[String, Double]) {
  def name = direction.stringRep + ".in." + queryCondition + ".vs." + baselineCondition
  def toFile = {
    val glupfile = TempFile.createTempFile(s"${name}.txt")
    mybiotools.writeToFile(glupfile, differentialExpressions.toSeq.sortBy(_._2).reverse.map(_.productIterator.mkString(" ")).mkString("\n"))
    glupfile
  }
}

case class DifferentialExpressionResult(baselineCondition: String, otherConditions: Seq[String], baselineExpressions: Series[String, Double], contrastToBaseline: Frame[String, String, Double], pValues: Series[String, Double]) {

  /* upregulated genes in queryCondition wrt baseLine */
  def upregulatedGenes(baseLine: String, queryCondition: String, pThreshold: Double): GeneList = {
    val genes =
      if (baseLine == baselineCondition)
        contrastToBaseline.firstCol(queryCondition).filter(_ > minimumLog2FoldChange).apply(pValues.filter(_ <= pThreshold).index.toSeq: _*).toSeq
      else if (queryCondition == baselineCondition)
        contrastToBaseline.firstCol(baseLine).mapValues(-1 * _).filter(_ > minimumLog2FoldChange).apply(pValues.filter(_ <= pThreshold).index.toSeq: _*).toSeq
      else
        (contrastToBaseline.firstCol(queryCondition) - contrastToBaseline.firstCol(baseLine)).filter(_ > minimumLog2FoldChange).apply(pValues.filter(_ <= pThreshold).index.toSeq: _*).toSeq

    GeneList(baseLine, queryCondition, Upregulated, genes.map(_._1).toSet, genes.toMap)
  }

  /* downregulated genes in queryCondition wrt baseLine */
  def downregulatedGenes(baseLine: String, queryCondition: String, pThreshold: Double): GeneList = {
    val genes = if (baseLine == baselineCondition)
      contrastToBaseline.firstCol(queryCondition).filter(_ < (-1) * minimumLog2FoldChange).apply(pValues.filter(_ <= pThreshold).index.toSeq: _*).toSeq
    else if (queryCondition == baselineCondition)
      contrastToBaseline.firstCol(baseLine).mapValues(-1 * _).filter(_ < (-1) * minimumLog2FoldChange).apply(pValues.filter(_ <= pThreshold).index.toSeq: _*).toSeq
    else
      (contrastToBaseline.firstCol(queryCondition) - contrastToBaseline.firstCol(baseLine)).filter(_ < (-1) * minimumLog2FoldChange).apply(pValues.filter(_ <= pThreshold).index.toSeq: _*).toSeq

    GeneList(baseLine, queryCondition, Downregulated, genes.map(_._1).toSet, genes.toMap)

  }

  def significantGenes = {
    val pselect: Set[String] = pValues.filter(_ < significanceLevel).toSeq.map(_._1).toSet

    val effectsizeselect: Set[String] = contrastToBaseline.toRowSeq.map {
      case (genename, contrasttobaseline) =>
        val min = contrasttobaseline.min.get
        val max = contrasttobaseline.max.get
        (genename, (max - min), max)
    }.filter(x => x._2 >= minimumLog2FoldChange || math.abs(x._3) >= minimumLog2FoldChange).map(_._1).toSet
    pselect & effectsizeselect
  }
}

object DESeq {

  def runDESeq(
    rawCounts2: Frame[String, String, Long],
    conditions: Map[String, String],
    thresholdDropIfCoverageBelowInAllSamples: Double,
    normalization: NormalizationMethod,
    spikeInData: Set[SpikeIn],
    spikeInPerSample: Map[String, SpikeInMix],
    lengths: Map[String, Int],
    readLength: Int,
    secondaryDimension: Map[String, String],
    conditionOrderOnPlots: Seq[String]
  ): DifferentialExpressionResultAsFile = {

    val (expressedGeneNames, sf, normalizedData) = getExpressedGenesAndSizeFactors(rawCounts2, lengths, thresholdDropIfCoverageBelowInAllSamples, spikeInData, spikeInPerSample, normalization, readLength)

    val sizeFactors = if (normalization == DeSeqNormalization) None else Some(sf)

    val expressedGenes = replaceNAWithZero(rawCounts2).row(expressedGeneNames: _*)

    val samples = expressedGenes.colIx.toSeq.filter(x => conditions.contains(x))

    val countsFile = TempFile.createTempFile(".csv")
    expressedGenes.col(samples: _*).writeCsvFile(countsFile.getAbsolutePath, withColIx = true, withRowIx = true)

    val sizeFactorsFile = TempFile.createTempFile(".csv")

    mybiotools.writeToFile(
      sizeFactorsFile,
      samples.map(x => sizeFactors.map(_.get(x).get).getOrElse("1")).mkString("", "\n", "\n")
    )

    val conditionsFile = TempFile.createTempFile(".txt")
    mybiotools.writeToFile(
      conditionsFile,
      samples map conditions mkString ("", "\n", "\n")
    )

    val executable = TempFile.getExecutableFromJar("/deseq.R")

    val outfile = TempFile.createTempFile(".txt")

    val rcmd = s"""/usr/bin/env Rscript --vanilla --default-packages=base,utils,stats ${executable} ${countsFile.getAbsolutePath} ${conditionsFile.getAbsolutePath} ${sizeFactors.map(x => "1").getOrElse("0")} ${outfile.getAbsolutePath} ${sizeFactorsFile.getAbsolutePath} """

    // println(rcmd)

    val (stdout, stderr, success) = mybiotools.execGetStreamsAndCode(rcmd, unsuccessfulOnErrorStream = false)

    if (!success) throw new RuntimeException(stdout + "\n" + stderr)

    val plots = {
      val inmemory = DifferentialExpressionResultAsFile.readFile(outfile, samples.map(conditions).toSet)
      val significantgenes = inmemory.significantGenes.toSeq
      val logNormalizedWithPseudo = addPseudoCoverage(normalizedData.row(significantgenes: _*), amount = 0.01, readLength = readLength, transcriptLengths = lengths).mapValues(math.log10)

      significantgenes.map { gene =>

        val plot = detailedGeneExpressionPlot(
          logNormalizedWithPseudo.row(gene),
          reverseMap(conditions).toSeq.sortBy(x => conditionOrderOnPlots.indexOf(x._1)),
          reverseMap(secondaryDimension).toSeq.sortBy(x => conditionOrderOnPlots.indexOf(x._1)),
          Map(gene -> gene),
          "log10(depthnormalized+pseudo)",
          gene
        )

        val pf = TempFile.createTempFile(".png")
        writeBinaryToFile(
          pf.getAbsolutePath,
          mybiotools.plots.renderToByteArray(plot, "application/pdf", 1)
        )
        (gene, pf)

      }

    }

    DifferentialExpressionResultAsFile(outfile, conditions.values.toSet, plots)

  }

  // def runDESeqWithSubjects(rawCounts2: Frame[String, String, Long],
  //   conditions: Map[String, String],
  //   subjects: Map[String, String],
  //   thresholdDropIfBelowInAllSamplesPerBp: Double,
  //   normalization: NormalizationMethod,
  //   spikeInData: Set[SpikeIn],
  //   lengths: Map[String, Int]): DifferentialExpressionResultAsFile = {

  //   val (expressedGeneNames, sf) = getExpressedGenesAndSizeFactors(rawCounts2, lengths, 0, thresholdDropIfBelowInAllSamplesPerBp, spikeInData, normalization)

  //   val sizeFactors = if (normalization == DeSeqNormalization) None else Some(sf)

  //   val expressedGenes = replaceNAWithZero(rawCounts2, 0).row(expressedGeneNames: _*)

  //   val samples = expressedGenes.colIx.toSeq.filter(x => conditions.contains(x) && subjects.contains(x))

  //   val countsFile = TempFile.createTempFile(".csv")
  //   expressedGenes.col(samples: _*).writeCsvFile(countsFile.getAbsolutePath, withColIx = true, withRowIx = true)

  //   val sizeFactorsFile = TempFile.createTempFile(".csv")

  //   mybiotools.writeToFile(sizeFactorsFile,
  //     samples.map(x => sizeFactors.map(_.get(x).get).getOrElse("1")).mkString("", "\n", "\n"))

  //   val conditionsFile = TempFile.createTempFile(".txt")
  //   mybiotools.writeToFile(conditionsFile,
  //     samples map conditions mkString ("", "\n", "\n"))

  //   val subjectsFile = TempFile.createTempFile(".txt")
  //   mybiotools.writeToFile(subjectsFile,
  //     samples map subjects mkString ("", "\n", "\n"))

  //   val executable = TempFile.getExecutableFromJar("/deseq.withsubjects.R")

  //   val outfile = TempFile.createTempFile(".txt")

  //   val rcmd = s"""/usr/bin/env Rscript --vanilla --default-packages=base,utils,stats ${executable} ${countsFile.getAbsolutePath} ${conditionsFile.getAbsolutePath} ${subjectsFile.getAbsolutePath} ${sizeFactors.map(x => "1").getOrElse("0")} ${outfile.getAbsolutePath} ${sizeFactorsFile.getAbsolutePath} """

  //   // println(rcmd)

  //   val (stdout, stderr, success) = mybiotools.execGetStreamsAndCode(rcmd, unsuccessfulOnErrorStream = false)

  //   if (!success) throw new RuntimeException(stdout + "\n" + stderr)

  //   DifferentialExpressionResultAsFile(outfile, conditions.values.toSet)

  // }

}