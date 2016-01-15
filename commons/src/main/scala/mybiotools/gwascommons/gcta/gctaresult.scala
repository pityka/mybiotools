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

package mybiotools.gwascommons.gcta

import mybiotools.stat.Effect
import mybiotools.SummaryStat
import mybiotools._

case class GCTAResult(
    varianceComponents: Map[String, Effect],
    logL: Double,
    logL0: Option[Double],
    LRT: Option[Double],
    df: Option[Int],
    pValue: Option[Double],
    n: Int
) {

  def totalExplainedVariance: Effect = Effect(varianceComponents.filter(_._1.endsWith("/Vp")).map(_._2.slope).sum, math.sqrt(varianceComponents.filter(_._1.endsWith("/Vp")).map(x => x._2.sd * x._2.sd).sum))

  def prettyPrint = {
    "Source\tVariance\tSD\n" +
      (("total" -> totalExplainedVariance) :: varianceComponents.toList).map(x => s"${x._1}\t${x._2.slope}\t${x._2.sd}").mkString("", "\n", "\n") +
      s"logL\t$logL\n" +
      s"logL0\t$logL0\n" +
      s"LRT\t$LRT\n" +
      s"df\t$df\n" +
      s"pValue\t$pValue\n" +
      s"n\t$n\n"

  }
}

object GCTAResult {
  def fromHSQLinesWithoutHeader(lines: IndexedSeq[String]): GCTAResult = {
    val splitteds = lines.map(x => mybiotools.fastSplitSetSeparator(x, Set(' ', '\t')))
    val l2 = splitteds.filter(_.size == 2).map(x => x(0) -> x(1)).toMap
    val n = l2("n").toInt
    val pValue = l2.get("Pval").map(_.toDouble)
    val LRT = l2.get("LRT").map(_.toDouble)
    val logL0 = l2.get("logL0").map(_.toDouble)
    val logL1 = l2("logL").toDouble
    val df = l2.get("df").map(_.toInt)
    val varianceComponents = splitteds.filter(_.size == 3).map { line =>
      val name = line(0)
      val variance = line(1).toDouble
      val sd = line(2).toDouble
      name -> Effect(variance, sd)
    }.toMap
    GCTAResult(varianceComponents, logL1, logL0, LRT, df, pValue, n)
  }
}

case class SummarizedGCTAResult(
    varianceComponents: Map[String, SummaryStat],
    errors: Map[String, SummaryStat],
    pValues: SummaryStat,
    ns: SummaryStat,
    total: (SummaryStat, SummaryStat)
) {

  def prettyPrint =
    "Total V(g)/Vp: \t" + total._1.median.format(5) + ":" + total._1.iqr.format(5) + ":" + total._1.count.format(5) + "\n" +
      "Total V(g)/Vp SD: \t" + total._2.median.format(5) + ":" + total._2.iqr.format(5) + ":" + total._2.count.format(5) + "\n" +
      "Variance Components:\n" + varianceComponents.toSeq.map(x => x._1 + "\t" + x._2.median.format(5) + ":" + x._2.iqr.format(5) + ":" + x._2.count).mkString("", "\n", "\n") +
      "SDs:\n" + errors.toSeq.map(x => x._1 + "\t" + x._2.median.format(5) + ":" + x._2.iqr.format(5) + ":" + x._2.count).mkString("", "\n", "\n") +
      "pValues\t" + pValues.median + ":" + pValues.iqr.format(5) + ":" + pValues.count + "\nNs\t" +
      ns.median.format(5) + ":" + ns.iqr.format(5) + ":" + ns.count
}

object SummarizedGCTAResult {
  def apply(res: Seq[GCTAResult]): SummarizedGCTAResult = {
    val varianceComponentNames = res.flatMap(_.varianceComponents.keys).toSeq.distinct
    SummarizedGCTAResult(
      varianceComponents = varianceComponentNames.map(name => name -> SummaryStat(res.map(_.varianceComponents(name).slope), true)).toMap,
      errors = varianceComponentNames.map(name => name -> SummaryStat(res.map(_.varianceComponents(name).sd), true)).toMap,
      pValues = SummaryStat(res.flatMap(_.pValue), true),
      ns = SummaryStat(res.map(_.n), true),
      total = (SummaryStat(res.map(_.totalExplainedVariance.slope)), SummaryStat(res.map(_.totalExplainedVariance.sd)))
    )
  }
  def createResampleBins(seq1: Seq[GCTAResult]): Seq[(Int, SummarizedGCTAResult)] = {
    if (seq1.size > 0) scala.util.Try {
      val seq = seq1.map(x => x.n -> x)
      val ns: Seq[Int] = seq.map(_._1)
      val min = ns.min
      val max = ns.max
      val breaks = (math.pow(2 * ns.size, 0.5).toInt + 1)
      val step = (max - min) / breaks

      seq.groupBy(i => ((i._1 - min) / step).toInt * step + min).map(x => x._1 -> SummarizedGCTAResult(x._2.map(_._2))).toSeq
    }.toOption.getOrElse(Seq())
    else Seq()
  }

}

case class GCTAResampledResultSummary(main: Option[GCTAResult], bootstrap: SummarizedGCTAResult, randomized: SummarizedGCTAResult, randomPhenotype: SummarizedGCTAResult, resample: Seq[(Int, SummarizedGCTAResult)]) {
  def prettyPrint = s"""
  |GCTAResampledResult: 
  |
  |Main:
  |${main.map(_.prettyPrint).getOrElse("")}
  |
  |Bootstrap: 
  |${bootstrap.prettyPrint}
  |
  |Randomized: 
  |${randomized.prettyPrint}
  |
  |RandomPhenotype: 
  |${randomPhenotype.prettyPrint}
  |
  |Resamples: 
  |${resample.sortBy(_._1).map(x => x._1 + ":\n " + x._2.prettyPrint).mkString("", "\n", "\n")}
  """.stripMargin
}

