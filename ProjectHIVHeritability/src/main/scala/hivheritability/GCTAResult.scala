package hivheritability

import mybiotools.stat.Effect
import mybiotools.SummaryStat
import mybiotools.gwascommons.gcta._
import mybiotools.workflows._

case class OneRoundResult(human: Option[GCTAResampledResultSummary], hiv1: Seq[GCTAResampledResultSummary], hiv2: Seq[GCTAResampledResultSummary], hiv12: Seq[GCTAResampledResultSummary], humanhiv1: Seq[GCTAResampledResultSummary], humanhiv2: Seq[GCTAResampledResultSummary], humanhiv12: Seq[GCTAResampledResultSummary])

case class GCTAPooledResampledResults(main: SummarizedGCTAResult, bootstrap: SummarizedGCTAResult, randomized: SummarizedGCTAResult, randomPhenotypes: SummarizedGCTAResult, resample: Seq[(Int, SummarizedGCTAResult)]) {
  def prettyPrint = s"""
  |GCTAPooledResampledResults: 
  |
  |Main:
  |${main.prettyPrint}
  |
  |Bootstrap: 
  |${bootstrap.prettyPrint}
  |
  |Randomized: 
  |${randomized.prettyPrint}
  |
  |RandomPhenotype: 
  |${randomPhenotypes.prettyPrint}
  |
  |Resamples: 
  |${resample.sortBy(_._1).map(x => x._1 + ":\n " + x._2.prettyPrint).mkString("", "\n", "\n")}
  """.stripMargin
}

object GCTAPooledResampledResults {
  def apply(raw: Seq[GCTAResampleOutput]): GCTAPooledResampledResults = GCTAPooledResampledResults(
    main = SummarizedGCTAResult(raw.flatMap(_.main.result)),
    bootstrap = SummarizedGCTAResult(raw.flatMap(_.bootstraps.flatMap(_.result))),
    randomized = SummarizedGCTAResult(raw.flatMap(_.randomizations.flatMap(_.result))),
    randomPhenotypes = SummarizedGCTAResult(raw.flatMap(_.randomPhenotypes.flatMap(_.result))),
    resample = SummarizedGCTAResult.createResampleBins(raw.flatMap(_.resamples.flatMap(_.result)))
  )
}