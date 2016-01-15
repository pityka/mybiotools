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

package dispensability

import mybiotools.stat.{ MultinomialRandomGenerator, RandomShuffler }
import mybiotools.SummaryStat
import scala.util.Random
import org.apache.commons.math3.random.{ RandomGenerator, RandomDataGenerator }
import org.apache.commons.math3.stat.inference.MannWhitneyUTest
import mybiotools.plots.ScatterPlot
import org.apache.commons.math3.random.MersenneTwister
import org.apache.commons.math3.distribution.BinomialDistribution
import mybiotools.stat.Poisson
import dispensability.RelativeMutationRateModel.{ Lambdas, Posteriors }
import org.apache.commons.math3.util.FastMath
import org.apache.commons.math3.distribution.PoissonDistribution
import org.saddle._
import dispensability.tasks.OptimalParameter
import dispensability.DownsamplingHelpers.GeneId
import mybiotools.gwascommons._
import java.io.File
import mybiotools._
import mybiotools._
import mybiotools.stat._
import java.io.File
import scala.collection.JavaConversions._
import org.saddle._
import mybiotools.stringstore._
import com.typesafe.config.Config
import mybiotools.tasks._
import dispensability.tasks._
import scala.concurrent.Future
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import org.apache.commons.math3.random.{ MersenneTwister, RandomDataGenerator }
import mybiotools.plots.ScatterPlot._
import mybiotools.plots._
import java.awt.Color
import de.erichseifert.gral.graphics._
import de.erichseifert.gral.plots.colors._
import de.erichseifert.gral.util.Insets2D;
import de.erichseifert.gral.plots.{ Plot, XYPlot };
import de.erichseifert.gral.plots.axes.AxisRenderer
import java.awt.Font
import de.erichseifert.gral.util.{ Orientation, Location, Dimension2D }
import de.erichseifert.gral.plots.legends.Legend
import java.awt.{ Stroke, BasicStroke }
import mybiotools.gwascommons._
import mybiotools.eq._
import mybiotools.stat.RegressionResult
import dispensability.tasks.OptimalParameter
import AbsoluteMutationRateModel.{ Parameters => AbsParam }
import RelativeMutationRateModel.{ Parameters => RelParam }

object GeneDescription {

  case class Classification(haploinsufficient: Set[Gene], haplosufficient: Set[Gene], undecided: Set[Gene], underpowered: Set[Gene]) {
    val all = (haploinsufficient ++ haplosufficient ++ undecided ++ underpowered)
    assert(all.size == haploinsufficient.size + haplosufficient.size + underpowered.size + undecided.size)
  }

  def makeClassifications[Data <: PerGeneCountData, Param](
    data: Data,
    model: Model[Data, Param, _],
    params: Seq[OptimalParameter[Param]],
    seed: Int,
    output: String,
    countData: DataForAbsoluteModel
  ): Map[String, Classification] = {

    val countMap: Map[Gene, DatumForAbsoluteModel] = countData.vector zip countData.genes map (x => x._2 -> x._1) toMap

    val rnd = new MersenneTwister(seed)

    val posteriorClassificationThreshold = 0.99
    val powerThreshold = 0.1

    val meanparam = model.parameterMean(params.map(_.optimum))

    val power = model.powerToHaveHighPostieriorOfHaploinsufficiency(posteriorClassificationThreshold, data, meanparam, 100, rnd)

    val posterior = model.posteriorOfHaploinsufficient(data, meanparam)

    val pvalues = model.poissonTestOfHaploInsufficiency(data, meanparam)

    val zipped = data.genes zip power zip posterior zip pvalues map {
      case (((gene, power), posterior), pvalues) => (gene, power, posterior, pvalues)
    }

    writeToFile(
      output + ".posteriors.txt",
      "name\tpower\tphi\tphs\tpvalnothi\tpvalnoths\tsyncount\tobservedtruncationcount\tpSyn\tpStop\tpFS\texpectedtruncationcount\n" + zipped.map {
        case (gene, power, Posteriors(phi, phs), (pvalnothi, pvalnoths)) =>
          s"${gene.hgnc.value}\t$power\t$phi\t$phs\t$pvalnothi\t$pvalnoths\t${countMap(gene).observedSynonyms.value}\t${countMap(gene).observedTruncations.value}\t${gene.probSynonym}\t${gene.probStop}\t${gene.probFS}\t${countMap(gene).expectedTruncations.value}"
      }.mkString("\n")
    )

    val posteriorClassification = {
      val underpowered = zipped.filter(_._2 < powerThreshold).map(_._1).toSet
      val powered = zipped.filter(_._2 >= powerThreshold)
      val hi = powered.filter(_._3.pHI >= posteriorClassificationThreshold).map(_._1).toSet
      val hs = powered.filter(_._3.pHS >= posteriorClassificationThreshold).map(_._1).toSet
      val unknown = powered.filter(x => x._3.pHI < posteriorClassificationThreshold && x._3.pHS < posteriorClassificationThreshold).map(_._1).toSet
      Classification(hi, hs, unknown, underpowered)
    }

    val poissonTestClassification = {
      val pthresholdHI = highestSignificantPValueByFDR(0.05, zipped.map(_._4._1))
      val pthresholdHS = highestSignificantPValueByFDR(0.05, zipped.map(_._4._2))
      val higherThanHI = zipped.filter(_._4._1 < pthresholdHI).map(_._1).toSet
      val lowerThanHS = zipped.filter(_._4._2 < pthresholdHS).map(_._1).toSet
      val underpowered = zipped.filter(x => x._4._2 >= pthresholdHS && x._4._1 >= pthresholdHI).map(_._1).toSet
      val undecided = (higherThanHI & lowerThanHS)
      val hi = lowerThanHS &~ undecided
      val hs = higherThanHI &~ undecided
      Classification(hi, hs, undecided, underpowered)
    }

    val noTruncationClassification = {
      val notruncation = data.genes.filter(g => countMap(g).observedTruncations.value == 0).toSet
      val withtruncation = data.genes.filter(g => countMap(g).observedTruncations.value > 0).toSet
      Classification(notruncation, withtruncation, Set(), Set())
    }

    Map("PoissonTest" -> poissonTestClassification, "PosteriorThreshold" -> posteriorClassification, "UnmodeledNotruncation" -> noTruncationClassification)

  }

  def descriptiveAnalysis(
    externalAnnotations: List[(String, PhenotypeScale, Boolean, File)],
    genesets: Map[String, Map[GeneSetKey, Set[Gene]]],
    data: PerGeneCountData,
    output: String,
    classifications: Map[String, Classification]
  ) = {

    val allgenes: Seq[Gene] = data.genes
    val countMap: Map[Gene, (ObservedTruncationCount, ExpectedTruncationCount)] = data.observedCounts zip data.genes zip data.expectedCounts map (x => x._1._2 -> (x._1._1, x._2)) toMap
    def extractExpectedVsObserved(set: Set[Gene]) = set map (g => countMap(g)._2.value -> countMap(g)._1.value.toDouble) toVector

    {
      classifications.foreach {
        case (name, classification) =>

          writeToFile(
            output + s".$name.classification.gmt.txt",
            List(
              "signHaploInsufficient\twhatever\t" + classification.haploinsufficient.map(_.hgnc.value).toSeq.sorted.mkString("\t"),
              "signHaploSufficient\twhatever\t" + classification.haplosufficient.map(_.hgnc.value).toSeq.sorted.mkString("\t"),
              "undecided\twhatever\t" + classification.undecided.map(_.hgnc.value).toSeq.sorted.mkString("\t"),
              "underpowered\twhatever\t" + classification.underpowered.map(_.hgnc.value).toSeq.sorted.mkString("\t")
            ).mkString("\n")
          )

          pdfToFile(
            new File(output + s".$name.tmp2.pdf"),
            HistogramPlot.createHistogramPlotMultipleAsScatter(
              ("o/e", Color.black, data.observedCounts zip data.expectedCounts map (x => x._1.value / x._2.value)) ::
                ("o/e HI", Color.red, zip(data.observedCounts, data.expectedCounts, data.genes).filter(x => classification.haploinsufficient.contains(x._3)).map(x => x._1.value / x._2.value)) ::
                ("o/e Hs", Color.blue, zip(data.observedCounts, data.expectedCounts, data.genes).filter(x => classification.haplosufficient.contains(x._3)).map(x => x._1.value / x._2.value)) ::
                Nil
            )
          )

          pdfToFile(
            output + s".$name.scatter.pdf",
            nice(ScatterPlot.createScatterPlotFromMultiple(
              data = List(
                ScatterPlot.Label("Haploinsufficient genes (" + classification.haploinsufficient.size + ")", Color.red, circle(2.0)) -> seq2(extractExpectedVsObserved(classification.haploinsufficient)),
                ScatterPlot.Label("Non-haploinsufficient genes (" + classification.haplosufficient.size + ")", Color.blue, circle(2.0)) -> seq2(extractExpectedVsObserved(classification.haplosufficient)),
                ScatterPlot.Label("Undecided (" + classification.undecided.size + ")", Color.gray, circle(2.0)) -> seq2(extractExpectedVsObserved(classification.undecided)),
                ScatterPlot.Label("Underpowered (" + classification.underpowered.size + ")", Color.black, circle(2.0)) -> seq2(extractExpectedVsObserved(classification.underpowered))
              // ScatterPlot.Label("disp" + dispensable.size, Color.green, circle(2.0)) -> seq2(dispensable map (x => x._2.value -> x._1.value.toDouble))

              ),
              xlab = "Expected truncations",
              ylab = "Observed truncations",
              draw1Line = true,
              xlim = Some((0, 200)),
              ylim = Some((0, 200))
            ))
          )

          {
            val n = ((classification.haplosufficient.size.toInt + classification.undecided.size.toInt))
            pdfToFile(
              output + s".$name.scatter2.pdf",
              nice(ScatterPlot.createScatterPlotFromMultiple(
                data = List(
                  ScatterPlot.Label("Haploinsufficient genes (" + classification.haploinsufficient.size + ")", Color.red, circle(2.0)) -> seq2(extractExpectedVsObserved(classification.haploinsufficient)),
                  ScatterPlot.Label("Non-haploinsufficient genes (" + n + ")", Color.gray, circle(2.0)) -> seq2(extractExpectedVsObserved(classification.haplosufficient ++ classification.undecided)),
                  ScatterPlot.Label("Underpowered (" + classification.underpowered.size + ")", Color.black, circle(2.0)) -> seq2(extractExpectedVsObserved(classification.underpowered))
                // ScatterPlot.Label("disp" + dispensable.size, Color.green, circle(2.0)) -> seq2(dispensable map (x => x._2.value -> x._1.value.toDouble))

                ),
                xlab = "Expected truncations",
                ylab = "Observed truncations",
                draw1Line = true,
                xlim = Some((0, 200)),
                ylim = Some((0, 200))
              ))
            )
          }
          {
            val n = classification.haplosufficient.size + classification.undecided.size + classification.underpowered.size
            pdfToFile(
              output + s".$name.scatter3.pdf",
              nice(ScatterPlot.createScatterPlotFromMultiple(
                data = List(
                  ScatterPlot.Label("", Color.red, circle(4.0)) -> seq2(extractExpectedVsObserved(classification.haploinsufficient)),
                  ScatterPlot.Label("", Color.gray, circle(4.0)) -> seq2(extractExpectedVsObserved(classification.haplosufficient ++ classification.undecided ++ classification.underpowered) ++ extractExpectedVsObserved(classification.undecided) ++ extractExpectedVsObserved(classification.underpowered))
                // ScatterPlot.Label("disp" + dispensable.size, Color.green, circle(2.0)) -> seq2(dispensable map (x => x._2.value -> x._1.value.toDouble))

                ),
                xlab = "Expected truncations",
                ylab = "Observed truncations",
                draw1Line = true,
                xlim = Some((0, 140)),
                ylim = Some((0, 140))
              ))
            )
          }
      }
    }

    {
      def enrichment(
        classification: Classification,
        name1: String,
        genesets: Map[String, Map[GeneSetKey, Set[Gene]]]
      ) = {
        val backgroundgeneset: Set[String] = classification.all.map(_.hgnc.value)
        val haploinsufficientGenes: Set[String] = classification.haploinsufficient.map(_.hgnc.value)

        genesets.foreach {
          case (name, db) =>

            val representative = RepresentativeCategories.createRepresentativeCategories(in = db, misc = GeneSetKey(StringStore("Misc")), seed = 40)

            val uncorrected = db.filter(x => (x._2.map(_.hgnc.value) & backgroundgeneset).size > 100).map {
              case (name, apriori) =>
                val aprioriInBackground: Set[String] = apriori.map(_.hgnc.value) & backgroundgeneset

                val pvalueForEnrichment = mybiotools.stat.OverRepresentation.geneSet[String](
                  numberOfAllGenes = backgroundgeneset.size,
                  aprioriSet = aprioriInBackground,
                  targetSet = haploinsufficientGenes.toSet
                )

                val pvalueForDepletion = mybiotools.stat.OverRepresentation.geneSetForDepletion[String](
                  numberOfAllGenes = backgroundgeneset.size,
                  aprioriSet = aprioriInBackground,
                  targetSet = haploinsufficientGenes.toSet
                )

                (name.name.value, pvalueForEnrichment, aprioriInBackground.size, representative.get(name).map(_.name.value).getOrElse("Misc"), (aprioriInBackground & haploinsufficientGenes).size, pvalueForDepletion)
            }.toIndexedSeq

            val significantEnriched = {
              val bhthreshold: Double = highestSignificantPValueByFDR(0.05, uncorrected.map(_._2))
              uncorrected.filter(_._2 <= bhthreshold)
            }
            val significantDepleted = {
              val bhthreshold: Double = highestSignificantPValueByFDR(0.05, uncorrected.map(_._6))
              uncorrected.filter(_._6 <= bhthreshold)
            }

            writeToFile(output + s".$name1.$name.enrichment.txt", "name\tpvalue\tpathwaysizeinbackground\trepresentative\tintersectsize\tbackgroundsize\n" + significantEnriched.sortBy(_._2).map(x => x._1 + "\t" + x._2 + "\t" + x._3 + "\t" + x._4 + "\t" + x._5 + "\t" + backgroundgeneset.size).mkString("\n"))
            writeToFile(output + s".$name1.$name.enrichment.representative.txt", significantEnriched.groupBy(_._4).toSeq.sortBy(_._2.size * (-1)).map(x => x._1 + " " + x._2.size).mkString("\n"))

            writeToFile(output + s".$name1.$name.depletion.txt", "name\tpvalue\tpathwaysizeinbackground\trepresentative\tintersectsize\tbackgroundsize\n" + significantDepleted.sortBy(_._6).map(x => x._1 + "\t" + x._6 + "\t" + x._3 + "\t" + x._4 + "\t" + x._5 + "\t" + backgroundgeneset.size).mkString("\n"))
            writeToFile(output + s".$name1.$name.depletion.representative.txt", significantDepleted.groupBy(_._4).toSeq.sortBy(_._2.size * (-1)).map(x => x._1 + " " + x._2.size).mkString("\n"))
        }
      }

      classifications.foreach {
        case (name, classification) =>
          enrichment(classification, name, genesets)

      }

    }

    {
      val externalAnnotationData: Seq[(String, (PhenotypeScale, Series[String, Double]))] =
        externalAnnotations.map {
          case (name, scale, includeMissingASZero, file) =>
            val data = {
              val raw = openSource(file)(Input.readPerGeneVariable)
              if (includeMissingASZero) allgenes.toSeq.map(_.hgnc).map(x => x.value -> raw.get(x).getOrElse(0.0)) else raw.toSeq.map(x => x._1.value -> x._2)
            }
            (name, (scale, Series(data.toSeq: _*)))
        }

      def regression(predictorHI: Series[String, Double], name: String) = {
        import mybiotools.gwascommons._

        val predictorDeNovo = Series(allgenes.map(g => g.hgnc.value -> (g.probStop + g.probFS)): _*)

        val tablestring = "annotation,interpretation,slope,pvalue,test\n" + (externalAnnotationData.map {
          case (annotationName, (scale, annotationData)) =>

            val scoreOfHI: Seq[Double] = predictorHI.toSeq.filter(_._2 == 1.0).map(x => annotationData.get(x._1)).filter(_.isDefined).map(_.get)
            val scoreOfHS: Seq[Double] = predictorHI.toSeq.filter(_._2 == 0.0).map(x => annotationData.get(x._1)).filter(_.isDefined).map(_.get)

            // pdfToFile(output + s".annotation.histogram.$name.$annotationName.pdf",
            //   mybiotools.plots.nice(mybiotools.plots.HistogramPlot.createHistogramPlotMultipleAsScatter(
            //     List(("Haploinsufficient", Color.red, scoreOfHI.filter(_ < 2.0)),
            //       ("Not haploinsufficient", Color.blue, scoreOfHS.filter(_ < 2.0))
            //     ),
            //     breaks = 100,
            //     relative = true,
            //     ylab = "Relative frequency",
            //     xlab = annotationName)))

            val r: AnyRef = scale match {
              case Rank => {
                val m: MannWhitneyU.MWUTestResult = mybiotools.stat.MannWhitneyU.mannWhitneyUTest(scoreOfHI, scoreOfHS)
                // log.info(s"$name Non parametric two-tailed Mann-Whitney U of " + annotationName + " in HI vs HS genes: " + m)
                m
              }
              case Linear => {
                mybiotools.stat.LinearRegression.linearRegression(
                  data = Frame("phi" -> predictorHI, "denovo" -> predictorDeNovo, "y" -> annotationData),
                  yKey = "y",
                  covariates = Seq("phi", "denovo"),
                  missingMode = mybiotools.stat.DropSample,
                  lambda = 0.0
                )
              }
              case Logistic => {
                mybiotools.stat.LogisticRegression.logisticRegression(
                  data = Frame("phi" -> predictorHI, "denovo" -> predictorDeNovo),
                  outcomes = annotationData.mapValues(_ > 0.0),
                  covariateNames = Seq("phi", "denovo"),
                  missingMode = mybiotools.stat.DropSample,
                  maxIter = 50,
                  epsilon = 1E-10
                )
              }
              case Count =>
                mybiotools.stat.PoissonRegression.poissonRegression(
                  data = Frame("phi" -> predictorHI, "denovo" -> predictorDeNovo),
                  outcomes = annotationData,
                  covariateNames = Seq("phi", "denovo"),
                  missingMode = mybiotools.stat.DropSample
                )
            }
            // println(s"$name $scale regression of '$annotationName ~ P(HI)>=$classificationThreshold + denovo' : \n" + (r match {
            //   case x: mybiotools.stat.RegressionResult => x.table
            //   case Right(x: mybiotools.stat.RegressionResult) => x.table
            //   case x => x.toString
            // }))

            (r match {
              case x: RegressionResult => x
              case Right(x: RegressionResult) => x
              case x => x
            }) match {
              case MannWhitneyU.MWUTestResult(p, z) => {
                val textual = if (z > 0) "Larger" else "Smaller"
                if (p >= 0.05) None
                else Some(s"$annotationName,$textual,$z,$p,Rank(z-value)")
              }
              case x: RegressionResult => {
                val slope = x.covariates("phi")._1.slope
                val slopetext = {
                  val r = slope
                  if (scale == Logistic) math.exp(r)
                  else r
                }
                val pvalue = x.covariates("phi")._2.pValue
                val interpretation = scale match {
                  case Linear | Count if slope < 0 => "Smaller"
                  case Linear | Count if slope >= 0 => "Larger"
                  case Logistic if slope >= 0 => "Enrichment"
                  case Logistic if slope < 0 => "Depletion"
                  case _ => ""
                }
                val scaletext = scale match {
                  case Linear | Count => scale.toString
                  case Logistic => "Logistic(OR)"
                  case _ => throw new RuntimeException("can't happen")
                }

                if (pvalue >= 0.05) None
                else Some(s"$annotationName,$interpretation,$slopetext,$pvalue,$scaletext")
              }
              case x => {
                println(x)
                println(annotationName)
                println(annotationData)
                None
              }
            }

        } filter (_.isDefined) map (_.get) mkString ("\n"))
        writeToFile(output + s".$name.classification.regressions.csv", tablestring)
      }

      classifications.foreach {
        case (name, classification) =>
          val merged = (classification.haploinsufficient ++ classification.haplosufficient)
          val allpowered = (classification.haploinsufficient ++ classification.haplosufficient ++ classification.undecided)
          regression(
            Series(allgenes.filter(g => merged.contains(g)).map(g => g.hgnc.value -> (if (classification.haploinsufficient.contains(g)) 1.0 else 0.0)): _*),
            name + ".HIVsHS"
          )

          regression(
            Series(allgenes.filter(g => merged.contains(g)).map(g => g.hgnc.value -> (if (classification.haploinsufficient.contains(g)) 1.0 else 0.0)): _*),
            name + ".HIVsOtherPowered"
          )

          regression(
            Series(allgenes.filter(g => (classification.all).contains(g)).map(g => g.hgnc.value -> (if (classification.haploinsufficient.contains(g)) 1.0 else 0.0)): _*),
            name + ".HIVsAll"
          )

      }

    }
  }

}