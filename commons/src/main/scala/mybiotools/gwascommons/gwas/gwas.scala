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

package mybiotools.gwascommons.gwas

import mybiotools.gwascommons._
import mybiotools.gwascommons.genotypedata._
import mybiotools.stringstore.String8
import mybiotools.stat.LinearRegression._
import mybiotools.stat.LogisticRegression._
import mybiotools.stat._
import mybiotools._
import org.saddle._
import PlinkPhenotypeCoding._
import scala.util.{ Try, Failure, Success }
import mybiotools.mapreduce._
import associationresults.FullAssociationResult
import mybiotools.stringstore._

sealed trait GeneticModel
case object Additive extends GeneticModel {
  override def toString = "ADD"
}
case object Recessive extends GeneticModel {
  override def toString = "REC"
}
case object Dominant extends GeneticModel {
  override def toString = "DOM"
}
case object HeterozygousAdvantage extends GeneticModel {
  override def toString = "HETADV"
}
case object ImputationError extends GeneticModel {
  override def toString = "IMPERROR"
}

sealed trait InteractionModel {
  def function: (Double, Double) => Double
  def removeMainEffectOfInteraction: Boolean
}
case object ProductInteraction extends InteractionModel {
  override def toString = "PROD"
  def function = (x: Double, y: Double) => x * y
  def removeMainEffectOfInteraction = false
}
case object ProductDominantInteraction extends InteractionModel {
  override def toString = "PRODDOM"
  def function = (x: Double, y: Double) => scala.math.min(1.0, x) * scala.math.min(1.0, y)
  def removeMainEffectOfInteraction = false
}

object GWAS {

  case class SingleSNPResult(
    regressionResult: Try[RegressionResult],
    locus: LocusData,
    geneticModel: GeneticModel,
    phenoName: String,
    interaction: Option[(String, InteractionModel)]
  )

  case class InMemoryBatchResult(results: Iterable[SingleSNPResult])

  type I = List[InMemoryBatchResult]
  type A = Iterable[(LocusData, Vector[Option[WithVariantDosage]])]

  def singleMarkerTestOverBatch(
    iter: SNPMajorLocusIterator,
    covariates: Frame[Individual, String, Double],
    phenotypes: List[(String, PhenotypeScale, Seq[String])],
    model: GeneticModel,
    interactionnames: Seq[String],
    interactionmodels: Seq[InteractionModel],
    threads: Int,
    individualSubset: Option[Set[Individual]],
    wholeGenomeEpistasisScreen: Boolean,
    prefilterWithScoreTest: Boolean
  )(reducefunction: (I, I) => I): Try[I] = {

    val interactionnamesPlaceHolder = if (interactionnames.size > 0) interactionnames.map(x => Some(x)) else List(None)

    def buildNonNAList(data: Array[Double]): Array[Int] = {
      val n = data.size
      val buff = scala.collection.mutable.ArrayBuffer[Int]()

      var i = 0
      while (i < n) {
        if (!data(i).isNaN) {
          buff.append(i)
        }
        i += 1
      }
      buff.toArray

    }

    def subsetArray(data: Array[Double], indices: Array[Int]): Array[Double] = {
      val r = Array.ofDim[Double](indices.size)
      var i = 0
      val n = indices.length
      while (i < n) {
        r(i) = data(indices(i))
        i += 1
      }
      r
    }

    def zipMap(d1: Array[Double], d2: Array[Double], fun: (Double, Double) => Double): Array[Double] = {
      assert(d1.length == d2.length)
      val r = Array.ofDim[Double](d1.size)
      var i = 0
      val n = d1.length
      while (i < n) {
        r(i) = fun(d1(i), d2(i))
        i += 1
      }
      r
    }

    val individuals = iter.individuals
    val indIdx = Index(individuals: _*)

    val preparationPhenotype = interactionnamesPlaceHolder.flatMap { iname =>
      phenotypes.map {
        case (phenoName, scale, covariateNames) =>
          val (nonLeakyDataFrame, nonLeakyPheno, nullfit) = {
            val joined = prepareLeakyDataTable("intercept" +: (covariateNames ++ iname.toList).distinct :+ phenoName, covariates, DropSample).rfilterIx((ind: Individual) => individualSubset.map(_.contains(ind)).getOrElse(true))

            val nullfit: Option[Vec[Double]] = if (!prefilterWithScoreTest) None else {

              Regression.regression(
                covariates = joined.filterIx(_ != "intercept"),
                covariateNames = covariateNames,
                phenoscale = scale,
                phenoName = phenoName
              ).toOption.flatMap(_ match {
                  case x: RegressionResult => Some(Vec(x.intercept._1.slope +: covariateNames.map(y => x.covariate(y).get._1.slope): _*))
                  case f => {
                    None
                  }
                })
            }

            val pheno: Series[Individual, Double] = joined.firstCol(phenoName)
            val design = joined.col(("intercept" +: (covariateNames ++ iname.toList).distinct): _*)
            (design, pheno, nullfit)
          }

          val individualsToKeepSorted: List[Int] = indIdx(indIdx.toSeq.filter(i => nonLeakyPheno.index.contains(i) && nonLeakyDataFrame.rowIx.contains(i)): _*).sorted.toList

          // Reorder data frame rows, insert NA's if necessary
          val nonLeakySNPOrderDataFrame = scala.collection.immutable.ListMap(nonLeakyDataFrame.reindexRow(indIdx).toColSeq.map(x => x._1 -> x._2.toVec.toSeq.toArray): _*)
          val nonLeakySNPOrderDataFrameIndKept = nonLeakyDataFrame.reindexRow(indIdx).rowAt(individualsToKeepSorted: _*)
          val nonLeakySNPOrderPheno = nonLeakyPheno.reindex(indIdx).toVec

          val variableColumns = if (interactionmodels.isEmpty || interactionnames.isEmpty) 1 else 2

          (phenoName, scale, iname) -> ((
            individualsToKeepSorted.toArray,
            nonLeakySNPOrderDataFrame,
            nonLeakySNPOrderPheno,
            FixPart(nonLeakySNPOrderDataFrameIndKept.toMat, nonLeakySNPOrderPheno(individualsToKeepSorted: _*), variableColumns),
            FixPart(nonLeakySNPOrderDataFrameIndKept.filterIx(_ != iname).toMat, nonLeakySNPOrderPheno(individualsToKeepSorted: _*), variableColumns),
            nullfit
          ))
      }
    }.toMap

    val mapfunction: (A => I) = { list =>
      InMemoryBatchResult(
        phenotypes.flatMap {
          case (phenoName, phenoscale, covariateNames) =>
            interactionnamesPlaceHolder
              .flatMap { iname =>
                val (individualsToKeepSorted, nonLeakySNPOrderDataFrame, nonLeakySNPOrderPheno, fixPartWithInameShared, fixPartWithoutInameShared, nullfit) = preparationPhenotype((phenoName, phenoscale, iname))

                val fixPartWithIname = fixPartWithInameShared.copy
                val fixPartWithoutIname = fixPartWithoutInameShared.copy

                list.filter(locusdata => if (wholeGenomeEpistasisScreen) iname.get > locusdata._1.name.value else true)
                  .flatMap {
                    case (locusdata, variantDosages) =>

                      val data: Array[Double] = {
                        val arr = Array.ofDim[Double](variantDosages.size)
                        var i = 0
                        while (i < arr.size) {
                          val elem = variantDosages(i)
                          val recoded = if (elem.isDefined) recodeGenotype(elem.get.variantAlleleDosage, model) else Double.NaN
                          arr(i) = recoded
                          i += 1
                        }
                        arr
                      }

                      val nonnalistdata = buildNonNAList(data)

                      val snpdataseries = data

                      if (!hasVariance(data))
                        List(SingleSNPResult(Failure(new RuntimeException("No variation in Genotype. " + locusdata.name)), locusdata, model, "?", None))
                      else {

                        val idxToKeep1: Array[Int] = intersectOrderedArray(
                          individualsToKeepSorted,
                          nonnalistdata
                        )

                        // SNP data has no missing individuals in addition to what is missing in the phenotype, covariates or the interaction term
                        val noExtraMissingIndividuals = idxToKeep1.size == individualsToKeepSorted.size

                        // Is there a need to subset the rows?
                        val idxToKeep = if (individualsToKeepSorted.size == nonnalistdata.size && noExtraMissingIndividuals && individualsToKeepSorted.size == indIdx.length) None else Some(idxToKeep1)

                        val snpdataWithoutNA = if (idxToKeep.isEmpty) snpdataseries else subsetArray(snpdataseries, idxToKeep.get)

                        val phenoWithoutNA: Array[Double] = if (idxToKeep.isEmpty) nonLeakySNPOrderPheno else subsetArray(nonLeakySNPOrderPheno, idxToKeep.get)

                        val nonLeakySNPOrderDataFrameWithoutNA: Map[String, Array[Double]] = if (idxToKeep.isEmpty) nonLeakySNPOrderDataFrame else nonLeakySNPOrderDataFrame.map(x => x._1 -> subsetArray(x._2, idxToKeep.get))

                        val interactionterms = if (iname.isEmpty) Nil else
                          interactionmodels.map { imodel =>

                            (iname.get, imodel, "SNPx" + iname.get -> zipMap(nonLeakySNPOrderDataFrameWithoutNA(iname.get), snpdataWithoutNA, imodel.function))
                          }

                        if (interactionterms.size == 0) {

                          val designFrameWithoutNA =
                            if (noExtraMissingIndividuals && phenoscale == Linear) None
                            else Some(Mat(nonLeakySNPOrderDataFrameWithoutNA.map(x => Vec(x._2)).toSeq :+ Vec(snpdataWithoutNA): _*))

                          val fixvarpart =
                            if (!noExtraMissingIndividuals) None else
                              Some((fixPartWithIname, snpdataWithoutNA :: Nil))

                          singleMarkerTest(
                            nonLeakySNPOrderDataFrame.map(_._1).toSeq :+ "SNP",
                            designFrameWithoutNA,
                            fixvarpart,
                            phenoWithoutNA,
                            phenoscale,
                            phenoName,
                            nullfit.map(f => DataForScoreTest(Vec(f.toSeq :+ 0.0: _*), 0.05, 1))
                          ) :: Nil map (_ match {
                              case Left(e) => SingleSNPResult(Failure(new RuntimeException(e.toString + (if (e.isInstanceOf[Exception]) e.asInstanceOf[Exception].printStackTrace))), locusdata, model, phenoName, None) //
                              case Right(r: RegressionResult) => SingleSNPResult(Success(r), locusdata, model, phenoName, None)
                              case Right(e: FailedRegression) => SingleSNPResult(Failure(new RuntimeException(e.toString + (if (e.isInstanceOf[Exception]) e.asInstanceOf[Exception].printStackTrace))), locusdata, model, phenoName, None) //
                            })
                        } else {
                          interactionterms.map {
                            case (iname, imodel, iterm) =>

                              val columnsInDesign = {
                                val all = (nonLeakySNPOrderDataFrameWithoutNA.toSeq) :+ ("SNP", snpdataWithoutNA) :+ iterm
                                if (imodel.removeMainEffectOfInteraction) all.filterNot(_._1 == iname)
                                else all
                              }

                              val designFrameWithoutNA = if (noExtraMissingIndividuals && phenoscale == Linear) None
                              else Some(Mat(columnsInDesign.map(x => Vec(x._2)): _*))

                              val fixvarpart = if (noExtraMissingIndividuals)
                                Some(((if (imodel.removeMainEffectOfInteraction) fixPartWithoutIname else fixPartWithIname), snpdataWithoutNA :: iterm._2 :: Nil))
                              else None

                              singleMarkerTest(
                                columnsInDesign.map(_._1).toSeq,
                                designFrameWithoutNA,
                                fixvarpart,
                                phenoWithoutNA,
                                phenoscale,
                                phenoName,
                                nullfit.map(f => DataForScoreTest(Vec((f.toSeq :+ 0.0 :+ 0.0): _*), 0.05, 2))
                              ) match {
                                  case Left(e) => SingleSNPResult(Failure(new RuntimeException(e.toString + (if (e.isInstanceOf[Exception]) e.asInstanceOf[Exception].printStackTrace))), locusdata, model, phenoName, Some((iname, imodel))) //
                                  case Right(r: RegressionResult) => SingleSNPResult(Success(r), locusdata, model, phenoName, Some((iname, imodel)))
                                  case Right(e: FailedRegression) => SingleSNPResult(Failure(new RuntimeException(e.toString + (if (e.isInstanceOf[Exception]) e.asInstanceOf[Exception].printStackTrace))), locusdata, model, phenoName, Some((iname, imodel))) //

                                }
                          }
                        }
                      }
                  }
              }
        }
      ) :: Nil
    }

    MapReduceTraversal.traverse(
      collection = iter.loci.grouped(100),
      mapreduce = new MapReduceScheme[A, I] {
        val map = mapfunction
        val reduce = reducefunction
      },
      numberOfReducers = threads,
      numberOfMappers = threads
    ).map(i => reducefunction(i, Nil))

  }

  def recodeGenotype(minorDosage: Double, model: GeneticModel): Double = model match {
    case Additive => minorDosage
    case Recessive => scala.math.max(0, minorDosage - 1.0)
    case Dominant => scala.math.min(1.0, minorDosage)
    case HeterozygousAdvantage => if (minorDosage <= 1.0) minorDosage else (2.0 - minorDosage)
    case ImputationError => minorDosage match {
      case x if x <= 0.5 => minorDosage
      case x if x <= 1.0 => 1.0 - minorDosage
      case x if x <= 1.5 => minorDosage - 1.0
      case x if x <= 2.0 => 2.0 - minorDosage
    }
  }

  def recodeGenotypesDropNA(
    individuals: Index[Individual],
    genotypes: Vector[Option[WithVariantDosage]],
    model: GeneticModel
  ): Series[Individual, Double] = {
    val data = genotypes.map(x => x.map(y => recodeGenotype(y.variantAlleleDosage, model)).getOrElse(Double.NaN))
    val (dataNotNA, idxNotNA) = data.zipWithIndex.filter(x => !x._1.isNaN).unzip
    Series(Vec(dataNotNA: _*), individuals.at(idxNotNA.toArray))
  }

  def associationResultToByteArrays(r: Try[RegressionResult], locus: LocusData, model: GeneticModel, phenoname: String, interaction: Option[(String, InteractionModel)],
    includeFull: Boolean = true): (Iterable[Array[Byte]], Iterable[Array[Byte]]) = {

    val chr = locus.genomicLocation.map(_.chromosome).getOrElse(".")
    val bp = locus.genomicLocation.map(_.basePairPosition).getOrElse(".")
    val snpName = locus.name
    val test = model match {
      case Additive => "ADD"
      case Recessive => "REC"
      case Dominant => "DOM"
      case HeterozygousAdvantage => "HETADV"
      case ImputationError => "IMPERROR"
    }

    val interactionstring = interaction.map(x => x._1 + " " + x._2).getOrElse("")
    val prefix = if (interaction.isEmpty) "SNP" else "SNPx"

    r match {
      case Success(regressionResult: ScoreTestResult) => {
        val b = FullAssociationResult.encodeSize(FullAssociationResult(
          name = StringStore(snpName),
          genomicLocation = locus.genomicLocation.getOrElse(GenomicLocation(0, 0)),
          pValue = regressionResult.pValue,
          test = StringStore(test),
          effectSize = Double.NaN,
          statistic = regressionResult.statistic,
          error = None,
          allele = SimpleAllele.makeSingleton(locus.variant.map(_.toString).getOrElse(".")),
          phenotypeName = StringStore(phenoname),
          interaction = interaction.map(x => StringStore(x._1 + " " + x._2)),
          nonMiss = regressionResult.numberOfSamples,
          exception = Some("ST(df:" + regressionResult.df + ")")
        ).toBytes)
        (List(b), Nil)
      }
      case Success(regressionResult) => {
        val (snp, cov) = regressionResult.covariates.partition(_._1.startsWith(prefix))
        val snpstrings = snp.map {
          case (name, (effect, testresult)) =>
            val nonmissing = regressionResult.numberOfSamples
            val testStr = name.replaceAllLiterally("SNP", test)

            FullAssociationResult.encodeSize(FullAssociationResult(
              name = StringStore(snpName),
              genomicLocation = locus.genomicLocation.getOrElse(GenomicLocation(0, 0)),
              pValue = testresult.pValue,
              test = StringStore(testStr),
              effectSize = effect.slope,
              statistic = testresult.statistic,
              error = Some(effect.sd),
              allele = SimpleAllele.makeSingleton(locus.variant.map(_.toString).getOrElse(".")),
              phenotypeName = StringStore(phenoname),
              interaction = interaction.map(x => StringStore(x._1 + " " + x._2)),
              nonMiss = nonmissing,
              exception = None
            ).toBytes)
        }

        val covstrings = if (includeFull) cov.map {
          case (name, (effect, testresult)) =>
            val nonmissing = regressionResult.numberOfSamples
            val testStr = if (name.contains("SNP")) name.replaceAllLiterally("SNP", test) else name + "_" + test
            FullAssociationResult.encodeSize(FullAssociationResult(
              name = StringStore(snpName),
              genomicLocation = locus.genomicLocation.getOrElse(GenomicLocation(0, 0)),
              pValue = testresult.pValue,
              test = StringStore(testStr),
              effectSize = effect.slope,
              statistic = testresult.statistic,
              error = Some(effect.sd),
              allele = SimpleAllele.makeSingleton(locus.variant.map(_.toString).getOrElse(".")),
              phenotypeName = StringStore(phenoname),
              interaction = interaction.map(x => StringStore(x._1 + " " + x._2)),
              nonMiss = nonmissing,
              exception = None
            ).toBytes)

        }
        else Nil

        (snpstrings, covstrings)
      }
      case Failure(e) => {
        val err = FullAssociationResult.encodeSize(FullAssociationResult(
          name = StringStore(snpName),
          genomicLocation = locus.genomicLocation.getOrElse(GenomicLocation(-1, -1)),
          pValue = Double.NaN,
          test = StringStore(test),
          effectSize = Double.NaN,
          statistic = Double.NaN,
          error = Some(Double.NaN),
          allele = SimpleAllele.makeSingleton(locus.variant.map(_.toString).getOrElse(".")),
          phenotypeName = StringStore(phenoname),
          interaction = interaction.map(x => StringStore(x._1 + " " + x._2)),
          nonMiss = -1,
          exception = Some(e.toString)
        ).toBytes)
        (List(err), List(err))
      }
    }

  }

  def renderPlinkAssocLines(r: Try[RegressionResult], locus: LocusData, model: GeneticModel, phenoname: String, interaction: Option[(String, InteractionModel)],
    includeFull: Boolean = true): (Iterable[String], Iterable[String]) = {

    val chr = locus.genomicLocation.map(_.chromosome).getOrElse(".")
    val bp = locus.genomicLocation.map(_.basePairPosition).getOrElse(".")
    val snpName = locus.name
    val test = model match {
      case Additive => "ADD"
      case Recessive => "REC"
      case Dominant => "DOM"
      case HeterozygousAdvantage => "HETADV"
      case ImputationError => "IMPERROR"
    }

    val interactionstring = interaction.map(x => x._1 + " " + x._2).getOrElse("")
    val prefix = if (interaction.isEmpty) "SNP" else "SNPx"

    r match {
      case Success(regressionResult: ScoreTestResult) => {
        val l = List(chr + " " + snpName + " " + bp + " " + locus.variant.getOrElse(".") + " " + test + s" ${regressionResult.numberOfSamples} NA ${formatDouble(regressionResult.statistic, 4)} ${regressionResult.pValue} " + phenoname + " " + interactionstring + s" SCORETEST(df:${regressionResult.df}")
        (l, l)
      }
      case Success(regressionResult) => {
        val (snp, cov) = regressionResult.covariates.partition(_._1.startsWith(prefix))
        val snpstrings = snp.map {
          case (name, (effect, testresult)) =>
            val nonmissing = regressionResult.numberOfSamples
            val testStr = name.replaceAllLiterally("SNP", test)
            chr + " " + snpName + " " + bp + " " + locus.variant.getOrElse(".") + " " + testStr + " " + nonmissing + " " + formatDouble(effect.slope, 4) + " " + formatDouble(testresult.statistic, 4) + " " + testresult.pValue + " " + phenoname + " " + interactionstring + " " + formatDouble(effect.sd, 4)
        }

        val covstrings = if (includeFull) cov.map {
          case (name, (effect, testresult)) =>
            val nonmissing = regressionResult.numberOfSamples
            val testStr = name + "_" + test
            chr + " " + snpName + " " + bp + " " + locus.variant.getOrElse(".") + " " + testStr + " " + nonmissing + " " + formatDouble(effect.slope, 4) + " " + formatDouble(testresult.statistic, 4) + " " + testresult.pValue + " " + phenoname + " " + interactionstring + " " + formatDouble(effect.sd, 4)

        }
        else List("")

        (snpstrings, covstrings)
      }
      case Failure(e) => {
        val err = List(chr + " " + snpName + " " + bp + " " + locus.variant.getOrElse(".") + " " + test + " NA NA NA NA " + phenoname + " " + interactionstring + " NA error:" + e.toString)
        (err, err)
      }
    }

  }

  private def singleMarkerTest(
    covarnamesWithIntercept: Seq[String],
    covariatesNonLeaky: Option[Mat[Double]],
    fixvarpart: Option[(FixPart, List[Array[Double]])],
    outcome: Array[Double],
    phenoscale: PhenotypeScale,
    phenoName: String,
    dataForScoreTest: Option[DataForScoreTest]
  ): Either[_, RegressionResultOrFailure] = {

    val covarnames = covarnamesWithIntercept

    phenoscale match {
      case Count | Rank => {

        catchToLeft(PoissonRegression.poissonRegression(
          covariateMatrix = covariatesNonLeaky.get,
          outcome = Vec(outcome: _*),
          covariateNames = covarnamesWithIntercept,
          maxIter = 100,
          relativeTolerance = 1E-5
        ))

      }
      case Linear => {

        val covarnamesWithOutIntercept = covarnames filterNot (_ == "intercept")
        if (fixvarpart.isEmpty) {
          val designmatrix = covariatesNonLeaky.get
          catchToLeft(interpretRawResultsLight(
            linearRegression(DataForRegression(outcome, designmatrix, covarnamesWithOutIntercept), 0.0),
            covarnamesWithOutIntercept, 0.0
          ))
        } else {
          val fix = fixvarpart.get._1
          val flattened = {
            val res = Array.ofDim[Double](fixvarpart.get._2.length * fixvarpart.get._2.head.length)
            val n = fixvarpart.get._2.length
            val m = fixvarpart.get._2.head.length
            var i = 0
            while (i < n) {
              System.arraycopy(fixvarpart.get._2(i), 0, res, i * m, m)
              i += 1
            }
            res
          }
          val variable = new org.ejml.simple.SimpleMatrix(fixvarpart.get._2.size, fixvarpart.get._2.head.length, true, flattened: _*)
          catchToLeft(interpretRawResultsLight(
            linearRegressionFixPart(fix, variable, outcome, 0.0),
            covarnamesWithOutIntercept, 0.0
          ))
        }

      }
      case Logistic => {
        val outcome2 = outcome.map(convertToBooleanNonMissing)

        LogisticRegression.logisticRegressionFullData(
          includedCovariatesMat = covariatesNonLeaky.get,
          filteredOutcomes = outcome2,
          covariateNamesWithIntercept = covarnames,
          maxIter = 40,
          epsilon = 1E-5,
          doScoreTest = dataForScoreTest
        )
      }
    }
  }

}
