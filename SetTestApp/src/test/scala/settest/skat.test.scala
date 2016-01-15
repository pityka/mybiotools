package settest

import mybiotools.gwascommons._
import mybiotools.gwascommons.associationresults._
import mybiotools.stringstore._

import org.scalatest.FunSpec
import org.scalatest.Matchers
import mybiotools.saddlehelpers._
import org.saddle._
import settest.tests._

class SkatSpec extends FunSpec with Matchers {

  describe("write tped") {
    val frame: Frame[Individual, String, Double] = Frame(
      Individual("f1", "1") -> Series("1_rs1" -> 0.0),
      Individual("f2", "1") -> Series("1_rs1" -> 1.0)
    ).T
    val tpedstrings = Skat.writeTpedToString(frame, 0.1)
    val expectedtped = """0 1_rs1 0 0 B B A B"""
    val expectedtfam = """f1 1 0 0 0 -9
|f2 1 0 0 0 -9""".stripMargin
    it("simple") {
      tpedstrings.tped should equal(expectedtped)
      tpedstrings.tfam should equal(expectedtfam)
    }
  }

  describe("skat pheno write") {

    val individuals = List(Individual("1"), Individual("2"))

    val covariates = Frame(Individual("1") -> Series("cov1" -> 1.0, "cov2" -> 2.0, "ph" -> 1.0), Individual("2") -> Series("cov1" -> Double.NaN, "cov2" -> 3.0, "ph" -> Double.NaN)).T
    val covariateNames = List("cov1", "cov2")
    it("test") {
      val res = Skat.prepareSkatPhenoStrings(individuals, covariates, covariateNames, "ph", Linear)
      val expectedPheno = "1.0\nNA"
      val expectedCovar = "2.0\n3.0"
      res.pheno should equal(expectedPheno)
      res.covar should equal(expectedCovar)

    }
    it("old") {
      def old(
        individuals: Seq[Individual],
        covariates: Frame[Individual, String, Double],
        covariateNames: Seq[String],
        phenoName: String,
        phenoscale: PhenotypeScale
      ): (String, String) = {

        import org.saddle.scalar._

        val indset = individuals.toSet

        val covariatesInOrder: Seq[(Individual, Series[String, Double])] = covariates.toRowSeq.sortBy(x => individuals.indexOf(x._1)).filter(x => indset.contains(x._1))

        def convertValue(x: Double, phenoscale: PhenotypeScale) =
          if (phenoscale == Linear) x
          else x - 1.0

        val phenotypeValuesInOrder: Seq[Tuple1[Option[Double]]] = covariatesInOrder.map(x => Tuple1(Scalar.scalarToOption(x._2.first(phenoName)).map(x => convertValue(x, phenoscale))))

        val covarsToWrite: (Seq[Seq[Option[Double]]], Seq[String]) = {
          (
            covariatesInOrder.map { x => covariateNames.map { n => Scalar.scalarToOption(x._2.first(n)) } },
            covariateNames
          )
        }

        import mybiotools.tabular._
        import TabSerialization._
        import RProtocol._

        val covariateString = TableWriter.write(toTab(covarsToWrite))
        val phenoString = TableWriter.write(toTab(phenotypeValuesInOrder))

        (phenoString, covariateString)

      }

      val res = old(individuals, covariates, covariateNames, "ph", Linear)

      val expectedPheno = "V1\r\n1.0\r\nNA"
      val expectedCovar = "cov1,cov2\r\n1.0,2.0\r\nNA,3.0"
      res._1 should equal(expectedPheno)
      res._2 should equal(expectedCovar)
    }
  }

}