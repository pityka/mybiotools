package settest

import mybiotools.gwascommons._
import mybiotools.gwascommons.associationresults._
import mybiotools.stringstore._

import org.scalatest.FunSpec
import org.scalatest.Matchers
import mybiotools.saddlehelpers._
import org.saddle._
import settest.tests._
import org.apache.commons.math3.random.{ RandomGenerator, RandomDataGenerator, Well44497b }

class PowerSpec extends FunSpec with Matchers {

  def power_linear(
    tests: List[settest.Test],
    numSamples: Int,
    numFeatures: Int,
    n: Int,
    effectSize: Double,
    significanceLevel: Double,
    rand: RandomGenerator
  ) = {

    def simulateRandomColumn(freq: Double): Vec[Double] = Vec(1 to numSamples map (i => new RandomDataGenerator(rand).nextBinomial(2, freq).toDouble): _*)
    val x = Mat(1 to numFeatures map (i => simulateRandomColumn(0.2)): _*)

    def makeY(x: Mat[Double], effectSize: Double): Vec[Double] = {
      Vec(0 until x.numRows map { i => new RandomDataGenerator(rand).nextGaussian(0.0, 1.0) + effectSize * x.raw(i, 0)
      }: _*)
    }

    val inds: Index[Individual] = Index(1 to numSamples map (i => Individual(i.toString)): _*)

    val ps = 1 to n map { i =>
      val yy = makeY(x, effectSize)

      val frame = Frame(
        ("Y" -> Series(yy, inds)) +:
          (("intercept") -> Series(vec.ones(inds.length), inds)) +:
          x.cols.zipWithIndex.map(i => i._2.toString -> Series(i._1, inds)): _*
      )

      tests.map { test =>
        test -> test.runTest(
          data = frame,
          phenoName = "Y",
          phenoScale = Linear,
          covariateNames = Seq(),
          snpNames = 0 until numFeatures map (i => i.toString)
        ).get.pValue
      }.toMap
    }

    tests.map { test =>
      test -> ps.count(_(test) <= significanceLevel).toDouble / ps.size
    }.toMap
  }

  describe("dfs") {
    ignore("dfs") {
      val effectSizes = 1 to 10 map { i =>
        (i - 1).toDouble -> power_linear(
          tests = List(
            tests.SkatTest(tests.Skat.SKATStandard, "davies", (0.5, 0.5)),
            tests.GlobalTest(mybiotools.stat.GlobalTest.BetaPDFAtHalfMean(0.5, 0.5))
          ),
          numSamples = 500,
          numFeatures = 100,
          n = 1000,
          effectSize = (i - 1).toDouble / 10,
          significanceLevel = 0.05,
          new Well44497b(1)
        )
      }
      println(effectSizes)
    }
  }

}