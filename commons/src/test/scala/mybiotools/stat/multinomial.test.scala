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

package mybiotools.stat

import org.scalatest.FunSpec
import org.scalatest.PrivateMethodTester
import org.scalatest.Matchers
import mybiotools._
import org.scalatest.prop.Checkers
import org.scalacheck.Prop._
import org.scalacheck._
import org.apache.commons.math3.random.RandomDataGenerator

class MultiNomSpec extends FunSpec with Matchers with PrivateMethodTester with Checkers {

  describe("multinom") {
    it("naive method agrees conditional and expected") {

      val rnd = new RandomDataGenerator
      val errors: Seq[(Double, (Double, Double, Double))] = 1 to 10 flatMap { i =>

        val n = 15000
        val probs = {
          val b = 0 until n map (i => rnd.nextUniform(0.01, 1000.0))
          val sum = b.sum
          b.map(_ / sum)
        }
        val multinom = new MultinomialRandomGenerator[Int](rnd.getRandomGenerator, false, probs.zipWithIndex.map(_.swap): _*)

        val nsamples = 3000000
        val t1 = System.nanoTime
        val sample2 = multinom.sample(nsamples)
        val t2 = (System.nanoTime - t1)

        var t3 = System.nanoTime
        val sample1 = (1 to nsamples).map { i =>
          multinom.next
        }
        val t4 = System.nanoTime - t3

        println(t2.toDouble / t4)

        sample1.groupBy(x => x).toSeq.map(x => x._1 -> x._2.size).map {
          case (elem, draws1) =>
            val draws2 = sample2(elem)
            val expected = (probs(elem) * nsamples)

            println(draws2 + " " + draws1 + " " + expected)

            probs(elem) -> ((draws1.toDouble - expected) / expected,
              (draws2.toDouble - expected) / expected,
              (draws1.toDouble - draws2) / expected)
        }

      }

      errors.groupBy(x => (x._1 * 10.0).toInt / 10.0).toSeq.map {
        case (p, errors) =>
          val (naiveerror, conditionalerror, comparison) = (SummaryStat(errors.map(_._2._1)), SummaryStat(errors.map(_._2._2)), SummaryStat(errors.map(_._2._3)))
          p -> (naiveerror, conditionalerror, comparison)
      }.sortBy(_._1).foreach {
        case (p, (naive, conditional, comparison)) =>
          println(p + " " + naive.mean.format(4) + "/" + naive.stddev.format(4) + " " + conditional.mean.format(4) + "/" + conditional.stddev.format(4) + " " + comparison.mean.format(4) + "/" + comparison.stddev.format(4))

          math.abs(naive.mean) should be < 0.005
          math.abs(conditional.mean) should be < 0.005
          math.abs(comparison.mean) should be < 0.005
        // math.abs(naive.stddev - 0.0) should be < 0.05
        // math.abs(conditional.stddev - 0.0) should be < 0.05
        // math.abs(comparison.stddev - 0.0) should be < 0.05
      }

    }

  }

}