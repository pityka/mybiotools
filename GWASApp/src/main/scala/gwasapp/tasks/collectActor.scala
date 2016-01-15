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

package gwasapp.tasks

import akka.actor.{ ActorRef, Actor, Props, ActorSystem, PoisonPill, ActorLogging }
import akka.routing.BalancingPool
import scala.collection.mutable.Queue
import com.typesafe.config.{ ConfigFactory, Config }
import akka.dispatch.PriorityGenerator
import akka.dispatch.BoundedPriorityMailbox
import scala.concurrent.duration._
import akka.pattern.ask
import scala.concurrent.Future
import mybiotools.gwascommons.gwas.GWAS._
import mybiotools.gwascommons.gwas._
import mybiotools.gwascommons._
import scala.util._
import java.io.OutputStream
import mybiotools.stat._

case object AskCollector
case object CollectorAnswer
case class NumberOfTestsDone(n: Long)

class CollectorActor(
    snponlywriter: OutputStream,
    effectWriter: OutputStream,
    keepPValueThreshold: Double,
    numberOfPhenotypes: Long,
    numberOfInteractions: Long,
    numberOfInteractionModels: Long,
    writeFullResults: Boolean
) extends Actor with ActorLogging {

  var jobCounter = 0

  val normalQuantile = math.abs((new org.apache.commons.math3.distribution.NormalDistribution()).inverseCumulativeProbability(keepPValueThreshold))

  override def receive = {
    case AskCollector => if (jobCounter < 10) {
      jobCounter += 1
      sender ! CollectorAnswer
    } else {
      self forward AskCollector
    }

    case results: InMemoryBatchResult => {
      jobCounter -= 1
      val numberOfTestsDone = results.results.map { singlesnp =>
        val r = singlesnp.regressionResult
        val ld = singlesnp.locus
        val model = singlesnp.geneticModel
        val phenoName = singlesnp.phenoName
        val interaction = singlesnp.interaction
        if (keepPValueThreshold == 1.0 || r.toOption.map(_.covariates.filterKeys(_.startsWith("SNP")).map(x => x._1 -> math.abs(x._2._1.slope / x._2._1.sd)).max._2 >= normalQuantile).getOrElse(false)) {
          val (snpstrings, covstrings) = associationResultToByteArrays(r, ld, model, phenoName, interaction, writeFullResults)

          snpstrings.foreach { line =>
            snponlywriter.write(line)
            if (writeFullResults) {
              effectWriter.write(line)
            }

          }
          if (writeFullResults) {
            covstrings.foreach { line =>
              effectWriter.write(line)
            }
          }

        }

        r match {
          case x: Success[_] => 1L
          case x: Failure[_] if phenoName == "?" => numberOfPhenotypes * math.max(1, numberOfInteractions) * math.max(1, numberOfInteractionModels)
          case x: Failure[_] => 1L
        }
      }.sum
      sender ! NumberOfTestsDone(numberOfTestsDone)
    }
  }
}