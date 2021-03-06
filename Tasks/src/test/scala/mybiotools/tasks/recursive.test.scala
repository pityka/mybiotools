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

package mybiotools.tasks
import org.scalatest._
import akka.testkit.TestKit
import akka.testkit.ImplicitSender
import akka.testkit.EventFilter
import scala.concurrent.duration._

import scala.io.Source
import mybiotools.tasks._
import mybiotools.tasks.simpletask._
import akka.actor.{ Actor, PoisonPill, ActorRef, Props, ActorSystem }
import akka.actor.Actor._
import scala.concurrent._
import duration._
import Duration._
import com.typesafe.config.ConfigFactory
import scala.concurrent.ExecutionContext.Implicits.global

object Fib {

  def serial(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case x => serial(n - 1) + serial(n - 2)
  }

  case class FibInput(n: Option[Int], tag: Option[List[Boolean]]) extends SimplePrerequisitive[FibInput]
  object FibInput {
    def apply(n: Int): FibInput = FibInput(Some(n), tag = Some(Nil))
  }

  case class FibOut(n: Int) extends Result

  object fibtask {
    def apply(in: FibInput)(implicit ce: TaskSystemComponents): ProxyTaskActorRef =
      newTask(
        in, identity[FibInput], CPUMemoryRequest(cpu = 1, memory = 1)
      ) {

        case (
          FibInput(
            Some(n),
            Some(tag)), ce) =>
          import ce._
          implicit val components = ce.toTaskSystemComponents

          // log.info("FIB " + n + " " + tag)

          n match {
            case 0 => FibOut(0)
            case 1 => FibOut(1)
            case n =>
              {
                val f1 = fibtask(FibInput(Some(n - 1), Some(false :: tag))).?[FibOut]
                val f2 = fibtask(FibInput(Some(n - 2), Some(true :: tag))).?[FibOut]
                val f3 = for {
                  r1 <- f1
                  r2 <- f2
                } yield FibOut(r1.n + r2.n)
                LauncherActor.block(CPUMemoryRequest(1, 100)) {
                  Await.result(f3, atMost = 500 seconds)
                }
              }

          }

      }

  }
}

class RecursiveTestSuite extends FunSuite with Matchers with BeforeAndAfterAll {
  val string = """
akka.loglevel = "INFO"
tasks.cacheEnabled = false
tasks.disableRemoting = true

"""

  val system = customTaskSystem(new LocalConfiguration(4, 1000), ConfigFactory.parseString(string))
  import Fib._
  import system._

  test("long") {
    val n = 16
    val r = fibtask(FibInput(n)).?![FibOut].n
    expectResult(r)(serial(n))
  }

  override def afterAll {
    Thread.sleep(1500)
    system.shutdown

  }
}