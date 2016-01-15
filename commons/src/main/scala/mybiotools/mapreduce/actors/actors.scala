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

// MapReduce with akka actors. Obsolete and overly complex, see ParIterator.scala
package mybiotools.mapreduce.actors

import akka.actor.{ ActorRef, Actor, Props, ActorSystem, PoisonPill, ActorLogging }
import akka.routing.BalancingPool
import scala.collection.mutable.Queue
import com.typesafe.config.{ ConfigFactory, Config }
import akka.dispatch.PriorityGenerator
import akka.dispatch.BoundedPriorityMailbox
import scala.concurrent.duration._
import akka.pattern.ask
import scala.concurrent.Future

case class WorkPiece[A](content: A)
case class Reduceable[I](content: I)
case class Reduceables[I](op1: I, op2: I)
case object LastMessage
case object Free
case object CanIProduce
case class LastMessageWithCount(n: Long)

class MapActor[A, I](
    reducemaster: ActorRef,
    map: A => I
) extends Actor with ActorLogging {

  override def receive = {
    case w: WorkPiece[A] => {
      // log.info(w.toString)
      // log.info("block")
      reducemaster ! Reduceable(map(w.content))
      // log.info("unblock")
    }
  }
}

class ReduceActor[I](
    targetActor: ActorRef,
    reduce: (I, I) => I
) extends Actor with ActorLogging {
  override def receive = {
    case Reduceables(o1, o2) => {
      // log.info("block")
      targetActor ! Reduceable(reduce(o1.asInstanceOf[I], o2.asInstanceOf[I]))
      // log.info("unblock")
    }
  }
}

class BufferActor(recipient: ActorRef) extends Actor with ActorLogging {
  override def receive = {
    case x => {
      // log.info("block")
      recipient forward x
      // log.info("unblock")
    }
  }
}

class ReduceMaster[I](
    numberOfReducers: Int,
    mapReduceMaster: ActorRef,
    reduce: (I, I) => I
) extends Actor with ActorLogging {
  private val buffer = Queue[I]()
  private var received = 0L

  // This actor has an unbounded mailbox and forwards every message to self
  val bufferActor = context.actorOf(Props(new BufferActor(self)).withDispatcher("unbounded-pinned"), name = "bufferActor")

  val reduceRouter = context.actorOf(BalancingPool(numberOfReducers).props(routeeProps = Props(new ReduceActor(bufferActor, reduce))).withDispatcher("reducerouter-bounded-pinned"), name = "reducerouter")

  def done(expected: Long) = {
    // log.info("rec" + received + ",exp: " + expected + "," + (received >= 2 * expected - 1))

    received >= 2 * expected - 1
  }

  override def receive = {
    case x: Reduceable[I] => {
      received += 1
      if (buffer.size < 1) {
        // log.info("s" + x.toString)
        buffer += x.content
        mapReduceMaster ! Free
      } else {
        // log.info("y" + x)
        val o1 = buffer.dequeue
        // log.info("block")
        reduceRouter ! Reduceables(o1, x.content)
        // log.info("unblock")
      }
    }
    case lwc: LastMessageWithCount => if (!done(lwc.n)) {
      bufferActor forward lwc
    } else {
      // log.info(buffer.toString)
      sender ! buffer.headOption
    }

  }

}

class MapReduceMaster[A, I](
    numberOfReducers: Int,
    numberOfMappers: Int,
    bufferFactor: Double,
    map: A => I,
    reduce: (I, I) => I
) extends Actor with ActorLogging {

  val reduceMaster = context.actorOf(Props(new ReduceMaster(numberOfReducers, self, reduce)).withDispatcher("reducemaster-bounded-pinned"), name = "reducemaster")

  val mapRouter = context.actorOf(BalancingPool(numberOfMappers).props(routeeProps = Props(new MapActor(reduceMaster, map))).withDispatcher("maprouter-bounded-pinned"), name = "maprouter")

  def behaviourAfterLastMessage: Receive = {
    case x: WorkPiece[_] => { log.info("message after last") }
  }

  override def postStop {
    // log.info("MRM stop")
  }

  def free = concurrentJobs < math.min(numberOfMappers, numberOfReducers) * bufferFactor

  var received = 0L
  var concurrentJobs = 0

  var producers = Set[ActorRef]()

  override def receive = {
    case CanIProduce => {
      if (free) {
        sender ! true
      } else producers += sender
    }
    case Free => {
      concurrentJobs -= 1
      if (free) {
        val xo = producers.headOption
        xo.foreach { x =>
          producers -= x
          x ! true
        }
      }
    }
    case x: WorkPiece[A] => {
      // log.info(x.toString)
      // log.info("block")
      mapRouter ! x
      // log.info("unblock")
      received += 1
      concurrentJobs += 1
    }
    case LastMessage => {
      // log.info("lastmessage")
      reduceMaster forward LastMessageWithCount(received)
      context.become(behaviourAfterLastMessage)
    }
  }

}

class MapReduceSystem[A, I](
    numberOfReducers: Int,
    numberOfMappers: Int,
    bufferFactor: Double,
    map: A => I,
    reduce: (I, I) => I
) {

  private val strconfig = s"""  
  |akka.loggers = []
  |akka.loglevel = "ERROR"
  |akka.actor.unstarted-push-timeout = -1s
  |akka.logger-startup-timeout = 10000s 
  |
  |unbounded-pinned {
  |executor = "thread-pool-executor"
  |type = PinnedDispatcher
  |}
  |
  |akka.actor.deployment {
  | /mrmaster/maprouter {
  |   router = balancing-pool
  |   nr-of-instances = $numberOfMappers
  |   pool-dispatcher {
  |    fork-join-executor.parallelism-min = 1
  |    fork-join-executor.parallelism-max = $numberOfMappers
  |    throughput = 100
  |   }
  | }
  |
  |/mrmaster/reducemaster/reducerouter {
  |   router = balancing-pool
  |   nr-of-instances = $numberOfReducers
  |   pool-dispatcher {
  |    fork-join-executor.parallelism-min = 1
  |    fork-join-executor.parallelism-max = $numberOfReducers
  |    throughput = 100
  |   }
  | }
  |}
  |  
  |maprouter-bounded-pinned {
  |executor = "thread-pool-executor"
  |type = PinnedDispatcher
  |}
  |
  |reducerouter-bounded-pinned {
  |executor = "thread-pool-executor"
  |type = PinnedDispatcher
  |}
  |
  |reducemaster-bounded-pinned {
  |executor = "thread-pool-executor"
  |type = PinnedDispatcher
  |}
  |
  |mrmmaster-bounded-pinned {
  |executor = "thread-pool-executor"
  |type = PinnedDispatcher
  |}
  |
  |""".stripMargin
  private val config = ConfigFactory.parseString(strconfig)
  private val system = ActorSystem("MapReduce" + this.hashCode, config)

  private val mrm = system.actorOf(
    Props(new MapReduceMaster[A, I](
      numberOfReducers,
      numberOfMappers,
      bufferFactor,
      map,
      reduce
    )).withDispatcher("mrmmaster-bounded-pinned"),
    name = "mrmaster"
  )

  def add(a: A) = {
    implicit val to = akka.util.Timeout(168, java.util.concurrent.TimeUnit.HOURS)
    scala.concurrent.Await.ready(mrm ? CanIProduce, scala.concurrent.duration.Duration.Inf)
    mrm ! WorkPiece(a)
  }

  def last: Future[Option[I]] = {
    implicit val to = akka.util.Timeout(168, java.util.concurrent.TimeUnit.HOURS)
    val x = (mrm ? LastMessage).asInstanceOf[Future[Option[I]]]
    x
  }

  def close = {
    system.shutdown
  }

}