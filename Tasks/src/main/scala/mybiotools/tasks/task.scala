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

import akka.actor.{ Actor, PoisonPill, ActorRef, ActorContext, ActorRefFactory }
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.Future
import mybiotools.config.Config.configInstance
import TaskSystemTimeouts._
import akka.pattern.ask
import java.io.File
import scala.concurrent.ExecutionContext

case class ProxyTaskActorRef(private val actor: ActorRef) {
  def ~>(child: ProxyTaskActorRef) = {
    ProxyTask.addTarget(actor, child.actor)
    child
  }
  def ?[T <: Result](implicit ec: ExecutionContext) = ProxyTask.getBackResultFuture(actor).asInstanceOf[Future[T]]
  def ?![T <: Result](implicit ec: ExecutionContext) = ProxyTask.getBackResult(actor).asInstanceOf[T]

  def <~(result: Result) {
    ProxyTask.sendStartData(actor, List(result))
  }

}

// This is the output of a task
trait Result extends KryoSerializable {
  def verifyAfterCache(implicit service: FileServiceActor, context: ActorRefFactory): Boolean = true
}

abstract class ResultWithSharedFiles(sf: SharedFile*) extends Result with Product {
  def files = sf
  override def verifyAfterCache(implicit service: FileServiceActor, context: ActorRefFactory) = files.forall(_.isAccessible) && productIterator.filter(_.isInstanceOf[ResultWithSharedFiles]).forall(_.asInstanceOf[ResultWithSharedFiles].verifyAfterCache)
}

// This is the prerequisitives of a task
trait Prerequisitive[+A] extends KryoSerializable {
  def ready: Boolean

  def persistent: Prerequisitive[A] = this
}

trait SimplePrerequisitive[+A] extends Prerequisitive[A] with Product { self =>

  def ready = productIterator.forall {
    case x: Option[_] => x.isDefined
    case _ => throw new RuntimeException("SimplePrerequisitive should be a product of Options")
  }
}

@SerialVersionUID(1L)
case class NodeLocalCacheActor(actor: ActorRef) extends KryoSerializable

@SerialVersionUID(1L)
case class QueueActor(actor: ActorRef) extends KryoSerializable

@SerialVersionUID(1L)
case class CacheActor(actor: ActorRef) extends KryoSerializable

@SerialVersionUID(1L)
case class LauncherActor(actor: ActorRef) extends KryoSerializable

case class HostForMPI(hostname: String, slots: Int)

object LauncherActor {
  def block[T](request: CPUMemoryRequest)(k: => T)(implicit l: LauncherActor) = {
    l.actor ! BlockOn(request)
    val x = k
    l.actor ! BlockOff(request)
    k
  }
}

case class ComputationEnvironment(
    val resourceAllocated: CPUMemoryAllocated,
    val availableHostsForMPI: Seq[HostForMPI],
    implicit val components: TaskSystemComponents,
    implicit val log: akka.event.LoggingAdapter,
    implicit val launcher: LauncherActor
) extends KryoSerializable {

  implicit def fs: FileServiceActor = components.fs

  implicit def actorsystem: akka.actor.ActorSystem = components.actorsystem

  implicit def filePrefix: FileServicePrefix = components.filePrefix

  implicit def nodeLocalCache: NodeLocalCacheActor = components.nodeLocalCache

  implicit def queue: QueueActor = components.queue

  implicit def cache: CacheActor = components.cache

  def mpiHostFile = {
    val tmp = mybiotools.TempFile.createTempFile(".txt")
    mybiotools.writeToFile(tmp, availableHostsForMPI.map(x => s"${x.hostname} slots=${x.slots}").mkString("\n"))
    tmp
  }

  def toTaskSystemComponents =
    components

}

case class AddTarget(target: ActorRef)
case class AddTargetNoCheck(target: ActorRef)
case class WhatAreYourChildren(notification: ActorRef)
case object GetBackResult
private case class ChildrenMessage(value: Set[ActorRef], notification: ActorRef)
private case class InternalMessageFromTask(actor: ActorRef, result: Result) extends KryoSerializable
private case class InternalMessageTaskFailed(actor: ActorRef, cause: Throwable) extends KryoSerializable
private case class FailureMessageFromProxyToProxy(cause: Throwable)

case class MessageFromTask(result: Result) extends KryoSerializable
case object SaveDone

object ProxyTask {

  def getBackResultFuture(actor: ActorRef, timeoutp: Int = ProxyTaskGetBackResult)(implicit ec: ExecutionContext): Future[Result] = {

    implicit val timout = akka.util.Timeout(timeoutp seconds)
    val f = (actor ? (GetBackResult)).asInstanceOf[Future[Result]]
    f.onFailure {
      case x => mybiotools.logger.Logger.error(x, "Exception in getbackresult: " + x)
    }
    f
  }

  def getBackResult(actor: ActorRef, timeoutp: Int = ProxyTaskGetBackResult)(implicit ec: ExecutionContext): Result = scala.concurrent.Await.result(getBackResultFuture(actor, timeoutp), timeoutp second)

  def addTarget(parent: ActorRef, child: ActorRef) {

    val f = parent ! AddTarget(child)
  }

  def sendStartData(target: ActorRef, stuff: Seq[Result]) {

    stuff.foreach { x =>
      (target ! x)
    }
  }

  def sendStartDataWithRetry(target: ActorRef, stuff: Seq[Result]) {
    sendStartData(target, stuff)

  }

}

private class Task[P <: Prerequisitive[P]](
    runTask: (P, ComputationEnvironment) => Result,
    launcherActor: ActorRef,
    balancerActor: ActorRef,
    fileServiceActor: ActorRef,
    globalCacheActor: ActorRef,
    nodeLocalCache: ActorRef,
    resourceAllocated: CPUMemoryAllocated,
    hostsForMPI: Seq[HostForMPI],
    fileServicePrefix: FileServicePrefix
) extends Actor with akka.actor.ActorLogging {

  override def preStart {
    log.debug("Prestart of Task class")
  }

  override def postStop {
    log.debug("Task stopped.")
  }

  private[this] var done = false
  private[this] var started = false
  private[this] var notificationRegister: List[ActorRef] = List[ActorRef]()
  private[this] val mainActor = this.self
  private var resultG: Result = null

  private def startTask(msg: P) {
    started = true

    Future {
      try {
        log.debug("Starttask from the executing dispatcher (future).")

        val result = runTask(
          msg,
          ComputationEnvironment(
            resourceAllocated,
            hostsForMPI,
            TaskSystemComponents(
              QueueActor(balancerActor),
              FileServiceActor(fileServiceActor),
              context.system,
              CacheActor(globalCacheActor),
              NodeLocalCacheActor(nodeLocalCache),
              fileServicePrefix
            ),
            getApplicationLogger(runTask)(context.system),
            LauncherActor(launcherActor)
          )
        )

        resultG = result

        log.debug("Task job ended. sending to launcher. from the executing dispatcher (future).")

        log.debug("Sending results over to proxies, etc: " + notificationRegister.toString)

        notificationRegister.foreach(_ ! MessageFromTask(resultG))

        launcherActor ! InternalMessageFromTask(mainActor, result)

        log.debug("Task ended. Result sent to launcherActor. Taking PoisonPill")

        self ! PoisonPill

      } catch {
        case x: Exception => {
          val y = x.getStackTrace
          x.printStackTrace()
          log.error(x, "Exception caught in the executing dispatcher of a task. " + x.getMessage)
          launcherActor ! InternalMessageTaskFailed(mainActor, x)
          self ! PoisonPill
        }
        case x: AssertionError => {
          val y = x.getStackTrace
          x.printStackTrace()
          log.error(x, "Exception caught in the executing dispatcher of a task. " + x.getMessage)
          launcherActor ! InternalMessageTaskFailed(mainActor, x)
          self ! PoisonPill
        }
      }
    }(context.dispatcher)
  }

  def receive = {
    case msg: Prerequisitive[_] => {
      log.debug("StartTask, from taskactor")
      startTask(msg.asInstanceOf[P])
    }
    case RegisterForNotification(ac) => {
      log.debug("Received: " + ac.toString)
      notificationRegister = ac :: notificationRegister
    }
    case x => log.debug("received unknown message" + x)
  }
}

abstract class ProxyTask(
    starter: ActorRef,
    fileServiceActor: ActorRef,
    fileServicePrefix: FileServicePrefix,
    cacheActor: ActorRef
) extends Actor with akka.actor.ActorLogging {

  protected type MyPrerequisitive <: Prerequisitive[MyPrerequisitive]

  protected type MyResult <: Result

  protected def resourceConsumed = CPUMemoryRequest(cpu = 1, memory = 500)

  def handleIncomingResult(r: Result) {
    incomings = updatePrerequisitive.apply((incomings, r))
  }

  val runTaskClass: java.lang.Class[_ <: Computation[MyPrerequisitive, MyResult]]

  private[this] var _targets: Set[ActorRef] = Set[ActorRef]()

  private[this] var _channels: Set[ActorRef] = Set[ActorRef]()

  def emptyResultSet: MyPrerequisitive

  def updatePrerequisitive: PartialFunction[(MyPrerequisitive, Result), MyPrerequisitive]

  protected var incomings: MyPrerequisitive = emptyResultSet

  private[this] var result: Option[Result] = None

  private var taskIsQueued = false

  private var targetNegotiation = 0

  private def distributeResult {
    log.debug("Distributing result to targets: " + _targets.toString + ", " + _channels.toString)
    result.foreach(r => _targets.foreach { t => t ! r })
    result.foreach(r => _channels.foreach { ch => ch ! r })
  }

  private def notifyListenersOnFailure(cause: Throwable) {
    _targets.foreach(t => t ! FailureMessageFromProxyToProxy(cause))
    _channels.foreach(t => t ! akka.actor.Status.Failure(cause))
  }

  private def startTask: Unit = {
    if (result.isEmpty) {

      val s = ScheduleTask(
        TaskDescription(runTaskClass.getName, incomings), resourceConsumed,
        starter, // new ActorInEnvelope(starter),
        fileServiceActor,
        fileServicePrefix,
        cacheActor
      )

      log.debug("proxy submitting ScheduleTask object to queue.")
      log.debug(s.description.hashCode.toString)
      log.debug(s.description.taskImplementation.hashCode.toString)
      log.debug(s.description.startData.hashCode.toString)

      starter ! s
    }
  }

  override def preStart() = {
    log.debug("ProxyTask prestart.")
    if (incomings.ready) {
      startTask
    }
  }

  override def postStop() = {
    log.debug("ProxyTask stopped.")
  }

  def receive = {
    case AddTarget(target) => {
      if (target == self) sender ! false
      else {
        targetNegotiation += 1
        target ! WhatAreYourChildren(sender)
      }

    }
    case WhatAreYourChildren(notificationActor) => {
      log.debug("whatareyourchildren")
      if (targetNegotiation > 0) {
        self forward WhatAreYourChildren(notificationActor)
      } else {
        sender ! ChildrenMessage(_targets, notificationActor)
      }
    }
    case ChildrenMessage(mytargets, notificationActor) => {
      targetNegotiation -= 1
      val newtarget = sender
      val success = if (newtarget == self || mytargets.contains(self)) {
        log.error("Adding " + newtarget.toString + " to the dependency graph would introduce a cycle in the graph.")
        false
      } else {
        log.debug("target added")
        _targets = _targets + newtarget
        result.foreach(r => newtarget ! r)
        true
      }
      notificationActor ! success
    }
    case msg: Result => {
      log.debug("Message received, handling in handleIncomingResult. ")
      handleIncomingResult(msg)
      if (incomings.ready && !taskIsQueued) startTask
    }
    case MessageFromTask(incomingResult) => {
      log.debug("Message received from: " + sender.toString + ", ")
      if (result.isEmpty) result = Some(incomingResult)
      distributeResult
    }
    case GetBackResult => {
      log.debug("GetBackResult message received. Registering for notification: " + sender.toString)
      _channels = _channels + sender //.asInstanceOf[Channel[Result]]
      distributeResult
    }
    case ATaskWasForwarded => {
      log.debug("The loadbalancer received the message and queued it.")
      taskIsQueued = true
    }
    case TaskFailedMessageToProxy(sch, cause) => {
      log.error(cause, "Execution failed. ")
      notifyListenersOnFailure(cause)
      self ! PoisonPill
    }
    case FailureMessageFromProxyToProxy(cause) => {
      notifyListenersOnFailure(cause)
      self ! PoisonPill
    }
    case msg => log.debug("Unhandled message " + msg.toString.take(100) + sender.toString)
  }

}

