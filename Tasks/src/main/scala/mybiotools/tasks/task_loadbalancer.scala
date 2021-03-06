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

import akka.actor.{ Actor, PoisonPill, ActorRef, Cancellable, Props }
import akka.actor.Actor._
import scala.concurrent.Future
import scala.concurrent.duration._
import java.util.concurrent.{ TimeUnit, ScheduledFuture }
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit.{ SECONDS }
// import mybiotools.tasks.util.PhiAccrualFailureDetector
import akka.remote.DeadlineFailureDetector
import akka.remote.FailureDetector.Clock
import akka.remote.DisassociatedEvent

import scala.collection.immutable.Seq
import TaskSystemTimeouts._
import scala.collection.mutable.Queue
import akka.pattern.ask

import mybiotools.config.Config.configInstance
import mybiotools.eq._
import monitor._

// case object ShutdownTaskSystem

@SerialVersionUID(1L)
private case object ATaskWasForwarded

@SerialVersionUID(1L)
private case class QueueInfo(q: Map[ScheduleTask, List[ActorRef]])

@SerialVersionUID(1L)
private case object GetQueueInformation

@SerialVersionUID(1L)
private case class QueryTask(sch: ScheduleTask, ac: ActorRef) extends KryoSerializable

@SerialVersionUID(1L)
private case class LauncherDown(ac: ActorRef) extends KryoSerializable

@SerialVersionUID(1L)
private case class TaskDone(sch: ScheduleTask) extends KryoSerializable

@SerialVersionUID(1L)
private case class TaskFailedMessageToQueue(sch: ScheduleTask, cause: Throwable) extends KryoSerializable

@SerialVersionUID(1L)
private case class TaskFailedMessageToProxy(sch: ScheduleTask, cause: Throwable) extends KryoSerializable

@SerialVersionUID(1L)
private case class AnswerFromCache(message: Either[TaskNotFoundInCache, Option[Result]], sender: ActorRef, sch: ScheduleTask) extends KryoSerializable

@SerialVersionUID(1L)
private case class AskForWork(resources: CPUMemoryAvailable) extends KryoSerializable

@SerialVersionUID(1L)
private case object CheckHeartBeat

private class HeartBeatActor(target: ActorRef) extends Actor with akka.actor.ActorLogging {

  private var scheduledHeartBeats: Cancellable = null

  private var failureDetector = new DeadlineFailureDetector(FailureDetectorConstants.acceptableHeartbeatPause)

  override def preStart {
    log.debug("HeartBeatActor start for: " + target + " " + failureDetector.acceptableHeartbeatPause)

    import context.dispatcher

    scheduledHeartBeats =
      context.system.scheduler.schedule(
        initialDelay = 0 seconds,
        interval = LauncherActorHeartBeatInterval,
        receiver = self,
        message = CheckHeartBeat
      )

    // context.system.eventStream.subscribe(self, classOf[RemoteClientShutdown])
    // context.system.eventStream.subscribe(self, classOf[RemoteServerClientDisconnected])
  }

  override def postStop {
    scheduledHeartBeats.cancel
    log.info("HeartBeatActor stopped.")
  }

  private def targetDown {
    context.system.eventStream.publish(LauncherDown(target))

    self ! PoisonPill
  }

  def receive = {
    case DisassociatedEvent(localAddress, remoteAddress, inbound) if remoteAddress === target.path.address => {
      log.warning("DisassociatedEvent received. TargetDown.")
      targetDown
    }
    // case RemoteServerClientDisconnected(transport, Some(remoteAddress)) if remoteAddress == target.path.address => {
    //   log.info("RemoteServerClientDisconnected. TargetDown.")
    //   targetDown
    // }
    case CheckHeartBeat => {
      if (!failureDetector.isAvailable) {
        targetDown
      } else {
        target ! Ping
      }

    }

    case Pong | true => {
      failureDetector.heartbeat

    }
  }

}

class TaskQueue extends Actor with akka.actor.ActorLogging {

  // ActorRef here is the proxy of the task
  private val queuedTasks = collection.mutable.Map[ScheduleTask, List[ActorRef]]()

  // Map(task -> (launcher,allocated,list of proxies))
  private val routedMessages = scala.collection.mutable.Map[ScheduleTask, (ActorRef, CPUMemoryAllocated, List[ActorRef])]()

  private val knownLaunchers = scala.collection.mutable.HashSet[ActorRef]()

  private def enQueue(sch: ScheduleTask, ac: List[ActorRef]) {
    if (!routedMessages.contains(sch)) {
      queuedTasks.get(sch) match {
        case None => queuedTasks.update(sch, ac)
        case Some(acs) => queuedTasks.update(sch, (ac ::: acs).distinct)
      }
    }
  }

  // This is true, while waiting for response from the tasklauncher
  private var negotiation = false

  private def removeFromRoutedMessages(sch: ScheduleTask) {
    // Remove from the list of sent (running) messages
    routedMessages.remove(sch)
  }

  private def taskDone(sch: ScheduleTask) {
    // Remove from the list of sent (running) messages
    removeFromRoutedMessages(sch)
    // If present in the queue, remove from the queue
    // val msgs2 = queue.filter(x => x._1 == sch)
    // msgs2.foreach(x => queue.dequeueAll(_ == x))
    queuedTasks.remove(sch)
  }

  private def taskFailed(sch: ScheduleTask, cause: Throwable) {
    // Remove from the list of sent (running) messages
    routedMessages.get(sch).foreach {
      case (launcher, allocation, proxies) =>
        routedMessages.remove(sch)
        if (TaskSystemConstants.resubmitFailedTask) {
          log.error(cause, "Task execution failed ( resubmitting infinite time until done): " + sch.toString)
          enQueue(sch, proxies)
          log.info("Requeued 1 message. Queue size: " + queuedTasks.keys.size)
        } else {
          proxies.foreach { ac => ac ! TaskFailedMessageToProxy(sch, cause) }
          log.error(cause, "Task execution failed: " + sch.toString)
        }
    }

  }

  private def launcherCrashed(crashedLauncher: ActorRef) {
    // put back the jobs into the queue
    val msgs = routedMessages.toSeq.filter(_._2._1 === crashedLauncher).map(_._1)
    msgs.foreach { (sch: ScheduleTask) =>
      val proxies = routedMessages(sch)._3
      routedMessages.remove(sch)
      enQueue(sch, proxies)
    }
    log.info("Requeued " + msgs.size + " messages. Queue size: " + queuedTasks.keys.size)

    knownLaunchers -= crashedLauncher
  }

  override def preStart = {
    log.debug("TaskQueue starting.")
  }

  override def postStop {
    log.info("TaskQueue stopped.")
  }

  def receive = {
    case m: ScheduleTask => {
      log.debug("Received ScheduleTask.")
      if ((queuedTasks.contains(m) && (!queuedTasks(m).has(sender)))) {
        enQueue(m, sender :: Nil)
      } else if (routedMessages.get(m).map {
        case (_, _, proxies) =>
          !proxies.isEmpty && !proxies.contains(sender)
      }.getOrElse(false)) {
        log.warning("Scheduletask received multiple times from different proxies. Not queuing this one, but delivering result if ready. {}", m)
        val (launcher, allocation, proxies) = routedMessages(m)
        routedMessages.update(m, (launcher, allocation, sender :: proxies))
      } else {
        val ch = sender
        m.cacheActor ! CheckResult(m, ch)
      }
    }
    case x: AnswerFromCache => {
      val ch = x.sender
      val m = x.sch
      log.debug("Cache answered.")
      x.message match {
        case Right(Some(r)) => {
          log.debug("Replying with a Result found in cache.")
          ch ! (MessageFromTask(r.asInstanceOf[Result]))
        }
        case Right(None) => {
          log.debug("Task is not found in cache. Enqueue. ")
          enQueue(m, ch :: Nil)
          ch ! ATaskWasForwarded
        }
        case Left(_) => {
          log.debug("Task is not found in cache. Enqueue. ")
          enQueue(m, ch :: Nil)
          ch ! ATaskWasForwarded
        }

      }
    }
    case AskForWork(resource) => {
      if (!negotiation) {
        log.debug("AskForWork. Sender: {}. Resource: {}. Negotition state: {}. Queue state: {}", sender, resource, negotiation, queuedTasks.map(x => (x._1.description.taskImplementation, x._1.resource)).toSeq)

        val launcher = sender
        val dequeued = queuedTasks.find { case (k, v) => resource.canFulfillRequest(k.resource) }
        dequeued.foreach {
          case (k, v) =>
            queuedTasks.remove(k)
        }

        if (dequeued.isEmpty) {
          // log.debug(s"Nothing to dequeue. Available resource of launcher: $resource . Queue: $queuedTasks")
        }

        dequeued.foreach { task =>
          negotiation = true
          log.debug("Dequeued. Sending task to " + launcher)
          log.debug(negotiation.toString)

          if (!knownLaunchers.contains(launcher)) {
            knownLaunchers += launcher
            context.actorOf(Props(new HeartBeatActor(launcher)).withDispatcher("heartbeat"), "heartbeatOf" + launcher.path.address.toString.replace("://", "___") + launcher.path.name)
            context.system.eventStream.subscribe(self, classOf[LauncherDown])
          }

          try {

            // val resp = (launcher.?(ScheduleWithProxy(task._1, new ActorInEnvelope(task._2)))(timeout = 30 seconds))
            val resp = (launcher.?(ScheduleWithProxy(task._1, task._2))(timeout = 30 seconds))

            import context.dispatcher

            resp onFailure {
              case error => {
                log.debug("TaskLauncher did not Ack'd back on sending task. Requeueing.")
                negotiation = false
                queuedTasks.update(task._1, task._2)
              }
            }

            resp onSuccess {
              case Ack(allocated) => {
                log.debug("ScheduleTask sent to launcher.")
                log.debug("Queue size: " + queuedTasks.size)
                negotiation = false
                routedMessages += (task._1 -> (launcher, allocated, task._2))
              }
            }
          } catch {
            case x: java.nio.channels.ClosedChannelException => {
              log.debug("TaskLauncher did not Ack'd back on sending task. Requeueing.")
              queuedTasks.update(task._1, task._2)
            }
            case e: Throwable => {
              negotiation = false
              log.error(e.getMessage)
              throw e
            }
          }
        }
      } else {
        log.debug("AskForWork received but currently in negotiation state.")
      }
    }
    case m: TaskDone => {
      taskDone(m.sch)
    }
    case TaskFailedMessageToQueue(sch, cause) => taskFailed(sch, cause)
    case m: LauncherDown => {
      log.info("LauncherDown: " + m)

      launcherCrashed(m.ac)

    }
    case Ping => {
      sender ! true
      sender ! Pong
    }

    case HowLoadedAreYou => {
      // EventHandler.debug(this,queue.toString+routedMessages.toString)
      val qs = QueueStat(queuedTasks.toList.map(_._1).map(x => (x.description.taskImplementation, x.resource)).toList, routedMessages.toSeq.map(x => x._1.description.taskImplementation -> x._2._2).toList)
      context.system.eventStream.publish(qs)
      sender ! qs
    }

    case GetQueueInformation => sender ! QueueInfo(queuedTasks.toMap)

    case m => log.warning("Unhandled message. " + m.toString)
  }
}