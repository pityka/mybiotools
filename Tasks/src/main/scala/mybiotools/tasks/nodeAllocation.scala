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

import akka.actor.{ Actor, PoisonPill, ActorRef, Props, Cancellable }
import scala.concurrent.duration._
import java.util.concurrent.{ TimeUnit, ScheduledFuture }
import TaskSystemTimeouts._
import TaskAllocationConstants._
import mybiotools._
import java.net.InetSocketAddress
import akka.actor.Actor._
import akka.event.LoggingAdapter
import scala.concurrent._
import monitor._
import scala.util._

private case class Idling(state: Long)

private case object Working

private case object MeasureTime

private case object WhatAreYouDoing

private case object PrepareForShutdown

private case object ReadyForShutdown

private case class NodeComingUp(node: Node)

private case class InitFailed(nodename: PendingJobId)

private case class NodeIsDown(node: Node)

case class Node(name: RunningJobId, size: CPUMemoryAvailable, launcherActor: ActorRef)

trait ShutdownNode {

  def shutdownRunningNode(nodeName: RunningJobId): Unit

  def shutdownPendingNode(nodeName: PendingJobId): Unit

}

trait CreateNode {
  def requestNewNodes(types: Map[CPUMemoryRequest, Int]): Unit
}

trait DecideNewNode {
  def needNewNode(q: QueueStat, registeredNodes: Seq[CPUMemoryAvailable], pendingNodes: Seq[CPUMemoryAvailable]): Map[CPUMemoryRequest, Int]
}

trait NodeRegistry {

  def allRegisteredNodes: Set[Tuple2[RunningJobId, CPUMemoryAvailable]] // name and resources

  def pendingNodes: Set[Tuple2[PendingJobId, CPUMemoryAvailable]]

  def registerNode(n: Node): Unit

  def deregisterNode(n: Node): Unit

  val unmanagedResource: CPUMemoryAvailable

  def initfailed(n: PendingJobId): Unit

}

trait GridJobRegistry extends NodeRegistry with CreateNode with ShutdownNode {

  def log: LoggingAdapter

  def requestOneNewJobFromGridScheduler(k: CPUMemoryRequest): Try[Tuple2[PendingJobId, CPUMemoryAvailable]]

  def initializeNode(n: Node): Unit

  def convertRunningToPending(p: RunningJobId): Option[PendingJobId] = Some(PendingJobId(p.value))

  private val jobregistry = scala.collection.mutable.Set[Tuple2[RunningJobId, CPUMemoryAvailable]]()

  private val pending = scala.collection.mutable.Set[Tuple2[PendingJobId, CPUMemoryAvailable]]()

  private def toPend(p: PendingJobId, size: CPUMemoryAvailable) {
    pending += ((p, size))
  }

  def allRegisteredNodes = Set[Tuple2[RunningJobId, CPUMemoryAvailable]](jobregistry.toSeq: _*)

  def pendingNodes = {
    Set[Tuple2[PendingJobId, CPUMemoryAvailable]](pending.toSeq: _*)
  }

  def requestNewNodes(types: Map[CPUMemoryRequest, Int]) = {
    if (types.values.sum > 0) {
      if (MaxNodes > (jobregistry.size + pending.size)) {

        log.info("Request " + types.size + " node. One from each: " + types.keySet)

        types.foreach {
          case (request, n) =>
            val jobinfo = requestOneNewJobFromGridScheduler(request)

            jobinfo match {
              case Failure(e) => log.warning("Request failed: " + e.getMessage + " " + e)
              case _ => ()
            }

            jobinfo.foreach { ji =>
              val jobid = ji._1
              val size = ji._2
              toPend(jobid, size)
            }
        }

      } else {
        log.info("New node request will not proceed: pending nodes or reached max nodes. max: " + MaxNodes + ", pending: " + pending.size + ", running: " + jobregistry.size)
      }
    }
  }

  def refreshPendingList: List[PendingJobId] = pending.toList.map(_._1)

  private def registerJob(id: RunningJobId, size: CPUMemoryAvailable) {
    val elem = (id, size)
    jobregistry += elem
    val pendingID = convertRunningToPending(id)
    if (pendingID.isDefined) {
      scala.util.Try {
        pending -= (pending.filter(_._1 == (pendingID.get)).head)
      }
    } else {
      val activePendings = refreshPendingList
      val removal = pending.toSeq.map(_._1).filter(x => !activePendings.contains(x))
      removal.foreach { r =>
        pending -= (pending.filter(_._1 == r).head)
      }
    }

    log.debug(s"registerJob: $id , $size . ")
  }

  def registerNode(n: Node) {
    log.debug("Registering node: " + n)
    val jobid = n.name
    val size = n.size
    registerJob(jobid, size)
    initializeNode(n)
  }

  def deregisterNode(n: Node) {
    jobregistry -= ((n.name, n.size))
  }

  def initfailed(pendingID: PendingJobId) {
    (pending.filter(_._1 == (pendingID)).headOption).foreach { x =>
      pending -= x
    }
  }

}

trait SimpleDecideNewNode extends DecideNewNode {

  def needNewNode(q: QueueStat, registeredNodes: Seq[CPUMemoryAvailable], pendingNodes: Seq[CPUMemoryAvailable]): Map[CPUMemoryRequest, Int] = {
    // QueueStat(queued: List[(String, CPUMemoryRequest)], running: List[(String,CPUMemoryAllocated)]

    val resourceNeeded: List[CPUMemoryRequest] = q.queued.map(_._2)

    val availableResources: List[CPUMemoryAvailable] = (registeredNodes ++ pendingNodes).toList

    val (remainingResources, allocatedResources) = resourceNeeded.foldLeft((availableResources, List[CPUMemoryRequest]())) {
      case ((available, allocated), request) =>

        val (prefix, suffix) = available.span(x => !x.canFulfillRequest(request))
        val chosen = suffix.headOption
        chosen.foreach(x => assert(x.canFulfillRequest(request)))

        val transformed = chosen.map(_.substract(request))
        if (chosen.isDefined) (prefix ::: (transformed.get :: suffix.tail)).filterNot(_.isEmpty) -> (request :: allocated)
        else (available, allocated)
    }

    val nonAllocatedResources: Map[CPUMemoryRequest, Int] = {
      val map1 = resourceNeeded.groupBy(x => x).map(x => x._1 -> x._2.size)
      val map2 = allocatedResources.groupBy(x => x).map(x => x._1 -> x._2.size)
      (mybiotools.addMaps(map1, map2)(_ - _)).filter(x => { assert(x._2 >= 0); x._2 > 0 })

    }

    // val allJobsEventuallyAllocatable = {
    //   val availdistinct = available.distinct
    //   resourceNeeded.distinct.forall(request => availdistinct.exists(resource => resource.canFulfillRequest(request._1)))
    // }

    if (!nonAllocatedResources.isEmpty
      && (pendingNodes.size < MaxPendingNodes)) {

      nonAllocatedResources

    } else Map()
  }

}

trait LSFShutdown extends ShutdownNode {

  def log: LoggingAdapter

  def shutdownRunningNode(nodeName: RunningJobId) {
    try {
      val command = "bkill " + nodeName.value.toInt.toString
      log.info("Issuing command: " + command)
      execGetStreamsAndCode(command, atMost = 30 seconds)
    } catch {
      case e: Exception => log.error(e, "Some error happened during bkill.")
    }
    ()
  }

  def shutdownPendingNode(n: PendingJobId) = shutdownRunningNode(RunningJobId(n.value))
}

trait LSFNodeRegistryImp extends Actor with GridJobRegistry {

  val masterAddress: InetSocketAddress

  def requestOneNewJobFromGridScheduler(resourceRequest: CPUMemoryRequest): Try[Tuple2[PendingJobId, CPUMemoryAvailable]] = {
    val jarpath = NewNodeJarPath

    // val NumberOfCoresOfNewLauncher = resourceRequest.cpu._1
    // val RequestedMemOfNewNode = resourceRequest.memory

    val command = scala.sys.process.Process("bsub" :: "-n" :: NumberOfCoresOfNewLauncher.toString
      :: "-M " + RequestedMemOfNewNode * 1000
      :: """-R span[ptile=%d] """.format(NumberOfCoresPerNode)
      :: "-R rusage[mem=%d]".format(RequestedMemOfNewNode) ::
      "-u" :: EmailAddress.toString ::
      "-q" :: QueueName.toString :: Nil)

    val command2 = """java -Xmx%sM -Dhosts.RAM=%s -Dhosts.list="$LSB_HOSTS" %s -Dconfig.file=%s -Dhosts.master=%s -Dhosts.gridengine=LSF -XX:ParallelGCThreads=%s -XX:CICompilerCount=%s %s """.format(
      (RequestedMemOfNewNode * JVMMaxHeapFactor).toInt,
      RequestedMemOfNewNode,
      AdditionalSystemProperties.mkString(" "),
      System.getProperty("config.file"),
      masterAddress.getHostName + ":" + masterAddress.getPort,
      math.max(6, NumberOfCoresOfNewLauncher).toString,
      math.max(6, NumberOfCoresOfNewLauncher).toString,
      jarpath
    )

    val command2is = new java.io.ByteArrayInputStream(command2.getBytes("UTF-8"));
    val jobid = Try {

      val (stdout, stderr, success) = execGetStreamsAndCode(command #< command2is, atMost = 30 seconds)

      log.debug("Bsub says:" + stdout.mkString("\n") ++ stderr.mkString("\n"))
      if (success && stderr == Nil) {
        val id = "<\\d+>".r.findFirstIn(stdout.head).get.tail.dropRight(1).toInt.toString
        (PendingJobId(id), CPUMemoryAvailable(NumberOfCoresOfNewLauncher, RequestedMemOfNewNode))
      } else throw new RuntimeException("Error in bsub: " + stdout.mkString("\n") + "\n" + stderr.mkString("\n"))

    }

    jobid
  }

  def initializeNode(node: Node) {
    val ac = node.launcherActor //.revive

    // I think these actors could be moved to a normal Dispatcher
    val ackil = context.actorOf(Props(new LSFNodeKiller(ac, node)).withDispatcher("my-pinned-dispatcher"), "nodekiller" + node.name.value.replace("://", "___"))

  }

}

class LSFNodeRegistry(
  val masterAddress: InetSocketAddress,
  val targetQueue: ActorRef,
  override val unmanagedResource: CPUMemoryAvailable
) extends LSFNodeRegistryImp with NodeCreatorImpl with SimpleDecideNewNode with LSFShutdown with akka.actor.ActorLogging

trait NodeCreatorImpl extends Actor with CreateNode with DecideNewNode with NodeRegistry with ShutdownNode {

  def log: LoggingAdapter

  val targetQueue: ActorRef

  private var scheduler: Cancellable = null

  override def preStart {
    log.info("NodeCreator start. Monitoring actor: " + targetQueue)

    import context.dispatcher

    scheduler = context.system.scheduler.schedule(
      initialDelay = QueueCheckInitialDelay seconds,
      interval = QueueCheckInterval seconds,
      receiver = self,
      message = MeasureTime
    )

    context.system.eventStream.subscribe(self, classOf[NodeIsDown])

  }

  override def postStop {
    scheduler.cancel
    log.info("NodeCreator stopping.")
    allRegisteredNodes.foreach { node =>
      log.info("Shutting down node " + node)
      shutdownRunningNode(node._1)
    }
    pendingNodes.foreach { node =>
      shutdownPendingNode(node._1)
    }
    log.info("Shutted down all registered nodes.")
  }

  def startNewNode(types: Map[CPUMemoryRequest, Int]) {
    requestNewNodes(types)
  }

  def receive = {
    case MeasureTime => {
      log.debug("Tick from scheduler.")

      targetQueue ! HowLoadedAreYou
    }

    case m: QueueStat => {
      if (logQueueStatus) {
        log.info(s"Queued tasks: ${m.queued.size}. Running tasks: ${m.running.size}. Pending nodes: ${pendingNodes.size} . Running nodes: ${allRegisteredNodes.size}. Largest request: ${m.queued.sortBy(_._2.cpu).lastOption}/${m.queued.sortBy(_._2.memory).lastOption}")
      }
      startNewNode(needNewNode(m, allRegisteredNodes.toSeq.map(_._2) ++ Seq(unmanagedResource), pendingNodes.toSeq.map(_._2)))
    }

    case NodeComingUp(node) => {
      log.debug("NodeComingUp: " + node)
      registerNode(node)
    }

    case NodeIsDown(node) => {
      log.debug("NodeIsDown: " + node)
      deregisterNode(node)
    }

    case InitFailed(pending) => {
      log.error("Node init failed: " + pending)
      initfailed(pending)
      shutdownPendingNode(pending)
    }
    case GetNodeRegistryStat => sender ! NodeRegistryStat(allRegisteredNodes, pendingNodes)

  }

}

class LSFNodeKiller(
  val targetLauncherActor: ActorRef,
  val targetNode: Node
) extends NodeKillerImpl with LSFShutdown with akka.actor.ActorLogging

trait NodeKillerImpl extends Actor with ShutdownNode {

  def log: LoggingAdapter

  val targetLauncherActor: ActorRef

  val targetNode: Node

  private var scheduler: Cancellable = null

  override def preStart {
    log.debug("NodeKiller start. Monitoring actor: " + targetLauncherActor + " on node: " + targetNode.name)

    import context.dispatcher

    scheduler = context.system.scheduler.schedule(
      initialDelay = 0 seconds,
      interval = NodeKillerMonitorInterval seconds,
      receiver = self,
      message = MeasureTime
    )

    context.system.eventStream.subscribe(self, classOf[LauncherDown])

  }

  override def postStop {
    scheduler.cancel
    log.info("NodeKiller stopped.")
  }

  var lastIdleSessionStart: Long = System.nanoTime()

  var lastIdleState: Long = 0L

  var targetIsIdle = true

  def shutdown {
    log.info("Shutting down target node: name= " + targetNode.name + " , actor= " + targetLauncherActor)
    shutdownRunningNode(targetNode.name)
    context.system.eventStream.publish(NodeIsDown(targetNode))
    scheduler.cancel
    self ! PoisonPill
  }

  def receive = {
    case LauncherDown(down) if targetLauncherActor == down => shutdown
    case MeasureTime => {
      if (targetIsIdle &&
        (System.nanoTime() - lastIdleSessionStart) >= KillIdleNodeAfterSeconds * 1E9) {
        try {
          log.info("Target is idle. Start shutdown sequence. Send PrepareForShutdown to " + targetLauncherActor)
          targetLauncherActor ! PrepareForShutdown
          log.info("PrepareForShutdown sent to " + targetLauncherActor)
        } catch {
          case x: java.nio.channels.ClosedChannelException => shutdown
        }
      } else {
        targetLauncherActor ! WhatAreYouDoing
      }
    }

    case Idling(state) => {
      if (lastIdleState < state) {
        lastIdleSessionStart = System.nanoTime()
        lastIdleState = state
      }
      targetIsIdle = true

    }

    case Working => {
      targetIsIdle = false
    }

    case ReadyForShutdown => shutdown
  }

}

trait SelfShutdown extends Actor with akka.actor.ActorLogging {

  def id: RunningJobId

  def balancerActor: ActorRef

  def shutdownRunningNode(d: RunningJobId)

  def shutdown {
    shutdownRunningNode(id)
  }

  override def preStart {
    context.system.eventStream.subscribe(self, classOf[LauncherDown])
    context.system.eventStream.subscribe(self, classOf[akka.remote.DisassociatedEvent])

  }
  def receive = {
    case LauncherDown(actor) if actor == balancerActor => {
      log.error("LauncherDown for balancerActor received. Shutting down.")
      shutdown
    }
    case de: akka.remote.DisassociatedEvent => {
      log.error("DisassociatedEvent. " + de.remoteAddress + " vs " + balancerActor.path.address)
      if (de.remoteAddress == balancerActor.path.address) {
        shutdown
      }
    }
  }
}

class LSFSelfShutdown(val id: RunningJobId, val balancerActor: ActorRef) extends SelfShutdown with LSFShutdown

class SGESelfShutdown(val id: RunningJobId, val balancerActor: ActorRef) extends SelfShutdown with DRMAA.DRMAAShutdown

trait ShutdownReaper extends Reaper {

  def id: RunningJobId

  def shutdownRunningNode(d: RunningJobId)

  // Shutdown
  def allSoulsReaped(): Unit = {
    log.info(s"All souls reaped. Call shutdown node on $id.")
    shutdownRunningNode(id)
  }
}

class LSFReaper(val id: RunningJobId) extends ShutdownReaper with LSFShutdown
class SGEReaper(val id: RunningJobId) extends ShutdownReaper with DRMAA.DRMAAShutdown
