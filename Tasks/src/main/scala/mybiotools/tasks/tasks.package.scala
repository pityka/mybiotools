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

package mybiotools

import mybiotools.config.Config.configInstance
import java.io.File
import akka.actor.{ ActorRef, ActorSystem }
import scala.concurrent.Future
import akka.event.LogSource
import java.util.concurrent.TimeUnit.{ MILLISECONDS, NANOSECONDS, SECONDS }

import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

import scala.collection.JavaConversions._

import akka.actor.{ Props, ActorRefFactory, ActorContext }
import com.typesafe.config.{ Config, ConfigFactory }

package object tasks {

  def createLogger(s: AnyRef)(implicit component: TaskSystemComponents): akka.event.LoggingAdapter = component.getLogger(s)

  implicit def fs(implicit component: TaskSystemComponents): FileServiceActor = component.fs

  implicit def actorsystem(implicit component: TaskSystemComponents): ActorSystem = component.actorsystem

  implicit def filePrefix(implicit component: TaskSystemComponents): FileServicePrefix = component.filePrefix

  implicit def nodeLocalCache(implicit component: TaskSystemComponents): NodeLocalCacheActor = component.nodeLocalCache

  implicit def queue(implicit component: TaskSystemComponents): QueueActor = component.queue

  implicit def cache(implicit component: TaskSystemComponents): CacheActor = component.cache

  def remoteCacheAddress(implicit t: QueueActor, s: ActorContext): String = if (t.actor.path.address.host.isDefined && t.actor.path.address.port.isDefined)
    t.actor.path.address.host.get + ":" + t.actor.path.address.port.get
  else s.system.settings.config.getString("akka.remote.netty.tcp.hostname") + ":" + s.system.settings.config.getString("akka.remote.netty.tcp.port")

  def defaultTaskSystem: TaskSystem = defaultTaskSystem(configInstance, None)
  def defaultTaskSystem(string: String): TaskSystem = defaultTaskSystem(configInstance, Some(ConfigFactory.parseString(string)))
  def defaultTaskSystem(defaultConf: Config, extraConf: Option[Config]): TaskSystem = {
    val akkaconf = ConfigFactory.parseResources("akkaoverrides.conf")

    val conf = if (extraConf.isDefined)
      com.typesafe.config.ConfigFactory.defaultOverrides.withFallback(extraConf.get).withFallback(akkaconf).withFallback(defaultConf).withFallback(configInstance)
    else com.typesafe.config.ConfigFactory.defaultOverrides.withFallback(akkaconf).withFallback(defaultConf).withFallback(configInstance)
    new TaskSystem(MasterSlaveGridEngineChosenFromConfig, conf)
  }
  def customTaskSystem(hostConfig: MasterSlaveConfiguration, extraConf: Config): TaskSystem = {
    val akkaconf = ConfigFactory.parseResources("akkaoverrides.conf")

    val conf = com.typesafe.config.ConfigFactory.defaultOverrides.withFallback(extraConf).withFallback(akkaconf).withFallback(com.typesafe.config.ConfigFactory.defaultReference)
    new TaskSystem(hostConfig, conf)
  }

  /**
   *
   * This is a single threaded computation, do not use futures here
   */
  type Computation[A <: Prerequisitive[_], B <: Result] = Function2[A, ComputationEnvironment, B]

  type UpdatePrerequisitive[A <: Prerequisitive[A]] = PartialFunction[(A, Result), A]

  def identity[A <: Prerequisitive[A]]: UpdatePrerequisitive[A] = {
    case (x, _) => x
  }

  def newTask[A <: Result, B <: Prerequisitive[B]](
    prerequisitives: B,
    update: PartialFunction[(B, Result), B] = identity[B],
    resource: CPUMemoryRequest = CPUMemoryRequest(cpu = 1, memory = 500)
  )(f: Computation[B, A])(implicit components: TaskSystemComponents): ProxyTaskActorRef =
    {
      implicit val queue = components.queue
      implicit val fileService = components.fs
      implicit val cache = components.cache
      implicit val context = components.actorsystem
      implicit val prefix = components.filePrefix

      ProxyTaskActorRef(
        context.actorOf(
          Props(
            new ProxyTask(queue.actor, fileService.actor, prefix, cache.actor) {
              type MyPrerequisitive = B
              type MyResult = A
              def emptyResultSet = prerequisitives
              override def resourceConsumed = resource
              def updatePrerequisitive = update

              val runTaskClass = f.getClass

            }
          ).withDispatcher("proxytask-dispatcher")
        )
      )
    }

  def MasterSlaveGridEngineChosenFromConfig: MasterSlaveConfiguration = {
    if (TaskSystemConstants.disableRemoting) mybiotools.tasks.LocalConfigurationFromConfig
    else configInstance.getString("hosts.gridengine") match {
      case x if x == "LSF" => mybiotools.tasks.LSFMasterSlave
      case x if x == "SGE" => mybiotools.tasks.SGEMasterSlave
      case x if x == "EC2" => mybiotools.tasks.EC2MasterSlave
      case x if x == "NOENGINE" => mybiotools.tasks.MasterSlaveFromConfig
      case _ => mybiotools.tasks.MasterSlaveFromConfig
    }
  }

  def measureTime[T](group: String)(f: => T)(collector: ActorRef): T = {
    val t1 = System.nanoTime
    val x = f
    val t2 = System.nanoTime
    collector ! util.CollectableData(group, (t2 - t1) / 1E9)
    x
  }

  def getLogger(sourceObject: AnyRef)(implicit as: ActorSystem) = {
    implicit val logSource: LogSource[AnyRef] = new LogSource[AnyRef] {
      def genString(o: AnyRef): String = o.getClass.getName
      override def getClazz(o: AnyRef): Class[_] = o.getClass
    }
    akka.event.Logging(as, sourceObject)(logSource)
  }

  def getApplicationLogger(sourceObject: AnyRef)(implicit as: ActorSystem) = {
    implicit val logSource: LogSource[AnyRef] = new LogSource[AnyRef] {
      def genString(o: AnyRef): String = "APPLICATION-" + o.getClass.getName
      override def getClazz(o: AnyRef): Class[_] = o.getClass
    }
    akka.event.Logging(as, sourceObject)(logSource)
  }

  object TaskSystemTimeouts {
    // Everything in seconds
    val ProxyTaskGetBackResult = configInstance.getInt("tasks.proxytaskGetBackResultTimeoutInSeconds") //60 * 60 * 168 * 4
    val PingTimeout = 60
    val LauncherActorHeartBeatInterval = Duration(configInstance.getMilliseconds("tasks.failuredetector.heartbeat-interval"), MILLISECONDS) // seconds
    val WaitForSaveTimeOut = 60 * 2
  }

  object TaskSystemConstants {
    val fileSendChunkSize = configInstance.getBytes("tasks.fileSendChunkSize").toInt

    val includeFullPathInDefaultSharedName = configInstance.getBoolean("tasks.includeFullPathInDefaultSharedName")

    val resubmitFailedTask = configInstance.getBoolean("tasks.resubmitFailedTask")

    val logToStandardOutput = configInstance.getBoolean("tasks.stdout")

    val verifySharedFileInCache = configInstance.getBoolean("tasks.verifySharedFileInCache")

    val disableRemoting = configInstance.getBoolean("tasks.disableRemoting")

    val nonLocalFileSystems = configInstance.getStringList("tasks.nonLocalFileSystems").map(f => new java.io.File(f))

    val skipContentHashVerificationAfterCache = configInstance.getBoolean("tasks.skipContentHashVerificationAfterCache")

  }

  object FailureDetectorConstants {

    val acceptableHeartbeatPause = Duration(configInstance.getMilliseconds("tasks.failuredetector.acceptable-heartbeat-pause"), MILLISECONDS)
  }

  sealed trait GridEngine
  case object LSFGrid extends GridEngine
  case object EC2Grid extends GridEngine
  case object SGEGrid extends GridEngine
  case object NoGrid extends GridEngine
  case object SSHGrid extends GridEngine

  object TaskAllocationConstants {

    val KillIdleNodeAfterSeconds = configInstance.getLong("tasks.elastic.killIdleNodeAfterSeconds")

    val MaxNodes = configInstance.getInt("tasks.elastic.maxNodes")

    val NumberOfCoresOfNewLauncher = configInstance.getInt("tasks.elastic.lsf.newNodeSize")

    val NumberOfCoresPerNode = scala.util.Try(configInstance.getInt("tasks.elastic.lsf.span")).toOption.getOrElse(NumberOfCoresOfNewLauncher)

    val MaxPendingNodes = configInstance.getInt("tasks.elastic.maxPending")

    val NewNodeJarPath = configInstance.getString("tasks.elastic.mainClassWithClassPathOrJar")

    val RequestedMemOfNewNode = configInstance.getInt("tasks.elastic.lsf.requestedMemOfNewNode")

    val EmailAddress = configInstance.getString("tasks.elastic.lsf.email")

    val QueueName = configInstance.getString("tasks.elastic.lsf.queue")

    val AdditionalSystemProperties: List[String] = configInstance.getStringList("tasks.elastic.lsf.additionalSystemProperties").toArray.map(_.asInstanceOf[String]).toList

    val QueueCheckInterval = configInstance.getInt("tasks.elastic.queueCheckInterval")

    val QueueCheckInitialDelay = configInstance.getInt("tasks.elastic.queueCheckInitialDelay")

    val NodeKillerMonitorInterval = configInstance.getInt("tasks.elastic.nodeKillerMonitorInterval")

    val JVMMaxHeapFactor = configInstance.getDouble("tasks.elastic.jvmMaxHeapFactor")

    val logQueueStatus = configInstance.getBoolean("tasks.elastic.logQueueStatus")

  }

}