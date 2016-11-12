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

import mybiotools.tasks._
import akka.actor.{ Actor, ActorRef, PoisonPill, ActorSystem, Props, ActorRefFactory }
import mybiotools._
import java.io.File
import akka.actor.Actor._
import mybiotools.config.Config.configInstance
import java.net.InetSocketAddress
import java.net.NetworkInterface
import java.net.InetAddress
import DRMAA._
import scala.concurrent.duration._
import scala.concurrent._
import akka.pattern.ask
import com.typesafe.config.{ ConfigFactory, Config }
import scala.concurrent.Await
import collection.JavaConversions._
import akka.util.Timeout

sealed trait Role
object MASTER extends Role
object SLAVE extends Role

trait HostConfiguration {

  val myAddress: InetSocketAddress

  val myCardinality: Int

  val availableMemory: Int

}

trait CacheHostConfiguration {

  val cacheAddress: Option[InetSocketAddress]

}

// sge_hostname_var contains the current hostname
trait SGEHostConfiguration extends HostConfiguration {

  val sge_hostname_var: String

  private val myPort = {
    val s = new java.net.ServerSocket(0);
    val p = s.getLocalPort()
    s.close
    p
  }

  lazy val myAddress = new InetSocketAddress(sge_hostname_var, myPort)

}

// lsb_host_var contains the hostname as many times as many cpu is allocated on that node. It should contain only 1 hostname
trait LSFHostConfiguration extends HostConfiguration {

  val lsb_hosts_var: String

  val reservedCPU: Int

  private lazy val hostsAndSlots: Seq[(String, Int)] = {
    val splitted = lsb_hosts_var.split("\\s+").toList
    val unordered = splitted.groupBy(x => x).map(x => x._1 -> x._2.size).toSeq
    unordered.sortBy(x => splitted.indexOf(x._1))
  }

  private lazy val hosts: Tuple2[String, Int] = hostsAndSlots.head

  private val myPort = {
    val s = new java.net.ServerSocket(0);
    val p = s.getLocalPort()
    s.close
    p
  }

  private def mpiAvailable: Boolean = searchForFileInPath("mpirun")

  lazy val myAddress = new InetSocketAddress(hosts._1, myPort)

  lazy val myCardinality = math.max(0, hosts._2 - reservedCPU)

  lazy val hostsForMPI: Seq[HostForMPI] = {
    if (mpiAvailable) hostsAndSlots.map(x => HostForMPI(x._1, x._2))
    else Nil
  }

}

trait MasterSlaveConfiguration extends HostConfiguration with CacheHostConfiguration {

  lazy val master: InetSocketAddress = if (System.getProperty("hosts.master", "") != "") {
    val h = System.getProperty("hosts.master").split(":")(0)
    val p = System.getProperty("hosts.master").split(":")(1).toInt
    new InetSocketAddress(h, p)
  } else myAddress

  lazy val myRole = if (System.getProperty("hosts.master", "") != "") SLAVE else {
    if (myAddress == master) MASTER else SLAVE
  }

  lazy val cacheAddress: Option[InetSocketAddress] = if (configInstance.getString("hosts.remoteCacheAddress") != "none") {
    val h = configInstance.getString("hosts.remoteCacheAddress").split(":")(0)
    val p = configInstance.getString("hosts.remoteCacheAddress").split(":")(1).toInt
    Some(new InetSocketAddress(h, p))
  } else None

}

class LocalConfiguration(val myCardinality: Int, val availableMemory: Int) extends MasterSlaveConfiguration {
  override lazy val myRole = MASTER

  override lazy val master = new InetSocketAddress("localhost", 0)

  val myAddress = master
}

object LocalConfigurationFromConfig extends LocalConfiguration(configInstance.getInt("hosts.numCPU"), configInstance.getInt("hosts.RAM"))

/**
 * Needs a hosts.master system property to infer master location and role
 * Self address is bind to the hosts.hostname config.
 * Port is chosen automatically.
 * Cardinality is determined from hosts.numCPU config
 */
object MasterSlaveFromConfig extends MasterSlaveConfiguration {
  val myPort = {
    val s = new java.net.ServerSocket(0);
    val p = s.getLocalPort()
    s.close
    p
  }

  val hostname = configInstance.getString("hosts.hostname")

  val myAddress = new java.net.InetSocketAddress(hostname, myPort)

  val myCardinality = configInstance.getInt("hosts.numCPU")

  val availableMemory = configInstance.getInt("hosts.RAM")

}

/**
 * A master slave configuration for full manual setup.
 */
class ManualMasterSlaveConfiguration(
    val myCardinality: Int,
    val availableMemory: Int,
    role: Role,
    masterAddress: InetSocketAddress,
    val myAddress: InetSocketAddress
) extends MasterSlaveConfiguration {
  override lazy val myRole = role
  override lazy val master = masterAddress
}

object LSFMasterSlave extends MasterSlaveConfiguration with LSFHostConfiguration {

  val lsb_hosts_var: String = System.getProperty("hosts.list", "") match {
    case x if x == "" => configInstance.getString("hosts.list")
    case x => x
  }

  val reservedCPU: Int = configInstance.getInt("hosts.reservedCPU")

  // TODO ask LSF
  val availableMemory = configInstance.getInt("hosts.RAM")

}

// if hosts.master is not present, current is master.
object SGEMasterSlave extends MasterSlaveConfiguration with SGEHostConfiguration {

  val sge_hostname_var = (System.getProperty("hosts.hostname", "") match {
    case x if x == "" => System.getenv("HOSTNAME") match {
      case x if x == null => configInstance.getString("hosts.hostname")
      case x => x
    }
    case x => x
  })

  val myCardinality = (System.getProperty("hosts.numCPU", "") match {
    case x if x == "" => System.getenv("NSLOTS") match {
      case x if x == null => configInstance.getString("hosts.numCPU")
      case x => x
    }
    case x => x
  }).toInt

  val availableMemory = configInstance.getInt("hosts.RAM")

}

case class TaskSystemComponents(
    val queue: QueueActor,
    val fs: FileServiceActor,
    val actorsystem: ActorSystem,
    val cache: CacheActor,
    val nodeLocalCache: NodeLocalCacheActor,
    val filePrefix: FileServicePrefix
) {

  def getLogger(caller: AnyRef) = getApplicationLogger(caller)(actorsystem)

  def registerApplicationFileLogger(f: File) = {
    val fileLogger = actorsystem.actorOf(Props(new mybiotools.tasks.FileLogger(f, Some("APPLICATION"))))
    val fileLoggerAll = actorsystem.actorOf(Props(new mybiotools.tasks.FileLogger(new File(f.getAbsolutePath + ".log.all"))))
    actorsystem.eventStream.subscribe(fileLogger, classOf[akka.event.Logging.LogEvent])
    actorsystem.eventStream.subscribe(fileLoggerAll, classOf[akka.event.Logging.LogEvent])
    fileLogger
  }

  def childPrefix(name: String) = this.copy(filePrefix = this.filePrefix.append(name))
}

class TaskSystem private[tasks] (val hostConfig: MasterSlaveConfiguration, config: Config) {

  val configuration = {
    val actorProvider = hostConfig match {
      case x: LocalConfiguration => "akka.actor.LocalActorRefProvider"
      case _ => "akka.remote.RemoteActorRefProvider"
    }

    val numberOfAkkaRemotingThreads = if (hostConfig.myRole == MASTER) 6 else 2

    val configSource = s"""
task-worker-blocker-dispatcher.fork-join-executor.parallelism-max = ${hostConfig.myCardinality}
task-worker-blocker-dispatcher.fork-join-executor.parallelism-min = ${hostConfig.myCardinality}
akka {
  actor {
    provider = "${actorProvider}"
  }
  remote {
    enabled-transports = ["akka.remote.netty.tcp"]
    netty.tcp {
      hostname = "${hostConfig.myAddress.getHostName}"
      port = ${hostConfig.myAddress.getPort.toString}
      server-socket-worker-pool.pool-size-max = ${numberOfAkkaRemotingThreads}
      client-socket-worker-pool.pool-size-max = ${numberOfAkkaRemotingThreads}
    }
 }
}
  """ + (if (TaskSystemConstants.logToStandardOutput) """
    akka.loggers = ["akka.event.Logging$DefaultLogger"]
    """
    else "")

    val customConf = ConfigFactory.parseString(configSource)

    val classpathConf = ConfigFactory.load("akkaoverrides.conf")

    customConf.withFallback(config).withFallback(classpathConf)

  }

  val system = ActorSystem("tasks", configuration)

  val logger = system.actorOf(Props(new mybiotools.tasks.LogPublishActor(None)))
  system.eventStream.subscribe(logger, classOf[akka.event.Logging.LogEvent])

  private val tasksystemlog = tasks.getLogger(this)(system)

  def registerApplicationFileLogger(f: File) = components.registerApplicationFileLogger(f)

  def registerFileLogger(f: File) = {
    val fileLogger = system.actorOf(Props(new mybiotools.tasks.FileLogger(f)))
    system.eventStream.subscribe(fileLogger, classOf[akka.event.Logging.LogEvent])
    fileLogger
  }

  def registerFileLoggerToErrorStream(f: File) = {
    val fileLogger = system.actorOf(Props(new mybiotools.tasks.FileLogger(f)))
    system.eventStream.subscribe(fileLogger, classOf[akka.event.Logging.Error])
    fileLogger
  }

  def getLogger(caller: AnyRef) = getApplicationLogger(caller)(system)

  val ElasticNodeAllocationEnabled = config.getBoolean("tasks.elastic.enabled")

  val logFile: Option[File] = config.getString("tasks.logFile") match {
    case x if x == "" => None
    case x => Some(new File(x + System.nanoTime.toString + ".log"))
  }

  logFile.foreach { f =>
    registerFileLogger(f)
    registerFileLoggerToErrorStream(new File(f.getCanonicalPath + ".errors"))
  }

  tasksystemlog.info("My address, numCPU, memory and role: " + hostConfig.myAddress.toString + "," + hostConfig.myCardinality.toString + "," + hostConfig.availableMemory + "," + hostConfig.myRole.toString)

  if (hostConfig.myCardinality > Runtime.getRuntime().availableProcessors()) {
    tasksystemlog.warning("Number of CPUs in the machine is " + Runtime.getRuntime().availableProcessors + ". numCPU should not be greater than this.")
  }

  tasksystemlog.info("Master node address is: " + hostConfig.master.toString)

  def printQueueToLog {
    implicit val timeout = akka.util.Timeout(10 seconds)
    import system.dispatcher
    (balancerActor ? GetQueueInformation).onSuccess {
      case queue: QueueInfo => tasksystemlog.info("Queue content: " + queue)
    }

  }

  private val numberOfCores: Int = hostConfig.myCardinality

  private val availableMemory: Int = hostConfig.availableMemory

  private val host: InetSocketAddress = hostConfig.myAddress

  private lazy val balancerAndCacherAddress = hostConfig.master

  private val isLauncherOnly = hostConfig.myRole == SLAVE

  val statCollector = system.actorOf(Props[util.StatCollector], name = "statcollector")

  val gridengine = config.getString("hosts.gridengine") match {
    case x if x == "EC2" => EC2Grid
    case x if x == "LSF" => LSFGrid
    case x if x == "SGE" => SGEGrid
    case x if x == "SSH" => SSHGrid
    case _ => NoGrid
  }

  if (!isLauncherOnly && ElasticNodeAllocationEnabled && gridengine == SGEGrid) {
    DRMAA.testDRMAAConnection
  }

  val reaperActor = gridengine match {
    case EC2Grid => system.actorOf(Props(new EC2Reaper(cacheFile.toList ++ logFile.toList, config.getString("tasks.fileServiceBaseFolder"))), name = "reaper")
    case LSFGrid => system.actorOf(Props(new LSFReaper(RunningJobId(getNodeName))), name = "reaper")
    case SGEGrid => system.actorOf(Props(new SGEReaper(RunningJobId(getNodeName))), name = "reaper")
    case _ => system.actorOf(Props[ProductionReaper], name = "reaper") // this also works for SSHgrid
  }

  val remotenoderegistry = if (isLauncherOnly && ElasticNodeAllocationEnabled) {
    val remotepath = s"akka.tcp://tasks@${balancerAndCacherAddress.getHostName}:${balancerAndCacherAddress.getPort}/user/noderegistry"
    val noderegistry = Await.result(system.actorSelection(remotepath).resolveOne(600 seconds), atMost = 600 seconds)
    tasksystemlog.info("NodeRegistry: " + noderegistry)
    Some(noderegistry)
  } else None

  val cacheFile: Option[File] = try {
    if (config.getBoolean("tasks.cacheEnabled") && !isLauncherOnly) {
      gridengine match {
        case EC2Grid => throw new RuntimeException("LevelDB cache not implemented in EC2")
        case _ => Some(new File(config.getString("tasks.cacheFile")))
      }
    } else None
  } catch {
    case e: Throwable => {
      initFailed
      throw e
    }
  }

  if (gridengine == EC2Grid && !isLauncherOnly) {
    system.actorOf(Props(new S3Updater(cacheFile.toList ++ logFile.toList, config.getString("tasks.fileServiceBaseFolder"))), name = "s3updater")
  }

  val fileActor = try {
    if (!isLauncherOnly) {

      val filestore = gridengine match {
        case EC2Grid if config.getBoolean("tasks.elastic.aws.useS3FileStore") => {
          new S3Storage(config.getString("tasks.elastic.aws.fileStoreBucket"))
        }
        case _ => {
          val folder1 = new File(config.getString("tasks.fileServiceBaseFolder")).getCanonicalFile
          if (folder1.isFile) {
            tasksystemlog.error("$folder1 is a file. Calling System.exit(1)")
            System.exit(1)
          }
          if (!folder1.isDirectory) {
            tasksystemlog.warning(s"Folder $folder1 does not exists. mkdir ")
            folder1.mkdirs
          }
          val folders2 = config.getStringList("tasks.fileServiceExtendedFolders").map(x => new File(x).getCanonicalFile).toList.filter(_.isDirectory)
          val centralized = config.getBoolean("tasks.fileServiceBaseFolderIsShared")
          if (folder1.list.size != 0) {
            tasksystemlog.warning(s"fileServiceBaseFolder (${folder1.getCanonicalPath}) is not empty. This is only safe if you restart a pipeline. ")
          }
          new FolderFileStorage(folder1, centralized, folders2)
        }
      }

      tasksystemlog.info("FileStore: " + filestore)

      val threadpoolsize = config.getInt("tasks.fileServiceThreadPoolSize")

      val ac = system.actorOf(Props(new FileService(filestore, threadpoolsize)).withDispatcher("my-pinned-dispatcher"), "fileservice")
      reaperActor ! WatchMe(ac)
      ac
    } else {
      val actorpath = s"akka.tcp://tasks@${balancerAndCacherAddress.getHostName}:${balancerAndCacherAddress.getPort}/user/fileservice"
      val globalFileService = Await.result(system.actorSelection(actorpath).resolveOne(600 seconds), atMost = 600 seconds)

      tasksystemlog.info("FileService: " + globalFileService)
      globalFileService
    }
  } catch {
    case e: Throwable => {
      initFailed
      throw e
    }
  }

  val cacherActor = try {
    if (!isLauncherOnly && hostConfig.cacheAddress.isEmpty) {

      val cache: Cache = config.getBoolean("tasks.cacheEnabled") match {
        case true => {
          if (cacheFile.get.isFile) {
            val copy = new File(cacheFile.get.getAbsolutePath + ".leveldb")
            if (copy.isDirectory) {
              tasksystemlog.info("Using migrated cache db: " + copy.getAbsolutePath)
              new LevelDBCache(copy, akka.serialization.SerializationExtension(system))
            } else {
              copy.mkdir
              tasksystemlog.info(s"Migrating cache dump (${cacheFile.get.getAbsolutePath}) to leveldb: " + copy.getAbsolutePath)
              LevelDBHelper.migrateSimpleKVStore(cacheFile.get, copy)
              tasksystemlog.info("Using migrated cache db: " + copy.getAbsolutePath)
              new LevelDBCache(copy, akka.serialization.SerializationExtension(system))
            }

          } else new LevelDBCache(cacheFile.get, akka.serialization.SerializationExtension(system))
        }
        case false => new DisabledCache
      }
      val ac = system.actorOf(Props(new TaskResultCache(cache, FileServiceActor(fileActor))).withDispatcher("my-pinned-dispatcher"), "cache")
      reaperActor ! WatchMe(ac)
      ac
    } else {
      if (hostConfig.cacheAddress.isEmpty) {
        val actorpath = s"akka.tcp://tasks@${balancerAndCacherAddress.getHostName}:${balancerAndCacherAddress.getPort}/user/cache"
        Await.result(system.actorSelection(actorpath).resolveOne(600 seconds), atMost = 600 seconds)
      } else {
        val actorpath = s"akka.tcp://tasks@${hostConfig.cacheAddress.get.getHostName}:${hostConfig.cacheAddress.get.getPort}/user/cache"
        Await.result(system.actorSelection(actorpath).resolveOne(600 seconds), atMost = 600 seconds)
      }
    }
  } catch {
    case e: Throwable => {
      initFailed
      throw e
    }
  }

  val balancerActor = try {
    if (!isLauncherOnly) {
      val ac = system.actorOf(Props[TaskQueue].withDispatcher("taskqueue"), "queue")
      reaperActor ! WatchMe(ac)
      ac
    } else {
      val actorpath = s"akka.tcp://tasks@${balancerAndCacherAddress.getHostName}:${balancerAndCacherAddress.getPort}/user/queue"
      val ac = Await.result(system.actorSelection(actorpath).resolveOne(600 seconds), atMost = 600 seconds)
      tasksystemlog.info("Queue: " + ac)
      ac
    }
  } catch {
    case e: Throwable => {
      initFailed
      throw e
    }
  }

  // start up noderegistry
  val noderegistry: Option[ActorRef] = if (!isLauncherOnly && ElasticNodeAllocationEnabled) {
    val resource = CPUMemoryAvailable(cpu = hostConfig.myCardinality, memory = hostConfig.availableMemory)
    val ac = gridengine match {
      case LSFGrid => system.actorOf(Props(new LSFNodeRegistry(hostConfig.myAddress, balancerActor, resource)).withDispatcher("my-pinned-dispatcher"), "noderegistry")
      case SGEGrid => system.actorOf(Props(new SGENodeRegistry(hostConfig.myAddress, balancerActor, resource)).withDispatcher("my-pinned-dispatcher"), "noderegistry")
      case EC2Grid => system.actorOf(Props(new EC2NodeRegistry(hostConfig.myAddress, balancerActor, resource)).withDispatcher("my-pinned-dispatcher"), "noderegistry")
      case SSHGrid => system.actorOf(Props(new SSHNodeRegistry(hostConfig.myAddress, balancerActor, resource)).withDispatcher("my-pinned-dispatcher"), "noderegistry")
      case NoGrid => throw new RuntimeException("ElasticNodeAllocation and NoGrid are incompatible")
    }

    reaperActor ! WatchMe(ac)

    Some(ac)
  } else None

  val queueActor = QueueActor(balancerActor)

  val fileServiceActor = FileServiceActor(fileActor)

  val nodeLocalCacheActor = system.actorOf(Props[NodeLocalCache].withDispatcher("my-pinned-dispatcher"), name = "nodeLocalCache")

  reaperActor ! WatchMe(nodeLocalCacheActor)

  val nodeLocalCache = NodeLocalCacheActor(nodeLocalCacheActor)

  implicit val components = TaskSystemComponents(
    queue = queueActor,
    fs = fileServiceActor,
    actorsystem = system,
    cache = CacheActor(cacherActor),
    nodeLocalCache = nodeLocalCache,
    filePrefix = FileServicePrefix(Vector())
  )

  private val launcherActor = if (numberOfCores > 0) {
    val hostsForMPI: Seq[HostForMPI] = hostConfig match {
      case hc: LSFHostConfiguration => hc.hostsForMPI
      case _ => Nil
    }
    val refresh: FiniteDuration = (new DurationLong(config.getMilliseconds("tasks.askInterval")).millisecond)
    val ac = system.actorOf(Props(new TaskLauncher(balancerActor, nodeLocalCacheActor, CPUMemoryAvailable(cpu = numberOfCores, memory = availableMemory), refreshRate = refresh, hostsForMPI = hostsForMPI)).withDispatcher("launcher"), "launcher")
    reaperActor ! WatchMe(ac)
    Some(ac)
  } else None

  if (isLauncherOnly && ElasticNodeAllocationEnabled && launcherActor.isDefined) {
    val nodeName = getNodeName

    tasksystemlog.info("This is worker node. ElasticNodeAllocation is enabled. Node name: " + nodeName)
    if (nodeName != null) {
      // ActorInEnvelope
      val acenv = launcherActor.get

      val resource = CPUMemoryAvailable(hostConfig.myCardinality, hostConfig.availableMemory)
      remotenoderegistry.get ! NodeComingUp(Node(RunningJobId(nodeName), resource, acenv))

      val balancerHeartbeat = system.actorOf(Props(new HeartBeatActor(balancerActor)).withDispatcher("heartbeat"), "heartbeatOf" + balancerActor.path.address.toString.replace("://", "___") + balancerActor.path.name)

      gridengine match {
        case LSFGrid => system.actorOf(Props(new LSFSelfShutdown(RunningJobId(nodeName), balancerActor)).withDispatcher("my-pinned-dispatcher"))
        case SGEGrid => system.actorOf(Props(new SGESelfShutdown(RunningJobId(nodeName), balancerActor)).withDispatcher("my-pinned-dispatcher"))
        case EC2Grid => system.actorOf(Props(new EC2SelfShutdown(RunningJobId(nodeName), balancerActor)).withDispatcher("my-pinned-dispatcher"))
        case SSHGrid => system.actorOf(Props(new SSHSelfShutdown(RunningJobId(nodeName), balancerActor)).withDispatcher("my-pinned-dispatcher"))
        case NoGrid => throw new RuntimeException("ElasticNodeAllocation and NoGrid are incompatible")

      }

    } else {
      tasksystemlog.warning("Nodename/jobname is not defined.")
    }
  }

  scala.util.Try {
    if (hostConfig.myRole == MASTER) {
      val address = config.getString("tasks.monitor")
      val actorpath = s"akka.tcp://taskmonitorweb@$address/user/monitorendpoint"
      val ac = Await.result(system.actorSelection(actorpath).resolveOne(600 seconds), atMost = 600 seconds)
      tasksystemlog.info("Monitor: " + ac)
      setupMonitor(ac)

    }
  }

  def setupMonitor(mon: ActorRef) {
    import system.dispatcher

    {
      implicit val timeout = akka.util.Timeout(50 seconds)
      (mon ? monitor.Introduction(
        queueActor = balancerActor,
        fileActor = fileActor,
        nodeRegistry = noderegistry,
        configuration = config
      )).onSuccess {
        case monitor.ListenerEnvelope(listener, id) => {

          system.eventStream.subscribe(listener, classOf[LogMessage])
          system.eventStream.subscribe(listener, classOf[monitor.QueueStat])
          system.eventStream.subscribe(listener, classOf[monitor.NodeRegistryStat])
          implicit val timeout = akka.util.Timeout(4 * 168 hours)
          (mon ? monitor.WaitingForShutdownCommand(id)) onSuccess {
            case monitor.ShutdownCommand => {
              tasksystemlog.warning("ShutdownCommand received from monitor.")
              this.shutdown
            }
          }
        }
      }
    }

  }

  def initFailed {
    if (isLauncherOnly) {
      remotenoderegistry.foreach(_ ! InitFailed(PendingJobId(getNodeName)))
    }
  }

  def shutdown {
    if (hostConfig.myRole == MASTER) {
      implicit val timeout = akka.util.Timeout(10 seconds)
      import system.dispatcher

      val map = scala.util.Try {
        Await.result(statCollector ? util.ShowStats, atMost = 10 seconds)
      }
      tasksystemlog.info("Gathered stats: " + map)

      balancerActor ! PoisonPill
      cacherActor ! PoisonPill
      fileActor ! PoisonPill
      system.actorSelection("/user/fileservice_*") ! PoisonPill
      system.actorSelection("/user/cache_*") ! PoisonPill

      launcherActor.foreach(_ ! PoisonPill)
      if (!isLauncherOnly) {
        noderegistry.foreach(_ ! PoisonPill)
      }
      nodeLocalCacheActor ! PoisonPill
    } else {
      system.shutdown
    }

  }

  scala.sys.addShutdownHook {
    tasksystemlog.warning("JVM is shutting down - will call tasksystem shutdown.")
    shutdown
    tasksystemlog.warning("JVM is shutting down - called tasksystem shutdown.")
  }

  def getNodeName: String = gridengine match {
    case LSFGrid => System.getenv("LSB_JOBID")
    case SSHGrid => {
      val pid = java.lang.management.ManagementFactory.getRuntimeMXBean().getName().split("@").head
      val hostname = configInstance.getString("hosts.hostname")
      hostname + ":" + pid
    }
    case SGEGrid => System.getenv("JOB_ID")
    case EC2Grid => EC2Operations.readMetadata("instance-id").head
    case NoGrid => throw new RuntimeException("ElasticNodeAllocation and NoGrid are incompatible")
  }

}
