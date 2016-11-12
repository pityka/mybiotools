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

// import org.ggf.drmaa.{ Session, JobTemplate, SessionFactory, DrmaaException }
import TaskAllocationConstants._
import java.net.InetSocketAddress
import akka.actor.Actor._
import akka.actor.{ ActorRef, Actor, Props }
import java.net.InetSocketAddress
import mybiotools.logger.Logger
import scala.util._

object DRMAA {

  def testDRMAAConnection {
    // val factory = SessionFactory.getFactory();
    // val session = factory.getSession();
    // session.init("");
    // mybiotools.logger.Logger.info("Testing DRMAA connection..")
    // mybiotools.logger.Logger.info("DRMAA System is: " + session.getDrmSystem())
    // session.exit();
  }

  class DRMAANodeKiller(
    val targetLauncherActor: ActorRef,
    val targetNode: Node
  ) extends NodeKillerImpl with DRMAAShutdown with akka.actor.ActorLogging

  class SGENodeRegistry(
    val masterAddress: InetSocketAddress,
    val targetQueue: ActorRef,
    override val unmanagedResource: CPUMemoryAvailable
  ) extends DRMAAJobRegistry with NodeCreatorImpl with SimpleDecideNewNode with DRMAAShutdown

  trait DRMAAShutdown extends ShutdownNode {

    def shutdownRunningNode(nodeName: RunningJobId) {
      // val factory = SessionFactory.getFactory();
      // val session = factory.getSession();
      // try {
      //
      //   session.init("");
      //   session.control(nodeName.value, Session.TERMINATE);
      //
      // } catch {
      //   case e: Exception => Logger.warning("Some error in drmaa (possible other session is running." + e)
      // } finally {
      //   session.exit();
      // }
    }

    def shutdownPendingNode(node: PendingJobId) = shutdownRunningNode(RunningJobId(node.value))

  }

  trait DRMAAJobRegistry extends akka.actor.Actor with GridJobRegistry with akka.actor.ActorLogging {

    val masterAddress: InetSocketAddress

    def requestOneNewJobFromGridScheduler(resourceRequest: CPUMemoryRequest): Try[Tuple2[PendingJobId, CPUMemoryAvailable]] = {
      import scala.collection.JavaConversions._

      // val NumberOfCoresOfNewLauncher = resourceRequest.cpu._1
      // val RequestedMemOfNewNode = resourceRequest.memory
      //
      // Logger.debug(this, "request new node start (send new job to gridengine).")
      //
      // val jarpath = NewNodeJarPath
      // val mem = RequestedMemOfNewNode
      // val email = EmailAddress
      //
      // val javaargs: List[String] = "-Dconfig.file=" + System.getProperty("config.file") :: "-Dhosts.gridengine=SGE" ::
      //   AdditionalSystemProperties :::
      //   "-Dhosts.master=" + masterAddress.getHostName + ":" + masterAddress.getPort :: jarpath :: Nil
      //
      // val factory = SessionFactory.getFactory();
      // val session = factory.getSession();
      //
      // val jobid = Try {
      //   session.init(null);
      //   val jt = session.createJobTemplate();
      //   jt.setRemoteCommand("/usr/bin/java");
      //   jt.setArgs(javaargs);
      //   jt.setEmail(Set(email))
      //   jt.setNativeSpecification("-V -cwd -pe ptile " + NumberOfCoresOfNewLauncher.toString)
      //
      //   val jobid = session.runJob(jt)
      //
      //   session.deleteJobTemplate(jt);
      //
      //   (
      //     PendingJobId(jobid),
      //     CPUMemoryAvailable(NumberOfCoresOfNewLauncher, RequestedMemOfNewNode)
      //   )
      // }
      //
      // session.exit()
      //
      // Logger.debug(this, "job submitted. jobid: " + jobid)
      //
      // jobid
      ???
    }

    def initializeNode(node: Node) {
      val ac = node.launcherActor //.revive

      val ackil = context.actorOf(Props(new DRMAANodeKiller(ac, node)))

    }

  }
}
