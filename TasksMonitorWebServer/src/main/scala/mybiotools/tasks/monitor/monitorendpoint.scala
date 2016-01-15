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

package mybiotools.tasks.monitor

import mybiotools.eq._
import akka.actor._
import akka.pattern.ask
import spray.routing.RequestContext
import mybiotools.tasks._
import mybiotools.tasks.monitor.client._

class Listener extends Actor {

  var past: List[Any] = Nil

  var listeners = List[ActorRef]()

  def receive = {
    case ctx: RequestContext => {
      val newlistener = context.actorOf(Props(classOf[LogEventStream], ctx, self))
      listeners = newlistener :: listeners
      past.foreach { newlistener ! _ }
    }
    case IStopped => {
      listeners = listeners.filterNot(_ === sender)
    }
    case x => {
      listeners.foreach { _ ! x }
      past = x :: past
    }

  }
}

class MonitorEndpoint extends Actor {

  private var systems = List[(Int, Introduction, ActorRef)]()

  private var systemShutdownListeners = List[(Int, ActorRef)]()

  private val filemap = collection.mutable.Map[Int, Map[String, SharedFile]]()

  private def getSystem(i: Int) = systems.find(_._1 === i)

  def receive = {

    case StreamEventsTo(i, rctx) => getSystem(i).foreach { _._3 ! rctx }

    case GetFileServiceInputStream(i, p) => getSystem(i).foreach { system =>
      implicit val fileActor = FileServiceActor(system._2.fileActor)
      scala.util.Try(SharedFile.getStreamToFile(filemap(i)(p))).toOption.foreach { r =>
        sender ! r
      }

    }

    case ListSharedFiles(i, p) => getSystem(i).foreach { system =>
      implicit val timeout = akka.util.Timeout(1000, java.util.concurrent.TimeUnit.SECONDS)
      import context.dispatcher
      val outersender = sender
      system._2.fileActor.?(GetListOfFilesInStorage(p)).onSuccess {
        case files: List[SharedFile] => {
          filemap.get(i) match {
            case None => filemap.update(i, files.map(x => x.name -> x).toMap)
            case Some(x) => filemap.update(i, files.map(x => x.name -> x).toMap ++ x)
          }

          outersender ! files.map(_.name)
        }
      }
    }

    case GetConfig(i) => {
      sender ! getSystem(i).map(_._2.configuration.root.render).getOrElse("")
    }

    case i: Introduction => {
      if (getSystem(i.hashCode).isEmpty) {
        val ac = context.actorOf(Props[Listener], "listener" + i.hashCode)
        sender ! ((ListenerEnvelope(ac, i.hashCode)))
        systems = (i.hashCode, i, ac) :: systems
        val hearbeat = context.actorOf(Props(classOf[HeartBeatActor], i.queueActor).withDispatcher("heartbeat"), "hearbeat" + i.hashCode)
        hearbeat ! Pong
        context.system.eventStream.subscribe(self, classOf[LauncherDown])
      } else {
        sender ! ListenerEnvelope(getSystem(i.hashCode).get._3, i.hashCode)
      }

    }

    case LauncherDown(ac) => {
      systems = systems.filterNot(_._2.queueActor === ac)
    }

    case WaitingForShutdownCommand(i) => {
      systemShutdownListeners = (i, sender) :: systemShutdownListeners
    }

    case ShutdownSystem(i) => {
      systemShutdownListeners.find(_._1 === i).foreach(_._2 ! ShutdownCommand)
      // systems.find(_._1 === i).foreach(_._3 ! PoisonPill)
      // systems = systems.filterNot(_._1 === i)
      systemShutdownListeners = systemShutdownListeners.filterNot(_._1 === i)
    }

    case ListSystems => sender ! systems.map(_._1)
    case GetQueueStat(i) => getSystem(i).foreach(_._2.queueActor.!(HowLoadedAreYou)(sender))
    case GetNodeRegistryStatForSystem(i) => getSystem(i).foreach(system => if (system._2.nodeRegistry.isDefined) system._2.nodeRegistry.foreach(_.!(GetNodeRegistryStat)(sender)) else sender ! None)
  }
}