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

import akka.actor.{ Actor, ActorRef }
import akka.pattern.ask
import concurrent.duration._
import concurrent.ExecutionContext
import concurrent.Future

@SerialVersionUID(1L)
case class LookUp(s: String)

@SerialVersionUID(1L)
case class Save(s: String, v: Any)

@SerialVersionUID(1L)
case object YouShouldSetIt

object NodeLocalCache {

  // This method blocks. Only ever call from Computation.apply
  def getItemBlocking[A](key: String)(orElse: => A)(implicit nlc: NodeLocalCacheActor): A = {
    implicit val to = akka.util.Timeout(168 hours)
    val f = (nlc.actor ? LookUp(key))
    val answer = concurrent.Await.result(f, Duration.Inf)
    answer match {
      case YouShouldSetIt => {
        val c = orElse
        nlc.actor ! Save(key, c)
        c
      }
      case x => x.asInstanceOf[A]
    }
  }
}

class NodeLocalCache extends Actor with akka.actor.ActorLogging {
  private val map = scala.collection.mutable.Map[String, Option[Any]]()

  private val waitingList = scala.collection.mutable.ListBuffer[Tuple2[String, ActorRef]]()

  def receive = {
    case LookUp(s) => {
      val r = map.get(s)
      if (r.isEmpty) {
        log.debug("LookUp(" + s + "): Not Found. Reply with YouShouldSetIt")
        map += s -> None
        sender ! YouShouldSetIt
      } else {
        if (r.get.isDefined) {
          log.debug("LookUp(" + s + "): Found. Reply with item.")
          sender ! r.get.get
        } else {
          log.debug("LookUp(" + s + "): Item is under production, adding sender to waiting list.")
          waitingList += s -> sender
        }
      }

    }
    case Save(s, r) => {
      log.debug("Save(" + s + "): Distributing to waiting list.")
      map += s -> Some(r)
      waitingList.filter(_._1 == s).map(_._2).foreach(_ ! r)
    }
  }

}