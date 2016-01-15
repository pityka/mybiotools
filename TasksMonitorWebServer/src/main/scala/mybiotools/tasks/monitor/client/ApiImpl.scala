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

package mybiotools.tasks.monitor.client

import scala.concurrent._
import akka.actor.ActorRef
import akka.pattern.ask
import scala.concurrent.ExecutionContext.Implicits.global
import mybiotools.tasks._
import mybiotools.tasks.monitor._

class ApiImpl(endpoint: ActorRef) extends Api {
  implicit val timeout = akka.util.Timeout(1000, java.util.concurrent.TimeUnit.SECONDS)

  def list(): Future[Seq[Int]] = (endpoint ? ListSystems).asInstanceOf[Future[Seq[Int]]]

  def getQueueStat(i: Int): Future[QueueStat] = (endpoint ? GetQueueStat(i)).asInstanceOf[Future[QueueStat]]

  def getNodeRegistryStat(i: Int): Future[Option[NodeRegistryStat]] = (endpoint ? GetNodeRegistryStatForSystem(i)).asInstanceOf[Future[Option[NodeRegistryStat]]]

  def shutdown(i: Int) = {
    endpoint ! ShutdownSystem(i)
  }

  def getConfigString(i: Int): Future[String] = {
    (endpoint ? GetConfig(i)).asInstanceOf[Future[String]]
  }

  def listSharedFiles(i: Int, pattern: String): Future[List[String]] = {
    (endpoint ? ListSharedFiles(i, pattern)).asInstanceOf[Future[List[String]]]
  }

  def getFileServiceInputStream(i: Int, name: String): Future[java.io.InputStream] = {
    (endpoint ? GetFileServiceInputStream(i, name)).asInstanceOf[Future[java.io.InputStream]]
  }
}