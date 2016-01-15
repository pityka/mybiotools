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

import scala.concurrent.duration._
import akka.actor._
import akka.pattern.ask
import spray.routing.{ HttpService, RequestContext }
import spray.routing.directives.CachingDirectives
import spray.can.server.Stats
import spray.can.Http
import spray.httpx.encoding.Gzip
import spray.util._
import spray.http._
import spray.http.StatusCodes._
import MediaTypes._
import CachingDirectives._
import upickle._
import scala.concurrent._
import spray.http.HttpHeaders._
import spray.http.CacheDirectives._
import spray.routing.authentication._
import mybiotools.tasks.monitor.client._

object AutowireServer extends autowire.Server[String, upickle.Reader, upickle.Writer] {
  def read[Result: upickle.Reader](p: String) = upickle.read[Result](p)
  def write[Result: upickle.Writer](r: Result) = upickle.write(r)
}

class RouterActor(val endpoint: ActorRef) extends Actor with MyRoutes {

  def actorRefFactory = context

  def receive = runRoute(route)
}

trait MyRoutes extends HttpService {

  def endpoint: ActorRef

  implicit def executionContext = actorRefFactory.dispatcher

  lazy val clientApi = new client.ApiImpl(endpoint)

  def myUserPassAuthenticator(userPass: Option[UserPass]): Future[Option[String]] =
    Future {
      if (userPass.exists(up => up.user == "singleuser" && up.pass == "jelszo")) Some("singleuser")
      else None
    }

  val route =
    authenticate(BasicAuth(myUserPassAuthenticator _, realm = "blah")) { userName =>
      get {
        pathSingleSlash {
          complete {
            HttpEntity(
              MediaTypes.`text/html`,
              views.Template.txt
            )
          }
        } ~
          path("assets" / Segments) { ss =>
            getFromResource(ss.mkString("/"))
          } ~
          path("fs" / Segment / Segment) { (id, name) =>
            if (scala.util.Try(id.toInt).toOption.isEmpty) reject()
            else {
              detach() {
                respondWithMediaType(`application/octet-stream`) {
                  onComplete(clientApi.getFileServiceInputStream(id.toInt, name)) {
                    case scala.util.Success(stream) => complete(stream.toByteArrayStream(1024 * 256))
                    case scala.util.Failure(x) => complete(spray.http.StatusCodes.NotFound)
                  }

                }
              }
            }
          } ~
          path("logstream" / Segment) { id =>
            respondWithHeader(`Cache-Control`(`no-cache`)) {
              respondWithHeader(`Connection`("Keep-Alive")) {
                respondWithMediaType(MediaType.custom("text/event-stream")) { ctx =>
                  if (scala.util.Try(id.toInt).toOption.isDefined) {
                    endpoint ! StreamEventsTo(id.toInt, ctx)
                  } else reject()
                }
              }
            }
          }
      } ~
        post {
          path("api" / Segments) { s =>
            extract(_.request.entity.asString) { e =>
              {
                val pickled: Future[String] = AutowireServer.route[Api](clientApi)(
                  autowire.Core.Request(s, upickle.read[Map[String, String]](e))
                )
                onComplete(pickled) {
                  case scala.util.Success(r) => {
                    respondWithMediaType(`application/json`) {
                      complete(r)
                    }
                  }
                  case scala.util.Failure(x) => {
                    println(x)
                    complete(InternalServerError)
                  }
                }
              }
            }
          }
        }
    }

}

case object IStopped
trait EventStream extends Actor {

  val ctx: RequestContext

  val parent: ActorRef

  import context.dispatcher

  private var first = true

  private var scheduler: Cancellable = null

  def send[T: upickle.Writer](payload: T) = {
    val message = "data: " + upickle.write[T](payload) + "\n\n"
    scheduler = context.system.scheduler.schedule(
      initialDelay = 60 seconds,
      interval = 0 seconds,
      receiver = self,
      message = PoisonPill)
    if (!first) {
      ctx.responder ! MessageChunk(message).withAck("ack")
    } else {
      ctx.responder ! ChunkedResponseStart(HttpResponse(entity = HttpEntity(MediaType.custom("text/event-stream"), message))).withAck("ack")
      first = false
    }
  }

  override def postStop {
    ctx.responder ! ChunkedMessageEnd
    parent ! IStopped
  }

  def receiveStub: Receive = {
    case ev: Http.ConnectionClosed => {
      ctx.responder ! ChunkedMessageEnd
      parent ! IStopped
      self ! PoisonPill
    }
    case "ack" => {
      scheduler.cancel
    }
  }
}

class LogEventStream(val ctx: RequestContext, val parent: ActorRef) extends EventStream {

  def handler: Receive = ({
    case x: mybiotools.tasks.LogMessage => send(x)
  })

  def receive = handler orElse super.receiveStub
}
