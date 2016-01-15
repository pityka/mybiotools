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

import upickle._
import scala.concurrent.ExecutionContext.Implicits.global
import spray.http.{ MediaTypes, HttpEntity }
import mybiotools.tasks._
import akka.actor.{ ActorSystem, Props }
import akka.io.IO
import spray.can.Http
import mybiotools.config.Config.configInstance
import com.typesafe.config.{ ConfigFactory, Config }

object TasksMonitorWebApp extends App {
  val test = scala.util.Try { args(0).toBoolean }.toOption.getOrElse(false)

  val akkahost = scala.util.Try { configInstance.getString("akkahost") }.toOption.getOrElse("localhost")
  val httphost = scala.util.Try { configInstance.getString("httphost") }.toOption.getOrElse("localhost")
  val akkaport = scala.util.Try { configInstance.getInt("akkaport") }.toOption.getOrElse(25555)
  val httpport = scala.util.Try { configInstance.getInt("httpport") }.toOption.getOrElse(8080)

  val configuration = {
    val actorProvider = "akka.remote.RemoteActorRefProvider"

    val configSource = s"""
akka {
  actor {
    provider = "${actorProvider}"
  }
  remote {
    enabled-transports = ["akka.remote.netty.tcp"]
    netty.tcp {
      hostname = "${akkahost}"
      port = ${akkaport}
    }
 }
}
akka.loggers = ["akka.event.Logging$$DefaultLogger"]
akka.loglevel = DEBUG
  """

    val customConf = ConfigFactory.parseString(configSource)

    val classpathConf = ConfigFactory.load("akkaoverrides.conf")

    customConf.withFallback(classpathConf)

  }

  implicit val system = ActorSystem("taskmonitorweb", configuration)

  val endpoint = system.actorOf(Props[MonitorEndpoint], "monitorendpoint")

  if (test) {
    val testtasksystem = customTaskSystem(new LocalConfiguration(1, 1000), ConfigFactory.parseString(""" tasks.logFile=""
      akka.loggers = []
      """))

    testtasksystem.setupMonitor(endpoint)
  }

  // create and start our service actor
  val routerActor = system.actorOf(Props(classOf[RouterActor], endpoint), "monitor-router")

  // start a new HTTP server on port 8080 with our service actor as the handler
  IO(Http) ! Http.Bind(routerActor, httphost, port = httpport)

}
