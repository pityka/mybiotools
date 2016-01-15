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

import scala.scalajs.js.annotation.{ JSExport, JSName }
import org.scalajs.dom
import dom.html
import scala.util.Random
import scala.concurrent.Future
import scalajs.concurrent.JSExecutionContext.Implicits.runNow
import scalatags.JsDom.all._
import upickle._
import autowire._
import rx._
import scalajs.js
import mybiotools.tasks._
import rx.ops._
import mybiotools.tasks.monitor._

class EventSource(target: String) extends dom.EventTarget {
  def close(): Unit = js.native
}

object Client extends autowire.Client[String, upickle.Reader, upickle.Writer] {
  override def doCall(req: Request): Future[String] = {
    dom.ext.Ajax.post(
      url = "/api/" + req.path.mkString("/"),
      data = upickle.write(req.args)
    ).map { x =>
        x.responseText
      }
  }

  def read[Result: upickle.Reader](p: String) = upickle.read[Result](p)
  def write[Result: upickle.Writer](r: Result) = upickle.write(r)
}

@JSExport
object TasksMonitorWebClient {
  implicit def rxFrag[T <% Frag](r: Rx[T]): Frag = {
    def rSafe: dom.Node = r().render
    var last = rSafe
    Obs(r, skipInitial = true) {
      val newLast = rSafe
      js.Dynamic.global.last = last
      last.parentNode.replaceChild(newLast, last)
      last = newLast
    }
    last
  }

  implicit def rxAttrValue[T: AttrValue]: AttrValue[Rx[T]] = new RxAttrValue[T, Rx[T]]
  implicit def varAttrValue[T: AttrValue]: AttrValue[Var[T]] = new RxAttrValue[T, Var[T]]

  class RxAttrValue[T, F <: Rx[T]](implicit av: AttrValue[T]) extends AttrValue[F] {
    override def apply(t: dom.Element, a: Attr, rv: F): Unit = {
      Obs(rv)(av.apply(t, a, rv()))
    }
  }

  @JSExport
  def main(target: html.Div): Unit = {

    val systemlist = Var[List[Int]](Nil)
    val loglist = scala.collection.mutable.Map[Int, Var[List[LogMessage]]]()

    val currentsystem = Var[Int](0)

    val currentQueueStat: Rx[Option[QueueStat]] = currentsystem.map { id =>
      if (id == 0) Future.successful(None)
      else Client[Api].getQueueStat(id).call().map { queuestat =>
        Some(queuestat)
      }
    }.async(None)

    val currentNodeRegistryStat: Rx[Option[NodeRegistryStat]] = currentsystem.map { id =>
      if (id == 0) Future.successful(None)
      else Client[Api].getNodeRegistryStat(id).call()
    }.async(None)

    val currentConfig: Rx[Option[String]] = currentsystem.map { id =>
      if (id == 0) Future.successful(None)
      else Client[Api].getConfigString(id).call().map(Some.apply)
    }.async(None)

    val currentFileList = Var[List[String]](Nil)
    var eventSource: Option[EventSource] = None

    def updateCurrent(id: Int) = {
      currentsystem() = id
    }

    def logMessageReceived(id: Int, e: LogMessage) = {
      loglist(id)() = e :: loglist(id)()
    }

    def searchFiles(id: Int, pattern: String) = {
      updateCurrent(id)
      Client[Api].listSharedFiles(id, pattern).call().foreach { x =>
        currentFileList() = x
      }
    }

    def shutdown(id: Int) = {
      Client[Api].shutdown(id).call()
    }

    def renderQueueStat(qs: QueueStat) = {
      div(
        div(cls := "uk-grid uk-grid-small")(
          div(cls := "uk-width-3-6")(
            div(cls := "uk-panel")(
              h5(cls := "uk-panel-title")("Qeued"),
              table(cls := "uk-table uk-table-striped uk-table-condensed")(
                thead(
                  tr(
                    th("Task"),
                    th("Request")
                  )),
                tbody(
                  qs.queued.map {
                    case (task, request) =>
                      tr(
                        td(task),
                        td(request.toString)
                      )
                  }
                ))
            )
          )
        ),
        div(cls := "uk-grid uk-grid-small")(
          div(cls := "uk-width-1-6")(
            div(cls := "uk-panel")(
              h5(cls := "uk-panel-title")("Running"),
              table(cls := "uk-table uk-table-striped uk-table-condensed")(
                thead(
                  tr(
                    th("Task"),
                    th("Allocated")
                  )),
                tbody(
                  qs.running.map {
                    case (task, allocated) =>
                      tr(
                        td(task),
                        td(allocated.toString)
                      )
                  }
                ))
            )
          )
        )
      )
    }

    def renderNodeRegistryStat(qs: NodeRegistryStat) = {
      div(cls := "uk-grid uk-grid-small")(
        div(cls := "uk-width-1-6")(
          div(cls := "uk-panel")(
            h5(cls := "uk-panel-title")("Running"),
            table(cls := "uk-table uk-table-striped uk-table-condensed")(
              thead(
                tr(
                  th("RunningJobId"),
                  th("Available")
                )),
              tbody(
                qs.running.toList.map {
                  case (task, request) =>
                    tr(
                      td(task.toString),
                      td(request.toString)
                    )
                }
              ))
          )
        ),
        div(cls := "uk-width-1-6")(
          div(cls := "uk-panel")(
            h5(cls := "uk-panel-title")("Pending"),
            table(cls := "uk-table uk-table-striped uk-table-condensed")(
              thead(
                tr(
                  th("PendingJobId"),
                  th("Available")
                )),
              tbody(
                qs.pending.toList.map {
                  case (task, allocated) =>
                    tr(
                      td(task.toString),
                      td(allocated.toString)
                    )
                }
              ))
          )
        )
      )
    }

    val displayList = Rx {
      ul(cls := "uk-nav uk-nav-side")(
        systemlist().map { id =>
          Rx {
            li(cls := (if (id == currentsystem()) "uk-active" else ""))(

              a(
                id,
                onclick := { () => updateCurrent(id) }
              )
            )
          }
        }
      )
    }

    val displayQueueStat = Rx {
      div(currentQueueStat().map(renderQueueStat).getOrElse(span()))
    }

    val displayNodeRegistryStat = Rx {
      div(currentNodeRegistryStat().map(renderNodeRegistryStat).getOrElse(span()))
    }

    val displayConfig = Rx {
      div(pre(code(currentConfig())))
    }

    val displayLog = Rx {
      val id = currentsystem()
      div(cls := "uk-panel")(
        table(cls := "uk-table uk-table-striped uk-table-condensed")(
          thead(
            tr(
              th("level"),
              th("message"),
              th("time"),
              th("cause")
            )),
          tbody(
            if (id != 0) {
              loglist(id)().map { log =>
                tr(
                  td(log.level),
                  td(log.message),
                  td(log.time),
                  td(log.cause)
                )
              }
            }
          ))
      )

    }

    val displayFileList = Rx {
      div(
        {
          val inp = input().render
          val b = button(onclick := { () => searchFiles(currentsystem(), inp.value) })

          span(inp, b)
        },
        ul(
          currentFileList().map(f =>
            li(a(f,
              href := s"/fs/${currentsystem()}/$f"
            )
            )
          )
        )
      )
    }

    val displayShutdown = Rx {
      div(cls := "uk-panel")(
        button(cls := "uk-button uk-button-danger")(
          "shutdown",
          onclick := { () => shutdown(currentsystem()) }
        )
      )
    }

    Client[Api].list().call().foreach { ids =>
      systemlist() = ids.toList
      ids.foreach { id =>
        loglist.update(id, Var(Nil))
        val eventSource = new EventSource("/logstream/" + id)
        eventSource.addEventListener("message", (e: dom.MessageEvent) => {
          logMessageReceived(
            id,
            upickle.read[LogMessage](e.data.toString))
        })
        eventSource.addEventListener("error", (e: dom.Event) => { println("error " + e.toString) })
        eventSource.addEventListener("open", (e: dom.Event) => { println("open " + e.toString) })
      }
    }

    val hiddenornot = Rx {
      if (currentsystem() == 0) "uk-hidden" else ""
    }

    val tabs = div(cls := hiddenornot)(ul(
      "data-uk-switcher".attr := "{connect:'#tabcontent'}",
      "data-uk-tab".attr := "",
      cls := "uk-tab")(
        li(a("Queue/Nodes")),
        li(a("Config")),
        li(a("Files")),
        li(a("Log")),
        li(a("Shutdown"))
      ),
      ul(id := "tabcontent", cls := "uk-switcher")(
        li(div(
          displayQueueStat,
          displayNodeRegistryStat
        )
        ),
        li(displayConfig),
        li(displayFileList),
        li(displayLog),
        li(displayShutdown)
      ))

    target.appendChild(
      div(cls := "uk-grid")(
        div(id := "systemlist", cls := "uk-width-1-4")(displayList),
        div(cls := "uk-width-3-4")(
          tabs
        )
      ).render
    )
  }
}
