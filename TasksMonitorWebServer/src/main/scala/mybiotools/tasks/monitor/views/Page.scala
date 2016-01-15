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

package mybiotools.tasks.monitor.views

object Template {
  import scalatags.Text.all._
  import scalatags.Text.tags2.title
  val txt =
    "<!DOCTYPE html>" +
      html(
        head(
          title("TasksMonitor"),
          meta(httpEquiv := "Content-Type", content := "text/html; charset=UTF-8"),
          script(`type` := "text/javascript", src := "/assets/tasksmonitorwebclient-fastopt.js"),
          script(`type` := "text/javascript", src := "//localhost:12345/workbench.js"),
          script(`type` := "text/javascript", src := "/assets/META-INF/resources/webjars/jquery/2.1.4/jquery.min.js"),
          script(`type` := "text/javascript", src := "/assets/META-INF/resources/webjars/uikit/2.20.3/js/uikit.min.js"),

          link(
            rel := "stylesheet",
            `type` := "text/css",
            href := "/assets/META-INF/resources/webjars/uikit/2.20.3/css/uikit.min.css"
          )
        ),
        body(margin := 0)(
          div(id := "page"),
          script("mybiotools.tasks.monitor.client.TasksMonitorWebClient().main(document.getElementById('page'))")

        )
      )
}