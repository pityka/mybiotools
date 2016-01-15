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

import org.scalatest.FunSuite

import mybiotools._

class ExecTestSuite extends FunSuite {

  test("timeout") {
    import scala.concurrent.duration._
    var l = List[String]()
    intercept[java.util.concurrent.TimeoutException](exec("sleep 5000", atMost = 1 milliseconds) { ln => l = ln :: l }())
  }

  test("plain echo") {
    var l = List[String]()
    val b = exec(List("echo", "asdf")) { ln => l = ln :: l }()
    assertResult(List("asdf")) { l }
    assertResult(0) { b }
  }

  test("plain echo 2") {
    var l = List[String]()
    val b = exec("uname -a")()()
    assertResult(0) { b }
  }

  test("stderr") {
    var l = List[String]()
    val b = exec(List("sh", "sdfsda")) { ln => } { ln => l = ln :: l }
    assertResult(List("sh: sdfsda: No such file or directory")) { l }
    assertResult(127) { b }
  }

  test("stdout 2") {
    var l = List[String]()
    val b = exec(List("sh", "-c", "echo a;echo a")) { ln => l = ln :: l }()
    assertResult(List("a", "a")) { l }
    assertResult(0) { b }
  }

  test("stdout stderr") {
    var l = List[String]()
    var l2 = List[String]()
    exec(List("sh", "-c", "echo a;dfds;echo b;asdf")) { ln => l2 = ln :: l2 } { ln => l = ln :: l }
    assertResult(List("a", "b")) { l2.reverse }
    assertResult(List("sh: dfds: command not found", "sh: asdf: command not found")) { l.reverse }
  }

  test("stdout stderr, execStreamsAsList") {
    val l = execStreamsAsList(List("sh", "-c", "echo a;dfds;echo b;asdf"))
    assertResult(List("a", "b")) { l._1 }
    assertResult(List("sh: dfds: command not found", "sh: asdf: command not found")) { l._2 }
  }

  test("stdout stderr, execIsFinishedWithoutError") {
    var l = List[String]()
    var l2 = List[String]()
    val b = (execIsFinishedWithoutErrorStream(List("sh", "-c", "echo a;dfds;echo b;asdf"))(
      (ln: String) => { l2 = ln :: l2 },
      (ln: String) => { l = ln :: l }
    ))
    assertResult(List("a", "b")) { l2.reverse }
    assertResult(List("sh: dfds: command not found", "sh: asdf: command not found")) { l.reverse }
    assertResult(false) { b }
  }

  test("stdout stderr, execIsFinishedWithoutError 2") {
    var l = List[String]()
    var l2 = List[String]()
    val b = execIsFinishedWithoutErrorStream(List("sh", "-c", "echo a;echo b;") #| "wc -l") { ln =>
      l2 = ln :: l2
    }
    assertResult(List("       2")) { l2.reverse }
    assertResult(List()) { l.reverse }
    assertResult(true) { b }
  }
}
