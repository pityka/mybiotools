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

import org.scalatest._

import mybiotools.tasks._

import org.scalatest.FunSpec
import org.scalatest.Matchers
import scala.concurrent.ExecutionContext.Implicits.global

object Tests {

  case class IntWrapper(i: Int) extends Result

  case class Counter(count: Option[Int]) extends SimplePrerequisitive[Counter]

  val updateCounter: UpdatePrerequisitive[Counter] = {
    case (old, i: IntWrapper) => old.copy(count = Some(i.i))
  }

  val ts = defaultTaskSystem
  import ts._

  def t1(i: Option[Int]) = newTask(
    Counter(i),
    updateCounter
  ) {
      case (Counter(Some(c)), ce) =>
        import ce._
        IntWrapper(c + 1)
    }

  val r1 = t1(Some(0)).?![IntWrapper].i
  val r2 = (t1(Some(1)) ~> t1(None) ~> t1(None)).?![IntWrapper].i

  ts.shutdown

}

class TaskDSLTestSuite extends FunSuite with Matchers {

  test("chains should work") {
    Tests.r1 should equal(1)
    Tests.r2 should equal(4)
  }

}