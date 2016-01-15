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

package rnaseq.tasks

import mybiotools.tasks._
import java.io.File
import mybiotools._

case class Aggregator2Input[T1, T2, R <: Result](
    p1: Option[T1],
    p2: Option[T2],
    factory: (T1, T2) => R,
    update: UpdatePrerequisitive[Aggregator2Input[T1, T2, R]]
) extends Prerequisitive[Aggregator2Input[T1, T2, R]] {
  def ready = p1.isDefined && p2.isDefined
}

object aggregator2 {
  def apply[T1, T2, R <: Result](update: UpdatePrerequisitive[Aggregator2Input[T1, T2, R]])(f: (T1, T2) => R)(implicit components: TaskSystemComponents) = {
    val in = Aggregator2Input(None, None, f, update)
    newTask(
      in, in.update orElse identity[Aggregator2Input[T1, T2, R]], CPUMemoryRequest(cpu = 1, memory = 100)
    ) {

      case (
        Aggregator2Input(
          Some(p1),
          Some(p2),
          factory,
          _), ce) =>
        import ce._

        factory(p1, p2)
    }
  }
}

