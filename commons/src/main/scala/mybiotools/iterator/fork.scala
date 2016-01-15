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

package mybiotools.iterator

sealed trait State
case object TakeMore extends State
case object Done extends State

class PimpedIterator[T](iter: Iterator[T]) {
  def exhaust = while (iter.hasNext) iter.next
}

trait Accumulator[T] {
  def accumulate(t: T): State
  def predicate(t: T): Boolean
}

trait CopyAccumulator[T] extends Accumulator[T] {

  def predicate(t: T): Boolean

  val buffer = scala.collection.mutable.ArrayBuffer[T]()

  def accumulate(t: T) = {
    buffer += t
    TakeMore
  }
}

class Mapper[T](accumulators: List[Accumulator[T]]) {

  val activeAccumulators = scala.collection.mutable.ArrayBuffer(accumulators: _*)

  private def process(t: T): Unit = {
    activeAccumulators.foreach { a =>
      if (a.predicate(t)) {
        a.accumulate(t) match {
          case TakeMore => ()
          case Done => activeAccumulators -= a
        }
      }
    }
  }

  def map(iter: Iterator[T]): Iterator[T] = {
    iter.map { t =>
      process(t)
      t
    }
  }

  def mapUntilDone(iter: Iterator[T]): Iterator[T] = new Iterator[T] {
    var done = false
    def hasNext = !done && iter.hasNext
    def next = {
      val t = iter.next

      process(t)

      if (activeAccumulators.isEmpty) done = true

      t
    }
  }

}