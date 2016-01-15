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

package mybiotools

import scala.concurrent.{ ExecutionContext, future, Future, blocking, Promise }
import scala.concurrent.Await.result
import scala.concurrent.duration.Duration.Inf
import java.util.concurrent.{ BlockingQueue, Executors, LinkedBlockingQueue }
import java.util.concurrent.ForkJoinPool
import java.util.concurrent.Executors
import scala.collection.GenTraversableOnce
import scala.collection.JavaConversions._
import scala.util._

object ParIterator {

  def reduce[A](i: Iterator[A], cpus: Int)(f: (A, A) => A): A = {
    val fjp = mybiotools.concurrent.newJavaForkJoinPoolWithNamePrefix("pariterator-reduce", cpus)
    // val st = Executors.newSingleThreadExecutor

    val consumer = ExecutionContext.fromExecutorService(fjp)
    // val producer = ExecutionContext.fromExecutor(st)
    val queue: BlockingQueue[Future[A]] = new LinkedBlockingQueue(cpus + 1)

    def takeReady: Option[Future[A]] = {
      import scala.collection.JavaConversions._

      val r = queue.iterator.find(x => x.isCompleted)
      r.foreach { x =>
        queue.remove(x)
      }
      r

    }

    def take: Future[A] = takeReady.getOrElse(queue.take())

    var r: Future[A] = null

    if (!i.hasNext) throw new IllegalArgumentException("Empty iterator")

    while (i.hasNext || queue.peek != null) {
      if (!i.hasNext) {
        val f1 = take
        if (queue.peek == null) {
          r = f1
        } else {
          val f2 = take
          implicit val e = consumer
          queue.put(f1.flatMap(x => f2.map(y => f(x, y))))
        }
      } else {
        val f1 = takeReady.getOrElse(Future.successful(i.next))
        val f2 =
          if (i.hasNext) {
            if (queue.remainingCapacity > 0) Some(takeReady.getOrElse(Future.successful(i.next)))
            else Some(take)
          } else {
            if (queue.peek != null) Some(take) else None
          }
        implicit val e = consumer

        f2.foreach { f2 =>
          queue.put(f1.flatMap(x => f2.map(y => f(x, y))))
        }

        if (f2.isEmpty) {
          r = f1
        }

      }
    }

    result(r, Inf)

  }

  def map[A, B](i: GenTraversableOnce[A], cpus: Int, ordered: Boolean = true)(f: A => B): Iterator[B] = {
    // this is a potential resource leak: if the the caller does not pull all elements out of i then these won't be closed
    val fjp = mybiotools.concurrent.newJavaForkJoinPoolWithNamePrefix("pariterator-map", cpus)
    val st = Executors.newSingleThreadExecutor

    val consumer = ExecutionContext.fromExecutorService(fjp)
    val producer = ExecutionContext.fromExecutor(st)
    val capacity = if (ordered) cpus else math.max(cpus / 2, 1)
    val queue: BlockingQueue[Option[Future[Try[B]]]] = new LinkedBlockingQueue(capacity)

    def firstCompletedWithIndex[T](futures: Seq[Future[T]]): Future[(T, Int)] = {
      var p = Promise[(T, Int)]()
      val completeFirst: (Try[T], Int) => Boolean = {
        case (t, i) =>
          p.tryComplete(t.map(x => (x, i)))
      }
      futures.zipWithIndex.foreach { case (f, i) => f.onComplete(completeFirst(_, i))(consumer) }
      p.future
    }

    Future {

      try {

        i.foreach { x =>
          queue.put(Some(Future {
            Try(f(x))
          }(consumer)))
        }

        queue.put(None) // poison 

      } catch {
        case e: Throwable => queue.put(Some(Future.successful(Failure[B](e))))
      }

    }(producer)

    new Iterator[B] {

      private[this] var alive = true
      private[this] val buffer = new java.util.ArrayList[Option[Future[Try[B]]]]
      private[this] val max = if (ordered) 1 else math.max(cpus / 2, 1)

      override def next() =
        if (hasNext) {

          queue.drainTo(buffer, max - buffer.size)
          val (v, idx) = result(firstCompletedWithIndex(buffer.filter(_.isDefined).map(_.get)), Inf)
          buffer.remove(idx)
          v match {
            case Success(x) => x
            case Failure(e) => throw e
          }
        } else Iterator.empty.next()

      override def hasNext: Boolean = alive && take().isDefined

      private def take() = {
        if (buffer.size == 1 && buffer.head.isEmpty) {
          alive = false;
          fjp.shutdown
          st.shutdown
          None
        } else {
          if (buffer.isEmpty) {
            (queue.take()) match {
              case None => {
                alive = false;
                fjp.shutdown
                st.shutdown
              }
              case Some(x) => {
                buffer.append(Some(x))
              }
            }
          }
          buffer.headOption
        }

      }

    }
  }

}