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

package mybiotools.intervaltree

trait Interval {
  def from: Int
  def to: Int
  def size = to - from
  def intersects[T <: Interval](that: T): Boolean = that.size > 0 && this.size > 0 && (that.from < this.to && this.from < that.to)
}

case class IntervalTreeElement[T <: Interval](elem: T, max: Int)

case class IntervalWithPayLoad[T](from: Int, to: Int, payload: T) extends Interval

object IntervalTree {
  def makeTree[T <: Interval](i: List[T]): Tree[IntervalTreeElement[T]] = Tree.toTree(i.map(i => IntervalTreeElement(i, i.to))) { (elem, left, right) =>
    (elem, left, right) match {
      case (elem, None, None) => elem
      case (elem, Some(x), None) => elem.copy(max = (elem.max max x.max))
      case (elem, None, Some(x)) => elem.copy(max = (elem.max max x.max))
      case (elem, Some(x), Some(y)) => elem.copy(max = ((elem.max max x.max) max y.max))
    }
  }

  def lookup[Q <: Interval, T <: Interval](query: Q, tree: IntervalTree[T]): List[T] = {
    if (query.size == 0) Nil
    else
      tree match {
        case EmptyTree => Nil
        case NonEmptyTree(IntervalTreeElement(interval, maxTo), left, right) =>
          if (maxTo < query.from) Nil
          else if (interval.from >= query.to) lookup(query, left)
          else {
            val hit = if (interval.size > 0 && interval.from < query.to && interval.to > query.from) Some(interval) else None
            hit.toList ::: (lookup(query, left) ::: lookup(query, right))
          }
      }

  }

  def toPerChromosomeIntervalTree[T <: Interval](in: Iterator[(String, T)]): Map[String, IntervalTree[T]] = {

    val perchromosome = scala.collection.mutable.Map[String, List[T]]()

    in.foreach {
      case (chr, interval) =>
        perchromosome.get(chr) match {
          case None => perchromosome.update(chr, List(interval))
          case Some(x) => perchromosome.update(chr, interval :: x)
        }
    }

    perchromosome.map(x => x._1 -> IntervalTree.makeTree(x._2)).toMap

  }

}