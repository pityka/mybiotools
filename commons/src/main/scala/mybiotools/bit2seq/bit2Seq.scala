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

package mybiotools.bit2seq

import scala.collection.IndexedSeqLike
import collection.mutable.{ Buffer, BufferLike }
import collection.mutable.{ Builder, ArrayBuffer }
import collection.generic.{ CanBuildFrom, SeqFactory }
import scala.collection.mutable.BitSet
import collection.immutable.IndexedSeq

/** Represents a value with 4 states */
object Bit2State extends Enumeration {
  type Bit2 = Value
  val S1, S2, S3, S4 = Value

  def from2Bools(b1: Boolean, b2: Boolean): Bit2 = (b1, b2) match {
    case (true, true) => S4
    case (true, false) => S3
    case (false, true) => S2
    case (false, false) => S1
  }

  def toBools(value: Bit2): Tuple2[Boolean, Boolean] = value match {
    case S1 => (false, false)
    case S2 => (false, true)
    case S3 => (true, false)
    case S4 => (true, true)
  }

  def fromInt(i: Int): Bit2 = i match {
    case 0 => S1
    case 1 => S2
    case 2 => S3
    case 3 => S4
  }

}

import Bit2State._

/**
 * A mutable IndexedSeq of values with 4 states in a packed representation.
 * @note Prepend, remove, insertAll operations are not supported!
 */
final class Bit2Buffer private (val bits: BitSet, var length: Int) extends collection.mutable.IndexedSeq[Bit2]
    with Bit2SeqLike[Bit2Buffer] {
  // Mandatory re-implementation of `newBuilder` in `IndexedSeq`
  override protected[this] def newBuilder: Builder[Bit2, Bit2Buffer] =
    Bit2Buffer.newBuilder

  protected def fromBitSet(b: BitSet, l: Int) = new Bit2Buffer(b, l)

  def clear { bits.clear; length = 0 }

  def +=(b: Bit2): this.type = {
    val lbs = length * 2
    val tup = Bit2State.toBools(b)
    (tup._1, tup._2) match {
      case (false, false) => length += 1
      case (false, true) =>
        bits.add(lbs + 1); length += 1
      case (true, false) =>
        bits.add(lbs); length += 1
      case (true, true) => bits.add(lbs + 1); bits.add(lbs); length += 1
    }
    this
  }

  def +=:(elem: Bit2): this.type = { throw new java.lang.UnsupportedOperationException; this }

  def insertAll(n: Int, elems: Traversable[Bit2]) { throw new java.lang.UnsupportedOperationException }

  def remove(idx: Int): Bit2 = { throw new java.lang.UnsupportedOperationException; S1 }

  // override def companion = Bit2Buffer

}

object Bit2Buffer {

  import mybiotools.bit2seq.Bit2State.Bit2
  protected def fromSeq(buf: Seq[Bit2]): Bit2Buffer = {
    val lb = scala.collection.mutable.ListBuffer[Int]()
    var lbs = 0
    buf.foreach { s =>
      val tup = Bit2State.toBools(s)
      if (tup._1) lb.append(lbs)
      if (tup._2) lb.append(lbs + 1)
      lbs += 2
    }
    new Bit2Buffer(BitSet(lb: _*), buf.size)
  }

  def apply(Bit2s: Bit2*) = fromSeq(Bit2s)

  def newBuilder: Builder[Bit2, Bit2Buffer] =
    new ArrayBuffer mapResult fromSeq

  implicit def canBuildFromBit2Buffer: CanBuildFrom[Bit2Buffer, Bit2, Bit2Buffer] =
    new CanBuildFrom[Bit2Buffer, Bit2, Bit2Buffer] {
      def apply(): Builder[Bit2, Bit2Buffer] = newBuilder
      def apply(from: Bit2Buffer): Builder[Bit2, Bit2Buffer] = newBuilder
    }

  implicit def canBuildFromSeq: CanBuildFrom[Seq[_], Bit2, Bit2Buffer] =
    new CanBuildFrom[Seq[_], Bit2, Bit2Buffer] {
      def apply(): Builder[Bit2, Bit2Buffer] = newBuilder
      def apply(from: Seq[_]): Builder[Bit2, Bit2Buffer] = newBuilder
    }

}

/**
 * IndexedSeqLike of values with 4 states in a packed representation.
 * Representation is based on BitSet
 */
trait Bit2SeqLike[This]
    extends IndexedSeqLike[Bit2, This] {

  protected val bits: BitSet

  def length: Int

  protected def fromBitSet(b: BitSet, l: Int): This

  /** Mandatory implementation of `apply` in `IndexedSeq` */
  def apply(idx: Int): Bit2 = {
    if (idx < 0 || length <= idx) throw new IndexOutOfBoundsException
    // do the bits BitSet contains (idx)*2+1, (idx)*2 integers?
    val idx2 = (idx) * 2
    Bit2State.from2Bools(bits.contains(idx2), bits.contains(idx2 + 1))
  }

  // def :+( b: Bit2 ): This = {
  //   val lbs = length * 2
  //   val tup = Bit2State.toBools( b )
  //   ( tup._1, tup._2 ) match {
  //     case ( false, false ) => fromBitSet( bits, length + 1 )
  //     case ( false, true ) => fromBitSet( bits + ( lbs + 1 ), length + 1 )
  //     case ( true, false ) => fromBitSet( bits + ( lbs ), length + 1 )
  //     case ( true, true ) => fromBitSet( bits ++ List( lbs, lbs + 1 ), length + 1 )
  //   }
  // }

  def update(idx: Int, elem: Bit2) {
    if (idx < 0 || length <= idx) throw new IndexOutOfBoundsException
    val lbs = idx * 2
    val tup = Bit2State.toBools(elem)
    (tup._1, tup._2) match {
      case (false, false) =>
        bits.remove(lbs); bits.remove(lbs + 1)
      case (false, true) =>
        bits.remove(lbs); bits.add(lbs + 1)
      case (true, false) =>
        bits.remove(lbs + 1); bits.add(lbs)
      case (true, true) => bits.add(lbs); bits.add(lbs + 1)
    }
  }

  /** Optional re-implementation of foreach, to make it more efficient. */
  override def foreach[U](f: Bit2 => U): Unit = {
    val iter = bits.iterator
    var lastIndex = -1
    var first = -1
    var second = -1
    var step2 = false
    if (iter.hasNext) {
      second = iter.next
      while (iter.hasNext) {
        if (!step2) {
          first = second
          second = iter.next
        } else {
          first = iter.next
          second = if (iter.hasNext) iter.next else -1
          step2 = false
        }
        var firstIndex = first / 2
        while (lastIndex < firstIndex - 1) {
          f(Bit2State.from2Bools(false, false))
          lastIndex += 1
        }
        if (first % 2 == 1) {
          f(Bit2State.from2Bools(false, true))
        } else {
          if (second > -1 && second / 2 == firstIndex) {
            f(Bit2State.from2Bools(true, true))
            step2 = true
          } else {
            f(Bit2State.from2Bools(true, false))
          }
        }
        lastIndex += 1
      }
      var firstIndex = second / 2
      if (second > -1 && firstIndex != lastIndex) {
        while (lastIndex < firstIndex - 1) {
          f(Bit2State.from2Bools(false, false))
          lastIndex += 1
        }
        if (second % 2 == 1) {
          f(Bit2State.from2Bools(false, true))
          lastIndex += 1

        } else {
          f(Bit2State.from2Bools(true, false))
          lastIndex += 1

        }
      }
      while (lastIndex < length - 1) {
        f(Bit2State.from2Bools(false, false))
        lastIndex += 1
      }

    } else {
      if (length > 0) {
        for (i <- 0 to length - 1) f(Bit2State.from2Bools(false, false))
      }
    }

  }

}

