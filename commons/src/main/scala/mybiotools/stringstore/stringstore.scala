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

package mybiotools.stringstore

import mybiotools.config.Config.configInstance
import scala.collection.mutable.{ Builder, ArrayBuffer }
import scala.collection.generic.CanBuildFrom
import mybiotools.eq._

trait HasApplyByte {
  def length: Int
  def apply(i: Int): Byte
}

trait HasSubstring {
  def substring(i: Int, end: Int): SubString8
}

class SubString8 private[stringstore] (
    private val chars: Array[Byte],
    private val start: Int,
    val length: Int
) extends Serializable with IndexedSeq[Byte] with HasApplyByte with HasSubstring {

  def content = {
    val ar = Array.ofDim[Byte](length)
    System.arraycopy(chars, start, ar, 0, length)
    ar
  }

  def substring(i: Int, end: Int) = {
    if (end - i > length || i < 0) throw new ArrayIndexOutOfBoundsException(i - end)
    new SubString8(chars, start + i, end - i)
  }

  override def toString = value

  def toString8 = StringStore(this)

  def apply(i: Int) = if (i < start + length) chars(i + start) else throw new ArrayIndexOutOfBoundsException("")

  def value: String = new String(chars, start, length, "US-ASCII")

  def checkEquals(t: HasApplyByte): Boolean = {
    if (t.length === length) {
      var i = 0
      var b = true
      while (b && i < length) {
        b = (apply(i) === t.apply(i))
        i += 1
      }
      b
    } else false
  }

  override def equals(other: Any): Boolean = other match {
    case that: SubString8 => checkEquals(that)
    case that: String8 => checkEquals(that)
    case _ => false
  }

  override lazy val hashCode = com.google.common.hash.Hashing.murmur3_32.hashBytes(chars, start, length).asInt

}

final class String8 private[mybiotools] (
    val chars: Array[Byte]
) extends Serializable with IndexedSeq[Byte] with HasApplyByte with HasSubstring {

  // This is only not private due to tests :(
  def this(s: String) = this(getBytesFast(s))

  private[stringstore] def this(sub: SubString8) = this(sub.content)

  override def toString = value

  override def equals(other: Any): Boolean = other match {
    case that: String8 => java.util.Arrays.equals(chars, that.chars)
    case _ => false
  }

  override lazy val hashCode = com.google.common.hash.Hashing.murmur3_32.hashBytes(chars).asInt

  def value: String =
    new String(chars, "US-ASCII")

  def apply(i: Int): Byte = chars(i)

  def length = chars.length

  def substring(start: Int, end: Int) = {
    if (end - start > length || end - start < 0) throw new ArrayIndexOutOfBoundsException(start - end)
    new SubString8(chars, start, end - start)
  }

}

object String8 {
  def newBuilder: Builder[Byte, String8] =
    new ArrayBuffer[Byte] mapResult ((seq: ArrayBuffer[Byte]) => new String8(seq.toArray))
  implicit def canBuildFrom =
    new CanBuildFrom[Seq[Byte], Byte, String8] {
      def apply(): Builder[Byte, String8] = newBuilder
      def apply(from: Seq[Byte]): Builder[Byte, String8] = newBuilder
    }
}

object StringStore {

  private val doInternalization: Boolean = mybiotools.catchToLeft(configInstance.getBoolean("stringstore.intern")) match {
    case Left(_) => false
    case Right(x) => {
      if (x) {
        println("Intern Stringstore on")
      }
      x
    }
  }

  private val data = scala.collection.mutable.WeakHashMap[String8, String8]()

  private val rwlock = new java.util.concurrent.locks.ReentrantReadWriteLock();

  def apply(chars: Array[Byte]): String8 = apply(new String8(chars))

  def apply(s16: String): String8 = apply(new String8(s16))

  def apply(sub: SubString8): String8 = apply(new String8(sub))

  def apply(s: String8): String8 = {

    if (!doInternalization) s
    else {

      rwlock.readLock().lock();

      val x = try (data.get(s)) finally {
        rwlock.readLock().unlock();
      }

      x match {
        case None => {
          rwlock.writeLock().lock();
          try {
            data.update(s, s)
          } finally {
            rwlock.writeLock().unlock();

          }

          s
        }
        case Some(x) => x
      }
    }
  }
}