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
import mybiotools.eq._
import java.nio.ByteBuffer

package object stringstore {

  type SubS = HasApplyByte with HasSubstring

  implicit class WithString8Iterator(source: Iterator[Char]) {

    class LineIterator extends scala.collection.AbstractIterator[String8] with Iterator[String8] {

      val buffer = new mybiotools.coll.GrowableByteBuffer

      def isNewline(ch: Char) = ch === '\r' || ch === '\n'

      val iter = source.buffered

      def getc() = iter.hasNext && {
        val ch = iter.next()
        if (ch === '\n') false
        else if (ch === '\r') {
          if (iter.hasNext && iter.head === '\n')
            iter.next()

          false
        } else {
          buffer.push(ch.toByte)
          true
        }
      }
      def hasNext = iter.hasNext
      def next = {
        buffer.clear
        while (getc()) {}
        new String8(buffer.toArray)
      }
    }

    def getLines8: Iterator[String8] = new LineIterator
  }

  private[stringstore] def getBytesFast(str: String): Array[Byte] = {
    val buffer = Array.ofDim[Char](str.length)
    val length = str.length();
    str.getChars(0, length, buffer, 0);
    val b = Array.ofDim[Byte](length)
    var j = 0
    while (j < length) {
      b(j) = buffer(j).toByte;
      j += 1
    }

    b
  }

  def lexicographicCompare(x: Array[Byte], y: Array[Byte]): Int = {
    def loop(idx: Int): Int = {
      if (x.length <= idx) 0
      else if (x(idx) < y(idx)) -1
      else if (x(idx) > y(idx)) +1
      else loop(idx + 1)
    }
    if (x.length < y.length) -1
    else if (x.length > y.length) +1
    else loop(0)
  }

  implicit val String8Ordering = new scala.math.Ordering[String8] {
    def compare(x: String8, y: String8) = if (x eq y) 0 else lexicographicCompare(x.chars, y.chars)
  }

  implicit class String8InterpolationHelper(val sc: StringContext) extends AnyVal {
    def s8(args: Any*): String8 = StringStore(sc.parts.mkString)
  }

}