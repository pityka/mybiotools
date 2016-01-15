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

import htsjdk.samtools.util.{ BlockCompressedInputStream => BCIS }
import java.io.{ InputStream, InputStreamReader, BufferedReader, Writer }
import java.nio.ByteBuffer
import java.nio.charset.Charset
import scala.collection.mutable.StringBuilder
import scala.io.Source
import mybiotools.eq._
import mybiotools.stringstore._

package object flatindex {
  val LF = '\n'.toByte
  val CR = '\r'.toByte

  def compose[T, K](e1: InputStream => Option[T])(f: T => K) =
    (i: InputStream) => e1(i).map(f)

  def asciilineExtractor: InputStream => Option[String8] = {

    val sb = new mybiotools.coll.GrowableByteBuffer()
    var lastWasCR = false

    {
      (is: InputStream) =>

        def readChar: Option[Byte] = {
          val b = is.read

          if (b < 0) None
          else {
            Some(b.toByte)
          }
        }

        sb.clear
        val ch1 = readChar
        if (ch1.isEmpty) None
        else {
          if ((ch1.get === LF && !lastWasCR) || ch1.get === CR) {
            lastWasCR = ch1.isDefined && ch1.get === CR
            Some(s8"")
          } else {
            if (ch1.get !== LF) {
              sb.push(ch1.get.toByte)
            }
            var ch = readChar
            while (ch.isDefined && !(ch.get === LF || ch.get === CR)) {
              sb.push(ch.get.toByte)
              ch = readChar
            }
            lastWasCR = ch.isDefined && ch.get === CR
            if (ch.isEmpty && sb.isEmpty) None else Some(new String8(sb.toArray))
          }
        }

    }
  }

  def writeIndex[T](index: Iterable[(T, Long)], writer: Writer, f: T => String) {
    index.foreach {
      case (t, l) =>
        writer.write(f(t) + " " + l + "\n")
    }
  }

  def readIndex[T](s: Source, f: String => T): Iterator[(T, Long)] = {
    s.getLines.map { l =>
      val spl = l.fastSplitIterator(' ')
      (f(spl.next) -> spl.next.toLong)
    }
  }

  def lookup[T, K](is: BCIS, extractor: InputStream => Option[K], index: collection.Map[T, Long]): T => K = (t: T) => {
    is.seek(index(t))
    extractor(is).get
  }

  def indexStream[T](is: BCIS, extractor: InputStream => Option[T]): scala.collection.Map[T, Long] = {
    is.available
    var position = is.getFilePointer
    var elem = extractor(is)
    val mmap = scala.collection.mutable.HashMap[T, Long]()
    while (elem.isDefined) {
      mmap.update(elem.get, position)
      position = is.getFilePointer
      elem = extractor(is)
    }
    mmap
  }

  def indexAscii[KEY](is: BCIS)(sep: String8 => KEY) =
    indexStream(is, compose(asciilineExtractor)(sep))

  def lookupAscii[KEY, V](is: BCIS, index: collection.Map[KEY, Long])(sep: String8 => V): KEY => V =
    lookup(is, compose(asciilineExtractor)(sep), index)

}