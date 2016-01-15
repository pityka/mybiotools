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

package mybiotools.flatindex

import org.scalatest.FunSpec
import org.scalatest.Matchers
import collection.immutable.HashMap
import java.io._
import mybiotools._
import htsjdk.samtools.util.{ BlockCompressedInputStream, BlockCompressedOutputStream }
import mybiotools.stringstore._

class FlatIndexSpec extends FunSpec with Matchers {

  describe("indexStream") {
    def test(str: String) = {
      val tmp = TempFile.createTempFile("dfs")
      val os = new BlockCompressedOutputStream(tmp)
      str.getBytes("US-ASCII").foreach(os.write(_))
      os.close
      val is = new BlockCompressedInputStream(tmp)
      val extractor = asciilineExtractor
      val index = indexStream(is, extractor)
      val query = lookup(is, extractor, index)
      val elems = scala.io.Source.fromString(str).getLines.toList
      elems.map(x => query(StringStore(x)) should equal(StringStore(x)))

    }
    it("LF") {
      test("\n\n0\n11\n\n22\n333\n444")
      test("\n\n0\n11\n\n22\n333\n444\n\n")
    }
    it("CR") {
      test("\r\r0\r11\r\r22\r333\r444")
      test("\r\r0\r11\r\r22\r333\r444\r\r")
    }
    it("CRLF") {
      test("\r\n0\r\n11\r\n22\r\n333\r\n444")
      test("\r\n0\r\n11\r\n22\r\n333\r\n444\r\n")
    }
  }

  describe("lineExtractor") {

    def test(str: String) = {
      val is = new ByteArrayInputStream(str.getBytes("US-ASCII"))
      val extractor = asciilineExtractor
      val iter = new Iterator[String] {
        var b = extractor(is)
        def hasNext = b.isDefined
        def next = {
          val r = b.get
          b = extractor(is)
          r
        }
      }
      iter.toList should equal(scala.io.Source.fromString(str).getLines.toList)
    }

    it("LF") {
      test("\n\n0\n11\n\n22\n333\n444")
      test("\n\n0\n11\n\n22\n333\n444\n\n")
    }
    it("CR") {
      test("\r\r0\r11\r\r22\r333\r444")
      test("\r\r0\r11\r\r22\r333\r444\r\r")
    }
    it("CRLF") {
      test("\r\n\r\n0\r\n11\r\n\r\n22\r\n333\r\n444")
      test("\r\n\r\n0\r\n11\r\n\r\n22\r\n333\r\n444\r\n\r\n")
    }

  }

}