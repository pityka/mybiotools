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

import java.io.{ DataOutputStream, DataInputStream }

class SimpleKeyValueStore(stream: DataOutputStream) {

  def put(key: Array[Byte], value: Array[Byte]) {
    val zippedKey = SimpleKeyValueStore.compress(key)
    val keySize = zippedKey.size
    val zippedValue = SimpleKeyValueStore.compress(value)
    val valueSize = zippedValue.size
    stream.writeInt(keySize)
    stream.write(zippedKey, 0, keySize)
    stream.writeInt(valueSize)
    stream.write(zippedValue, 0, valueSize)
  }

}

object SimpleKeyValueStore {

  private def compress(ar: Array[Byte]): Array[Byte] = {
    val bos = new java.io.ByteArrayOutputStream(ar.length)
    val gzos = new java.util.zip.GZIPOutputStream(bos)
    gzos.write(ar)
    gzos.close
    bos.close
    bos.toByteArray
  }

  private def decompress(ar: Array[Byte]): Array[Byte] = {
    try {
      val bis = new java.io.ByteArrayInputStream(ar)
      val gzis = new java.util.zip.GZIPInputStream(bis)
      val x = try {
        com.google.common.io.ByteStreams.toByteArray(gzis)
      } catch {
        case _: Throwable => ar
      } finally {
        bis.close
        gzis.close
      }
      x
    } catch {
      case _: Throwable => ar
    }
  }

  def reconstruct(stream: DataInputStream): Iterator[(Array[Byte], Array[Byte])] = new Iterator[(Array[Byte], Array[Byte])] {

    var streamEnded = false

    var nextKeySize: Int = _
    var nextValueSize: Int = _
    var nextKey: Array[Byte] = _
    var nextValue: Array[Byte] = _

    readAhead

    def readAhead {
      try {
        nextKeySize = stream.readInt
        if (nextKeySize >= 0) {

          val bufKey = Array.ofDim[Byte](nextKeySize)
          val retKey = stream.read(bufKey)
          nextKey = bufKey

          nextValueSize = stream.readInt
          if (nextValueSize >= 0) {
            val bufVal = Array.ofDim[Byte](nextValueSize)
            val retVal = stream.read(bufVal)
            nextValue = bufVal
            if (retKey < 0 && retVal < 0) {
              streamEnded = true
            }
          } else {
            streamEnded = true
          }
        } else {
          streamEnded = true
        }

      } catch {
        case e: java.io.EOFException => {
          streamEnded = true
        }
      }
    }

    def hasNext = !streamEnded

    def next = {
      val ret = (decompress(nextKey), decompress(nextValue))
      readAhead
      ret
    }

  }
}