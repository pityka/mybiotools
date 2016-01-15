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

package mybiotools.coll
import java.nio.ByteBuffer

/**
 * This holds a java.nio.ByteBuffer and reallocates with twice the size if full.
 *
 * scala.collection.mutable.ArrayBuffer would do it, but that is not specialized for bytes.
 */
class GrowableByteBuffer(initialCapacity: Int) {

  def this() = this(8192)

  var buffer = ByteBuffer.allocate(initialCapacity)

  def clear = buffer.clear

  def isEmpty = buffer.position == 0

  private def reallocate {
    val copy = ByteBuffer.allocate(buffer.capacity * 2)
    copy.put(buffer.array, buffer.arrayOffset, buffer.position)
    buffer = copy
  }

  def push(b: Byte) {
    if (buffer.position < buffer.limit) {
      buffer.put(b)
    } else {
      reallocate
      push(b)
    }
  }

  def toArray = {
    val ar = Array.ofDim[Byte](buffer.position)
    buffer.flip
    buffer.get(ar, 0, ar.size)
    ar
  }

}
