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
import java.io.{ File, FileOutputStream, DataOutputStream, FileInputStream, DataInputStream }
import collection.JavaConversions._

sealed class SimpleDiskCache(file: File, akkaserialization: akka.serialization.Serialization) extends Cache with TaskSerializer {

  protected val serialization = akkaserialization

  override def toString = "SimpleDiskCache(size=" + map.size + ")"

  private case class Key(v: Array[Byte]) {

    override val hashCode = scala.util.hashing.MurmurHash3.bytesHash(v)

    def canEqual(other: Any): Boolean = other.isInstanceOf[Key]

    override def equals(that: Any): Boolean = that match {
      case t: Key => (t canEqual this) && java.util.Arrays.equals(this.v, t.v)
      case _ => false
    }
  }

  private val dataoutput = new DataOutputStream(new FileOutputStream(file, true))
  private val storeWriter = new SimpleKeyValueStore(dataoutput)
  private val map: Map[Key, Array[Byte]] = {
    val din = new DataInputStream(new FileInputStream(file))
    val m = SimpleKeyValueStore.reconstruct(din).map(x => Key(x._1) -> x._2).toMap
    din.close
    m
  }

  def shutDown = {
    dataoutput.close
  }

  def get(x: TaskDescription) = map.get(Key(serializeTaskDescription(x))).flatMap(x => scala.util.Try(deserializeResult(x)).toOption)

  def set(x: TaskDescription, r: Result) = {

    val k = serializeTaskDescription(x)
    val v = serializeResult(r)
    storeWriter.put(k, v)
    dataoutput.flush
  }

}