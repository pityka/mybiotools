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

package mybiotools.stat
import cern.jet.random.engine.RandomEngine
import org.apache.commons.math3.random.{ AbstractRandomGenerator, RandomGenerator, RandomAdaptor }
import scala.collection.generic._
import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds

class RandomGeneratorApache2Cold(val apache: RandomGenerator) extends RandomEngine {
  override def nextInt: Int = apache.nextInt
}

class RandomGeneratorApache2Scala(val apache: RandomGenerator) extends scala.util.Random(new RandomAdaptor(apache))

class SynchronizedRandomGenerator(rnd: RandomGenerator) extends AbstractRandomGenerator {
  def nextDouble = synchronized(rnd.nextDouble)
  def setSeed(l: Long) = synchronized(rnd.setSeed(l))
}

object RandomShuffler {

  def reservoirSampling[T, CC[X] <: Traversable[X]](xs: CC[T], size: Int, rng: RandomGenerator)(implicit bf: CanBuildFrom[CC[T], T, CC[T]]): CC[T] = {
    val buf = new ArrayBuffer[T]
    var i = 0

    xs.foreach { x =>
      i += 1
      if (buf.size < size) {
        buf.append(x)
      } else {
        val s = rng.nextInt(i)
        if (s < size) {
          buf(s) = x
        }
      }

    }

    (bf(xs) ++= buf).result()

  }

  def shuffle[T, CC[X] <: TraversableOnce[X]](xs: CC[T], rng: RandomGenerator)(implicit bf: CanBuildFrom[CC[T], T, CC[T]]): CC[T] = {
    val buf = new ArrayBuffer[T] ++= xs

    def swap(i1: Int, i2: Int) {
      val tmp = buf(i1)
      buf(i1) = buf(i2)
      buf(i2) = tmp
    }

    for (n <- buf.length to 2 by -1) {
      val k = rng.nextInt(n)
      swap(n - 1, k)
    }

    (bf(xs) ++= buf).result()
  }

}
