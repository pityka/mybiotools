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

package mybiotools.mapreduce

import scala.collection.generic.CanBuildFrom
import scala.collection.GenTraversableOnce
import scala.util.Try

trait MapReduceScheme[A, I] {
  val map: A => I
  val reduce: (I, I) => I
}

object MapReduceScheme {

  def combine[A, I1, I2](mr1: MapReduceScheme[A, I1], mr2: MapReduceScheme[A, I2]): MapReduceScheme[A, (I1, I2)] = new MapReduceScheme[A, (I1, I2)] {
    val map = (a: A) => (mr1.map(a), mr2.map(a))
    val reduce: ((I1, I2), (I1, I2)) => (I1, I2) = (i1: (I1, I2), i2: (I1, I2)) => (mr1.reduce(i1._1, i2._1), mr2.reduce(i1._2, i2._2))
  }

  def combine[A, I1, I2, I3](
    mr1: MapReduceScheme[A, I1],
    mr2: MapReduceScheme[A, I2],
    mr3: MapReduceScheme[A, I3]
  ): MapReduceScheme[A, (I1, I2, I3)] = new MapReduceScheme[A, (I1, I2, I3)] {
    val map = (a: A) => (mr1.map(a), mr2.map(a), mr3.map(a))
    val reduce = (i1: (I1, I2, I3), i2: (I1, I2, I3)) => (mr1.reduce(i1._1, i2._1), mr2.reduce(i1._2, i2._2), mr3.reduce(i1._3, i2._3))
  }

  def combine[A, I1, I2, I3, I4](
    mr1: MapReduceScheme[A, I1],
    mr2: MapReduceScheme[A, I2],
    mr3: MapReduceScheme[A, I3],
    mr4: MapReduceScheme[A, I4]
  ): MapReduceScheme[A, (I1, I2, I3, I4)] = new MapReduceScheme[A, (I1, I2, I3, I4)] {
    val map = (a: A) => (mr1.map(a), mr2.map(a), mr3.map(a), mr4.map(a))
    val reduce = (i1: (I1, I2, I3, I4), i2: (I1, I2, I3, I4)) => (mr1.reduce(i1._1, i2._1), mr2.reduce(i1._2, i2._2), mr3.reduce(i1._3, i2._3), mr4.reduce(i1._4, i2._4))
  }

  def combine[A](mrs: Map[String, MapReduceScheme[A, Any]]): MapReduceScheme[A, Map[String, Any]] = new MapReduceScheme[A, Map[String, Any]] {
    val map = (a: A) => mrs.map(x => x._1 -> x._2.map(a))
    val reduce = (i1: Map[String, Any], i2: Map[String, Any]) => mrs.map(x => x._1 -> x._2.reduce(i1(x._1), i2(x._1)))
  }

}

object MapReduceTraversal {

  def traverse[A, I](
    collection: GenTraversableOnce[A],
    mapreduce: MapReduceScheme[A, I],
    numberOfReducers: Int = 10,
    numberOfMappers: Int = 10
  ): Try[I] = {
    // val mrs = new MapReduceSystem[A, I](numberOfReducers, numberOfMappers, bufferFactor, mapreduce.map, mapreduce.reduce)
    // collection.foreach { x => mrs.add(x) }
    // val r = scala.concurrent.Await.result(mrs.last, scala.concurrent.duration.Duration.Inf)
    // mrs.close
    // r
    val mapped = mybiotools.ParIterator.map(collection, numberOfMappers, false)(mapreduce.map)
    scala.util.Try(mybiotools.ParIterator.reduce(mapped, numberOfReducers)(mapreduce.reduce))
  }

  def traverse[A, I](
    coll: GenTraversableOnce[A],
    concurrency: Int
  )(map1: A => I)(reduce1: (I, I) => I): Try[I] = traverse(coll, new MapReduceScheme[A, I] {
    val map = map1
    val reduce = reduce1
  }, concurrency, concurrency)

  implicit class EnrichedCollection[A](self: GenTraversableOnce[A]) extends AnyRef {
    def mapreduce[I](concurrency: Int)(map1: A => I)(reduce1: (I, I) => I) = MapReduceTraversal.traverse(self, new MapReduceScheme[A, I] {
      val map = map1
      val reduce = reduce1
    }, concurrency, concurrency) get

    def parmap[I](concurrency: Int)(map1: A => I) = mapreduce(concurrency)(x => Vector(map1(x)))(_ ++ _)
  }

}

