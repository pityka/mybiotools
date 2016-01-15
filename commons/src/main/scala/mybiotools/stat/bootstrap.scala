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

import org.apache.commons.math3.random._

trait ResamplingImpl {

  def rndFactory: RandomGenerator

  private def resampleIndexes(draws: Int, size: Int, rnd: RandomGenerator): Stream[Int] = {

    def loop(i: Int): Stream[Int] =
      if (i < draws) rnd.nextInt(size) #:: loop(i + 1)
      else Stream.empty
    loop(0)
  }

  private def bootstrapIndexes(size: Int, rnd: RandomGenerator): Stream[Int] = resampleIndexes(size, size, rnd)

  def bootstrapReplica[T](coll: IndexedSeq[T], rnd: RandomGenerator): Stream[T] = bootstrapIndexes(coll.size, rnd) map coll

  def resampledReplica[T](coll: IndexedSeq[T], draws: Int, rnd: RandomGenerator): Stream[T] = resampleIndexes(draws, coll.size, rnd) map coll

  def bootstrap[T, R](replicationCount: Int, coll: IndexedSeq[T], threads: Int)(f: (=> Stream[T], Int) => R): Seq[R] = {
    mybiotools.mapreduce.MapReduceTraversal.traverse(1 to replicationCount, threads)(i => f(bootstrapReplica(coll, rndFactory), i) :: Nil)(_ ++ _) get
  }

  def bootstrap[T, R](replicationCount: Int, coll: IndexedSeq[T])(f: (=> Stream[T], Int) => R): Seq[R] = {
    val rnd = rndFactory
    1 to replicationCount map (i => f(bootstrapReplica(coll, rnd), i))
  }

  def bootstrap[T, R](replicationCount: Int, coll: IndexedSeq[T], rnd: RandomGenerator)(f: (=> Stream[T], Int) => R): Seq[R] = {
    1 to replicationCount map (i => f(bootstrapReplica(coll, rnd), i))
  }

  def resample[T, R](replicationCount: Int, draws: Int, coll: IndexedSeq[T])(f: (=> Stream[T], Int) => R): Seq[R] = {
    val rnd = rndFactory
    1 to replicationCount map (i => f(resampledReplica(coll, draws, rnd), i))
  }

  def resample[T, R](replicationCount: Int, draws: Int, coll: IndexedSeq[T], rnd: RandomGenerator)(f: (=> Stream[T], Int) => R): Seq[R] = {
    1 to replicationCount map (i => f(resampledReplica(coll, draws, rnd), i))
  }

  def resample[T, R](replicationCount: Int, draws: Int, coll: IndexedSeq[T], threads: Int)(f: (=> Stream[T], Int) => R): Seq[R] = {
    mybiotools.mapreduce.MapReduceTraversal.traverse(1 to replicationCount, threads)(i => f(resampledReplica(coll, draws, rndFactory), i) :: Nil)(_ ++ _) get
  }

  def bernoulli(p: Double, rnd: RandomGenerator): Boolean = rnd.nextDouble < p

  def partition[T](coll: IndexedSeq[T], p: Double): (IndexedSeq[T], IndexedSeq[T]) = {
    val rnd = rndFactory
    coll partition (_ => bernoulli(p, rnd))
  }

}

object Resampling extends ResamplingImpl {
  def rndFactory = new Well19937c
}

class Resampler(rndFactory1: => RandomGenerator) extends ResamplingImpl {
  def rndFactory = rndFactory1
}