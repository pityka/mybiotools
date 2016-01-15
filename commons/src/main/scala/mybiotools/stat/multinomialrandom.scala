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

import scala.util.Random
import org.apache.commons.math3.random.{ RandomGenerator, RandomDataGenerator }
import org.apache.commons.math3.random.Well19937c

/**
 * Multinomial sampler
 *
 * http://www.sciencedirect.com/science/article/pii/016794739390115A
 * The next method uses the direct method
 * The sample next uses the conditional method with binomial generators from Colt package, and an R translation.
 *
 * @param rnd Commons Math RandomGenerator. Not synchronized.
 * @param sorted the weights are sorted in ascending order
 * @param weights probability distribution
 */
class MultinomialRandomGenerator[T](rnd: RandomGenerator, sorted: Boolean, weights: (T, Double)*) {

  def canEqual(other: Any): Boolean = other.isInstanceOf[MultinomialRandomGenerator[T]]

  override def hashCode: Int = weightsIndexed.hashCode

  override def equals(that: Any): Boolean = that match {
    case t: MultinomialRandomGenerator[T] => t.canEqual(this) && weightsIndexed == t.weightsIndexed
    case _ => false
  }

  def this(rnd: RandomGenerator, weights: (T, Double)*) = this(rnd, false, weights: _*)

  assert(weights.map(_._1).distinct.size == weights.size, " weights Ts should be distinct")
  assert(weights.size > 0, "weights.size <= 0")
  assert(weights.map(_._2).forall(x => 0.0 <= x && 1.0 >= x), "weights must be probabilities")
  assert(math.abs(weights.map(_._2).sum - 1.0) < 1E-8, "weights must sum to 1 . " + weights.map(_._2).sum)

  private val weightsIndexed = if (!sorted) weights.toSeq.sortBy(_._2).map(_._2).toIndexedSeq else weights.map(_._2).toIndexedSeq
  private val order = if (!sorted) weights.toSeq.zipWithIndex.sortBy(_._1._2).map(_._2).toIndexedSeq else weights.toSeq.zipWithIndex.map(_._2).toIndexedSeq

  // Colt's apparently faster binomial generator
  // This is inprecise when n*p is small (I tested against R and http://stackoverflow.com/questions/23561551/a-efficient-binomial-random-number-generator-code-in-java)
  // these n and p values will be overriden
  private val binom = new cern.jet.random.Binomial(1, 0.1, new RandomGeneratorApache2Cold(rnd))

  // This generator agrees with R but is slower. 
  private val randomDataGenerator = new RandomDataGenerator(rnd)

  // R switches methods when n*p < 30
  private def nextBinom(n: Int, p: Double) =
    if (n == 0 || p == 0.0) 0
    else if (p == 1.0) n
    else if (n * p < 30.0) randomDataGenerator.nextBinomial(n, p)
    else binom.nextInt(n, p)

  private val cumulativeWeights: IndexedSeq[(T, Double, Double)] = {
    val cdf = (if (!sorted) weights.sortBy(_._2) else weights).foldLeft(List[(T, Double, Double)]()) {
      case (list, (t0, w0)) =>
        list match {
          case Nil => (t0, 0.0, w0) :: list
          case (t1, start, end) :: xs => (t0, end, end + w0) :: (t1, start, end) :: xs
        }
    }.toIndexedSeq.reverse
    assert(math.abs(cdf.last._3 - 1.0) < 1E-8, cdf)
    val last = cdf.last

    cdf.dropRight(1) :+ (last._1, last._2, 1.0)
  }

  /**
   * Returns the next T according to the probabilities
   *
   * Direct method: throws a uniform [0,1) and looks it up in the CDF with binary search
   */
  def next: T = {
    val p = randomDataGenerator.nextUniform(0.0, 1.0, true)
    mybiotools.binarySearch[(T, Double, Double)](
      cumulativeWeights,
      (x: (T, Double, Double)) => x._3 < p,
      (x: (T, Double, Double)) => x._2 >= p
    ).right.get._2._1
    // cumulativeWeights.find(x => x._2 <= p && x._3 > p).get._1
  }

  /**
   * Returns the next T according to the probabilities
   *
   * Conditional method.
   * Faster than the direct method if the number of categories are fewer than the required sample size (n).
   * The underlying binomial sampling is using cern.jet.random.Binomial or a scala translation of R's rbinom.
   * http://www.sciencedirect.com/science/article/pii/S0167947307001405#bib9
   * http://www.sciencedirect.com/science/article/pii/016794739390115A
   */
  def sample(n: Int): Array[Int] = {
    val result = Array.fill[Int](cumulativeWeights.size)(0)
    var t = 0
    var i = 0
    val s = result.size
    while (i < s) {
      if (i == 0) {
        val next = nextBinom(n, weightsIndexed(0))
        result(order(0)) = next
        t += next
      } else {
        val p = math.min(weightsIndexed(i) / (1.0 - cumulativeWeights(i - 1)._3), 1.0)
        val next = nextBinom(
          n - t,
          p
        )
        result(order(i)) = next
        t += next
      }
      i += 1
    }
    result

  }

}

object MultinomialRandomGenerator {
  def apply[T](weights: (T, Double)*): MultinomialRandomGenerator[T] = new MultinomialRandomGenerator(new Well19937c, weights: _*)
}

