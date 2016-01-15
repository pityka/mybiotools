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

/** Gene Set Enrichment Analysis http://www.pnas.org/content/102/43/15545 */
object GSEA {

  private def absexp(i: Double, p: Double) =
    if (p == 0.0) 1.0
    else if (p == 1.0) math.abs(i)
    else math.pow(math.abs(i), p)

  def enrichmentScore(ordered: IndexedSeq[(Double, Boolean)], p: Double): Double = {
    val N = ordered.size
    val NH = ordered.filter(_._2).size
    val (scores, memberships) = ordered.unzip
    enrichmentScore(scores, memberships, p, NH, N)
  }
  def enrichmentScore(scores: IndexedSeq[Double], memberships: IndexedSeq[Boolean], p: Double, NH: Int, N: Int): Double = {

    val NR = {
      var s = 0.0
      var i = 0
      while (i < N) {
        if (memberships(i)) {
          s += absexp(i, p)
        }
        i += 1
      }
      s

    }

    var min = Double.PositiveInfinity
    var max = Double.NegativeInfinity
    var newHits = 0.0
    var newNonHits = 0
    var i = 0
    val NmNH = (N - NH).toDouble
    while (i < N) {

      val membership = memberships(i)
      val score = scores(i)

      if (membership) { newHits += absexp(score, p) }
      if (!membership) { newNonHits += 1 }

      val sum = (newHits / NR) - (newNonHits / NmNH)

      if (sum < min) { min = sum }
      if (sum > max) { max = sum }

      i += 1
    }

    if (math.abs(max) > math.abs(min)) max else min
  }

  /** This is not recommended in the original paper for expression data. */
  def permuteGenes(b: IndexedSeq[Boolean]): IndexedSeq[Boolean] = {
    scala.util.Random.shuffle(b)
  }

  def nominalPValue(n: Int, sorted: IndexedSeq[(Double, Boolean)], p: Double) = {
    val NH = sorted.filter(_._2).size
    val N = sorted.size
    val (scores, memberships) = sorted.unzip
    val es = enrichmentScore(scores, memberships, p, NH, N)

    val permutations = 1 to n map (i => enrichmentScore(scores, permuteGenes(memberships), p, NH, N))
    if (es > 0) {
      val tail = permutations.filter(_ > 0)
      tail.filter(_ > es).size / (tail.size.toDouble)
    } else {
      val tail = permutations.filter(_ <= 0)
      tail.filter(_ < es).size / (tail.size.toDouble)
    }
  }

}