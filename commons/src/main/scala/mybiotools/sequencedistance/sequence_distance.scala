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

import mybiotools.mymatrix._
import org.saddle._

/**
 * Implements various nucleotide sequence distances
 */
package object sequencedistance {
  // http://mvnrepository.com/artifact/com.googlecode.efficient-java-matrix-library/ejml/0.17
  // http://code.google.com/p/efficient-java-matrix-library/wiki/PrincipleComponentAnalysisExample
  // http://www.jblas.org/javadoc/index.html

  /**
   * Calculates pairwise "distance" matrix
   * @tparam K type of key
   * @tparam D type of datapoints
   * @tparam U type of "distance" data
   * @param alignment Input data
   * @param f Distance function.
   */
  def distanceMatrix[K, U, D](alignment: Map[K, D], threads: Int)(f: (D, D) => U)(implicit st1: ST[K], st2: ST[U], ord: Ordering[K]): Frame[K, K, U] = {
    import mybiotools.mapreduce.MapReduceTraversal._
    if (alignment.isEmpty) Frame[K, K, U]()
    else
      Frame(alignment.toSeq.parmap(threads) {
        case (k1, s1) =>
          k1 -> Series(alignment.toSeq.map {
            case (k2, s2) =>
              val v = f(s1, s2)
              k2 -> v
          }: _*)
      }: _*).sortedRIx.sortedCIx
  }

  def distanceMatrix[K, U, D](alignment: Map[K, D])(f: (D, D) => U)(implicit st1: ST[K], st2: ST[U], ord: Ordering[K]): Frame[K, K, U] = distanceMatrix(alignment, 1)(f)

  def correlationNucleotideUpperCase(s1: String, s2: String): Double = {
    var a = 0d
    var b = 0d
    var c = 0d
    var d = 0d
    var i = 0
    val k = s1.size
    while (i < k) {
      val s1i = s1(i)
      val s2i = s2(i)
      if (isNucleotide(s1i) && isNucleotide(s2i)) {
        if (s1i == s2i) {
          d += 3d
          a += 1d
        } else {
          d += 2d
          b += 1d
          c += 1d
        }
      }
      i += 1
    }
    (a * d - b * c) / (math.sqrt((a + b) * (a + c) * (b + d) * (c + d)))

  }

  def commondata(s1: String, s2: String): Double = {
    var sum = 0
    var i = 0
    val k = s1.size
    while (i < k) {
      val s1i = s1(i)
      val s2i = s2(i)
      if (isNucleotide(s1i) && isNucleotide(s2i)) {
        sum += 1
      }
      i += 1
    }
    sum
  }

  def pDistance(s1: String, s2: String): Double = {
    var sum = 0
    var diff = 0
    var i = 0
    val k = s1.size
    while (i < k) {
      val s1i = s1(i)
      val s2i = s2(i)
      if (isNucleotide(s1i) && isNucleotide(s2i)) {
        sum += 1
        if (((s1i == 'a' || s1i == 'A') && (s2i != 'a' && s2i != 'A')) ||
          ((s1i == 't' || s1i == 'T') && (s2i != 't' && s2i != 'T')) ||
          ((s1i == 'g' || s1i == 'G') && (s2i != 'g' && s2i != 'G')) ||
          ((s1i == 'c' || s1i == 'C') && (s2i != 'c' && s2i != 'C'))) diff += 1
      }
      i += 1
    }
    diff.toDouble / sum.toDouble
  }

  def pDistanceUpperCase(s1: String, s2: String): Double = {
    var sum = 0
    var diff = 0
    var i = 0
    val k = s1.size
    while (i < k) {
      val s1i = s1(i)
      val s2i = s2(i)
      if (isNucleotideUpperCase(s1i) && isNucleotideUpperCase(s2i)) {
        sum += 1
        if (s1i != s2i) diff += 1
      }
      i += 1
    }
    diff.toDouble / sum.toDouble
  }

  def pDistanceAA(s1: String, s2: String): Double = {
    var sum = 0
    var diff = 0
    for (i <- 0 to s1.size - 1) {
      if (isAminoAcid(s1(i)) && isAminoAcid(s2(i))) {
        sum += 1
        if (s1(i).toLower == s2(i).toLower) diff += 1
      }
    }
    diff.toDouble / sum.toDouble
  }

  def jc69Distance(s1: String, s2: String): Double = (-0.75) * scala.math.log(1.0 - 4.0 / 3.0 * pDistance(s1, s2)) match {
    case x if x.isNaN => 0
    case x => x
  }

  /**
   * LogDet distance, http://mbe.oxfordjournals.org/content/24/10/2277.full.pdf+html
   *
   * This uses an unconstrained symmetric rate matrix, use only for long sequences:
   * http://www.xungulab.com/publications/GuLi_MBE96.pdf:
   *   As the sequence length becomes longer than 2,000
   *   bp, the estimation bias becomes trivial.
   * Equation (28) in the latter is also suitable for relative rate test.
   *
   * Estimates the expected number of substitutions, see eq. (8) in Gu Li 96,
   * specifically if nucleotide frequencies are stationary and equal to 1/4 then logdet = 2*u*t, u being the mean mutation rate
   */
  def logdetDistance(s1: String, s2: String): Double = {
    import _root_.org.ejml.data.DenseMatrix64F
    import _root_.org.ejml.ops.CommonOps
    import scala.math.log
    assert(s1.size == s2.size)
    val mat = Array.ofDim[Int](16)

    def updateMat(c1: Char, c2: Char): Unit = (c1, c2) match {
      case ('A', 'A') => { mat(0) += 1 }
      case ('A', 'T') => { mat(1) += 1 }
      case ('A', 'G') => { mat(2) += 1 }
      case ('A', 'C') => { mat(3) += 1 }
      case ('T', 'A') => { mat(4) += 1 }
      case ('T', 'T') => { mat(5) += 1 }
      case ('T', 'G') => { mat(6) += 1 }
      case ('T', 'C') => { mat(7) += 1 }
      case ('G', 'A') => { mat(8) += 1 }
      case ('G', 'T') => { mat(9) += 1 }
      case ('G', 'G') => { mat(10) += 1 }
      case ('G', 'C') => { mat(11) += 1 }
      case ('C', 'A') => { mat(12) += 1 }
      case ('C', 'T') => { mat(13) += 1 }
      case ('C', 'G') => { mat(14) += 1 }
      case ('C', 'C') => { mat(15) += 1 }
    }

    var sum = 0
    var i = 0
    val k = s1.size
    while (i < k) {
      if (isNucleotide(s1(i)) && isNucleotide(s2(i))) {
        sum += 1
        updateMat(s1(i).toUpper, s2(i).toUpper)
      }
      i += 1
    }
    val relmat = DenseMatrix64F.wrap(4, 4, mat.map(_.toDouble / sum.toDouble))

    val det = CommonOps.det(relmat)
    var m = 0.0
    for (i <- 0 to 3) {
      var s1 = 0.0
      for (j <- 0 to 3) s1 += relmat.get(i, j)
      var s2 = 0.0
      for (j <- 0 to 3) s2 += relmat.get(j, i)
      m += log(s1) + log(s2)
    }

    val ret = -0.25 * (log(det) - 0.5 * m)
    if (ret.isNaN) 0 else ret
  }

  /**
   * Calculate shared branch lengths using an outgroup node. This only holds for ultrametric trees.
   *
   */
  def covarianceMatrixFromDistanceMatrix[T](dist: Frame[T, T, Double], outgroup: T)(implicit o: Ordering[T], st: ST[T]): Frame[T, T, Double] = {

    def getDist(a: T, b: T): Double = dist.firstCol(a).first(b)

    def getDistanceFromIntersectionToC(a: T, b: T, c: T) = (getDist(a, c) + getDist(b, c) - getDist(a, b)) / 2.0

    val nonOutgroupLeaves = dist.colIx.toSeq.filterNot(_ == outgroup)

    // shortest branch to the outgroup
    val distanceToOutgroup =
      nonOutgroupLeaves.combinations(2).map(x => getDistanceFromIntersectionToC(x(0), x(1), outgroup)).min

    Frame(nonOutgroupLeaves.map { b =>
      val br = getDist(outgroup, b) - distanceToOutgroup
      b ->
        Series(nonOutgroupLeaves.map { a =>
          val ar = getDist(outgroup, a) - distanceToOutgroup
          a -> (ar + br - getDist(a, b)) / 2.0
        }: _*)
    }: _*)

  }

}

