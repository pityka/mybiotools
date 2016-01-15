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

package mybiotools.sequence.alignment

import mybiotools.eq._
import mybiotools._
import mybiotools.stringstore._

trait KMer {
  def value: SubString8
  def h: Int
  override def equals(that: Any) = that match {
    case x: KMer => value.equals(x.value)
    case _ => false
  }

  override def hashCode = h
  override def toString = value.toString
}

case class MutableMatrix[@specialized(Int) T](rows: Int, cols: Int, data: Array[T]) {
  def apply(row: Int, col: Int): T = data(row * cols + col)
  def update(row: Int, col: Int, t: T): Unit = {
    data(row * cols + col) = t
  }
  override def toString = "MutableMatrix(" + rows + "," + cols + ",\n" + (0 until rows map (i => 0 until cols map (j => this(i, j)) mkString (" ")) mkString ("\n")) + ")"

  def copy(implicit ct: scala.reflect.ClassTag[T]) = {
    val d2 = Array.ofDim[T](rows * cols)
    System.arraycopy(data, 0, d2, 0, data.length)
    MutableMatrix(rows, cols, d2)
  }

}
object MutableMatrix {
  def apply(r: Int, c: Int): MutableMatrix[Int] = MutableMatrix(r, c, Array.fill(r * c)(0))
  def makeDouble(r: Int, c: Int): MutableMatrix[Double] = MutableMatrix(r, c, Array.fill(r * c)(0.0))
  def apply(s: String): MutableMatrix[Int] = {
    val lines = s.split("\n").map(_.split("\\s+"))
    val rows = lines.size
    val cols = lines.head.size
    MutableMatrix(rows, cols, lines.flatten.map(_.toInt).toArray)
  }
}

object OverlapPairwiseAlignment {

  def overlapAlignmentBacktrack(v: String, w: String, scores: Map[(Char, Char), Int], indelpenalty: Int): (MutableMatrix[Int], Int, Int, Int) = {
    val n = v.size
    val m = w.size
    val s = MutableMatrix(n + 1, m + 1)
    val b = MutableMatrix(n, m)
    for (i <- 0 to n) {
      s(i, 0) = 0 //-1 * i * indelpenalty
    }
    for (j <- 0 to m) {
      s(0, j) = -1 * j * indelpenalty
    }
    for (i <- 1 to n; j <- 1 to m) {
      val move = List(
        0 -> (s(i - 1, j) - indelpenalty),
        1 -> (s(i, j - 1) - indelpenalty),
        2 -> (s(i - 1, j - 1) + scores((v.charAt(i - 1), w.charAt(j - 1))))
      // 3 -> (if (j == m) s(i - 1, j) else -1 * Int.MaxValue)
      ).maxBy(_._2)
      b(i - 1, j - 1) = move._1
      s(i, j) = move._2

    }
    s(n, m) = (0 until m map (i => s(n, i)) max)
    val (mi, mj) = ((0 until s.rows).iterator.flatMap { i =>
      (0 until s.cols).iterator.map { j =>
        (i, j)
      }
    }).find(x => s(x._1, x._2) == s(n, m) && x._1 == n).get

    (b, s(n, m), mi - 1, mj - 1)
  }

  def overlapAlignmentEmit(v: String, w: String, backtrack: MutableMatrix[Int], mi: Int, mj: Int): (String, String) = {
    def loop(i: Int, j: Int, acc1: String, acc2: String): (String, String) = {
      if (i == -1 && j == -1) (acc1, acc2)
      else if (i == -1) ('-' +: acc1, w.charAt(j) +: acc2)
      else if (j == -1) (acc1, acc2)
      else if (i == v.size - 1 && j == w.size - 1 && (mi != i || mj != j)) loop(mi, mj, acc1, acc2)
      else {
        if (backtrack(i, j) == 0) loop(i - 1, j, v.charAt(i) +: acc1, '-' +: acc2)
        else if (backtrack(i, j) == 1) loop(i, j - 1, '-' +: acc1, w.charAt(j) +: acc2)
        else if (backtrack(i, j) == 2) loop(i - 1, j - 1, v.charAt(i) +: acc1, w.charAt(j) +: acc2)
        else loop(i - 1, j, acc1, w.charAt(j) +: acc2)
      }
    }

    loop(v.size - 1, w.size - 1, "", "")
  }

  def overlapAlignment(v: String, w: String, scores: Map[(Char, Char), Int], indelpenalty: Int): (Int, String, String) = {

    val (backtrack, maxScore, mi, mj) = overlapAlignmentBacktrack(v, w, scores, indelpenalty)
    val (s1, s2) = overlapAlignmentEmit(v, w, backtrack, mi, mj)
    (maxScore, s1, s2)
  }

}

object FittingPairwiseAlignment {

  def fittingAffineAlignmentBacktrack(v: String, w: String, scores: Map[(Char, Char), Int], gapopen: Int, gapextension: Int) = {

    val (vInt, wInt, scoresInt, char2int) = PenaltyHelper(v, w, scores)

    val n = v.size
    val m = w.size
    val lower = MutableMatrix(n + 1, m + 1)

    val blower = MutableMatrix(n, m)
    val bmiddle = blower.copy
    val bupper = blower.copy
    for (i <- 1 to n) {
      lower(i, 0) = 0 //-1 * math.max(0, i - 1) * gapextension - gapopen
    }
    for (j <- 1 to m) {
      lower(0, j) = -1 * math.max(0, j - 1) * gapextension - gapopen
    }

    val middle = lower.copy
    val upper = lower.copy

    var i = 1
    while (i <= n) {
      var j = 1
      while (j <= m) {
        {

          {
            val cost0 = (lower(i - 1, j) - gapextension)
            val cost3 = (middle(i - 1, j) - gapopen)
            if (cost0 >= cost3) {
              blower(i - 1, j - 1) = 0
              lower(i, j) = cost0
            } else {
              blower(i - 1, j - 1) = 3
              lower(i, j) = cost3
            }
          }

          {
            val cost1 = (upper(i, j - 1) - gapextension)
            val cost4 = (middle(i, j - 1) - gapopen)
            if (cost1 >= cost4) {
              bupper(i - 1, j - 1) = 1
              upper(i, j) = cost1
            } else {
              bupper(i - 1, j - 1) = 4
              upper(i, j) = cost4
            }
          }

          {
            val cost2 = (middle(i - 1, j - 1) + scoresInt(vInt(i - 1), wInt(j - 1)))
            val cost5 = (lower(i, j))
            val cost6 = (upper(i, j))
            if (cost2 >= cost5 && cost2 >= cost6) {
              bmiddle(i - 1, j - 1) = 2
              middle(i, j) = cost2
            } else if (cost5 >= cost2 && cost5 >= cost6) {
              bmiddle(i - 1, j - 1) = 5
              middle(i, j) = cost5
            } else {
              bmiddle(i - 1, j - 1) = 6
              middle(i, j) = cost6
            }
          }

        }
        j += 1
      }
      i += 1
    }

    middle(n, m) = (0 until n map (i => middle(i, m)) max)
    val (mi, mj) = ((0 until middle.rows).iterator.flatMap { i =>
      (0 until middle.cols).iterator.map { j =>
        (i, j)
      }
    }).find(x => middle(x._1, x._2) == middle(n, m) && x._2 == m).get

    (blower, bmiddle, bupper, middle(n, m), mi - 1, mj - 1)
  }

  def fittingAffineAlignmentEmit(v: String, w: String, backtracklower: MutableMatrix[Int], backtrackmiddle: MutableMatrix[Int], backtrackupper: MutableMatrix[Int], mi: Int, mj: Int): (String, String) = {
    def loop(i: Int, j: Int, k: Int, acc1: List[Char], acc2: List[Char]): (String, String) = {
      val mat = if (k == 0) backtracklower else if (k == 2) backtrackmiddle else backtrackupper
      if (i == -1 && j == -1) (acc1.mkString, acc2.mkString)
      else if (i == -1) loop(i, j - 1, k, '-' :: acc1, w.charAt(j) :: acc2)
      else if (j == -1) loop(i - 1, j, k, acc1, acc2)
      else if (i == v.size - 1 && j == w.size - 1 && (mi != i || mj != j)) loop(mi, mj, k, acc1, acc2)
      else {
        if (mat(i, j) == 0) loop(i - 1, j, 0, v.charAt(i) :: acc1, '-' :: acc2)
        else if (mat(i, j) == 3) loop(i - 1, j, 2, v.charAt(i) :: acc1, '-' :: acc2)
        else if (mat(i, j) == 1) loop(i, j - 1, 1, '-' :: acc1, w.charAt(j) :: acc2)
        else if (mat(i, j) == 4) loop(i, j - 1, 2, '-' :: acc1, w.charAt(j) :: acc2)
        else if (mat(i, j) == 2) loop(i - 1, j - 1, 2, v.charAt(i) :: acc1, w.charAt(j) ::
          acc2)
        else if (mat(i, j) == 5) loop(i, j, 0, acc1, acc2)
        else loop(i, j, 1, acc1, acc2)
      }
    }
    loop(v.size - 1, w.size - 1, 2, Nil, Nil)
  }

  def fittingAlignmentBacktrack(v: String, w: String, scores: Map[(Char, Char), Int], indelpenalty: Int): (MutableMatrix[Int], Int, Int, Int) = {
    val n = v.size
    val m = w.size

    val (vInt, wInt, scoresInt, char2int) = PenaltyHelper(v, w, scores)

    val s = MutableMatrix(n + 1, m + 1)
    val b = MutableMatrix(n, m)
    for (i <- 0 to n) {
      s(i, 0) = 0 //-1 * i * indelpenalty
    }
    for (j <- 0 to m) {
      s(0, j) = -1 * j * indelpenalty
    }
    for (i <- 1 to n; j <- 1 to m) {
      val move = List(
        0 -> (s(i - 1, j) - indelpenalty),
        1 -> (s(i, j - 1) - indelpenalty),
        2 -> (s(i - 1, j - 1) + scoresInt(vInt(i - 1), wInt(j - 1)))
      // 3 -> (if (j == m) s(i - 1, j) else -1 * Int.MaxValue)
      ).maxBy(_._2)
      b(i - 1, j - 1) = move._1
      s(i, j) = move._2

    }
    s(n, m) = (0 until n map (i => s(i, m)) max)
    val (mi, mj) = ((0 until s.rows).iterator.flatMap { i =>
      (0 until s.cols).iterator.map { j =>
        (i, j)
      }
    }).find(x => s(x._1, x._2) == s(n, m) && x._2 == m).get

    (b, s(n, m), mi - 1, mj - 1)
  }

  def fittingAlignmentEmit(v: String, w: String, backtrack: MutableMatrix[Int], mi: Int, mj: Int): (String, String) = {
    def loop(i: Int, j: Int, acc1: String, acc2: String): (String, String) = {
      if (i == -1 && j == -1) (acc1, acc2)
      else if (i == -1) ('-' +: acc1, w.charAt(j) +: acc2)
      else if (j == -1) (acc1, acc2)
      else if (i == v.size - 1 && j == w.size - 1 && (mi != i || mj != j)) loop(mi, mj, acc1, acc2)
      else {
        if (backtrack(i, j) == 0) loop(i - 1, j, v.charAt(i) +: acc1, '-' +: acc2)
        else if (backtrack(i, j) == 1) loop(i, j - 1, '-' +: acc1, w.charAt(j) +: acc2)
        else if (backtrack(i, j) == 2) loop(i - 1, j - 1, v.charAt(i) +: acc1, w.charAt(j) +: acc2)
        else loop(i - 1, j, acc1, w.charAt(j) +: acc2)
      }
    }

    loop(v.size - 1, w.size - 1, "", "")
  }

  def fittingAffineAlignment(v: String, w: String, scores: Map[(Char, Char), Int], indelpenalty: Int, gapextension: Int): (Int, String, String) = {

    val (blower, bmiddle, bupper, maxScore, mi, mj) = fittingAffineAlignmentBacktrack(v, w, scores, indelpenalty, gapextension)
    val (s1, s2) = fittingAffineAlignmentEmit(v, w, blower, bmiddle, bupper, mi, mj)
    (maxScore, s1, s2)
  }

  def fittingAlignment(v: String, w: String, scores: Map[(Char, Char), Int], indelpenalty: Int): (Int, String, String) = {

    val (backtrack, maxScore, mi, mj) = fittingAlignmentBacktrack(v, w, scores, indelpenalty)
    val (s1, s2) = fittingAlignmentEmit(v, w, backtrack, mi, mj)
    (maxScore, s1, s2)
  }

  def fittingAlignment(v: String, w: String, scores: Map[(Char, Char), Int], indelpenalty: Int, gapextension: Int): (Int, String, String) =
    if (indelpenalty == gapextension) fittingAlignment(v, w, scores, indelpenalty)
    else fittingAffineAlignment(v, w, scores, indelpenalty, gapextension)

}

object LocalPairwiseAlignment {

  def localAlignmentBacktrack(v: String, w: String, scores: Map[(Char, Char), Int], indelpenalty: Int): (MutableMatrix[Int], Int, Int, Int) = {
    val n = v.size
    val m = w.size
    val s = MutableMatrix(n + 1, m + 1)
    val b = MutableMatrix(n, m)
    for (i <- 0 to n) {
      s(i, 0) = -1 * i * indelpenalty
    }
    for (j <- 0 to m) {
      s(0, j) = -1 * j * indelpenalty
    }
    for (i <- 1 to n; j <- 1 to m) {
      val move = List(
        0 -> (s(i - 1, j) - indelpenalty),
        1 -> (s(i, j - 1) - indelpenalty),
        2 -> (s(i - 1, j - 1) + scores((v.charAt(i - 1), w.charAt(j - 1)))),
        3 -> 0
      ).maxBy(_._2)
      b(i - 1, j - 1) = move._1
      s(i, j) = move._2

    }
    s(n, m) = s.data.max
    val (mi, mj) = ((0 until s.rows).iterator.flatMap { i =>
      (0 until s.cols).iterator.map { j =>
        (i, j)
      }
    }).find(x => s(x._1, x._2) == s(n, m)).get
    (b, s(n, m), mi - 1, mj - 1)
  }

  def localAlignmentEmit(v: String, w: String, backtrack: MutableMatrix[Int], mi: Int, mj: Int): (String, String) = {
    def loop(i: Int, j: Int, acc1: String, acc2: String): (String, String) = {
      if (i == -1 && j == -1) (acc1, acc2)
      else if (i == -1) ('-' +: acc1, w.charAt(j) +: acc2)
      else if (j == -1) (v.charAt(i) +: acc1, '-' +: acc2)
      else if (i == v.size - 1 && j == w.size - 1 && (mi != i || mj != j)) loop(mi, mj, acc1, acc2)
      else {
        if (backtrack(i, j) == 0) loop(i - 1, j, v.charAt(i) +: acc1, '-' +: acc2)
        else if (backtrack(i, j) == 1) loop(i, j - 1, '-' +: acc1, w.charAt(j) +: acc2)
        else if (backtrack(i, j) == 2) loop(i - 1, j - 1, v.charAt(i) +: acc1, w.charAt(j) +: acc2)
        else (acc1, acc2)
      }
    }
    loop(v.size - 1, w.size - 1, "", "")
  }

  def localAlignment(v: String, w: String, score: Map[(Char, Char), Int], indelpenalty: Int): (Int, String, String) = {
    val (backtrack, maxScore, mi, mj) = localAlignmentBacktrack(v, w, score, indelpenalty)
    val (s1, s2) = localAlignmentEmit(v, w, backtrack, mi, mj)
    (maxScore, s1, s2)
  }

}

object PenaltyHelper {
  def apply(s1: String, s2: String, scores: Map[(Char, Char), Int]): (Array[Int], Array[Int], MutableMatrix[Int], Map[Char, Int]) = {
    val map: Map[Char, Int] = scores.keys.flatMap(x => x._1 :: x._2 :: Nil).toSeq.distinct.sorted.zipWithIndex.toMap
    val mm = MutableMatrix(map.keys.size, map.keys.size)
    scores.foreach { case ((i, j), k) => mm.update(map(i), map(j), k) }
    (s1.map(map).toArray, s2.map(map).toArray, mm, map)
  }
  def reverse(s1: Seq[Int], s2: Seq[Int], map: Map[Char, Int]) = {
    val rmap = map.map(_.swap)
    (s1 map rmap mkString, s2 map rmap mkString)
  }
}

object GlobalPairwiseProfileAlignment {

  type OrderedFastaSequenceData = Seq[(String, String)]

  case class Profile(frequencies: Array[Int]) {
    val sum = {
      var s = 0.0
      var i = 0
      while (i < frequencies.size) {
        s += frequencies(i)
        i += 1
      }
      s
    }
    def toRelative = {
      val sum = frequencies.sum.toDouble
      frequencies.map(_ / sum)
    }
    def entropy: Double = {
      toRelative.map(x => if (x == 0.0) 0.0 else x * math.log(x)).sum * -1

    }
  }

  def makeProfile(al: FastaSequenceData, char2int: Seq[Char]): Array[Profile] = {
    val coverage: Seq[Map[Char, Int]] = calculateCoverage(al)
    def map2profile(m: Map[Char, Int]): Profile = Profile(char2int.map(x => m.get(x).getOrElse(0)).toArray)
    coverage.map(map2profile).toArray
  }

  val pdotp: (Profile, Profile) => Double = (p1: Profile, p2: Profile) => {
    val sum1: Double = p1.sum

    val sum2: Double = p2.sum

    val sum1sum2 = sum1 * sum2

    var s = 0.0
    var i = 0
    while (i < p1.frequencies.size) {
      val f1 = p1.frequencies(i)
      val f2 = p2.frequencies(i)
      if (f1 != 0.0 && f2 != 0.0) {
        s += (f1 * f2) / (sum1sum2)
      }
      i += 1
    }
    -1 * (1.0 - s)
  }

  def clearGapsOnlyColumns(al: OrderedFastaSequenceData) = {
    val idx = al.map(_._1)
    idx zip al.map(_._2.toSeq).transpose.filter(_.exists(_ != '-')).transpose.map(_.mkString)
  }

  def makeProfileAlignmentUPGMAEditDistance(al: OrderedFastaSequenceData, scores: (Profile, Profile) => Double, indelpenalty: Double, gapextension: Double, refinementsteps: Int): OrderedFastaSequenceData = {
    val stage2 = makeProfileAlignmentUPGMA(al, GlobalPairwiseAlignment.editDistance, scores, indelpenalty, gapextension)
    clearGapsOnlyColumns(refinement(refinementsteps, stage2, calculateEditDistanceOfAlignedSequences, scores, indelpenalty, gapextension))
  }

  def makeProfileAlignmentMultiStage(al: OrderedFastaSequenceData, scores: (Profile, Profile) => Double, indelpenalty: Double, gapextension: Double, refinementsteps: Int): OrderedFastaSequenceData = {
    val stage1 = makeProfileAlignmentUPGMA(al, kmerdistance(_, _, 6), scores, indelpenalty, gapextension)
    val stage2 = makeProfileAlignmentUPGMA(stage1, calculateEditDistanceOfAlignedSequences, scores, indelpenalty, gapextension)
    clearGapsOnlyColumns(refinement(refinementsteps, stage2, calculateEditDistanceOfAlignedSequences, scores, indelpenalty, gapextension))
  }

  def refinement(count: Int, al: OrderedFastaSequenceData, distance: (String, String) => Double, scores: (Profile, Profile) => Double, indelpenalty: Double, gapextension: Double): OrderedFastaSequenceData =
    if (count == 0) al
    else {
      val refined = refinement(al, distance, scores, indelpenalty, gapextension)

      if (totalMismatch(al) <= totalMismatch(refined)) al
      else refinement(count - 1, refined, distance, scores, indelpenalty, gapextension)

    }

  private def refinement(al: OrderedFastaSequenceData, distance: (String, String) => Double, scores: (Profile, Profile) => Double, indelpenalty: Double, gapextension: Double): OrderedFastaSequenceData = {

    import com.apporiented.algorithm.clustering._
    import collection.JavaConversions._

    val root = upgma(al, distance)

    val keyset = al.map(_._1).toSet

    def getLeavesAbove(cl: Cluster[String]): Set[String] = {
      val below = getLeavesBelow(cl)
      keyset &~ below
    }

    def getLeavesBelow(cl: Cluster[String]): Set[String] = clustering.traverse(cl).toSet

    def refine1pass(cl: List[Cluster[String]], alignment: OrderedFastaSequenceData, scoreOld: Int): OrderedFastaSequenceData = cl match {
      case Nil => alignment
      case x :: xs => {

        val left = {
          val s = getLeavesAbove(x)
          alignment.filter(x => s.contains(x._1))
        }
        val right = {
          val s = getLeavesBelow(x)
          alignment.filter(x => s.contains(x._1))
        }
        if (left.size > 0 && right.size > 0) {

          val left1 = if (left.size == 1) left.map(x => x._1 -> x._2.filterNot(_ == '-')) else left
          val right1 = if (right.size == 1) right.map(x => x._1 -> x._2.filterNot(_ == '-')) else right

          val (_, f1, f2) = globalAffineProfileAlignment(left1, right1, scores, indelpenalty, gapextension)
          val al2 = f1 ++ f2

          val totalscore = totalMismatch(al2)

          if (totalscore < scoreOld) refine1pass(xs, al2, totalscore)
          else refine1pass(xs, alignment, scoreOld)

        } else refine1pass(xs, alignment, scoreOld)

      }
    }

    refine1pass(clustering.breadth_traverse(root).drop(1).reverse, al, totalMismatch(al))

  }

  def upgma(al: OrderedFastaSequenceData, distance: (String, String) => Double) = {
    import com.apporiented.algorithm.clustering._
    import collection.JavaConversions._

    val map = al.toMap

    val names = al.map(_._1).toArray

    val distances = al.map {
      case (_, seq1) =>
        al.map {
          case (_, seq2) =>
            distance(seq1, seq2)
        }.toArray
    }.toArray

    new DefaultClusteringAlgorithm().performClustering(
      distances,
      names,
      new AverageLinkageStrategy()
    )
  }

  def makeProfileAlignmentUPGMA(al: OrderedFastaSequenceData, distance: (String, String) => Double, scores: (Profile, Profile) => Double, indelpenalty: Double, gapextension: Double): OrderedFastaSequenceData = {
    import com.apporiented.algorithm.clustering._
    import collection.JavaConversions._

    val map = al.toMap

    val root = upgma(al, distance)

    def traverse(cl: Cluster[String]): OrderedFastaSequenceData =
      if (cl.isLeaf) Seq(cl.getName -> map(cl.getName))
      else {
        assert(cl.getChildren.size <= 2)

        val fs = cl.getChildren.map(ch => traverse(ch))
        val aligned = if (fs.size == 1) fs.head else {
          val (_, f1, f2) = globalAffineProfileAlignment(fs(0), fs(1), scores, indelpenalty, gapextension)
          f1 ++ f2
        }
        aligned
      }

    traverse(root)

  }

  def makeProfileAlignment(al: FastaSequenceData, scores: (Profile, Profile) => Double, indelpenalty: Double, gapextension: Double) = {

    def loop(list: List[OrderedFastaSequenceData]): OrderedFastaSequenceData = list match {
      case Nil => Seq()
      case x :: Nil => x
      case x :: xs => {
        val alignmentsSortedByScore = xs.map(y => y -> globalAffineProfileAlignment(x, y, scores, indelpenalty, gapextension)).sortBy(_._2._1 * (-1))
        val (score, f1, f2) = alignmentsSortedByScore.head._2
        val rest = alignmentsSortedByScore.drop(1).map(_._1)
        loop((f1 ++ f2) :: rest)

      }

    }

    loop(al.toList.map(x => Seq(x._1 -> x._2)))

  }

  def globalAffineProfileAlignment(v: OrderedFastaSequenceData, w: OrderedFastaSequenceData, score: (Profile, Profile) => Double, gapopen: Double, gapextension: Double): (Double, OrderedFastaSequenceData, OrderedFastaSequenceData) = {
    val char2int = (v.map(_._2).map(_.toSet).flatten ++ w.map(_._2).map(_.toSet).flatten).toSeq.distinct.sorted
    val p1 = makeProfile(v.toMap, char2int)
    val p2 = makeProfile(w.toMap, char2int)

    val ((lower, middle, upper), maxScore) = globalAffineProfileAlignmentBacktrack(p1, p2, score, gapopen, gapextension)
    val (s1, s2) = globalAffineProfileAlignmentEmit(p1, p2, lower, middle, upper)

    val al1 = v.map(x => x._1 -> ConsensusProfileProfile.introduceGaps(x._2, s1))
    val al2 = w.map(x => x._1 -> ConsensusProfileProfile.introduceGaps(x._2, s2))
    (maxScore, al1, al2)
  }

  def globalAffineProfileAlignmentEmit(v: Array[Profile], w: Array[Profile], backtracklower: MutableMatrix[Int], backtrackmiddle: MutableMatrix[Int], backtrackupper: MutableMatrix[Int]): (String, String) = {
    def loop(i: Int, j: Int, k: Int, acc1: List[Char], acc2: List[Char]): (String, String) = {
      val mat = if (k == 0) backtracklower else if (k == 2) backtrackmiddle else backtrackupper
      if (i == -1 && j == -1) (acc1.mkString, acc2.mkString)
      else if (i == -1) loop(i, j - 1, k, '-' :: acc1, 'X' :: acc2)
      else if (j == -1) loop(i - 1, j, k, 'X' :: acc1, '-' :: acc2)
      else {
        if (mat(i, j) == 0) loop(i - 1, j, 0, 'X' :: acc1, '-' :: acc2)
        else if (mat(i, j) == 3) loop(i - 1, j, 2, 'X' :: acc1, '-' :: acc2)
        else if (mat(i, j) == 1) loop(i, j - 1, 1, '-' :: acc1, 'X' :: acc2)
        else if (mat(i, j) == 4) loop(i, j - 1, 2, '-' :: acc1, 'X' :: acc2)
        else if (mat(i, j) == 2) loop(i - 1, j - 1, 2, 'X' :: acc1, 'X' ::
          acc2)
        else if (mat(i, j) == 5) loop(i, j, 0, acc1, acc2)
        else loop(i, j, 1, acc1, acc2)
      }
    }
    loop(v.size - 1, w.size - 1, 2, Nil, Nil)
  }

  def globalAffineProfileAlignmentBacktrack(v: Array[Profile], w: Array[Profile], scorefunction: (Profile, Profile) => Double, gapopen: Double, gapextension: Double) = {

    val n = v.size
    val m = w.size
    val lower = MutableMatrix.makeDouble(n + 1, m + 1)

    val blower = MutableMatrix(n, m)
    val bmiddle = blower.copy
    val bupper = blower.copy
    for (i <- 1 to n) {
      lower(i, 0) = -1 * math.max(0, i - 1) * gapextension - gapopen
    }
    for (j <- 1 to m) {
      lower(0, j) = -1 * math.max(0, j - 1) * gapextension - gapopen
    }

    val middle = lower.copy
    val upper = lower.copy

    var i = 1
    while (i <= n) {
      var j = 1
      while (j <= m) {

        {
          val cost0 = (lower(i - 1, j) - gapextension)
          val cost3 = (middle(i - 1, j) - gapopen)
          if (cost0 >= cost3) {
            blower(i - 1, j - 1) = 0
            lower(i, j) = cost0
          } else {
            blower(i - 1, j - 1) = 3
            lower(i, j) = cost3
          }
        }

        {
          val cost1 = (upper(i, j - 1) - gapextension)
          val cost4 = (middle(i, j - 1) - gapopen)
          if (cost1 >= cost4) {
            bupper(i - 1, j - 1) = 1
            upper(i, j) = cost1
          } else {
            bupper(i - 1, j - 1) = 4
            upper(i, j) = cost4
          }
        }

        {
          val cost2 = (middle(i - 1, j - 1) + scorefunction(v(i - 1), w(j - 1)))
          val cost5 = (lower(i, j))
          val cost6 = (upper(i, j))
          if (cost2 >= cost5 && cost2 >= cost6) {
            bmiddle(i - 1, j - 1) = 2
            middle(i, j) = cost2
          } else if (cost5 >= cost2 && cost5 >= cost6) {
            bmiddle(i - 1, j - 1) = 5
            middle(i, j) = cost5
          } else {
            bmiddle(i - 1, j - 1) = 6
            middle(i, j) = cost6
          }
        }

        j += 1
      }
      i += 1
    }

    (blower, bmiddle, bupper) -> middle(n, m)
  }

}

object GlobalPairwiseAlignment {

  def globalAlignmentBacktrack(v: String, w: String, scores: Map[(Char, Char), Int], indelpenalty: Int): (MutableMatrix[Int], Int) = {
    val (vInt, wInt, scoresInt, char2int) = PenaltyHelper(v, w, scores)

    val n = v.size
    val m = w.size
    val s = MutableMatrix(n + 1, m + 1)
    val b = MutableMatrix(n, m)
    for (i <- 0 to n) {
      s(i, 0) = -1 * i * indelpenalty
    }
    for (j <- 0 to m) {
      s(0, j) = -1 * j * indelpenalty
    }
    var i = 1
    while (i <= n) {
      var j = 1
      while (j <= m) {
        {
          {
            s(i, j) = math.max(
              math.max(s(i - 1, j) - indelpenalty, s(i, j - 1) - indelpenalty),
              s(i - 1, j - 1) + scoresInt(vInt(i - 1), wInt(j - 1))
            )
            if (s(i, j) == s(i - 1, j) - indelpenalty) {
              b(i - 1, j - 1) = 0
            } else if (s(i, j) == s(i, j - 1) - indelpenalty) {
              b(i - 1, j - 1) = 1
            } else if (s(i, j) == s(i - 1, j - 1) + scoresInt(vInt(i - 1), wInt(j - 1))) {
              b(i - 1, j - 1) = 2
            }
          }

        }
        j += 1
      }
      i += 1
    }
    b -> s(n, m)
  }

  def globalAffineAlignmentBacktrack(v: String, w: String, scores: Map[(Char, Char), Int], gapopen: Int, gapextension: Int) = {

    val (vInt, wInt, scoresInt, char2int) = PenaltyHelper(v, w, scores)

    val n = v.size
    val m = w.size
    val lower = MutableMatrix(n + 1, m + 1)

    val blower = MutableMatrix(n, m)
    val bmiddle = blower.copy
    val bupper = blower.copy
    for (i <- 1 to n) {
      lower(i, 0) = -1 * math.max(0, i - 1) * gapextension - gapopen
    }
    for (j <- 1 to m) {
      lower(0, j) = -1 * math.max(0, j - 1) * gapextension - gapopen
    }

    // for (i <- 1 to n) {
    //   middle(i, 0) = -1 * math.max(0, i - 1) * gapextension - gapopen
    // }
    // for (j <- 1 to m) {
    //   middle(0, j) = -1 * math.max(0, j - 1) * gapextension - gapopen
    // }
    // for (j <- 1 to m) {
    //   upper(0, j) = -1 * math.max(0, j - 1) * gapextension - gapopen
    // }
    // for (i <- 1 to n) {
    //   upper(i, 0) = -1 * math.max(0, i - 1) * gapextension - gapopen
    // }

    val middle = lower.copy
    val upper = lower.copy

    var i = 1
    while (i <= n) {
      var j = 1
      while (j <= m) {
        {

          // val move = List(
          //   0 -> (lower(i - 1, j) - gapextension),
          //   3 -> (middle(i - 1, j) - gapopen)
          // ).maxBy(_._2)
          // blower(i - 1, j - 1) = move
          // lower(i, j) = movecost

          {
            val cost0 = (lower(i - 1, j) - gapextension)
            val cost3 = (middle(i - 1, j) - gapopen)
            if (cost0 >= cost3) {
              blower(i - 1, j - 1) = 0
              lower(i, j) = cost0
            } else {
              blower(i - 1, j - 1) = 3
              lower(i, j) = cost3
            }
          }

          // val move = List(
          //   1 -> (upper(i, j - 1) - gapextension),
          //   4 -> (middle(i, j - 1) - gapopen)

          // ).maxBy(_._2)
          // bupper(i - 1, j - 1) = move
          // upper(i, j) = movecost

          {
            val cost1 = (upper(i, j - 1) - gapextension)
            val cost4 = (middle(i, j - 1) - gapopen)
            if (cost1 >= cost4) {
              bupper(i - 1, j - 1) = 1
              upper(i, j) = cost1
            } else {
              bupper(i - 1, j - 1) = 4
              upper(i, j) = cost4
            }
          }

          // val move = List(
          //   2 -> (middle(i - 1, j - 1) + scoresInt(vInt(i - 1), wInt(j - 1))),
          //   5 -> (lower(i, j)),
          //   6 -> (upper(i, j))
          // ).maxBy(_._2)
          // bmiddle(i - 1, j - 1) = move
          // middle(i, j) = movecost

          {
            val cost2 = (middle(i - 1, j - 1) + scoresInt(vInt(i - 1), wInt(j - 1)))
            val cost5 = (lower(i, j))
            val cost6 = (upper(i, j))
            if (cost2 >= cost5 && cost2 >= cost6) {
              bmiddle(i - 1, j - 1) = 2
              middle(i, j) = cost2
            } else if (cost5 >= cost2 && cost5 >= cost6) {
              bmiddle(i - 1, j - 1) = 5
              middle(i, j) = cost5
            } else {
              bmiddle(i - 1, j - 1) = 6
              middle(i, j) = cost6
            }
          }

        }
        j += 1
      }
      i += 1
    }

    (blower, bmiddle, bupper) -> middle(n, m)
  }

  def globalAffineAlignmentEmit(v: String, w: String, backtracklower: MutableMatrix[Int], backtrackmiddle: MutableMatrix[Int], backtrackupper: MutableMatrix[Int]): (String, String) = {
    def loop(i: Int, j: Int, k: Int, acc1: List[Char], acc2: List[Char]): (String, String) = {
      val mat = if (k == 0) backtracklower else if (k == 2) backtrackmiddle else backtrackupper
      if (i == -1 && j == -1) (acc1.mkString, acc2.mkString)
      else if (i == -1) loop(i, j - 1, k, '-' :: acc1, w.charAt(j) :: acc2)
      else if (j == -1) loop(i - 1, j, k, v.charAt(i) :: acc1, '-' :: acc2)
      else {
        if (mat(i, j) == 0) loop(i - 1, j, 0, v.charAt(i) :: acc1, '-' :: acc2)
        else if (mat(i, j) == 3) loop(i - 1, j, 2, v.charAt(i) :: acc1, '-' :: acc2)
        else if (mat(i, j) == 1) loop(i, j - 1, 1, '-' :: acc1, w.charAt(j) :: acc2)
        else if (mat(i, j) == 4) loop(i, j - 1, 2, '-' :: acc1, w.charAt(j) :: acc2)
        else if (mat(i, j) == 2) loop(i - 1, j - 1, 2, v.charAt(i) :: acc1, w.charAt(j) ::
          acc2)
        else if (mat(i, j) == 5) loop(i, j, 0, acc1, acc2)
        else loop(i, j, 1, acc1, acc2)
      }
    }
    loop(v.size - 1, w.size - 1, 2, Nil, Nil)
  }

  def globalAlignmentEmit(v: String, w: String, backtrack: MutableMatrix[Int]): (String, String) = {
    def loop(i: Int, j: Int, acc1: List[Char], acc2: List[Char]): (String, String) = {
      if (i == -1 && j == -1) (acc1.mkString, acc2.mkString)
      else if (i == -1) loop(i, j - 1, '-' :: acc1, w.charAt(j) :: acc2)
      else if (j == -1) loop(i - 1, j, v.charAt(i) :: acc1, '-' :: acc2)
      else {
        if (backtrack(i, j) == 0) loop(i - 1, j, v.charAt(i) :: acc1, '-' :: acc2)
        else if (backtrack(i, j) == 1) loop(i, j - 1, '-' :: acc1, w.charAt(j) :: acc2)
        else loop(i - 1, j - 1, v.charAt(i) :: acc1, w.charAt(j) :: acc2)
      }
    }
    loop(v.size - 1, w.size - 1, Nil, Nil)
  }

  def globalAffineAlignment(v: String, w: String, score: Map[(Char, Char), Int], gapopen: Int, gapextension: Int): (Int, String, String) = {
    val ((lower, middle, upper), maxScore) = globalAffineAlignmentBacktrack(v, w, score, gapopen, gapextension)
    val (s1, s2) = globalAffineAlignmentEmit(v, w, lower, middle, upper)
    (maxScore, s1, s2)
  }

  def globalAlignment(v: String, w: String, score: Map[(Char, Char), Int], indelpenalty: Int, gapextension: Int): (Int, String, String) = {
    if (indelpenalty == gapextension) {
      val (backtrack, maxScore) = globalAlignmentBacktrack(v, w, score, indelpenalty)
      val (s1, s2) = globalAlignmentEmit(v, w, backtrack)
      (maxScore, s1, s2)
    } else globalAffineAlignment(v, w, score, indelpenalty, gapextension)
  }

  def editDistance(v: String, w: String): Int = {
    val score = {
      val l = (v.toSet ++ w.toSet).toList
      l.flatMap { c1 =>
        l.map { c2 =>
          (c1, c2) -> (if (c1 == c2) 0
          else -1)
        }
      }
    }.toMap
    val (x, maxScore) = globalAlignmentBacktrack(v, w, score, 1)

    maxScore * -1
  }

}

object LongestCommonSubstring {

  def longestCommonSubstringBacktrack(v: String, w: String): MutableMatrix[Int] = {
    val n = v.size
    val m = w.size
    val s = MutableMatrix(n + 1, m + 1)
    val b = MutableMatrix(n, m)
    for (i <- 0 to n) {
      s(i, 0) = 0
    }
    for (j <- 0 to m) {
      s(0, j) = 0
    }
    for (i <- 1 to n; j <- 1 to m) {
      s(i, j) = math.max(
        math.max(s(i - 1, j), s(i, j - 1)),
        s(i - 1, j - 1) + (if (v.charAt(i - 1) == w.charAt(j - 1)) 1 else 0)
      )
      if (s(i, j) == s(i - 1, j)) {
        b(i - 1, j - 1) = 0
      } else if (s(i, j) == s(i, j - 1)) {
        b(i - 1, j - 1) = 1
      } else if (s(i, j) == s(i - 1, j - 1) + 1) {
        b(i - 1, j - 1) = 2
      }
    }
    b
  }

  def longestCommonSubstringEmit(v: String, w: String, backtrack: MutableMatrix[Int]): String = {
    def loop(i: Int, j: Int, acc: String): String = {
      if (i == -1 || j == -1) acc
      else {
        if (backtrack(i, j) == 0) loop(i - 1, j, acc)
        else if (backtrack(i, j) == 1) loop(i, j - 1, acc)
        else loop(i - 1, j - 1, v.charAt(i) +: acc)
      }
    }
    loop(v.size - 1, w.size - 1, "")
  }

  def longestCommonSubstring(v: String, w: String) = {
    val backtrack = longestCommonSubstringBacktrack(v, w)
    longestCommonSubstringEmit(v, w, backtrack)
  }

}

object ConsensusProfileProfile {

  def consensusProfileProfile(al1: FastaSequenceData, al2: FastaSequenceData, scores: Map[(Char, Char), Int], indelpenalty: Int, gapextension: Int) = {
    val cons1 = makeConsensus(al1, 0.0, 'N', List('-', '*', 'N', 'X'))
    val cons2 = makeConsensus(al2, 0.0, 'N', List('-', '*', 'N', 'X'))

    val (score, cons1aligned, cons2aligned) = GlobalPairwiseAlignment.globalAlignment(cons1, cons2, scores, indelpenalty, gapextension)

    al1.map(x => x._1 -> introduceGaps(x._2, cons1aligned)) ++
      al2.map(x => x._1 -> introduceGaps(x._2, cons2aligned))
  }

  def introduceGaps(v: String, gapped: String): String = {

    def loop(s: List[Char], g: List[Char], acc: List[Char]): List[Char] =
      if (s.isEmpty) g.reverse ::: acc
      else if (g.isEmpty) s.reverse ::: acc
      else if (g.head == '-') loop(s, g.drop(1), '-' :: acc)
      else loop(s.drop(1), g.drop(1), s.head :: acc)

    loop(v.toList, gapped.toList, Nil).reverse.mkString

  }

  def makeProfileAlignment(al: FastaSequenceData, scores: Map[(Char, Char), Int], indelpenalty: Int, gapextension: Int) = {

    def loop(list: List[FastaSequenceData]): FastaSequenceData = list match {
      case x :: Nil => x
      case l => loop(list.grouped(2).toList.map(x => if (x.size == 1) x(0) else consensusProfileProfile(x(0), x(1), scores, indelpenalty, gapextension)))
    }

    loop(al.toList.map(x => Map(x._1 -> x._2)))

  }

}