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

import mybiotools._
import mybiotools.workflows._
import mybiotools.tasks._
import com.typesafe.config.{ Config, ConfigFactory }
import mybiotools.config.Config.configInstance
import genotyper._
import scala.collection.JavaConversions._
import rnaseqalign.htseqcount._
import scala.collection.immutable.Queue
import java.io.File
import mybiotools.eq._
import mybiotools.sequence.alignment._
import org.saddle._

package object viralshortread {

  def callBinaryVariantsFromConsensus(data: Seq[Option[Char]]): Seq[(Char, Seq[Boolean])] = {
    val possibleStates = data.map(_.getOrElse('-')).distinct
    possibleStates.map(c => c -> data.map(_.getOrElse('-') === c))
  }

  def callBinaryVariantsFromFrequencyDistribution(data: Seq[Option[Map[Char, Int]]], minimumRate: Double, minimumDepth: Int): Seq[(Char, Seq[Option[Boolean]])] = {
    val possibleStates: Seq[Char] = (data.flatMap(_.toSeq.flatMap(_.keys.toSeq)) :+ '-').distinct
    possibleStates.map { c =>
      c -> data.map { (distribution: Option[Map[Char, Int]]) =>
        if (c == '-' && distribution.isEmpty) Some(true)
        else if (distribution.isDefined) {
          if (distribution.get.values.sum >= minimumDepth) distribution.get.get(c).map(x => Some(x.toDouble / distribution.get.values.sum >= minimumRate)).getOrElse(Some(false))
          else None
        } else None
      }
    }
  }

  def callBinaryVariantsFromFrequencyDistributionMinority(data: Seq[Option[Map[Char, Int]]], minimumRate: Double, minimumDepth: Int): Seq[(Char, Seq[Option[Boolean]])] = {
    val consensus = data.map(_.map(freq => freq.toSeq.maxBy(_._2)._1).getOrElse('-'))

    Seq('X' -> (data.zip(consensus).map {
      case (distribution: (Option[Map[Char, Int]]), cons: Char) =>
        if (distribution.isEmpty || distribution.get.values.sum < minimumDepth) None
        else Some(distribution.get.filter(_._1 != cons).values.sum.toDouble / distribution.get.values.sum.toDouble >= minimumRate)
    }))

  }

  def callDepthNormalizedCountsFromFrequencyDistribution(data: Seq[Option[Map[Char, Int]]]): Seq[(Char, Seq[Double])] = {
    val possibleStates: Seq[Char] = (data.flatMap(_.toSeq.flatMap(_.keys.toSeq)) :+ '-').distinct
    val depths: Seq[Int] = data.map(_.map(_.values.sum).getOrElse(0))
    val mediandepth = SummaryStat(depths).median
    val scalingFactors = depths.map(x => mediandepth.toDouble / x)

    possibleStates.map { c =>
      c -> data.zip(scalingFactors).map {
        case (distribution: (Option[Map[Char, Int]]), scaling: Double) =>
          if (c == '-' && distribution.isEmpty) mediandepth
          else if (distribution.isDefined) {
            distribution.get.get(c).map(_ * scaling).getOrElse(0.0)
          } else 0.0
      }
    }
  }

  def callDepthNormalizedMinorityCountsFromFrequencyDistribution(data: Seq[Option[Map[Char, Int]]]): Seq[(Char, Seq[Double])] = {
    val depths: Seq[Int] = data.map(_.map(_.values.sum).getOrElse(0))
    val mediandepth = SummaryStat(depths).median
    val scalingFactors = depths.map(x => mediandepth.toDouble / x)
    val consensus = data.map(_.map(freq => freq.toSeq.maxBy(_._2)._1).getOrElse('-'))

    Seq('X' -> (data.zip(scalingFactors)).zip(consensus).map {
      case ((distribution: (Option[Map[Char, Int]]), scaling: Double), cons: Char) =>
        if (distribution.isEmpty) 0.0
        else distribution.get.filter(_._1 != cons).values.sum * scaling
    })

  }

  def callVariants[S, T, K](d: Seq[(S, Map[ReferencePosition, T])])(callFn: Seq[Option[T]] => Seq[(Char, Seq[K])])(implicit st: ST[S], o: Ordering[S], st1: ST[K]): Frame[S, BinaryVariantKey, K] = {

    def removeDuplicates(s: Seq[(Char, Seq[K])]): Seq[(Char, Seq[K])] = s.foldLeft(List[(Char, Seq[K])]()) {
      case (acc, elem) =>
        if (acc.isEmpty) elem :: acc
        else if (acc.map(_._2).contains(elem._2)) acc
        else elem :: acc
    }

    val keyset: Set[ReferencePosition] = d.flatMap(_._2.keys).toSet

    val sampleidx: Seq[S] = d.map(_._1)

    Frame(keyset.toSeq.flatMap { position =>

      val sampleData: Seq[(Option[T])] = d.map(_._2.get(position))

      val calls: Seq[(Char, Seq[K])] = removeDuplicates(callFn(sampleData))

      calls.map {
        case (state, persamplestate) =>
          BinaryVariantKey(position, state) -> Series[S, K](sampleidx zip persamplestate: _*)
      }

    }: _*)
  }

  implicit def BinaryVariantKeyOrdering = scala.math.Ordering.by[BinaryVariantKey, (Int, Int, Int)](_ match {
    case BinaryVariantKey(PositionOnReference(i), char) => (i, Int.MaxValue, char.toInt)
    case BinaryVariantKey(InsertionIntoReference(i, j), char) => (i, j, char.toInt)
  })

  implicit def referencePositionOrdering = scala.math.Ordering.by[ReferencePosition, (Int, Int)](_ match {
    case PositionOnReference(i) => (i, Int.MaxValue)
    case InsertionIntoReference(i, j) => (i, j)
  })

  /* Maps a reference based coordinates onto a modified gapped reference. Insertions in the reference alignments are filled up with insertions from the original coordinates.  */
  def mapcoordinates(
    referenceAlignment: Cigar,
    position: ReferencePosition
  ): ReferencePosition = {
    val referenceLength = referenceAlignment.cigarElements.filter(_.operator match {
      case M | X | EQ => true
      case _ => false
    }).map(_.length).sum
    val referencePosition = position.ref
    val (_, offset, insertJustBefore) = referenceAlignment.cigarElements.foldLeft((0, 0, 0)) {
      case ((ungappedlength, insertLengthCumulative, insertJustBefore), CigarElement(length, operator)) =>
        if (ungappedlength >= referencePosition + 1) (ungappedlength, insertLengthCumulative, insertJustBefore)
        else {
          operator match {
            case M | X | EQ => (ungappedlength + length, insertLengthCumulative, insertJustBefore)
            case D | N | P => (ungappedlength, insertLengthCumulative + length, if (ungappedlength == referencePosition) length else 0)
            case I | S | H => throw new RuntimeException(s"invalid cigar operator ISH")
          }
        }
    }

    position match {
      case PositionOnReference(i) if i >= referenceLength => throw new RuntimeException(s"invalid position. Position on reference ($i) >= ref length ($referenceLength) (ref cigar: $referenceAlignment)")
      case PositionOnReference(i) => PositionOnReference(i + offset)
      case InsertionIntoReference(i, j) if j < insertJustBefore => PositionOnReference(i + offset + j - insertJustBefore)
      case InsertionIntoReference(i, j) => InsertionIntoReference(i + offset, j - insertJustBefore)
    }

  }

  def openAlignedStringIterator(f: File) = rnaseqalign.Helpers.openIteratorOnBam(f, true).filter(x => x._1.mapQ > 30 && x._1.unmappedFlag === false).map(x => AlignedStringWithStart(x._1, x._2, '-'))

  def sam2consensus(f: File, minimumDepth: Int): String = {
    val iter = openAlignedStringIterator(f)
    sam2sequence(iter)(consensusCall(_, minimumDepth)).filter(_._2 != '-').mkString
  }

  def sam2frequencies(f: File): List[(ReferencePosition, Map[Char, Int])] = {
    val iter = openAlignedStringIterator(f)
    sam2sequence(iter)(frequencyDistribution)
  }

  def parseCigar(cigar: Cigar, read: String, gap: Char): (String, List[(Int, String)]) = {
    val (insertions, edited, _) = cigar.cigarElements.foldLeft((List[(Int, String)](), read, 0)) {
      case ((acc, editedRead, pos), CigarElement(length, operator)) =>

        operator match {
          case H => (acc, editedRead, pos)
          case M | X | EQ => (acc, editedRead, pos + length)
          case D | N | P => (acc, (editedRead.take(pos) + List.fill(length)(gap).mkString + editedRead.drop(pos)), (pos + length))
          case I => ((pos, editedRead.drop(pos).take(length)) :: acc, (editedRead.take(pos) + editedRead.drop(pos + length)), pos)
          case S => (acc, (editedRead.take(pos) + editedRead.drop(pos + length)), pos)

        }
    }
    (edited, insertions)
  }

  def compositeCallFunction[T1, T2](fn1: Seq[Char] => T1, fn2: Seq[Char] => T2) = (chars: Seq[Char]) => (fn1(chars), fn2(chars))
  def compositeCallFunction[T1, T2, T3](fn1: Seq[Char] => T1, fn2: Seq[Char] => T2, fn3: Seq[Char] => T3) = (chars: Seq[Char]) => (fn1(chars), fn2(chars), fn3(chars))

  val consensusCall = (chars: Seq[Char], minimumDepth: Int) => {
    if (chars.size < minimumDepth) 'N' else chars.groupBy(x => x).map(x => x._1 -> x._2.size).maxBy(_._2)._1
  }

  val frequencyDistribution = (chars: Seq[Char]) => {
    if (chars.isEmpty) Map[Char, Int]() else chars.groupBy(x => x).map(x => x._1 -> x._2.size)
  }

  def entropy = (chars: Seq[Char], minimumDepth: Int) => {
    val frequency: Map[Char, Int] = frequencyDistribution(chars)
    val sum = frequency.values.sum.toDouble
    val relative = frequency.map(x => x._2 / sum)
    if (sum < minimumDepth) None
    else Some(relative.map(x => if (x === 0.0) 0.0 else x * math.log(x)).sum * -1)
  }

  def sam2sequence[@specialized(Char) T](it: Iterator[AlignedStringWithStart])(callFunction: Seq[Char] => T): List[(ReferencePosition, T)] = {

    def makeCall(pile: Vector[AlignedStringWithStart], position: Int) = {

      // println(position + " " + pile.size + " " + pile.last.sequence)

      val call = {
        val chars = {
          // pile.filter(x => x.start <= position && x.start + x.sequence.size > position).map {
          // case (AlignedStringWithStart(start, sequence)) =>
          // sequence.charAt(position - start)
          var i = 0
          var l = List[Char]()
          while (i < pile.size) {
            val x = pile(i)
            if (x.start <= position && x.start + x.sequence.size > position) {
              l = x.sequence.charAt(position - x.start) :: l
            }
            i += 1
          }
          l
        }
        PositionOnReference(position) -> callFunction(chars)
      }

      val insertionCalls: List[(ReferencePosition, T)] = {
        val insertions: Seq[String] = pile.map(read => read.insertions.get(position - read.start).getOrElse("-"))
        if (insertions.isEmpty || insertions.distinct === Seq("-")) Nil
        else {
          val max = insertions.maxBy(_.size).size
          (0 until max).toList map { i =>
            val callset = insertions.map(s => if (s.size > i) s.charAt(i) else '-')
            InsertionIntoReference(position, i) -> callFunction(callset)
          }
        }
      }

      val nextpile = {
        // pile.filter(x => x.start + x.sequence.length - 1 <= position)
        var i = 0
        var l = List[AlignedStringWithStart]()
        while (i < pile.size) {
          val x = pile(i)
          if (x.start + x.sequence.length - 1 > position) {
            l = x :: l
          }
          i += 1
        }
        l.toVector.reverse
      }
      val nextposition = position + 1

      (nextposition, nextpile, call :: insertionCalls.reverse)
    }

    def fold(pile: Vector[AlignedStringWithStart], position: Int, acc: List[(ReferencePosition, T)]): List[(ReferencePosition, T)] =
      if (it.isEmpty) {
        if (pile.isEmpty) acc
        else {
          val (nextposition, nextpile, call) = makeCall(pile, position)
          if (nextpile.isEmpty) call ::: acc
          else fold(nextpile, nextposition, call ::: acc)
        }
      } else {
        if (pile.isEmpty) {
          val nextread = it.next
          fold(pile :+ nextread, nextread.start, acc)
        } else if (pile.size === 1) {
          fold(pile :+ it.next, position, acc)
        } else {

          val startOfPenultimateRead = pile.dropRight(1).last.start

          if (position < startOfPenultimateRead) {
            val (nextposition, nextpile, call) = makeCall(pile, position)
            fold(nextpile, nextposition, call ::: acc)
          } else {
            fold(pile :+ it.next, position, acc)
          }

        }

      }

    fold(Vector(), 0, Nil).reverse

  }

}