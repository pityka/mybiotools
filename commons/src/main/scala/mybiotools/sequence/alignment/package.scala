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

package mybiotools.sequence
import mybiotools._
import mybiotools.stringstore._

package object alignment {

  def lengthOfCigar(c: Cigar) = c.cigarElements.map(_.length).sum

  def cigarFromAlignedSequence(s: String, gap: Char) = {

    def loop(i: Int, acc: List[CigarElement]): List[CigarElement] =
      if (i == s.size) acc
      else {
        val ch = s.charAt(i)
        if (ch == gap) {
          val nextidx = {
            val t = s.indexWhere(_ != gap, i)
            if (t < 0) s.size else t
          }
          loop(nextidx, CigarElement(nextidx - i, D) :: acc)
        } else {
          val nextidx = {
            val t = s.indexOf(gap, i)
            if (t < 0) s.size else t
          }
          loop(nextidx, CigarElement(nextidx - i, M) :: acc)
        }
      }

    Cigar(loop(0, Nil).reverse)

  }

  def kmercounts(text: String8, kmerSize: Int): scala.collection.Map[KMer, Int] = {

    def kmer(text: String8, start: Int, size: Int) = text.substring(start, start + size)

    val rollinghash = aminoacidRollingHash

    val mmap = scala.collection.mutable.AnyRefMap[KMer, Int]()

    var substring = kmer(text, 0, kmerSize)
    var hash = rollinghash.full(substring.content)

    def factory(s: SubString8, hash: Int) = new KMer {
      val value = s
      val h = hash
    }

    val s0 = factory(substring, hash)

    mmap.update(factory(substring, hash), 1)

    var i = 1
    while (i <= (text.size - kmerSize)) {

      val next = text(i + kmerSize - 1)

      hash = rollinghash.shift(substring(0), kmerSize, hash, next)

      substring = kmer(text, i, kmerSize)

      val s1 = factory(substring, hash)

      mmap.get(s1) match {
        case None => mmap.update(s1, 1)
        case Some(x) => mmap.update(s1, x + 1)
      }

      i += 1
    }

    mmap

  }

  def kmerdistance(s1: String, s2: String, kmerSize: Int): Double = {
    val count1 = kmercounts(StringStore(s1), kmerSize)
    val count2 = kmercounts(StringStore(s2), kmerSize)
    val joint = count1.keySet & count2.keySet
    val sumOfSharedOccurences = joint.map(x => math.min(count1(x), count2(x))).sum

    sumOfSharedOccurences / (math.min(s1.size, s2.size) + 1 - kmerSize).toDouble

  }

  def totalMismatch(al: Seq[(String, String)]) = {
    al.flatMap {
      case (_, s1) =>
        al.map {
          case (_, s2) =>
            calculateEditDistanceOfAlignedSequences(s1, s2)
        }
    }.sum
  }

  def calculateEditDistanceOfAlignedSequences(s1: String, s2: String) = {
    var i = 0
    var s = 0
    while (i < s1.size) {
      if (s1.charAt(i) != s2.charAt(i)) {
        s += 1
      }
      i += 1
    }
    s
  }

  def flipReverseComplement(msa: FastaSequenceData): FastaSequenceData =
    flipReverseComplement(msa, msa.head._2)

  def flipReverseComplement(msa: FastaSequenceData, head: String): FastaSequenceData =
    {
      val raw = msa
      raw.map {
        case (k, s) =>
          val ed = GlobalPairwiseAlignment.editDistance(head, s)
          val edRC = GlobalPairwiseAlignment.editDistance(head, sequence.reverseComplement(s))
          (k, if (ed < edRC) s else sequence.reverseComplement(s))
      }
    }

  def removePrimers(seq: String, primers: List[String]): String = {
    val gap = 100

    val penalties = {
      val l = List('A', 'T', 'G', 'C')
      l.flatMap { c1 =>
        l.map { c2 =>
          (c1, c2) -> (if (c1 == c2) 0
          else -1)
        }
      }
    }.toMap

    primers.foldLeft(seq) { (seq, primer) =>
      val al1 = FittingPairwiseAlignment.fittingAlignment(seq, primer, penalties, gap)
      val al2 = FittingPairwiseAlignment.fittingAlignment(seq, reverseComplement(primer), penalties, gap)
      val alignment = if (al1._1 > al2._1) al1 else al2

      val primerStart = seq.indexOf(alignment._2.filterNot(_ == '-'))

      val forward = primerStart < seq.size / 2

      if (forward) seq.drop(primerStart) else seq.take(primerStart + primer.size)
    }

  }

}