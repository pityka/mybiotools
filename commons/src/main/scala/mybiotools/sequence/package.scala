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
import mybiotools.eq._
import org.saddle._

/** Provides biosequence related utilities. */
package object sequence {

  val reorderNucleotides = reorderBytesFromString("ACTGN-")
  val reorderAminoAcids = reorderBytesFromString("ABCDEFGHIKLMNPQRSTVWXYZ-")
  val aminoacidRollingHash = new RollingHash(29, reorderAminoAcids)

  def reorderBytesFromString(s: String) = {
    val distinct: Set[Byte] = s.getBytes("US-ASCII").distinct.toSet
    val all: Set[Byte] = (Byte.MinValue to Byte.MaxValue).map(_.toByte).toSet
    val remaining = all &~ distinct
    val map: Array[Byte] = ((remaining.head +: distinct.toSeq) ++ remaining.toSeq.tail).zipWithIndex.sortBy(_._1).map(_._2.toByte).toArray
    assert(map.size == 256)
    assert(map.distinct.size == 256)
    (b: Byte) => map(b + 128)
  }

  def reverseComplement(pattern: String): String = {
    def complement(a: Char) = a match {
      case 'A' => 'T'
      case 'T' => 'A'
      case 'G' => 'C'
      case 'C' => 'G'
    }
    pattern.map(complement).reverse
  }

  def mafFilterOnIndicators(f: Frame[String, (Int, Char), Option[Boolean]], maf: Double) =
    f.filter { series =>
      val trueFreq = series.toVec.toSeq.count(x => x.isDefined && x.get).toDouble / series.toVec.toSeq.count(_.isDefined)

      (series.toVec.toSeq.count(_.isEmpty) <= series.length) &&
        (trueFreq >= maf && trueFreq <= (1 - maf))

    }

  def transformToIndicators(f: FastaSequenceData)(filter: Char => Boolean): Frame[String, (Int, Char), Option[Boolean]] = {
    val seq = f.toSeq
    val index = seq.map(_._1)
    val transposed: Seq[(Seq[Char], Int)] = seq.map(_._2.split("").filter(_.size > 0).toList.map(_.head)).transpose.zipWithIndex

    val dummies =
      (
        transposed.foldLeft(Vector[((Int, Char), Series[String, Option[Boolean]])]()) {
          case (list, (column, idx)) =>

            column
              .distinct
              .filter(filter)
              .sortBy(c => column.count(_ === c))
              .drop(1).map(char =>
                (idx, char) ->
                  Series(column.zipWithIndex.map(c =>
                    index(c._2) ->
                      (if (filter(c._1)) Some((c._1 === char)) else None)): _*)).toVector ++ list

        }
      ).groupBy(_._2.toVec).map(x => x._2.head).toSeq

    Frame(dummies: _*)

  }

  case class CramerV(chisquare: Double, cramerV: Double)

  def cramerV(l1: Vector[String], l2: Vector[String]) = {
    assert(l1.size == l2.size)
    val counts = scala.collection.mutable.Map[(String, String), Int]()
    l1 zip l2 foreach { p =>
      counts get p match {
        case None => counts.update(p, 1)
        case Some(c) => counts.update(p, c + 1)
      }
    }
    val n = l1.size.toDouble
    val distinct1 = l1.distinct
    val distinct2 = l2.distinct
    assert(distinct1.size > 1 && distinct2.size > 1)
    val counts1 = distinct1.map(d => d -> counts.filter(_._1._1 === d).values.sum).toMap
    val counts2 = distinct2.map(d => d -> counts.filter(_._1._2 === d).values.sum).toMap
    val possiblepairs = (distinct1 flatMap (x => distinct2 map (y => x -> y)))
    val chisquare = possiblepairs.map {
      case ((i, j)) =>
        val ni = counts1.get(i).getOrElse(0).toDouble
        val nj = counts2.get(j).getOrElse(0).toDouble
        val nij = counts.get((i, j)).getOrElse(0).toDouble
        math.pow(nij - ((ni * nj) / n), 2.0) / (ni * nj / n)
    }.sum
    val r = math.sqrt((chisquare / n) / math.min(distinct1.size - 1, distinct2.size - 1))

    // println(l1 + " " + l2 + " " + r)

    CramerV(chisquare, r)
  }

  def calculateLD(f1: FastaSequenceData, f2: FastaSequenceData): Seq[(Int, Int, Option[CramerV])] = {
    val commonkeys = (f1.keySet & f2.keySet).toSeq.sorted
    val columns1: Seq[(Vector[String], Int)] = commonkeys.map(k => f1(k).split("").filter(_.size > 0).toList).transpose.zipWithIndex.filterNot(x => x._1.forall(y => !isNucleotide(y.head))).map(x => x._1.toVector -> x._2)
    val columns2: Seq[(Vector[String], Int)] = commonkeys.map(k => f2(k).split("").filter(_.size > 0).toList).transpose.zipWithIndex.filterNot(x => x._1.forall(y => !isNucleotide(y.head))).map(x => x._1.toVector -> x._2)

    columns1.par.flatMap {
      case (c1, idx1) =>
        columns2.map {
          case (c2, idx2) =>
            val (common1, common2) = (c1 zip c2) filter (x => isNucleotide(x._1.head) && isNucleotide(x._2.head)) unzip

            if (common1.distinct.size > 1 && common2.distinct.size > 1)
              (idx1, idx2, Some(cramerV(common1, common2))) else (idx1, idx2, None)

        }
    }.seq

  }

  /**
   * Calculates all pairwise correlation and maskes (N) those positions in the second argument
   * which are correlated above threshold with any of the positions in the first alignment
   */
  // NEEDS TEST
  def maskPositionsInSecondWhichAreCorrelatedInTheFirst(
    f1: FastaSequenceData,
    f2: FastaSequenceData,
    threshold: Double
  ): FastaSequenceData = {
    val lds = calculateLD(f1, f2)
    val positionsToMask: Set[Int] = lds.filter(_._3.map(_.cramerV > threshold).getOrElse(false)).map(_._2).toSet
    f2.map(x => x._1 -> x._2.zipWithIndex.map(c => if (positionsToMask.contains(c._2)) 'N' else c._1).mkString)
  }

  /**
   * Select columns from fasta
   *
   * Selects columns from a nucleotide fasta with respect to the corresponding amino acid alignment.
   * Unselected codons are masked with 'NNN'.
   */
  def selectPositions(
    nucleotideFasta: FastaSequenceData,
    alignedAminoAcidFasta: FastaSequenceData,
    referenceName: String,
    selectAminoAcidColumns: Set[Int]
  ): FastaSequenceData = {

    val alnu = nucleotideFasta
    val alaa = alignedAminoAcidFasta

    val commonkeys = (alnu.keySet & alaa.keySet).toSeq

    val hxb2aa = alaa(referenceName)

    val indicesInGappedReference = (
      (0 until (hxb2aa.filterNot(_ == '-').size)).toSet &
      selectAminoAcidColumns
    ).map(i => indexInGapped(hxb2aa, i))

    padAlignment(
      commonkeys.map(k =>
      k -> selectFromNucleotideAminoAcidPositions(
        alnu(k).filterNot(_ == '-').toUpperCase,
        alaa(k).toUpperCase,
        indicesInGappedReference
      )).toMap,
      'N'
    )

  }

  /**
   * Translates position in an ungapped sequence to position in the gapped sequence
   *
   * @param seq gapped sequence
   * @param indexWithoutGap position in the ungapped raw sequence
   */
  def indexInGapped(seq: String, indexWithoutGap: Int): Int = {
    def loop(i: Int, acc: Int): Int =
      if (i == indexWithoutGap) acc - 1
      else {
        if (seq.charAt(acc) == '-') loop(i, acc + 1)
        else loop(i + 1, acc + 1)
      }
    loop(-1, 0)
  }

  /**
   * Select columns from sequence
   *
   * Selects columns from a nucleotide sequence with respect to the corresponding amino acid sequence, which might be gapped.
   * Processes sequence until the first nonmatching codon-aa pair (taking gaps and Ns into account).
   *
   * @param nuseq raw ungapped nucleotide sequence
   * @param aaseq amino acid sequence, might contain gaps
   * @param aapos selected positions in aaseq
   * @return The resulting nucleotide sequence will maintain the gaps of the amino acid sequence. Unselected codons are masked with 'NNN'.
   */
  def selectFromNucleotideAminoAcidPositions(
    nuseq: String,
    aaseq: String,
    aapos: Set[Int],
    throwExceptionOnMismatch: Boolean = true
  ): String = {
    require(nuseq.filter(x => x == '-').size == 0, "nucleotides should not have -")
    require(aaseq.size > 0, "empty seq")

    def loop(aas: String, nus: String, i: Int, acc: String): String =
      if (0 == aas.size || nus.size < 3) acc
      else {
        val aa = aas.head
        val codon = nus.take(3)
        val codontowrite = if (aapos.contains(i) && GeneticCode.matches(codon, aa)) codon else "NNN"

        // if (aa == '-' && codon == "NNN") loop(aas.tail, nus.drop(3), i + 1, acc + "---")
        if (aa == '-') loop(aas.tail, nus, i + 1, acc + "---")
        else if (GeneticCode.matches(codon, aa)) loop(aas.tail, nus.drop(3), i + 1, acc + codontowrite)
        else if (throwExceptionOnMismatch) throw new RuntimeException(s"Error at position $i codon $codon does not match $aa in sequences: \n nu: $nuseq \n al: $aaseq") else acc

      }

    loop(aaseq, nuseq, 0, "")

  }

  /**
   * Inserts back missing characters
   *
   * Muscle removes certain characters from sequence before aligning it.
   * This procedure inserts back those characters at the correct aligned position.
   */
  def patchAlignedSequence(aligned: String, unaligned: String): String = {

    def loop(aligned: String, unaligned: String, alignedWithoutGaps: String, acc: String, outstandingGaps: Int): String = {
      // println(aligned + " " + unaligned + " " + alignedWithoutGaps + " " + acc)
      if (unaligned.isEmpty && aligned.isEmpty) acc
      else if (!aligned.isEmpty && !unaligned.isEmpty && !alignedWithoutGaps.isEmpty && aligned.head == unaligned.head && aligned.head == alignedWithoutGaps.head)
        loop(aligned.drop(1), unaligned.drop(1), alignedWithoutGaps.drop(1), acc :+ aligned.head, outstandingGaps)
      else if (((unaligned.headOption === alignedWithoutGaps.headOption))) {
        if (outstandingGaps == 0) loop(aligned.drop(1), unaligned, alignedWithoutGaps, acc :+ '-', 0)
        else loop(aligned.drop(1), unaligned, alignedWithoutGaps, acc, outstandingGaps - 1)
      } else if (!aligned.isEmpty && aligned.head === '-') loop(aligned.drop(1), unaligned.drop(1), alignedWithoutGaps, acc :+ unaligned.head, outstandingGaps)
      else loop(aligned, unaligned.drop(1), alignedWithoutGaps, acc :+ unaligned.head, outstandingGaps + 1)
    }

    loop(aligned, unaligned.filterNot(_ === '-'), aligned.filterNot(_ === '-'), "", 0)

  }

  def verifyAminoAcidAndNucleotideSequences(
    nuseq: String,
    aaseq: String
  ): Boolean = {
    require(nuseq.filter(x => x == '-').size == 0)

    def loop(aas: String, nus: String): Boolean =
      if (aas.size == 0) true
      else {
        val aa = aas.head
        val codon = nus.take(3)
        if (aa == '-' && codon == "NNN") loop(aas.tail, nus.drop(3))
        else if (aa == '-') loop(aas.tail, nus)
        else if (GeneticCode.matches(codon, aa)) loop(aas.tail, nus.drop(3))
        else if (aa == 'X' && GeneticCode.hasAmbiguousNucleotideCodes(codon)) loop(aas.tail, nus.drop(3))
        else if (aa == 'X') loop(aas.tail, nus)
        else throw new RuntimeException(aas + "\n" + nus + "\n")
      }

    loop(aaseq, nuseq)

  }

  /**
   * Translates nucleotide sequence to amino acid sequence.
   *
   * @param nucleotides: string of nucleotides. can be uncapitalized. gaps and unknown are translated to X
   * @return amino acid sequence
   */
  def translate(nucleotides: String, keepGapsInFrame: Boolean): String = {
    val withOrWithOutGaps = if (keepGapsInFrame) nucleotides.replaceAllLiterally("-", "N") else nucleotides.replaceAllLiterally("-", "")
    (withOrWithOutGaps.toUpperCase grouped (3) map GeneticCode.codon2aa).mkString
  }

  def translateFasta(alignment: FastaSequenceData, keepGapsInFrame: Boolean): FastaSequenceData =
    alignment map (x => x._1 -> translate(x._2, keepGapsInFrame))

  /**
   * Aligns nucleotide sequence based on aligned protein sequence.
   *
   * The nucleotide sequence can longer in both directions and can contain missing fragments. protein sequence can not contain missing fragments which are present in the nucleotide sequence. possible modifications of the nucleotide sequence: insertion of --- , deletion of 1 or 2 nucleotides. In case the protein contains fragments not present in the nucleotide, the sequence will be processed until this fragment
   * If multiple stretches of the nucleotide sequence can be aligned on the protein, then the one with the highest number of matches will be returned.
   * @param nucleotides : unaligned nucleotides. Gaps ("-") are ignored. Unknowns("N") are translated as "X".
   * @param alignedProtein : aligned AA sequence.
   */
  def tranalign(nucleotides: String, alignedProtein: String): Option[String] = {

    def tranalignSuffix(nucleotides: String, alignedProtein: String, acc: String, numberOfMatch: Int): Option[Tuple2[String, Int]] = {
      if (alignedProtein.size == 0) Some((acc, numberOfMatch)) // Done
      else {
        if (alignedProtein.head == '-')
          tranalignSuffix(nucleotides, alignedProtein.drop(1), acc + "---", numberOfMatch)
        else if (nucleotides.size > 0 && GeneticCode.matches(nucleotides.take(3), alignedProtein.head))
          tranalignSuffix(nucleotides.drop(3), alignedProtein.drop(1), acc + nucleotides.take(3), numberOfMatch + 1)
        else if (alignedProtein.head == 'X') tranalignSuffix(nucleotides, alignedProtein.drop(1), acc + "---", numberOfMatch)
        else if (nucleotides.size >= 4 && GeneticCode.matches(nucleotides.take(4).drop(1), alignedProtein.head))
          tranalignSuffix(nucleotides.drop(4), alignedProtein.drop(1), acc + nucleotides.take(4).drop(1), numberOfMatch + 1)
        else if (nucleotides.size >= 5 && GeneticCode.matches(nucleotides.take(5).drop(2), alignedProtein.head))
          tranalignSuffix(nucleotides.drop(5), alignedProtein.drop(1), acc + nucleotides.take(5).drop(2), numberOfMatch + 1)
        else if (acc.size > 0) tranalignSuffix(nucleotides, alignedProtein.drop(1), acc + "---", numberOfMatch)
        else None
      }
    }

    nucleotides.replaceAllLiterally("-", "").tails.map(x => tranalignSuffix(x, alignedProtein, "", 0)).filter(_.isDefined).map(_.get).toList match {
      case Nil => None
      case x => x.maxBy(_._2)._1 match {
        case x if x == "" => None
        case x => Some(x)
      }
    }
  }

}