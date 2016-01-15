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

import mybiotools.{ fastSplitSeparator, SequenceKey, FastaSequenceData, padAlignment, formatters }
import formatters._
import Formatters._

package object g2gsequence {

  /**
   * Sequence variation position, reference
   *
   * @param referenceName name of the reference sequence. The coordinates map on this sequence.
   * @param positionIndex 1-based index of the position on the reference sequence
   * @param insertionsAfter number of insertions after @positionIndex on the reference sequence. 1 based. In a sequence "a-" the position of the dash is represented as (1,1), the position of the a is represented as (1,0)
   * @param pivotAminoAcid amino acid one vs all
   */
  case class MutationLabel(
      referenceName: String,
      positionIndex1Based: Int,
      insertionsAfter: Int,
      pivotAminoAcid: String
  ) {
    assert(pivotAminoAcid.length == 1)
    override def toString = List(referenceName, positionIndex1Based.toString, insertionsAfter, pivotAminoAcid.toString).mkString("_")
  }
  object MutationLabel {
    def fromString(s: String): MutationLabel = {
      val sp = fastSplitSeparator(s, '_')
      apply(sp(0), sp(1).toInt, sp(2).toInt, sp(3))
    }
  }

  implicit val MutationLabelordering = scala.math.Ordering.by((x: MutationLabel) => MutationLabel.unapply(x).get)

  type RawPhenotype[B, C] = Map[MutationLabel, Map[B, C]]

  def createBinaryPhenotypesWhereVariable(
    alignment: FastaSequenceData,
    referenceKey: SequenceKey,
    minimumDataCount: Int,
    minorAlleleCount: Int,
    includedSequenceKeys: Seq[SequenceKey],
    includePositionFilter0Based: Option[List[Int]],
    excludePositionFilter0Based: Option[List[Int]],
    forbiddenLetters: List[String] = List("*", "-", "X")
  ): Tuple2[RawPhenotype[SequenceKey, Option[Boolean]], Set[Int]] = {

    def findIndexOnWithoutGaps(s: String, idx: Int): (Int, Int) = {
      val prefix = s.take(idx + 1)
      val index = prefix.filter(_ != '-').size
      if (s(idx) != '-') (index, 0)
      else {
        val insertions = prefix.reverse.prefixLength(_ == '-')
        (index, insertions)
      }

    }

    // Pad the sequence, filter out reference and unneeded keys and transpose it
    val padded = padAlignment(alignment)
    val filtered = padded.filterKeys(key => key != referenceKey && includedSequenceKeys.contains(key))

    val letterMatrix = for ((key, seq) <- filtered) yield {
      seq.split("").toList // Make an Array[String] from String
    }
    val columnKeys = filtered.keys.toIndexedSeq // Fasta headers
    var listOfPhenotypes = List[Tuple2[MutationLabel, Map[SequenceKey, Option[Boolean]]]]() // Fill up this list
    val transposed = letterMatrix.transpose
    var listOfVariableSites = scala.collection.mutable.ListBuffer[Int]()
    transposed.zipWithIndex.foreach {
      case (column, index) =>
        // If a maskFilter or excludeFilter is passed, then use it.
        if ((includePositionFilter0Based == None || includePositionFilter0Based.get.contains(index)) && (excludePositionFilter0Based == None || !excludePositionFilter0Based.get.contains(index))) {

          // Count each the number of each letter/code in the column
          val bucketsOfLetterCounts = scala.collection.mutable.HashMap[String, Int]()
          var countOfValidLetter = 0
          for (letter <- column; if (!forbiddenLetters.contains(letter))) {
            val lc = letter
            countOfValidLetter += 1
            if (bucketsOfLetterCounts.contains(lc)) bucketsOfLetterCounts(lc) += 1
            else bucketsOfLetterCounts += (lc -> 1)
          }

          if (countOfValidLetter >= minimumDataCount) {
            // Use the minorAlleleCount criterium on the letters of the column
            val sorted = bucketsOfLetterCounts.filter(entry => entry._2 >= minorAlleleCount && entry._2 <= (countOfValidLetter - minorAlleleCount)).keys.toList.sorted

            if (sorted.size > 0) {
              listOfVariableSites.append(index)
            }

            // Generate all the binary phenotypes for the column
            for (pivotLetter <- sorted) {
              val phcol = column.zipWithIndex.map {
                case (x, index) =>
                  if (forbiddenLetters.contains(x))
                    columnKeys(index) -> None
                  else if (pivotLetter == x)
                    columnKeys(index) -> Some(true)
                  else columnKeys(index) -> Some(false)
              }.toList

              // Check whether the last created binary variable is the exact inverse of this one.
              //  If yes, then skip this. If not, then append to the return list
              val last = listOfPhenotypes.headOption
              var lastPhenotypeColumnIsNotTheInverseOfThisOne = false
              if (last != None) {
                val lastphcol = last.get._2
                if (findIndexOnWithoutGaps(padded(referenceKey), index) == (last.get._1.positionIndex1Based, last.get._1.insertionsAfter) && phcol.size == lastphcol.size) {
                  lastPhenotypeColumnIsNotTheInverseOfThisOne = phcol.exists {
                    case (seqKey, binaryData1) =>
                      formatters.inverseBoolean(binaryData1) != lastphcol(seqKey)
                  }
                } else lastPhenotypeColumnIsNotTheInverseOfThisOne = true
              } else lastPhenotypeColumnIsNotTheInverseOfThisOne = true

              if (lastPhenotypeColumnIsNotTheInverseOfThisOne) {

                val (indexOnReference, insertionsAfter) = findIndexOnWithoutGaps(padded(referenceKey), index)
                val mutationlabel = MutationLabel(referenceKey.toString, indexOnReference, insertionsAfter, pivotLetter.toUpperCase)
                listOfPhenotypes = (mutationlabel -> Map(phcol: _*)) :: listOfPhenotypes

              }
            }
          }

        }
    }
    (Map(listOfPhenotypes: _*), listOfVariableSites.toSet)
  }

  def formatPhenotypesForPlink[C <: Any](binaryVariables: RawPhenotype[SequenceKey, C], orderOfSequenceKeys: List[SequenceKey]): String = {

    val listOfRowsInTable: List[Map[Symbol, Any]] = orderOfSequenceKeys.map { seqKey =>
      var row = Map[Symbol, Any]()
      binaryVariables.foreach { entry =>
        row += (Symbol(entry._1.toString) -> entry._2.get(seqKey).getOrElse(None))
      }
      row += ('FID -> seqKey)
      row += ('IID -> 1)
      row
    }
    val k = List('FID, 'IID) ::: binaryVariables.keys.toSeq.sorted.map { x: MutationLabel => Symbol(x.toString) }.toList
    formatTable(listOfRowsInTable, keys = k, software = PlinkFam)
  }

  def formatPhenotypesForTFamTPed(binaryVariables: RawPhenotype[SequenceKey, Option[Boolean]], orderOfSequenceKeys: List[SequenceKey]): Tuple2[String, String] = {

    val tped: String = binaryVariables.map { bvar =>
      "0 " + bvar._1.toString + " 0 0 " + orderOfSequenceKeys.map { seqKey =>
        bvar._2.get(seqKey) match {
          case Some(Some(true)) => "T T"
          case Some(Some(false)) => "A A"
          case None => "0 0"
          case Some(None) => "0 0"
        }
      }.mkString(" ")
    }.mkString("\n")
    val tfam = orderOfSequenceKeys.map(x => x.toString + " 1 0 0 " + PlinkFam.missingValue + " " + PlinkFam.missingValue).mkString("\n")
    (tfam, tped)
  }

  def phenotypeSummaryStatistics(binaryVariables: RawPhenotype[SequenceKey, Option[Boolean]], orderOfSequenceKeys: List[SequenceKey]): Map[MutationLabel, Tuple3[Int, Int, Int]] = {
    for ((label, column) <- binaryVariables) yield {
      var trues = 0
      var falses = 0
      var nas = 0
      var other = 0
      orderOfSequenceKeys.foreach { seqKey =>
        column(seqKey) match {
          case Some(y) => y match {
            case true => trues += 1
            case false => falses += 1
          }
          case None => nas += 1
        }
      }
      (label -> Tuple3(trues, falses, nas))
    }
  }

  def formatPhenotypeSummaryStatistics(data: Map[MutationLabel, Tuple3[Int, Int, Int]]): String = {
    val table: List[Map[Symbol, Any]] = (for ((label, tuple3) <- data) yield {
      Map[Symbol, Any](
        'phenoType -> label,
        'cases -> tuple3._1, 'controls -> tuple3._2, 'missings -> tuple3._3
      )
    }).toList
    formatTable(table)
  }

}

