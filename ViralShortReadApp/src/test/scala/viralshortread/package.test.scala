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

package viralshortread

import org.scalatest.FunSuite
import org.scalatest.Matchers
import collection.immutable.HashMap
import rnaseqalign.htseqcount._
import mybiotools.sequence.alignment._

class ViralShortReadSpec extends FunSuite {

  test("mapcoordinates") {
    val referenceAlignment = Cigar(List(CigarElement(4, M), CigarElement(2, D), CigarElement(2, M), CigarElement(4, D), CigarElement(3, M)))

    assertResult(mapcoordinates(referenceAlignment, PositionOnReference(0)))(PositionOnReference(0))
    assertResult(mapcoordinates(referenceAlignment, PositionOnReference(1)))(PositionOnReference(1))
    assertResult(mapcoordinates(referenceAlignment, PositionOnReference(2)))(PositionOnReference(2))
    assertResult(mapcoordinates(referenceAlignment, PositionOnReference(3)))(PositionOnReference(3))
    assertResult(mapcoordinates(referenceAlignment, PositionOnReference(4)))(PositionOnReference(6))
    assertResult(mapcoordinates(referenceAlignment, PositionOnReference(5)))(PositionOnReference(7))
    assertResult(mapcoordinates(referenceAlignment, PositionOnReference(6)))(PositionOnReference(12))
    assertResult(mapcoordinates(referenceAlignment, PositionOnReference(7)))(PositionOnReference(13))
    assertResult(mapcoordinates(referenceAlignment, PositionOnReference(8)))(PositionOnReference(14))

    intercept[RuntimeException](mapcoordinates(referenceAlignment, PositionOnReference(9)))

    assertResult(mapcoordinates(referenceAlignment, InsertionIntoReference(0, 0)))(InsertionIntoReference(0, 0))
    assertResult(mapcoordinates(referenceAlignment, InsertionIntoReference(0, 1)))(InsertionIntoReference(0, 1))

    assertResult(mapcoordinates(referenceAlignment, InsertionIntoReference(1, 0)))(InsertionIntoReference(1, 0))
    assertResult(mapcoordinates(referenceAlignment, InsertionIntoReference(1, 1)))(InsertionIntoReference(1, 1))

    assertResult(mapcoordinates(referenceAlignment, InsertionIntoReference(2, 0)))(InsertionIntoReference(2, 0))
    assertResult(mapcoordinates(referenceAlignment, InsertionIntoReference(2, 1)))(InsertionIntoReference(2, 1))

    assertResult(mapcoordinates(referenceAlignment, InsertionIntoReference(3, 0)))(InsertionIntoReference(3, 0))
    assertResult(mapcoordinates(referenceAlignment, InsertionIntoReference(3, 1)))(InsertionIntoReference(3, 1))

    assertResult(mapcoordinates(referenceAlignment, InsertionIntoReference(4, 0)))(PositionOnReference(4))
    assertResult(mapcoordinates(referenceAlignment, InsertionIntoReference(4, 1)))(PositionOnReference(5))
    assertResult(mapcoordinates(referenceAlignment, InsertionIntoReference(4, 2)))(InsertionIntoReference(6, 0))

    assertResult(mapcoordinates(referenceAlignment, InsertionIntoReference(5, 0)))(InsertionIntoReference(7, 0))
    assertResult(mapcoordinates(referenceAlignment, InsertionIntoReference(5, 1)))(InsertionIntoReference(7, 1))
    assertResult(mapcoordinates(referenceAlignment, InsertionIntoReference(5, 2)))(InsertionIntoReference(7, 2))

    assertResult(mapcoordinates(referenceAlignment, InsertionIntoReference(6, 0)))(PositionOnReference(8))
    assertResult(mapcoordinates(referenceAlignment, InsertionIntoReference(6, 1)))(PositionOnReference(9))
    assertResult(mapcoordinates(referenceAlignment, InsertionIntoReference(6, 2)))(PositionOnReference(10))
    assertResult(mapcoordinates(referenceAlignment, InsertionIntoReference(6, 3)))(PositionOnReference(11))

    assertResult(mapcoordinates(referenceAlignment, InsertionIntoReference(7, 0)))(InsertionIntoReference(13, 0))
    assertResult(mapcoordinates(referenceAlignment, InsertionIntoReference(7, 1)))(InsertionIntoReference(13, 1))
    assertResult(mapcoordinates(referenceAlignment, InsertionIntoReference(7, 2)))(InsertionIntoReference(13, 2))

    assertResult(mapcoordinates(referenceAlignment, InsertionIntoReference(8, 0)))(InsertionIntoReference(14, 0))
    assertResult(mapcoordinates(referenceAlignment, InsertionIntoReference(8, 1)))(InsertionIntoReference(14, 1))
    assertResult(mapcoordinates(referenceAlignment, InsertionIntoReference(8, 2)))(InsertionIntoReference(14, 2))
  }

  test("sam2consensus on long sequence without insertions") {
    val kmersize = 100
    val textsize = 200000
    val alphabet = Vector('A', 'T', 'G', 'C')
    val rnd = scala.util.Random
    val text = 0 until textsize map (i => alphabet(rnd.nextInt(4))) mkString
    val kmers = 0 until textsize - kmersize + 1 map { start =>
      val original = text.substring(start, start + kmersize)
      val list = if (rnd.nextDouble < 0.0) {
        val insert = rnd.nextInt(original.size)
        List(insert -> "XXX")
      } else Nil
      AlignedStringWithStart(start, original, list.toMap)
    }

    assertResult(sam2sequence(kmers.iterator)(consensusCall(_, 1)).map(_._2).mkString)(text)

  }

  test("sam2consensus on long sequence with insertions") {
    val kmersize = 100
    val textsize = 20000
    val alphabet = Vector('A', 'T', 'G', 'C')
    val rnd = scala.util.Random
    val text = 0 until textsize map (i => alphabet(rnd.nextInt(4))) mkString
    val kmers = 0 until textsize - kmersize + 1 map { start =>
      val original = text.substring(start, start + kmersize)
      val list = if (rnd.nextDouble < 0.1) {
        val insert = rnd.nextInt(original.size)
        List(insert -> "XXX")
      } else Nil
      AlignedStringWithStart(start, original, list.toMap)
    }

    assertResult(sam2sequence(kmers.iterator)(consensusCall(_, 1)).filterNot(_._2 == '-').map(_._2).mkString)(text)

  }

  test("parseCigar") {
    assertResult(parseCigar(Cigar(List(
      CigarElement(1, S),
      CigarElement(1, M),
      CigarElement(1, S)
    )), "012", '-'))(("1", List()))

    assertResult(parseCigar(Cigar(List(CigarElement(3, S), CigarElement(98, M), CigarElement(16, S))), "GGCAATCCGCCTCCTGCATCCACCAATCGCCGGTCAGGAAGGCAGCCTACCCCGCTGTCTCCACCGTTGAGAGACACGCATCCTCAGGCCATGCAGTGGAAATCCACAACCTTCCAC", '-'))("AATCCGCCTCCTGCATCCACCAATCGCCGGTCAGGAAGGCAGCCTACCCCGCTGTCTCCACCGTTGAGAGACACGCATCCTCAGGCCATGCAGTGGAA", Nil)

    assertResult(parseCigar(Cigar(List(CigarElement(85, M), CigarElement(60, D), CigarElement(128, M), CigarElement(38, S))), "AGCCTTCAGAGCAAACACCGCAAATCCAGATTGGGACTTCAATCCCAACAAGGACCCCTGGCCAGACGCCAACAAGGTAGGAGCTGCTCAGGGCATACGACAAACCTTGCCAGCAAATCCGCCTCCTGCATCCACCAATCGCCAGTCGGGAAGGCAGCCTACCCCGCTGTCTCCACCGTTGAGAGACACGCATCCTCAGCCCATGCAGTGGAAATCCAGGGTGACACCTGCCGGTGGCCCCTGCTCAGGAA", '-'))(("AGCCTTCAGAGCAAACACCGCAAATCCAGATTGGGACTTCAATCCCAACAAGGACCCCTGGCCAGACGCCAACAAGGTAGGAGCT------------------------------------------------------------GCTCAGGGCATACGACAAACCTTGCCAGCAAATCCGCCTCCTGCATCCACCAATCGCCAGTCGGGAAGGCAGCCTACCCCGCTGTCTCCACCGTTGAGAGACACGCATCCTCAGCCCATGCAGTGGAA", List()))

    assertResult(parseCigar(Cigar(List(CigarElement(81, H), CigarElement(87, M), CigarElement(1, I), CigarElement(13, M), CigarElement(68, H))), "CAAATCCGCCTCCTGCATCCACCAATCGCCAGTCAGGAAGGCAGCCTACCCCGCTGTCTCCACCGTTGAGAGACACGCATCCTCAGGCCCATGCAGTGGAA", '-'))("CAAATCCGCCTCCTGCATCCACCAATCGCCAGTCAGGAAGGCAGCCTACCCCGCTGTCTCCACCGTTGAGAGACACGCATCCTCAGGCCATGCAGTGGAA", List(87 -> "C"))

    assertResult(parseCigar(Cigar(List(
      CigarElement(1, H),
      CigarElement(1, S),
      CigarElement(1, M),
      CigarElement(1, X),
      CigarElement(1, I),
      CigarElement(1, M),
      CigarElement(1, D),
      CigarElement(1, M),
      CigarElement(1, S),
      CigarElement(1, H)
    )), "0123456789", '-'))(("124-5789", List(2 -> "3")))

    assertResult(parseCigar(Cigar(List(
      CigarElement(1, S),
      CigarElement(1, M),
      CigarElement(1, X),
      CigarElement(3, I),
      CigarElement(1, M),
      CigarElement(1, D),
      CigarElement(1, M),
      CigarElement(1, I),
      CigarElement(1, S)
    )), "0123456789", '-'))(("126-7", List(5 -> "8", 2 -> "345")))
  }

  test("sam2consensus") {
    assertResult {
      sam2sequence(List(
        AlignedStringWithStart(0, "012", Map[Int, String]()),
        AlignedStringWithStart(3, "345", Map[Int, String]()),
        AlignedStringWithStart(6, "678", Map[Int, String]())
      ).iterator)(_.headOption.getOrElse('N')).map(_._2).mkString
    }("012345678")

    assertResult {
      sam2sequence(List(
        AlignedStringWithStart(0, "012", Map[Int, String]()),
        // AlignedStringWithStart(3, "345"),
        AlignedStringWithStart(6, "678", Map[Int, String]())
      ).iterator)(_.headOption.getOrElse('N')).map(_._2).mkString
    }("012NNN678")

    assertResult {
      sam2sequence(List(
        AlignedStringWithStart(0, "012", Map[Int, String]()),
        AlignedStringWithStart(2, "234", Map[Int, String]()),
        AlignedStringWithStart(6, "678", Map[Int, String]()),
        AlignedStringWithStart(7, "789", Map[Int, String]())
      ).iterator)(_.headOption.getOrElse('N')).map(_._2).mkString
    }("01234N6789")

    assertResult {
      sam2sequence(List(
        AlignedStringWithStart(0, "012", Map[Int, String]()),
        AlignedStringWithStart(0, "012", Map[Int, String]()),
        AlignedStringWithStart(2, "234", Map[Int, String]()),
        AlignedStringWithStart(6, "678", Map[Int, String]()),
        AlignedStringWithStart(7, "789", Map[Int, String]())
      ).iterator)(_.headOption.getOrElse('N')).map(_._2).mkString
    }("01234N6789")

    assertResult {
      sam2sequence(List(
        AlignedStringWithStart(0, "012", Map[Int, String](0 -> "ABC")),
        AlignedStringWithStart(3, "345", Map[Int, String]()),
        AlignedStringWithStart(6, "678", Map[Int, String]())
      ).iterator)(_.headOption.getOrElse('N')).map(_._2).mkString
    }("ABC012345678")

    assertResult {
      sam2sequence(List(
        AlignedStringWithStart(0, "012", Map[Int, String]()),
        AlignedStringWithStart(3, "345", Map[Int, String](0 -> "ABC")),
        AlignedStringWithStart(6, "678", Map[Int, String]())
      ).iterator)(_.headOption.getOrElse('N')).map(_._2).mkString
    }("012ABC345678")

    assertResult {
      sam2sequence(List(
        AlignedStringWithStart(0, "012", Map[Int, String]()),
        AlignedStringWithStart(3, "345", Map[Int, String]()),
        AlignedStringWithStart(6, "678", Map[Int, String](0 -> "ABC"))
      ).iterator)(_.headOption.getOrElse('N')).map(_._2).mkString
    }("012345ABC678")

    assertResult {
      sam2sequence(List(
        AlignedStringWithStart(0, "012", Map[Int, String]()),
        AlignedStringWithStart(3, "345", Map[Int, String]()),
        AlignedStringWithStart(6, "678", Map[Int, String](1 -> "XYZ", 0 -> "ABC"))
      ).iterator)(_.headOption.getOrElse('N'))
    }(List(
      PositionOnReference(0) -> '0',
      PositionOnReference(1) -> '1',
      PositionOnReference(2) -> '2',
      PositionOnReference(3) -> '3',
      PositionOnReference(4) -> '4',
      PositionOnReference(5) -> '5',
      InsertionIntoReference(6, 0) -> 'A',
      InsertionIntoReference(6, 1) -> 'B',
      InsertionIntoReference(6, 2) -> 'C',
      PositionOnReference(6) -> '6',
      InsertionIntoReference(7, 0) -> 'X',
      InsertionIntoReference(7, 1) -> 'Y',
      InsertionIntoReference(7, 2) -> 'Z',
      PositionOnReference(7) -> '7',
      PositionOnReference(8) -> '8'
    ))

  }

}