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

package vcfhelpers.parser

import mybiotools._
import mybiotools.eq._

object VCFParser {
  def parse(s: scala.io.Source, cpus: Int) = {
    val lines = s.getLines.toStream

    val (metas, rest) = lines.span(MetaLine.isMetaLine)

    val infometamap =
      metas.filter(InfoMetaLine.isInfoMetaLine).map(InfoMetaLine.apply).map(x => x.id -> x).toMap

    val header = HeaderLine(rest.head)

    val iter =
      if (cpus <= 1) rest.drop(1).filter(l => !l.startsWith("#")).map(l => LazyVCFDataWithoutGenotypes(l, header, infometamap)).iterator
      else ParIterator.map(rest.drop(1).filter(l => !l.startsWith("#")).iterator, cpus)(l => LazyVCFDataWithoutGenotypes(l, header, infometamap).resolve)

    VCFContents(metas.filterNot(InfoMetaLine.isInfoMetaLine).map(l => GenericMetaLine.apply(l)).toList, header, iter)
  }

}

case class VCFContents(metaLines: List[MetaLine], headerLine: HeaderLine, iter: Iterator[VCFDataWithoutGenotypes])

object Constants {
  val FIELDSEP = '\t'
  val ALTSEP = ','
  val INFOSEP = ';'
  val KVSEP = '='
  val FORMATHEADER = "FORMAT"
  val MISSING = "."
  val GTFORMATSEP = ':'
  val PHASED = '|'
  val UNPHASED = '/'
  val INFOSUBSEP = ','
  val PHASESEPS = Set(PHASED, UNPHASED)
  val INFOMETALINESEP = ','
}
import Constants._

sealed trait InfoNumber
case object OnePerAlternateAllele extends InfoNumber
case object OnePerAllele extends InfoNumber
case object OnePerGenotype extends InfoNumber
case object Unbounded extends InfoNumber
case class Finite(size: Int) extends InfoNumber

sealed trait InfoType
case object InfoInteger extends InfoType
case object InfoFloat extends InfoType
case object InfoFlag extends InfoType
case object InfoCharacter extends InfoType
case object InfoString extends InfoType

sealed trait VCFLine {
  def line: String
}

trait MetaLine extends VCFLine {
  assert(MetaLine.isMetaLine(line))
}

object MetaLine {
  def isMetaLine(l: String) = l.startsWith("##")
}

case class GenericMetaLine(line: String) extends VCFLine with MetaLine

case class InfoMetaLine(line: String) extends VCFLine with MetaLine {
  private val splitted = fastSplit1WideSeparator(line.drop(8).dropRight(1), INFOMETALINESEP)

  val id = splitted.find(_.startsWith("ID=")).get.drop(3)

  val number = splitted.find(_.startsWith("Number=")).get.drop("Number=".size) match {
    case "A" => OnePerAlternateAllele
    case "R" => OnePerAllele
    case "G" => OnePerGenotype
    case "." => Unbounded
    case x => Finite(x.toInt)
  }

  // type is a reserved keyword in scala..
  val tipe = splitted.find(_.startsWith("Type=")).get.drop("Type=".size) match {
    case "Integer" => InfoInteger
    case "Flag" => InfoFlag
    case "Float" => InfoFloat
    case "Character" => InfoCharacter
    case "String" => InfoString
  }
}

object InfoMetaLine {
  def isInfoMetaLine(l: String) = l.startsWith("##INFO")
}

case class HeaderLine(line: String) extends VCFLine {
  assert(HeaderLine.isHeaderLine(line), line)
  val splitted: Vector[String] = fastSplit1WideSeparatorIterator(line.drop(1), FIELDSEP).toVector
  val reverse: Map[String, Int] = splitted.zipWithIndex.toMap
  val numberOfGenotypeColumns = if (!(reverse.contains(FORMATHEADER) && splitted.size > 9)) 0 else splitted.size - 9
}
object HeaderLine {
  def isHeaderLine(l: String) = l.startsWith("#")
}

trait DataLine extends VCFLine

sealed trait InfoValue {
  def resolve: ResolvedInfoValue
}
sealed trait ResolvedInfoValue extends InfoValue
case object NoValue extends ResolvedInfoValue {
  def resolve = this
}
case class UnresolvedInfoValue(value: String, tipe: InfoType, number: InfoNumber) extends InfoValue {
  def resolve: ResolvedInfoValue = {
    if (tipe == InfoFlag) NoValue
    else {
      val spl = fastSplit1WideSeparator(value, INFOSUBSEP).toVector
      if (spl.size == 1 && number == Finite(1)) InfoSingleValue(spl.head, tipe, number)
      else InfoVectorValue(spl, tipe, number)
    }
  }
}
case class InfoSingleValue(value: String, tipe: InfoType, number: InfoNumber) extends ResolvedInfoValue {
  def resolve = this
}
case class InfoVectorValue(value: Vector[String], tipe: InfoType, number: InfoNumber) extends ResolvedInfoValue {
  def resolve = this
}

sealed trait InfoValueType
case class InfoValueInteger(value: Int) extends InfoValueType
case class InfoValueFloat(value: Double) extends InfoValueType
case class InfoValueCharacter(value: Char) extends InfoValueType
case class InfoValueString(value: String) extends InfoValueType

sealed trait GenotypeValue
case class UnresolvedGenotypeString(value: String) extends GenotypeValue

sealed trait VCFData

trait HasChrBpRefAlt extends VCFData {
  def chromosome: String
  def start: Int
  def reference: String
  def alternatives: IndexedSeq[String]
  def alleleToId(s: String): Int = if (s == reference) 0 else {
    val r = alternatives.indexOf(s)
    (if (r > 0) r + 1 else r)
  }
}

trait HasFilter extends VCFData {
  def filter: String
  def pass = filter === "PASS"
}

trait HasInfo extends VCFData {
  def info: Map[String, InfoValue]
  def hasAttribute(k: String): Boolean = info.contains(k)
  def getAttribute(k: String): Option[ResolvedInfoValue] = info.get(k).map(_.resolve)
}

trait HasGenotypes extends VCFData {
  def genotypes: IndexedSeq[GenotypeValue]
}

trait HasSummaryGenotypes extends VCFData {
  def homRefCount: Int
  def hetCount: Int
  def homVarCount: Int
  def genotypeCounts(a1: Int, a2: Int): Int
  def hetCountOfAllele(a1: Int): Int
}

trait VCFDataWithoutGenotypes extends HasChrBpRefAlt with HasFilter with HasSummaryGenotypes with HasInfo

case class ResolvedVCFDataWithoutGenotypes(
    chromosome: String,
    start: Int,
    reference: String,
    alternatives: IndexedSeq[String],
    filter: String,
    info: Map[String, ResolvedInfoValue],
    homRefCount: Int,
    hetCount: Int,
    homVarCount: Int,
    genotypecountmap: scala.collection.Map[Vector[Int], Int]
) extends VCFDataWithoutGenotypes {
  def genotypeCounts(a1: Int, a2: Int) = if (a1 < a2) genotypecountmap.get(Vector(a1, a2)).getOrElse(0) else genotypecountmap.get(Vector(a2, a1)).getOrElse(0)
  def hetCountOfAllele(a1: Int) = genotypecountmap.filter(x => x._1.distinct.size > 0 && x._1.contains(a1)).values.sum
}

case class LazyVCFDataWithoutGenotypes(line: String, header: HeaderLine, infoMap: Map[String, InfoMetaLine]) extends VCFDataWithoutGenotypes with DataLine {

  def resolve: ResolvedVCFDataWithoutGenotypes =
    ResolvedVCFDataWithoutGenotypes(chromosome, start, reference, alternatives, filter, info.map(x => x._1 -> x._2.resolve), homRefCount, hetCount, homVarCount, genotypecountmap)

  private lazy val splIter = fastSplit1WideSeparatorIterator(line, FIELDSEP).toStream

  lazy val chromosome = splIter(0)

  lazy val start = splIter(1).toInt

  lazy val reference = splIter(3)

  lazy val alternatives = {
    val v = fastSplit1WideSeparatorIterator(splIter(4), ALTSEP).toVector
    if (v.head === MISSING) Vector() else v
  }

  lazy val filter = splIter(6)

  lazy val info: Map[String, InfoValue] = {
    val pairs = fastSplit1WideSeparatorIterator(splIter(7), INFOSEP)
    scala.collection.immutable.ListMap(pairs.map { p =>
      val idx = p.indexOf(KVSEP)
      if (idx === -1) p -> NoValue
      else {
        val key = p.substring(0, idx)
        val metaline = infoMap(key)

        key -> UnresolvedInfoValue(p.substring(idx + 1, p.size), metaline.tipe, metaline.number)
      }
    }.toList: _*)
  }

  private lazy val genotypecountmap: scala.collection.Map[Vector[Int], Int] = {
    if (header.numberOfGenotypeColumns == 0) Map()
    else {
      val gtsubfieldidx: Int = fastSplit1WideSeparatorIterator(splIter(8), GTFORMATSEP).indexOf("GT")
      if (gtsubfieldidx === -1) Map()
      else {
        val mmap = scala.collection.mutable.ListMap[Vector[Int], Int]()
        var refrefcount = 0
        var ref1count = 0
        var al11count = 0
        var refcount = 0
        var al1count = 0
        splIter.drop(9).foreach { gtstring =>
          val gtsubfield = fastSplit1WideSeparatorIterator(gtstring, GTFORMATSEP).drop(gtsubfieldidx).next
          val genotypes = gtsubfield match {
            case "0/0" | "0|0" => refrefcount += 1
            case "0/1" | "0|1" | "1/0" | "1|0" => ref1count += 1
            case "1/1" | "1|1" => al11count += 1
            case "0" => refcount += 1
            case "1" => al1count += 1
            case "." | "./." | ".|." => {}
            case _ => {
              val genotypes = fastSplit1WideSetSeparatorIterator(gtsubfield, PHASESEPS).map { x => if (x === MISSING) -1 else x.toInt }.toVector.sorted
              if (genotypes.head > -1) {
                mmap.get(genotypes) match {
                  case None => mmap.update(genotypes, 1)
                  case Some(x) => mmap.update(genotypes, x + 1)
                }
              }
            }
          }

        }
        mmap.update(Vector(0, 0), refrefcount)
        mmap.update(Vector(0, 1), ref1count)
        mmap.update(Vector(1, 1), al11count)
        mmap.update(Vector(0), refcount)
        mmap.update(Vector(1), al1count)

        mmap
      }
    }
  }

  lazy val (homRefCount, hetCount, homVarCount) = {
    if (header.numberOfGenotypeColumns == 0) (0, 0, 0)
    else {
      val homrefcount = genotypecountmap.filter(_._1.sum == 0).values.sum
      val hetcount = genotypecountmap.filter(x => x._1.sum > 0 && x._1.contains(0)).values.sum
      val homvarcount = genotypecountmap.filter(x => x._1.sum > 0 && !x._1.contains(0)).values.sum
      (homrefcount, hetcount, homvarcount)
    }
  }

  def hetCountOfAllele(a1: Int) = genotypecountmap.filter(x => x._1.distinct.size > 0 && x._1.contains(a1)).values.sum

  def genotypeCounts(a1: Int, a2: Int) = if (a1 < a2) genotypecountmap.get(Vector(a1, a2)).getOrElse(0) else genotypecountmap.get(Vector(a2, a1)).getOrElse(0)

}

