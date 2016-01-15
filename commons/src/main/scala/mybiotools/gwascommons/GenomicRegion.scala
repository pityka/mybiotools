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

package mybiotools.gwascommons

import mybiotools.{ trimChrPrefix, chromosomeStringFromNumber, chromosomeNumberFromString, jumpDirectionalSearch, addMaps }
import mybiotools.stringstore._

trait AbstractRegion[T, GL <: GenomicLocationStub[T], K <: RegionStub[T, GL, K]] {
  def contains(gl: GL): Boolean
  def intersects(that: K): Boolean
}

case class Region(chromosomeAsT: String8, from: Int, to: Int) extends RegionStub[String8, GenomicLocation, Region] {
  def chromosome = chromosomeAsT.value
}

@SerialVersionUID(1L)
trait RegionBuilder[T, GL <: GenomicLocationStub[T], K <: RegionStub[T, GL, K]] extends Serializable {
  def build(chr: T, from: Int, to: Int): K
}

@SerialVersionUID(1L)
trait GenomicLocationBuilder[T, GL <: GenomicLocationStub[T]] extends Serializable {
  def build(chr: T, loc: Int): GL
}

trait RegionStub[@specialized(Int) T, GL <: GenomicLocationStub[T], K <: RegionStub[T, GL, K]] extends mybiotools.intervaltree.Interval { self: K =>

  def chromosomeAsT: T

  def from: Int

  def to: Int

  def chromosome: String

  assert(from <= to, s"from > to, $from < $to")

  def toLine = s"$chromosome\t$from\t$to"

  def toLineWithChr = s"chr$chromosome\t$from\t$to"

  def toGATKStringWithChr = "chr" + chromosome + ":" + (from + 1).toString + "-" + (to + 1).toString

  def extendBefore(i: Int)(implicit r: RegionBuilder[T, GL, K]) =
    if (i <= 0) this
    else r.build(chromosomeAsT, scala.math.max(0, from - i), to)

  def extendAfter(i: Int)(implicit r: RegionBuilder[T, GL, K]) =
    if (i <= 0) this
    else r.build(chromosomeAsT, from, to + i)

  def contains(gl: GL): Boolean = size > 0 && gl.chromosomeAsT == chromosomeAsT && gl.basePairPosition0based >= from && gl.basePairPosition0based < to

  def contains(that: K): Boolean = size > 0 && this.chromosomeAsT == that.chromosomeAsT && (this.from <= that.from && this.to >= that.to)

  def intersects(that: K): Boolean = that.size > 0 && this.size > 0 && this.chromosomeAsT == that.chromosomeAsT && (super.intersects(that))

  def collapse(that: K, collapsibleGap: Int)(implicit r: RegionBuilder[T, GL, K]): Option[K] = {
    if (this.chromosomeAsT == that.chromosomeAsT) {
      val left = if (this.from < that.from) this else that
      val right = if (this.from >= that.from) this else that
      if (left.to + collapsibleGap >= right.from) {
        if (left.to >= right.to) Some(r.build(left.chromosomeAsT, left.from, left.to))
        else Some(r.build(this.chromosomeAsT, left.from, right.to))
      } else None
    } else None
  }

  def subtract(that: K)(implicit r: RegionBuilder[T, GL, K]): List[K] = {
    if (this.chromosomeAsT != that.chromosomeAsT) List(this)
    else {
      if (that.contains(this)) Nil
      else {
        // left overlap
        if (this.from >= that.from && this.from < that.to && this.to > that.to) {
          List(r.build(this.chromosomeAsT, that.to, this.to))
        } else {
          // right overlap
          if (this.from < that.from && that.from < this.to && that.to > this.to) {
            List(r.build(this.chromosomeAsT, this.from, that.from))
          } else {
            if (this.contains(that)) {
              List(
                r.build(chromosomeAsT, this.from, that.from),
                r.build(chromosomeAsT, that.to, this.to)
              ).filter(_.size > 0)
            } else {
              List(this)
            }
          }
        }
      }
    }
  }

  def chunkToMaxSize(max: Int)(implicit r: RegionBuilder[T, GL, K]): List[K] = {
    if (max < 1) throw new IllegalArgumentException

    def loop(last: K, acc: List[K]): List[K] =
      if (last.size <= max) last :: acc
      else loop(r.build(last.chromosomeAsT, last.from + max, last.to), r.build(last.chromosomeAsT, last.from, last.from + max) :: acc)

    loop(this, Nil)
  }

}

object Region {

  def apply(chr: String, from: Int, to: Int): Region = Region(StringStore(chr), from, to)
  def apply(chr: Int, from: Int, to: Int): Region = Region(StringStore(chr.toString), from, to)

  def fromLine(s: String): Region = {
    val spl = mybiotools.fastSplitSetSeparator(s, Set('\t', ' '))
    val chr = new java.lang.String(spl(0))
    val from = spl(1).toInt
    val to = spl(2).toInt
    Region(chr, from, to)
  }

  def fromGenomicLocation(gl: GenomicLocation) =
    Region(gl.chromosomeAsT, gl.basePairPosition0based, gl.basePairPosition0based + 1)

  private def assertOrder(s: Seq[RegionStub[_, _, _]]) {
    if (s.size > 1) {
      s.sliding(2).foreach { x => assert(x.head.from <= x(1).from) }
    }
  }

  /** @param s should be ordered by from! */
  private[gwascommons] def collapseSameChromosome[T, GL <: GenomicLocationStub[T], K <: RegionStub[T, GL, K]](s: Seq[K], collapsibleGap: Int)(implicit rb: RegionBuilder[T, GL, K]): IndexedSeq[K] = {
    assertOrder(s)
    s.foldLeft(Vector[K]()) { (acc, region) =>
      acc match {
        case Vector() => Vector(region)
        case xs :+ x => region.collapse(x, collapsibleGap) match {
          case None => xs :+ x :+ region
          case Some(collapsed) => xs :+ collapsed
        }
      }
    }
  }

  private[gwascommons] def subtractSameChromosome[T, GL <: GenomicLocationStub[T], K <: RegionStub[T, GL, K]](x: Seq[K], y: Seq[K])(implicit rb: RegionBuilder[T, GL, K]): IndexedSeq[K] = {
    assertOrder(x)
    assertOrder(y)

    def recurse(op1: Option[K], ops2: Seq[K], acc: Vector[K]): Vector[K] =
      if (ops2.size == 0) op1 match {
        case None => acc
        case Some(x) => acc :+ x
      }
      else {
        val s = op1.get.subtract(ops2.head)
        s.size match {
          case 2 => recurse(Some(s(1)), ops2.tail, acc :+ s.head)
          case 1 => recurse(Some(s(0)), ops2.tail, acc)
          case 0 => recurse(None, Nil, acc)
        }

      }

    x.foldLeft(Vector[K]()) { (acc, region1) =>
      acc ++ recurse(Some(region1), y, Vector())
    }
  }

  def collapse[T, GL <: GenomicLocationStub[T], K <: RegionStub[T, GL, K]](s: Iterable[K], collapsibleGap: Int)(implicit rb: RegionBuilder[T, GL, K]): Iterable[K] =
    s.groupBy(_.chromosome).values.flatMap(samechr => collapseSameChromosome[T, GL, K](samechr.toSeq.sortBy(_.from), 0))
}

sealed trait RegionSet[T, GL <: GenomicLocationStub[T], K <: RegionStub[T, GL, K]] extends AbstractRegion[T, GL, K] {

  def toRegions: Set[K]

  def chunkToMaxSize(max: Int): Iterable[List[K]]

  def middlePoints: Vector[GL]

  def contains(g: GL): Boolean

  def intersects(g: K): Boolean

  def +(that: RegionSet[T, GL, K]): RegionSet[T, GL, K] = (this, that) match {
    case (x: Everything[_, _, _], _) => new Everything[T, GL, K] {}
    case (_, x: Everything[_, _, _]) => new Everything[T, GL, K] {}
    case (y: Empty[_, _, _], x) => x
    case (x, y: Empty[_, _, _]) => x
    case (x: RegionSet.RegionSetImpl[T, GL, K], y: RegionSet.RegionSetImpl[T, GL, K]) => x + y
  }
  def -(that: RegionSet[T, GL, K]): RegionSet[T, GL, K] = (this, that) match {
    case (y: Everything[_, _, _], _) => new Everything[T, GL, K] {}
    case (_, y: Everything[_, _, _]) => new Empty[T, GL, K] {}
    case (y: Empty[_, _, _], _) => new Empty[T, GL, K] {}
    case (x, y: Empty[_, _, _]) => x
    case (x: RegionSet.RegionSetImpl[T, GL, K], y: RegionSet.RegionSetImpl[T, GL, K]) => x - y
  }

  def maskTo[P <: HasGenomicLocation](sorted: Map[T, Vector[P]]): Map[T, Vector[P]]
}

trait Everything[T, GL <: GenomicLocationStub[T], K <: RegionStub[T, GL, K]] extends RegionSet[T, GL, K] {
  def chunkToMaxSize(max: Int): Iterable[List[K]] = throw new UnsupportedOperationException
  def contains(g: GL) = true
  def middlePoints = Vector()
  def maskTo[P <: HasGenomicLocation](sorted: Map[T, Vector[P]]): Map[T, Vector[P]] = sorted
  def intersects(r: K) = true
  def toRegions = Set[K]()
}

trait Empty[T, GL <: GenomicLocationStub[T], K <: RegionStub[T, GL, K]] extends RegionSet[T, GL, K] {
  def chunkToMaxSize(max: Int): Iterable[List[K]] = Nil
  def contains(g: GL) = false
  def middlePoints = Vector()
  def maskTo[P <: HasGenomicLocation](sorted: Map[T, Vector[P]]): Map[T, Vector[P]] = Map[T, Vector[P]]()
  def intersects(r: K) = false
  def toRegions = Set[K]()
}

object EmptyRegionSet extends Empty[String8, GenomicLocation, Region]
object EverythingRegionSet extends Everything[String8, GenomicLocation, Region]

object GenericRegionSet {
  def apply(i: Iterable[Region]) = RegionSet.apply[String8, GenomicLocation, Region](i)
}

object RegionSet {

  def fromGenomicLocations(i: Iterable[GenomicLocation]): GenericRegionSet =
    if (i.size == 0) EmptyRegionSet
    else this.apply(i.map(x => Region.fromGenomicLocation(x)))

  def apply[T, GL <: GenomicLocationStub[T], K <: RegionStub[T, GL, K]](i: Iterable[K])(implicit gb: GenomicLocationBuilder[T, GL], rb: RegionBuilder[T, GL, K], o: Ordering[T]): RegionSet[T, GL, K] = {
    val x: Map[T, IndexedSeq[K]] = {
      Region.collapse[T, GL, K](i, 0).groupBy(_.chromosomeAsT).map(x => x._1 -> x._2.toIndexedSeq.sortBy((x: K) => x.from)).map { x => { {}; x } }
    }

    new RegionSetImpl[T, GL, K](x)
  }

  def fromBedLines(lines: Iterator[String])(implicit gb: GenomicLocationBuilder[String8, GenomicLocation], rb: RegionBuilder[String8, GenomicLocation, Region]): RegionSet[String8, GenomicLocation, Region] = RegionSet[String8, GenomicLocation, Region](lines.map { line =>
    val splitted = mybiotools.fastSplitSetSeparator(line, Set('\t', ' '))
    val chr = new java.lang.String(splitted(0))
    val start = splitted(1).toInt
    val end = splitted(2).toInt
    val region = rb.build(StringStore(chr), start, end)
    region
  }.toList)

  @SerialVersionUID(1L)
  private class RegionSetImpl[T, GL <: GenomicLocationStub[T], K <: RegionStub[T, GL, K]](private val map: Map[T, IndexedSeq[K]])(implicit gb: GenomicLocationBuilder[T, GL], rb: RegionBuilder[T, GL, K], ord: Ordering[T]) extends RegionSet[T, GL, K] with Serializable {

    override def toString = "RegionSetImpl(" + map.values.map(_.toString).mkString(",") + ")"

    def toRegions = map.flatMap(_._2).toSet

    lazy val middlePoints: Vector[GL] = map.values.flatMap(chr => chr.map(r => gb.build(r.chromosomeAsT, (r.from + r.to) / 2))).toVector

    def +(that: RegionSetImpl[T, GL, K]) = {
      val mergedMaps = mybiotools.addMaps(this.map, that.map) { (x: IndexedSeq[K], y: IndexedSeq[K]) =>
        Region.collapseSameChromosome[T, GL, K](merge(x, y, Vector()), 0)
      }
      new RegionSetImpl[T, GL, K](mergedMaps)
    }

    def -(that: RegionSetImpl[T, GL, K]) = {
      val merged = this.map.map { (p: (T, IndexedSeq[K])) =>
        val key = p._1
        val value = p._2
        that.map.get(key) match {
          case None => key -> value
          case Some(v2) => key -> Region.subtractSameChromosome[T, GL, K](value, v2)
        }
      }
      new RegionSetImpl[T, GL, K](merged)
    }

    def chunkToMaxSize(max: Int): Iterable[List[K]] = map.toSeq.sortBy(_._1).flatMap {
      case (chr, regions) =>
        if (max < 1) throw new IllegalArgumentException

        def fold(elems: List[K], acc: (List[List[K]], Int)): List[List[K]] = elems match {
          case Nil => acc._1
          case x :: xs =>
            if (x.size + acc._2 <= max || (acc._2 == 0 && x.size > max)) fold(xs, ((x :: acc._1.head) :: acc._1.tail, acc._2 + x.size))
            else fold(x :: xs, (Nil :: acc._1, 0))
        }
        fold(regions.toList.flatMap(_.chunkToMaxSize(max)), (List(Nil), 0)).reverse

    }

    // chromosome must match!
    private def binarySearch(a: IndexedSeq[K], v: GL): Option[Int] = {
      def recurse(low: Int, high: Int): Option[Int] = (low + high) / 2 match {
        case _ if high < low => None
        case mid if a(mid).from > v.basePairPosition0based => recurse(low, mid - 1)
        case mid if a(mid).to <= v.basePairPosition0based => recurse(mid + 1, high)
        case mid => Some(mid)
      }
      recurse(0, a.size - 1)
    }

    // chromosome must match!
    private def binarySearch(a: IndexedSeq[K], v: K): Option[Int] = {
      def recurse(low: Int, high: Int): Option[Int] = (low + high) / 2 match {
        case _ if high < low => None
        case mid if a(mid).from >= v.to => recurse(low, mid - 1)
        case mid if a(mid).to <= v.from => recurse(mid + 1, high)
        case mid => Some(mid)
      }
      recurse(0, a.size - 1)
    }

    /** Merging sorted lists (from mergesort) */
    private def merge(xs: IndexedSeq[K], ys: IndexedSeq[K], acc: Vector[K]): Vector[K] =
      (xs, ys) match {
        case (x, _) if x.size == 0 => acc ++ ys
        case (_, y) if y.size == 0 => acc ++ xs
        case (x +: xs1, y +: ys1) =>
          if (x.from <= y.from) merge(xs1, ys, acc :+ x)
          else merge(xs, ys1, acc :+ y)
      }

    def contains(g: GL): Boolean = map.get(g.chromosomeAsT) match {
      case None => false
      case Some(regions) => binarySearch(regions, g).isDefined
    }

    def intersects(r: K): Boolean = r.size > 0 && (map.get(r.chromosomeAsT) match {
      case None => false
      case Some(regions) => binarySearch(regions, r).isDefined
    })

    def maskTo[P <: HasGenomicLocation](sorted: Map[T, Vector[P]]): Map[T, Vector[P]] = {
      map.map {
        case (chr, regions) =>
          sorted.get(chr).map { (query: Vector[P]) =>
            chr -> regions.map { region =>
              val startPoint = jumpDirectionalSearch(
                query,
                (x: P) => x.genomicLocation.basePairPosition0based >= region.from,
                forward = true
              ).map(_._1)

              val endPoint = jumpDirectionalSearch(
                query,
                (x: P) => x.genomicLocation.basePairPosition0based < region.to,
                forward = false
              ).map(_._1)

              if (startPoint.isDefined && endPoint.isDefined)
                query.slice(startPoint.get, endPoint.get + 1)
              else Vector()

            }.flatten.toVector

          }.getOrElse((chr, Vector[P]()))
      }
    }

  }
}