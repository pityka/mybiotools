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
import mybiotools.stringstore._

object RepresentativeCategories {

  def clearOverlappingPairs[K, E](in: Map[K, Set[E]], threshold: Double): Map[K, Set[E]] =
    {
      val removal = in.toSeq.map(_._1).combinations(2).flatMap {
        l =>
          val highoverlap = symmetricCovarageFirstOverSecond(in(l(0)), in(l(1))) > threshold
          if (highoverlap) (if (in(l(0)).size < in(l(1)).size) l(0) :: Nil else l(1) :: Nil) else Nil
      }.toSet
      in.filterNot(x => removal.contains(x._1))
    }

  def asymmetricCovarageFirstOverSecond[E](s1: Set[E], s2: Set[E]) = (s1 & s2).size.toDouble / s2.size

  def symmetricCovarageFirstOverSecond[E](s1: Set[E], s2: Set[E]) = (s1 & s2).size.toDouble / (s1 ++ s2).size

  def totalCoverages[E, K](in: Map[K, Set[E]]): Map[K, Double] = in.map {
    case (gsk, set) =>
      gsk -> in.filter(_._1 != gsk).map(x => asymmetricCovarageFirstOverSecond(set, x._2)).sum
  }

  def assign[E, K](pathway: Set[E], tops: Map[K, Set[E]], misc: K): K = {
    val (chosen, coverage) = tops.map {
      case (topgsk, topset) =>
        topgsk -> asymmetricCovarageFirstOverSecond(topset, pathway)
    }.toSeq.sortBy(x => -1 * x._2).head
    if (coverage >= 0.95) chosen else misc

  }

  def createRepresentativeCategories[E, K](in: Map[K, Set[E]], misc: K, seed: Int = 9): Map[K, K] = {
    val tc: Seq[K] = totalCoverages(in).toSeq.sortBy(x => -1 * x._2).map(_._1)
    val tops: Map[K, Set[E]] = tc.take(seed).map(x => x -> in(x)).toMap + (misc -> Set[E]())

    tc.map { k =>
      k -> assign(in(k), tops, misc)
    } toMap

  }

  def createDisjointRepresentativeCategories[E, K](in: Map[K, Set[E]], misc: K, seed: Int): Map[K, Set[E]] = {

    val tc = totalCoverages(in).toSeq.sortBy(x => -1 * x._2).map(_._1)

    val tops: Map[K, Set[E]] = tc.take(1).map(x => x -> in(x)).toMap + (misc -> Set[E]())

    val availablegenes = in.values.flatten.toSet &~ tops.values.flatten.toSet

    // println(availablegenes)
    // println(tc)
    // println(tops)

    val (updatedTops, _) = tc.drop(1).foldLeft((tops, availablegenes)) {
      case ((oldTops, remaininggenes), gsk) =>

        val pathway: Set[E] = in(gsk) & remaininggenes

        val seedset = if (oldTops.size >= seed) oldTops else (in.map(x => x._1 -> (x._2 & remaininggenes))) ++ oldTops

        val chosen: K = assign(pathway, seedset, misc)
        val newchosentop: Set[E] = (seedset(chosen) ++ (pathway))
        // println(chosen)
        // println(pathway)
        val newremaininggenes = remaininggenes &~ newchosentop

        // println((oldTops.updated(chosen, newchosentop)) -> newremaininggenes)

        (oldTops.updated(chosen, newchosentop)) -> newremaininggenes
    }

    {
      val allgenes = in.values.flatten.toSet
      val allgeneswithduplications = updatedTops.toSeq.flatMap(_._2.toSeq)
      assert(allgenes.size == allgeneswithduplications.size, s"$allgenes vs $allgeneswithduplications")

    }

    updatedTops

  }

}