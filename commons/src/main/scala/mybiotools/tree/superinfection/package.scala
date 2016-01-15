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

package mybiotools.tree

package object superinfection {

  private[superinfection] def isMixed(nodes: Set[Node]) = nodes.map(_.pattern).filter(_.isDefined).map(_.get).size > 1

  private[superinfection] def getMixedSets(b: Bipartition): List[Set[Node]] = {
    val leftIsMixed = isMixed(b.left)
    val rightIsMixed = isMixed(b.right)

    (leftIsMixed, rightIsMixed) match {
      case (true, false) => List(b.left)
      case (false, true) => List(b.right)
      case (true, true) => List(b.left, b.right)
      case (false, false) => { List() }
    }

  }

  private[superinfection] def setsAreCompatible(s1: Set[Node], s2: Set[Node]) =
    (s1 & s2).size == 0 && s1.size > 0 && s2.size > 0

  private[superinfection] def provesSuperinfection(b1: Bipartition, b2: Bipartition): Option[(Set[Node], Set[Node])] = {

    assert(b1.mergeToSet == b2.mergeToSet)
    assert(b1 != b2)
    assert(b1 != b2.flip)
    assert(b1.flip != b2)

    val b1MixedSets = getMixedSets(b1)
    val b2MixedSets = getMixedSets(b2)

    val iterator: Iterator[(Set[Node], Set[Node])] =
      b1MixedSets.iterator.map { set1 =>
        b2MixedSets.iterator.map { set2 =>
          (set1, set2)
        }
      }.flatten

    iterator.find(x => setsAreCompatible(x._1, x._2))

  }

  def findProof(tree: BipartitionSet): Option[(Set[Node], Set[Node])] = {

    val pairs: Iterator[Set[Bipartition]] = tree.bipartitions.subsets.dropWhile(_.size <= 1).takeWhile(_.size == 2)

    val proof: Option[(Set[Node], Set[Node])] = pairs.map { pair =>
      val list = pair.toList
      provesSuperinfection(list.head, list.last)
    }.find(_.isDefined).map(_.get)
    proof
  }

  def decideSuperinfection(tree: BipartitionSet) = findProof(tree).isDefined

}