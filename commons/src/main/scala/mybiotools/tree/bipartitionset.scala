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

object Bipartition {
  /** Copy. */
  def apply(other: Bipartition): Bipartition = {
    new Bipartition(other.left, other.right, other.support, other.distance)
  }
}

/**
 * Represents an inner node in a phylogenetic tree.
 *
 * An inner node in a phylogenetic tree dissects the set of leafs into 2 disjoint subsets (partitions).
 * Immutable.
 *
 * @param _left left set of leaf nodes
 * @param _right right set of leaf nodes
 * @param support Bayesian/bootstrap support of this inner bipartition
 * @param distance Evolutionary distance of this node from the parent node (in a tree representation)
 */
final class Bipartition(
    private val _left: Set[Node],
    private val _right: Set[Node],
    val support: Option[Double],
    val distance: Option[Double]
) {
  require((left & right).size == 0)
  require(left.forall(_.leaf) && right.forall(_.leaf))
  require(support match {
    case None => true
    case Some(x) => x <= 1 && x >= 0
  })
  require(distance match {
    case None => true
    case Some(x) => x >= 0
  })

  override def toString = _left.toString + "," + _right.toString + support.toString + distance.toString

  override def equals(other: Any): Boolean = other match {
    case x: Bipartition => (((left == x.left && right == x.right) || (right == x.left && left == x.right)) && support == x.support && distance == x.distance)
    case _ => false
  }

  override def hashCode: Int = 41 * (41 * (41 * (41 + smaller.hashCode) + larger.hashCode) + support.hashCode) + distance.hashCode

  def left = _left
  def right = _right

  /** Enumerate leaves. */
  def foreach[U](f: Node => U) {
    left.foreach[U](f)
    right.foreach[U](f)
  }

  /** Returns smaller partition (left or right) */
  def smaller: Set[Node] = if (left.size < right.size) left else right

  def larger: Set[Node] = if (left.size < right.size) right else left

  /** Returns a new object of this class with `nodes` filtered out. */
  def removeNodes(nodes: Set[Node]): Bipartition = {
    val l = _left filterNot (nodes.contains(_))
    val r = _right.filterNot(nodes.contains(_))
    assert((left & right).size == 0)
    new Bipartition(l, r, support, distance)
  }

  def size = left.size + right.size

  def mergeToSet: Set[Node] = left ++ right

  def flip = new Bipartition(right, left, support, distance)

}

/** Factories for BipartitionSet. */
object BipartitionSet {
  def apply(other: BipartitionSet): BipartitionSet = new BipartitionSet(other.bipartitions)
  def apply(set: Set[Bipartition]): BipartitionSet = new BipartitionSet(set)
  def apply(tree: Node): BipartitionSet = {
    var set = Set[Bipartition]()
    val leaves = tree.leaves
    for (node <- tree.descendentsAndMe) {
      val tuple: Tuple2[Option[Double], Option[Double]] = if (node.root) (Some(0), Some(0)) else if (node.leaf) (Some(1.0), node.distance) else (node.support, node.distance)
      set = set + new Bipartition(node.leaves, leaves -- node.leaves, tuple._1, tuple._2)
    }
    new BipartitionSet(set)
  }
}

/** Set of bipartitions. Represents a phylogenetic tree. */
final class BipartitionSet(
    private val _set: Set[Bipartition]
) {
  def bipartitions: Set[Bipartition] = _set
  override def toString = _set.toString

  override def equals(other: Any): Boolean = other match {
    case x: BipartitionSet => x.bipartitions == _set
    case _ => false
  }

  override def hashCode: Int = 41 * (41 + _set.hashCode)

  /** Enumerates all Bipartitions (inner nodes) */
  def foreach[U](f: Bipartition => U): Unit = _set.foreach(f)

  /** Size of the set. Number of inner nodes. */
  def size: Int = _set.size

  /** Enumerates all leaf nodes. Some nodes may be visited multiple times! */
  def foreachNode[U](f: Node => U) {
    for (bp <- bipartitions) bp.foreach(f)
  }

  /** Returns a new object of this class, `nodes` filtered out */
  def removeNodes(nodes: Set[Node]): BipartitionSet = {
    val s = (for (bip <- _set) yield bip.removeNodes(nodes)).toSet
    BipartitionSet(s)
  }

  /** Returns a new object of this class, containing only those inner nodes where support is greater then `thr` */
  def aboveThreshold(thr: Double): BipartitionSet = {
    val set = (for (bp <- _set; if bp.support.isDefined && bp.support.get >= thr) yield bp).toSet
    BipartitionSet(set)
  }

}