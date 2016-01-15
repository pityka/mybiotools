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

package mybiotools.intervaltree

sealed trait Tree[+T]
case object EmptyTree extends Tree[Nothing]
case class NonEmptyTree[T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

object Tree {

  def inOrder[T](t: Tree[T]) = {
    def inner(tree: Tree[T]): List[T] = tree match {
      case EmptyTree => Nil
      case NonEmptyTree(v, left, right) => inner(left) ++ (v :: inner(right))
    }
    inner(t)
  }

  def depth(tree: Tree[_]): Int = tree match {
    case EmptyTree => 0
    case NonEmptyTree(_, left, right) => 1 + (depth(left) max depth(right))
  }

  def bstHolds[T](tree: Tree[T])(implicit o: Ordering[T]): Boolean = tree match {
    case EmptyTree => true
    case NonEmptyTree(v, left: NonEmptyTree[T], right: NonEmptyTree[T]) => o.lteq(v, right.value) && o.gteq(v, left.value) && bstHolds(left) && bstHolds(right)
    case NonEmptyTree(v, EmptyTree, right: NonEmptyTree[T]) => o.lteq(v, right.value) && bstHolds(right)
    case NonEmptyTree(v, left: NonEmptyTree[T], EmptyTree) => o.gteq(v, left.value) && bstHolds(left)
    case NonEmptyTree(v, EmptyTree, EmptyTree) => true
  }

  def length(t: Tree[_]): Int = t match {
    case EmptyTree => 0
    case NonEmptyTree(_, left, right) => 1 + length(left) + length(right)
  }

  def isBalanced(t: Tree[_]): Boolean = t match {
    case EmptyTree => true
    case NonEmptyTree(_, left, right) => math.abs(depth(left) - depth(right)) <= 1 && isBalanced(left) && isBalanced(right)
  }

  def toTree[T](ls: List[T])(auxFun: (T, Option[T], Option[T]) => T = (x: T, y: Option[T], z: Option[T]) => x)(implicit o: Ordering[T]): Tree[T] = {

    def assertOrder(s: List[T]) {
      s match {
        case (x :: y :: xs) => (x :: y :: xs).sliding(2).foreach { x => assert(o.lteq(x.head, x(1))) }
        case _ => {}
      }
    }

    assertOrder(ls)

    def toTreeAux(ls: List[T], n: Int): (List[T], Tree[T]) = {
      if (n <= 0)
        (ls, EmptyTree)
      else {
        val (lls, lt) = toTreeAux(ls, n / 2) // construct left sub-tree
        val x :: xs = lls // extract root node
        val (xr, rt) = toTreeAux(xs, n - n / 2 - 1) // construct right sub-tree 

        val leftAux: Option[T] = lt match {
          case EmptyTree => None
          case NonEmptyTree(v, _, _) => Some(v)
        }
        val rightAux: Option[T] = rt match {
          case EmptyTree => None
          case NonEmptyTree(v, _, _) => Some(v)
        }

        (xr, NonEmptyTree(auxFun(x, leftAux, rightAux), lt, rt)) // construct tree
      }
    }

    val (ls_1, tree) = toTreeAux(ls, ls.length)
    tree
  }
}