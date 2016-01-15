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

import java.io.Writer
import scala.util.parsing.combinator._

/** Factories for `Node`. */
object Node {
  def apply(other: Node): Node = {
    new Node(other.name, other.distance, other.support, None, other.pattern, Set())
  }

  // This is for testing
  def withPattern(name: String, pattern: String): Node =
    new Node(Some(name), None, None, None, Some(pattern), Set())

  def apply(name: String, distance: Double, support: Double): Node = {
    new Node(name = Some(name), distance = Some(distance), support = Some(support), parent = None, children = Set(), pattern = None)
  }

  def apply(name: String, distance: Double): Node = new Node(Some(name), Some(distance), None, None)

  def apply(name: String, distance: Double, pattern: String) =
    new Node(
      name = Some(name),
      distance = Some(distance),
      support = None,
      pattern = Some(pattern),
      children = Set(),
      parent = None
    )

  def apply(distance: Double, support: Option[Double], children: Set[Node]): Node = new Node(None, Some(distance), support, None, pattern = None, children)
  def apply(children: Set[Node]) = new Node(None, None, None, None, pattern = None, children)

}

/** Parse Newick formatted tree files. */
object Tree {
  import java.io.FileReader

  private class NewickParser(
    minSupport: Double,
    maxSupport: Double
  )(patternFunction: String => String)
      extends JavaTokenParsers {
    // (nev:tavolsag)supp:tavolsag
    // node -> nev:tavolsag|(nodes)sup:tavolsag
    // root -> (nodes);

    def normalize(sup: Double): Double = (sup - minSupport) / (maxSupport - minSupport)
    def newickNameRegexp = "[^;:,()]+".r

    def nodes: Parser[Set[Node]] = rep1sep(node, ",") ^^ { case ns => Set() ++ ns }
    def leaf: Parser[Node] =
      newickNameRegexp ~ opt(":" ~ floatingPointNumber) ^^ {
        case name ~ Some(":" ~ dist) => Node(name, dist.toDouble, patternFunction(name))
        case name ~ None => Node.withPattern(name, patternFunction(name))
      }

    def inner: Parser[Node] = "(" ~ nodes ~ ")" ~ (floatingPointNumber.?) ~ opt(":" ~ floatingPointNumber) ^^ {
      case "(" ~ nodes ~ ")" ~ support ~ Some(":" ~ distance) =>
        Node(
          distance.toDouble,
          support.map(x => normalize(x.toDouble)),
          nodes
        )
      case "(" ~ nodes ~ ")" ~ support ~ None => new Node(
        name = None,
        support = support.map(_.toDouble),
        distance = None,
        children = nodes,
        parent = None,
        pattern = None
      )
    }

    def node: Parser[Node] = (leaf | inner)
    def root: Parser[Node] = "(" ~> nodes <~ ")" ~ (floatingPointNumber.?) ~ opt(":" ~ floatingPointNumber) ~ ";" ^^ { case ns => Node(ns) }

  }

  def readNewick(
    input: String,
    minsupport: Double = 0.0,
    maxsupport: Double = 1.0
  )(implicit patternFunction: String => String = (x: String) => x): Node = {
    val np = new NewickParser(minsupport, maxsupport)(patternFunction)
    val res = np.parseAll(np.root, input)
    if (res.successful) res.get else throw new RuntimeException("Parse failed. Badly formatted input. " + res.toString)
  }

}

/**
 * Node of a linked graph, representing phylogenetic trees.
 *
 * Mutable.
 *
 */
final class Node(
    val name: Option[String],
    val distance: Option[Double],
    val support: Option[Double],
    private var parent: Option[Node] = None,
    val pattern: Option[String] = None,
    private var children: Set[Node] = Set()
) {
  require(!leaf || !name.isEmpty) // leaf => !name.isEmpty
  require(support match {
    case None => true
    case Some(x) => x <= 1 && x >= 0
  })
  require(distance match {
    case None => true
    case Some(x) => x >= 0
  })

  for (child <- children) child.setParent(this)

  override def toString = {
    val n = if (!name.isEmpty) name.get.toString else ""
    n + getClass + hashCode.toString
  }

  def setParent(n: Node) {
    if (n.ancestorsAndMe.contains(this)) throw new RuntimeException("Cycle in the graph. ")
    val oldparent = parent
    parent = Some(n)
    n.addChild(this)
    if (oldparent != None) {
      oldparent.get.removeChild(this)
      n.setParent(oldparent.get)
    }
  }

  def clearParent {
    val oldparent = parent
    parent = None
    if (oldparent != None) oldparent.get.removeChild(this)
  }

  def getParent = if (root) throw new RuntimeException("Root's parent") else parent.get

  def getChildren = children

  def root = (parent.isEmpty)

  def leaf = children.isEmpty

  def addChild(ch: Node) {
    if (ch.descendentsAndMe.contains(this)) throw new RuntimeException("Cycle in graph")
    children = children + ch
    ch.parent = Some(this)
  }

  def removeChild(ch: Node) {
    children = children.filterNot(ch == _)
  }

  def removeAllChildren {
    children = Set()
  }

  def leaves: Set[Node] = {
    if (children.isEmpty) {
      Set(this)
    } else {
      val list = for (ch <- children) yield {
        ch.leaves
      }
      list.flatten
    }
  }

  def outputNewick(ios: Writer) {
    if (children.isEmpty) {
      ios.write(name.get)
      if (!distance.isEmpty) ios.write(':' + distance.get.toString)
      if (parent.isEmpty) ios.write(';')
    } else {
      ios.write('(')
      var counter = 0
      for (child <- children) {
        child.outputNewick(ios)
        if (counter != children.size - 1) ios.write(',')
        counter += 1
      }
      ios.write(')')
      if (!support.isEmpty) ios.write(support.get.toString)
      if (parent.isEmpty) {
        if (!distance.isEmpty) {
          ios.write(':' + distance.get.toString)
        }
        ios.write(';')
      } else {
        if (!distance.isEmpty) {
          ios.write(':' + distance.get.toString)
        }
      }
    }
  }

  def descendentsAndMe: Set[Node] = {
    if (children.isEmpty) {
      Set(this)
    } else {
      val list = for (ch <- children) yield {
        ch.descendentsAndMe
      }
      list.flatten + this
    }
  }

  def ancestorsAndMe: List[Node] = {
    if (parent.isEmpty) {
      List(this)
    } else {
      val list = parent.get.ancestorsAndMe
      this :: list
    }
  }

  override def clone: Node = {
    val n = new Node(name, distance, support, None, pattern, Set())
    n.children = for (child <- children) yield {
      val x = child.clone
      x.parent = Some(n)
      x
    }
    n
  }

  def getSiblings: Set[Node] = {
    if (parent.isEmpty) Set() else parent.get.children.filterNot(_ == this)
  }

  def lowestCommonAncestor(node1: Node): Node = {
    val list1 = this.ancestorsAndMe.reverse
    val list2 = node1.ancestorsAndMe.reverse
    (list1 zip list2).takeWhile(x => x._1 == x._2).last._1
  }

  // return (lastCommonAncestor,List(path)), path contains lca
  def pathTo(other: Node): (Node, List[Node]) = {
    if (other == this) (this, List[Node]()) else {
      val list1 = this.ancestorsAndMe.reverse
      val list2 = other.ancestorsAndMe.reverse
      val commonList = (list1 zip list2).takeWhile(x => x._1 == x._2).map(_._1)
      val lca = commonList.last
      (lca, (list1 filterNot (commonList contains)).reverse ::: (lca :: (list2 filterNot (commonList contains))))
    }
  }

  def getDistanceTo(other: Node): Double = {
    val x = distance match {
      case None => throw new RuntimeException("No distance set.")
      case Some(x) => {
        val (lca, path) = pathTo(other)
        var sum = 0.0
        path.foreach { n =>
          if (lca != n) {
            sum += n.distance.get
          }
        }
        sum.asInstanceOf[Double]
      }
    }
    x
  }

  def distanceMatrixFromThisNode: Map[String, Map[String, Double]] = {
    (for (leaf1 <- leaves) yield {
      (leaf1.name.get -> (for (leaf2 <- leaves) yield (leaf2.name.get -> leaf1.getDistanceTo(leaf2))).toMap)
    }).toMap
  }

}
