package hivheritability
import jebl.evolution.trees.{ RootedTree, RootedTreeUtils, Tree }
import jebl.evolution.graphs.{ Node, Edge }
import scala.collection.JavaConversions._

case class Bipartition(left: Set[Node], right: Set[Node], edge: Option[Edge]) {
  def resolveNames(tree: Tree) = (left.map(n => tree.getTaxon(n).getName), right.map(n => tree.getTaxon(n).getName))
}
case class BipartitionSet(bps: Set[Bipartition], tree: Tree) {
  def largestMonophyleticSplit(tipNames: Set[String]): Option[Bipartition] = {
    val tips = tree.getTaxa.filter(x => tipNames.contains(x.getName)).map(t => tree.getNode(t))
    assert(tips.forall(n => tree.isExternal(n)))
    bps.filter(bp => bp.left == tips || bp.right == tips).headOption

  }
}

object BipartitionSet {

  def apply(tree: RootedTree): BipartitionSet = {
    val leaves = tree.getExternalNodes.toSet
    BipartitionSet((for (node <- tree.getNodes) yield {
      val leavesFromNode: Set[Node] = getLeavesRecursive(tree, node)
      val edge = if (tree.isRoot(node)) None else Some(tree.getEdge(node, tree.getParent(node)))
      Bipartition(leavesFromNode, leaves &~ leavesFromNode, edge)
    }).toSet, tree)
  }
}
