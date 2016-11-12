package hivheritability

import jebl.evolution.trees.{ RootedTree, RootedTreeUtils, Tree, RootedSubtree, Utils, SimpleRootedTree }
import mybiotools.gwascommons.genotypedata.GRM
import org.scalatest.FunSpec
import org.scalatest.Matchers
import collection.JavaConversions._
import collection.immutable.HashMap

class TreeManipulationsSpec extends FunSpec with Matchers {

  describe("small tree") {
    val newickstring = "(A:0.1,B:0.2,(C:0.3,(D:0.4,E:0.5):0.4):0.5);"
    val outgroup = Set("E", "D")
    val trees = readTreesFromString(newickstring)
    it("trees in file") {
      trees.size should equal(1)
    }
    it("root to outgroup") {
      val rooted = getRootedSubtree(trees.head, outgroup, Set())
      rooted.isDefined should be(true)
      (Utils.toNewick(rooted.get)) should equal("(C:0.29999999999999993,(A:0.09999999999999998,B:0.19999999999999996):0.5);")
    }
    it("get avg tree height") {
      val rooted = getRootedSubtree(trees.head, outgroup, Set()).get
      val avgbr = getAverageTreeHeight(rooted)
      avgbr should equal(0.5333333333333333)
    }
    it("get max tree height") {
      val rooted = getRootedSubtree(trees.head, outgroup, Set()).get
      val avgbr = getMaxTreeHeight(rooted)
      avgbr should equal(0.7)
    }
    it("read covariance") {
      val rooted = getRootedSubtree(trees.head, outgroup, Set()).get
      val covs = readCovarianceFromTree(rooted).toList
      val expected = List(
        ("A", "A", 0.6),
        ("A", "B", 0.5),
        ("A", "C", 0.0),
        ("B", "A", 0.5),
        ("B", "B", 0.7),
        ("B", "C", 0.0),
        ("C", "A", 0.0),
        ("C", "B", 0.0),
        ("C", "C", 0.29999999999999993)
      ).sorted
      covs.sorted should equal(expected)
    }
  }

  describe("large tree") {
    val string = mybiotools.openSource(getClass.getResource("/").getPath + "/RAxML_bestTree.treeML.tree")(_.mkString)
    val trees = readTreesFromString(string)
    val outgroup = Set(
      "Ref.A1.AU.03.PS1044_Day0.DQ676872",
      "Ref.A1.RW.92.92RW008.AB253421",
      "Ref.A1.UG.92.92UG037.AB253429",
      "Ref.A2.CD.97.97CDKTB48.AF286238",
      "Ref.A2.CM.01.01CM_1445MV.GU201516",
      "Ref.A2.CY.94.94CY017_41.AF286237"
    )
    val rooted = getRootedSubtree(trees.head, outgroup, Set()).get
    val covs = readCovarianceFromTree(rooted).toList
    val frame = iterToFrame(covs.iterator)

    it("trees in file") {
      trees.size should equal(1)
    }
    it("read covariance") {
      (covs.size) should equal(1125721)

      frame("C100450", "C100450").raw(0, 0) should equal(0.2558889730134579)
      frame("C100450", "C100576").raw(0, 0) should equal(0.12049879059361918)
      frame("C100576", "C100450").raw(0, 0) should equal(0.12049879059361918)
      frame("C100576", "C531839").raw(0, 0) should equal(0.0)
      frame.raw(frame.rowIx("C100576").head, frame.rowIx("C100450").head) should equal(0.12049879059361918)
    }
    ignore("write covariacne") {
      val grmwriter = new java.io.StringWriter
      val idwriter = new java.io.StringWriter
      GRM.write(frame, grmwriter, idwriter, (x: String) => x + " 1")
      val grm = grmwriter.toString
      val grmid = idwriter.toString
      println(grm.take(100))
      println(grmid.take(10))
      println(frame.raw(0, 0))

    }
  }
}
