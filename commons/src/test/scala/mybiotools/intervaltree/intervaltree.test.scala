
package mybiotools.intervaltree

import org.scalatest.FunSpec
import org.scalatest.PrivateMethodTester
import org.scalatest.Matchers
import org.scalatest.prop.Checkers
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.{ Arbitrary, Gen }

class IntervalTreeSpec extends FunSpec with Matchers with PrivateMethodTester with Checkers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(minSuccessful = 100000, maxDiscarded = 1000000)

  case class TestInterval(from: Int, to: Int) extends Interval

  implicit def arbInterval[TestInterval] =
    Arbitrary {
      for {
        from <- Gen.choose(0, 500)
        to <- Gen.choose(from, 500)
      } yield {
        TestInterval(from, to)
      }
    }

  describe("tree") {
    it("contains went in") {
      check { (a: List[Int]) =>
        Tree.inOrder(Tree.toTree(a.sorted)()) == (a.sorted)
      }
    }
    it("size") {
      check { (a: List[Int]) =>
        Tree.length((Tree.toTree(a.sorted)())) == a.size
      }
    }
    it("balanced") {
      check { (a: List[Int]) =>
        Tree.isBalanced(Tree.toTree(a.sorted)())
      }
    }
    it("bst") {
      check { (a: List[Int]) =>
        val x = Tree.bstHolds(Tree.toTree(a.sorted)())
        if (!x) { print(Tree.toTree(a.sorted)()); println(a) }
        x
      }
    }
  }
  describe("intervaltree") {
    it("find") {
      check { (a: List[TestInterval], b: TestInterval) =>
        val sorted = a.sortBy(_.from)
        val tree = IntervalTree.makeTree(sorted)
        val result = IntervalTree.lookup(b, tree)
        val resultByFilter = a.filter(_.intersects(b))
        val x = result.toSet == resultByFilter.toSet
        if (!x) {
          println(a)
          println(b)
          println(result)
          println(resultByFilter)
        }
        x
      }
    }
  }

}