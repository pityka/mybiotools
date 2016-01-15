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

import org.scalatest.FunSuite
import scala.io.Source
import mybiotools.tree._

class TreeTestSuite extends FunSuite {
  val jancsi = Node("jancsi", 0.3, 0.0)
  val julcsi = Node("juli", 10, 20 / 100)
  val banya = Node(name = "banya", distance = 1, support = 0)
  val kiralyfi = Node(name = "kiralyfi", distance = 1, support = 1 / 2)
  var kiralyficopy = Node(kiralyfi)
  test("Node.support in (0,1)") {
    intercept[IllegalArgumentException] {
      Node(name = "kiralyfi", distance = 0, support = 100)
    }
  }

  test("addChild,setParent,clearParent") {
    kiralyfi.addChild(jancsi)
    assertResult(kiralyfi) { jancsi.getParent }

    julcsi.setParent(kiralyfi)
    assertResult(kiralyfi) { julcsi.getParent }
    assertResult(Set(julcsi, jancsi)) { kiralyfi.getChildren }

    julcsi.clearParent
    assertResult(Set(jancsi)) { kiralyfi.getChildren }
    intercept[RuntimeException] { julcsi.getParent }
    julcsi.clearParent
    julcsi.setParent(kiralyfi)
  }

  test("getParent exception if root") {
    intercept[RuntimeException] { kiralyfi.getParent }
  }

  test("getSiblings") {
    assertResult(Set(julcsi)) { jancsi.getSiblings }
  }

  test("Tree should not contain cycles.") {
    intercept[RuntimeException] {
      assert(kiralyfi.getChildren.contains(julcsi))
      julcsi.addChild(kiralyfi)
    }
  }

  test("Insert") {
    assert(julcsi.getParent == kiralyfi)
    assert(jancsi.getParent == kiralyfi)
    assert(julcsi.leaf)
    assert(jancsi.leaf)
    julcsi.setParent(banya)

    assertResult(banya) { julcsi.getParent }
    assertResult(kiralyfi) { banya.getParent }
    assertResult(Set(julcsi)) { banya.getChildren }
    assertResult(Set(banya)) { jancsi.getSiblings }
    assertResult(true) { kiralyfi.root }
    assertResult(Set(julcsi, banya, jancsi)) { kiralyfi.descendentsAndMe.filterNot(kiralyfi == _) }
    assertResult(List(jancsi, kiralyfi)) { jancsi.ancestorsAndMe }
  }

  test("Lowest Common Ancestor") {
    assertResult(kiralyfi) { jancsi.lowestCommonAncestor(kiralyfi) }
    assertResult(kiralyfi) { jancsi.lowestCommonAncestor(banya) }
    assertResult(kiralyfi) { jancsi.lowestCommonAncestor(julcsi) }
    assertResult(kiralyfi) { julcsi.lowestCommonAncestor(jancsi) }
    assertResult(kiralyfi) { kiralyfi.lowestCommonAncestor(kiralyfi) }
    assertResult(banya) { banya.lowestCommonAncestor(julcsi) }
    assertResult(banya) { banya.lowestCommonAncestor(banya) }
    assertResult(julcsi) { julcsi.lowestCommonAncestor(julcsi) }
  }

  test("pathTo") {
    assertResult((kiralyfi, List(julcsi, banya, kiralyfi, jancsi).reverse)) { jancsi.pathTo(julcsi) }
    assertResult((kiralyfi, List(julcsi, banya, kiralyfi, jancsi))) { julcsi.pathTo(jancsi) }
    assertResult((kiralyfi, List(julcsi, banya, kiralyfi).reverse)) { kiralyfi.pathTo(julcsi) }
  }

  test("getDistanceTo") {
    assertResult(11.3) { julcsi.getDistanceTo(jancsi) }
    assertResult(11.3) { jancsi.getDistanceTo(julcsi) }
    assertResult(10)(julcsi.getDistanceTo(banya))
  }

  test("Clone") {
    val copynode = kiralyfi.clone
    assert(copynode != kiralyfi)
    assert(copynode.descendentsAndMe != kiralyfi.descendentsAndMe)
    assert(!(copynode.descendentsAndMe eq kiralyfi.descendentsAndMe))
  }

  test("Distance matrix") {
    val mat = kiralyfi.distanceMatrixFromThisNode
    val mat2 = Map(
      "juli" -> Map("juli" -> 0, "jancsi" -> 11.3),
      "jancsi" -> Map("juli" -> 11.3, "jancsi" -> 0)
    )
    assertResult(mat2) { mat }
  }

  test("Copy factory method and cycle exception.") {
    kiralyficopy = Node(kiralyfi)
    assert(kiralyficopy != kiralyfi)
    assert(!(kiralyficopy eq kiralyfi))
    assertResult(Set()) { kiralyficopy.getChildren }
    assertResult(true) { kiralyficopy.root }
    kiralyficopy.removeChild(jancsi)
    assertResult(Set(jancsi, banya)) { kiralyfi.getChildren }

    kiralyficopy.removeAllChildren
    kiralyficopy.setParent(julcsi)
    assertResult(true) { kiralyfi.root }
    assertResult(false) { kiralyficopy.root }
    assertResult(true) { kiralyficopy.leaf }
    assertResult(julcsi) { kiralyficopy.getParent }

    intercept[RuntimeException] { kiralyficopy.addChild(kiralyfi) }
  }

  test("setParent another") {
    intercept[RuntimeException] { banya.setParent(kiralyficopy) }
  }

  test("Output Newick format") {
    import java.io.StringWriter
    val input = """((A:2.0,B:3.0,C:1.0)0.5:1.0,G:3.0,(F:1.0,D:2.0)1.0:2.0,H H:1.0);"""
    val tree = Tree.readNewick(input)
    val sw = new StringWriter
    tree.outputNewick(sw)
    sw.close
    assertResult(input) { sw.toString }
  }

}

