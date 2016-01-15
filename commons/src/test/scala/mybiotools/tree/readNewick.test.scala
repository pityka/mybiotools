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

import org.scalatest.FunSuite
import scala.io.Source
import mybiotools.tree._

class TreeParserTestSuite extends FunSuite {
  test("Simple tree") {
    val tree = Tree.readNewick("((A:2,B:3,C:1)0.5:1,(F:1,D:2)1:2);")
    assertResult(2) { tree.getChildren.size }
    assertResult(8) { tree.descendentsAndMe.size }
    assertResult(5) { tree.leaves.size }
    val children = tree.getChildren
    assertResult(Set(List(0.5, 1), List(1, 2))) {
      (for (child <- children) yield {
        List(child.support.get, child.distance.get)
      }).toSet
    }
  }

  test("Simple tree without support values") {
    val tree = Tree.readNewick("((A:2,B:3,C:1):1,(F:1,D:2):2);")
    assertResult(2) { tree.getChildren.size }
    assertResult(8) { tree.descendentsAndMe.size }
    assertResult(5) { tree.leaves.size }
    val children = tree.getChildren

    assertResult(List(List(None, 1.0), List(None, 2))) {
      (for (child <- children) yield {
        List(child.support, child.distance.get)
      }).toList
    }
  }

  test("Another simple tree") {
    val tree = Tree.readNewick("((A B:2,B:3,C:1)0.5:1,G G:3,(F:1,D:2)1:2);")
    assertResult(3) { tree.getChildren.size }
    assertResult(9) { tree.descendentsAndMe.size }
    assertResult(6) { tree.leaves.size }
    val children = tree.getChildren
    assertResult(Set(1, 3, 2)) {
      (for (child <- children) yield {
        child.distance.get
      }).toSet
    }
    val tree2 = Tree.readNewick("((A:2,B:3,C:1)0.5:1,G:3,(F:1,D:2)1:2,H H:1);")
    assertResult(Set("A", "B", "C", "D", "F", "G", "H H")) {
      (
        for (leaf <- tree2.leaves) yield leaf.name.get
      ).toSet
    }
  }

  test("RAxML output") {
    import java.io.FileReader
    val source = io.Source.fromFile(getClass.getResource("/raxml.tree").getPath).mkString
    Tree.readNewick(source, 0, 100)
  }

  test("MrBayes output") {
    import java.io.FileReader
    val source = io.Source.fromFile(getClass.getResource("/mrbayes.tree").getPath).mkString
    Tree.readNewick(source)
  }

  // This is not an error anymore. Value of 1 will substituted.
  // test("Malformed input 1 : missing support") {
  //   intercept[RuntimeException] {
  //     Tree.readNewick("((A B:2,B:3,C:1):1,G G:3,(F:1,D:2):2);")
  //   }
  // }

  test("missing distance") {

    val tree = Tree.readNewick("((A B,B,C)0.1,G G,(F,D)0.2);")
    assertResult(3) { tree.getChildren.size }
    assertResult(9) { tree.descendentsAndMe.size }
    assertResult(6) { tree.leaves.size }
    val children = tree.getChildren

    assertResult(List(List(Some(0.1), None), List(None, None), List(Some(0.2), None))) {
      (for (child <- children) yield {
        List(child.support, child.distance)
      }).toList
    }
  }

  test("missing distance missing support") {

    val tree = Tree.readNewick("((A B,B,C),G G,(F,D));")
    assertResult(3) { tree.getChildren.size }
    assertResult(9) { tree.descendentsAndMe.size }
    assertResult(6) { tree.leaves.size }
    val children = tree.getChildren.toList

    assertResult(List(List(None, None), List(None, None), List(None, None))) {
      (for (child <- children) yield {
        List(child.support, child.distance)
      }).toList
    }
  }

  test("Malformed input 3 : unassertResulted end of file") {
    intercept[RuntimeException] {
      Tree.readNewick("((A:2,B:3,C:1)0.5:1,G:3,(F:1,D:2)1:2,H H:1)")
    }
  }

  test("Malformed input 4 : wrong parentheses") {
    intercept[RuntimeException] {
      Tree.readNewick("((A:2,B:3,C:1)0.5:1,G:3,(F:1,D:2,H H:1);")
    }
  }

}