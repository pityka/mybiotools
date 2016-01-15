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

package mybiotools.tree.superinfection

import org.scalatest.FunSpec
import org.scalatest.Matchers
import mybiotools.tree._
import superinfection._

class SuperinfectionSpec extends FunSpec with Matchers {

  val n1 = Node.withPattern("1", "1")
  val n2 = Node.withPattern("2", "1")
  val n3 = Node.withPattern("3", "0")
  val n4 = Node.withPattern("4", "0")
  val n5 = Node.withPattern("5", "0")
  val n6 = Node.withPattern("6", "1")

  describe("ismixed") {

    it("empty is not mixed") {
      isMixed(Set[Node]()) should equal(false)
    }
    it("size 1 is not mixed") {
      isMixed(Set[Node](n1)) should equal(false)
      isMixed(Set[Node](n3)) should equal(false)
    }
    it("1 1 is not mixed. 0 0 is not mixed") {
      isMixed(Set[Node](n1, n2)) should equal(false)
      isMixed(Set[Node](n3, n4)) should equal(false)
    }
    it("1 0 is mixed. 1 0 is mixed") {
      isMixed(Set[Node](n1, n3)) should equal(true)
      isMixed(Set[Node](n4, n2)) should equal(true)
      isMixed(Set[Node](n3, n2)) should equal(true)
      isMixed(Set[Node](n1, n4)) should equal(true)
    }
    it("mixed with 3 elements") {
      isMixed(Set[Node](n1, n3, n2)) should equal(true)
      isMixed(Set[Node](n4, n2, n3)) should equal(true)
      isMixed(Set[Node](n3, n2, n1)) should equal(true)
      isMixed(Set[Node](n1, n4, n2)) should equal(true)
    }

  }

  describe("getMixedSets") {
    it("empty") {
      getMixedSets(new Bipartition(Set[Node](), Set(), None, None)) should equal(List())
    }
    it("no mixed") {
      getMixedSets(new Bipartition(Set[Node](n1, n2), Set(n3, n4), None, None)) should equal(List())
    }
    it("right mixed") {
      getMixedSets(new Bipartition(Set[Node](n1), Set(n2, n3, n4), None, None)) should equal(List(Set(n2, n3, n4)))
    }
    it("left mixed") {
      getMixedSets(new Bipartition(Set[Node](n1, n3), Set(n2), None, None)) should equal(List(Set(n1, n3)))
    }
    it("both mixed") {
      getMixedSets(new Bipartition(Set[Node](n1, n3), Set(n2, n4), None, None)) should equal(List(Set(n1, n3), Set(n2, n4)))
    }
  }

  describe("sets are compatible") {
    it("empty empty is false") {
      setsAreCompatible(Set(), Set()) should equal(false)
    }
    it("nonempty empty is false") {
      setsAreCompatible(Set(n1), Set()) should equal(false)
    }
    it("empty nonempty is false") {
      setsAreCompatible(Set(), Set(n1)) should equal(false)
    }
    it("(n1,n2) (n3,n4) is true") {
      setsAreCompatible(Set(n1, n2), Set(n3, n4)) should equal(true)
    }
    it("(n1,n2,n3) (n3,n4) is false ") {
      setsAreCompatible(Set(n1, n2, n3), Set(n3, n4)) should equal(false)
    }
    it("(n1,n2,n3) (n3,n2,n1,n4) is false") {
      setsAreCompatible(Set(n1, n2, n3), Set(n1, n2, n3, n4)) should equal(false)
    }
    it("(n1,n2,n6,n4,n5)  is false") {
      setsAreCompatible(Set(n1, n2, n3), Set(n1, n2, n3, n4)) should equal(false)
    }
  }

  describe("proves superinfection") {
    it("empty empty should give error") {
      evaluating(provesSuperinfection(
        new Bipartition(Set(), Set(), None, None),
        new Bipartition(Set(), Set(), None, None)
      )) should produce[AssertionError]
    }
    it("empty nonempty should give error") {
      evaluating(provesSuperinfection(
        new Bipartition(Set(), Set(), None, None),
        new Bipartition(Set(n1), Set(), None, None)
      )) should produce[AssertionError]
    }
    it("(n1,n2)|(n3,n4) vs reverse") {
      evaluating(provesSuperinfection(
        new Bipartition(Set(n1, n2), Set(n3, n4), None, None),
        new Bipartition(Set(n3, n4), Set(n1, n2), None, None)
      )) should produce[AssertionError]
    }
    it("(n1,n2)|(n3,n4,n5,n6) vs (n1,n2,n3)|(n4,n5,n6) is false ") {
      (provesSuperinfection(
        new Bipartition(Set(n1, n2), Set(n3, n4, n5, n6), None, None),
        new Bipartition(Set(n1, n2, n6), Set(n4, n5, n3), None, None)
      )) should equal(None)
    }
    it("(n1,n2)|(n6,n4,n5) vs (n1,n2)|(n4,n5,n6) is error ") {
      evaluating(provesSuperinfection(
        new Bipartition(Set(n1, n2), Set(n6, n4, n5), None, None),
        new Bipartition(Set(n1, n2), Set(n4, n5, n3), None, None)
      )) should produce[AssertionError]
    }
    it("(n1,n2)|(n3,n4,n5,n6) vs (n1,n2,n3,n4)|(n5,n6) is false ") {
      (provesSuperinfection(
        new Bipartition(Set(n1, n2), Set(n3, n4, n5, n6), None, None),
        new Bipartition(Set(n1, n2, n3, n4), Set(n5, n6), None, None)
      )) should equal(None)
    }
    it("(n1,n2)|(n3,n4,n5,n6) vs (n1,n2,n3,n4,n5)|(n6) is false ") {
      (provesSuperinfection(
        new Bipartition(Set(n1, n2), Set(n3, n4, n5, n6), None, None),
        new Bipartition(Set(n1, n2, n3, n4, n5), Set(n6), None, None)
      )) should equal(None)
    }
    it("(n1,n2,n3)|(n6,n4,n5) vs (n1,n2,n3,n4)|(n6,n5) is true ") {
      (provesSuperinfection(
        new Bipartition(Set(n1, n2, n3), Set(n4, n5, n6), None, None),
        new Bipartition(Set(n1, n2, n3, n4), Set(n5, n6), None, None)
      )) should equal(Some((Set(n1, n2, n3), Set(n5, n6))))
    }
    it("(n1,n2,n6)|(n3,n4,n5) vs (n1,n2,n3,n4)|(n6,n5) is false ") {
      (provesSuperinfection(
        new Bipartition(Set(n1, n2, n6), Set(n4, n5, n3), None, None),
        new Bipartition(Set(n1, n2, n3, n4), Set(n5, n6), None, None)
      )) should equal(None)
    }
    it("(n1,n2,n6)|(n3,n4,n5) vs (n1,n2,n6,n4)|(n3,n5) is false ") {
      (provesSuperinfection(
        new Bipartition(Set(n1, n2, n6), Set(n4, n5, n3), None, None),
        new Bipartition(Set(n1, n2, n3, n4), Set(n5, n6), None, None)
      )) should equal(None)
    }
  }

  describe("decidesuperinfection") {
    it("((((n1,n2),n3),n4),(n6,n5)) is superinfected") {
      val set = Set(
        new Bipartition(Set(n1), Set(n2, n3, n4, n5, n6), None, None),
        new Bipartition(Set(n2), Set(n1, n3, n4, n5, n6), None, None),
        new Bipartition(Set(n3), Set(n1, n2, n4, n5, n6), None, None),
        new Bipartition(Set(n4), Set(n1, n3, n2, n5, n6), None, None),
        new Bipartition(Set(n5), Set(n1, n3, n4, n2, n6), None, None),
        new Bipartition(Set(n6), Set(n1, n3, n4, n5, n2), None, None),
        new Bipartition(Set(n1, n2), Set(n3, n4, n5, n6), None, None),
        new Bipartition(Set(n1, n2, n3), Set(n4, n5, n6), None, None),
        new Bipartition(Set(n5, n6), Set(n1, n2, n4, n3), None, None)
      )
      val bs = BipartitionSet(set)
      decideSuperinfection(bs) should equal(true)
    }
    it("((((n1,n2),n6),n4),(n3,n5)) is not superinfected") {
      val set = Set(
        new Bipartition(Set(n1), Set(n2, n3, n4, n5, n6), None, None),
        new Bipartition(Set(n2), Set(n1, n3, n4, n5, n6), None, None),
        new Bipartition(Set(n3), Set(n1, n2, n4, n5, n6), None, None),
        new Bipartition(Set(n4), Set(n1, n3, n2, n5, n6), None, None),
        new Bipartition(Set(n5), Set(n1, n3, n4, n2, n6), None, None),
        new Bipartition(Set(n6), Set(n1, n3, n4, n5, n2), None, None),
        new Bipartition(Set(n1, n2), Set(n3, n4, n5, n6), None, None),
        new Bipartition(Set(n1, n2, n6), Set(n4, n5, n3), None, None),
        new Bipartition(Set(n5, n3), Set(n1, n2, n4, n6), None, None)
      )
      val bs = BipartitionSet(set)
      decideSuperinfection(bs) should equal(false)
    }
  }

  describe("decide superinfectin from newick") {
    it("((((n1XXX,n2XXX),n6XXX),n4),(n3,n5)) is not superinfected") {

      val patternfunction = { (name: String) =>
        if (name.takeRight(3) == "XXX") "1"
        else "0"
      }

      val tree = Tree.readNewick("((((n1XXX,n2XXX),n6XXX),n4),(n3,n5));")(patternfunction)

      decideSuperinfection(BipartitionSet(tree)) should equal(false)
    }
    it("((((n1XXX,n2XXX),n6),n4),(n3XXX,n5)) is superinfected") {

      val patternfunction = { (name: String) =>
        if (name.takeRight(3) == "XXX") "1"
        else "0"
      }

      val tree = Tree.readNewick("((((n1XXX,n2XXX),n6),n4),(n3XXX,n5));")(patternfunction)

      decideSuperinfection(BipartitionSet(tree)) should equal(true)
    }
  }

}