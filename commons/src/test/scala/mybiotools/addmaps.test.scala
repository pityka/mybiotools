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

package mybiotools

import org.scalatest.FunSpec
import org.scalatest.Matchers
import collection.immutable.HashMap

class AddMapsSpec extends FunSpec with Matchers {

  describe("old") {
    it("dfs") {
      addMaps(Map(1 -> 2), Map(1 -> 3))((x, y) => x + y) should equal(Map(1 -> 5))
    }

  }

  describe("new") {
    it("dfs") {
      addMapsQuick(Map(1 -> 2), Map(1 -> 3)) { case ((k1, x), (k2, y)) => (k1, x + y) } should equal(Map(1 -> 5))
      addMapsQuick(HashMap(1 -> 2), HashMap(1 -> 3)) { case ((k1, x), (k2, y)) => (k1, x + y) } should equal(HashMap(1 -> 5))
    }

  }

  describe("innerJoin2Files") {

    val s1 = """s1 s2 s3
    |
  	|a1 a2 a3
  	|""".stripMargin

    val s2 = """s4,s1,s5
  	|a4,a1,a5
,,,""".stripMargin

    val source1 = io.Source.fromString(s1)
    val source2 = io.Source.fromString(s2)

    val r = innerJoin2Files(source1, source2, Set(' '), Set(','), 0, 1).toList
    it("dfs") {
      r should equal(List(
        Vector("s1", "s2", "s3", "s4", "s5"),
        Vector("a1", "a2", "a3", "a4", "a5")
      ))
    }

  }

}