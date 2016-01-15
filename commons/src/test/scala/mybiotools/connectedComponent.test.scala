
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

class ConnectedComponentSpec extends FunSpec with Matchers {
  describe("connected") {
    it("1") {
      (findConnectedComponentsByIntersection(Map(1 -> Set(1, 2, 3, 4), 2 -> Set(1, 2, 3)), 1)) should equal(Set(
        Map(1 -> Set(1, 2, 3, 4), 2 -> Set(1, 2, 3))
      ))
    }

    it("2") {
      findConnectedComponentsByIntersection(Map(1 -> Set(1, 2, 3, 4), 2 -> Set(1, 2, 3), 3 -> Set(5, 6, 7), 4 -> Set(7, 8, 9)), 1) should equal(Set(
        Map(1 -> Set(1, 2, 3, 4), 2 -> Set(1, 2, 3)),
        Map(3 -> Set(5, 6, 7), 4 -> Set(7, 8, 9))
      ))
    }
    it("3") {
      findConnectedComponentsByIntersection(Map(1 -> Set(1, 2), 2 -> Set(1, 2, 5, 6), 3 -> Set(3, 4, 6)), 1) should equal(
        Set(
          Map(
            1 -> Set(1, 2),
            2 -> Set(1, 2, 5, 6),
            3 -> Set(3, 4, 6)
          )

        )
      )
    }
  }
}