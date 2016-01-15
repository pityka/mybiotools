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

class SelectSharedSpec extends FunSpec with Matchers {

  describe("select shared elements") {

    it(" shared in 1") {
      val sets = List(Set(1, 2, 3, 10, 11), Set(4, 5, 6, 10, 11, 12), Set(7, 8, 9, 10, 12))

      selectSharedElements(sets, 1).toSet should equal(Set(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
    }

    it(" shared in 2") {
      val sets = List(Set(1, 2, 3, 10, 11), Set(4, 5, 6, 10, 11, 12), Set(7, 8, 9, 10, 12))

      selectSharedElements(sets, 2).toSet should equal(Set(10, 11, 12))
    }

    it(" shared in 3") {
      val sets = List(Set(1, 2, 3, 10, 11), Set(4, 5, 6, 10, 11, 12), Set(7, 8, 9, 10, 12))

      selectSharedElements(sets, 3).toSet should equal(Set(10))
    }

    it(" shared in 4") {
      val sets = List(Set(1, 2, 3, 10, 11), Set(4, 5, 6, 10, 11, 12), Set(7, 8, 9, 10, 12))

      selectSharedElements(sets, 4).toSet should equal(Set())
    }

  }

}