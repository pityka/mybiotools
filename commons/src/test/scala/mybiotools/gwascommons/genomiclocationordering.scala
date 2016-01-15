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
package gwascommons

import gwascommons._
import org.scalatest.FunSpec
import org.scalatest.Matchers

class GenomicLocationOrderingSpec extends FunSpec with Matchers {
  import gwascommons._

  describe("Lexicographical ordering of GenomicLocation objects") {

    it("GL(123,1) < GL(124,2)") {
      GenomicLocation(123, 1) should be < GenomicLocation(124, 1)
    }

    it("GL(123,1) <= GL(123,1)") {
      GenomicLocation(123, 1) should be === GenomicLocation(123, 1)
    }

    it("GL(122,1) < GL(123,1)") {
      GenomicLocation(122, 1) should be < GenomicLocation(123, 1)
    }

    it("GL(122,2) > GL(123,1)") {
      GenomicLocation(122, 2) should be > GenomicLocation(123, 1)
    }

    it("GL(122,2) < GL(123,3)") {
      GenomicLocation(122, 2) should be < GenomicLocation(124, 3)
    }

  }
}