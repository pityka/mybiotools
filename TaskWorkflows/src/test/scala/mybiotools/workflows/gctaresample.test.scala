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
package mybiotools.workflows

import mybiotools.gwascommons.genotypedata.GRM
import org.scalatest.FunSpec
import org.scalatest.Matchers
import collection.JavaConversions._
import collection.immutable.HashMap
import org.saddle._
import mybiotools.gwascommons._

class BootstrapGRMSpec extends FunSpec with Matchers {

  val i1 = Individual("1", "1")
  val i2 = Individual("2", "2")
  val i3 = Individual("3", "3")

  val grm = Frame(
    Individual("1", "1") -> Series(Individual("1", "1") -> 1.0, Individual("2", "2") -> 2.0, Individual("3", "3") -> 3.0),
    Individual("2", "2") -> Series(Individual("1", "1") -> 2.0, Individual("2", "2") -> 5.0, Individual("3", "3") -> 6.0),
    Individual("3", "3") -> Series(Individual("1", "1") -> 3.0, Individual("2", "2") -> 6.0, Individual("3", "3") -> 9.0)
  )
  describe("boostrap grm") {
    it("3x3") {
      val list = List(Individual("1", "1"), Individual("2", "2"), Individual("1", "1"), Individual("2", "2"), Individual("2", "2"))
      val bootstrapped = gctaresampletask.bootstrapGRM(grm, list)

      val k0 = Individual("0", "0")
      val k1 = Individual("1", "1")
      val k2 = Individual("2", "2")
      val k3 = Individual("3", "3")
      val k4 = Individual("4", "4")

      val expected = Frame(
        k0 -> Series(k0 -> 1.0, k1 -> 2.0, k2 -> 1.0, k3 -> 2.0, k4 -> 2.0),
        k1 -> Series(k0 -> 2.0, k1 -> 5.0, k2 -> 2.0, k3 -> 5.0, k4 -> 5.0),
        k2 -> Series(k0 -> 1.0, k1 -> 2.0, k2 -> 1.0, k3 -> 2.0, k4 -> 2.0),
        k3 -> Series(k0 -> 2.0, k1 -> 5.0, k2 -> 2.0, k3 -> 5.0, k4 -> 5.0),
        k4 -> Series(k0 -> 2.0, k1 -> 5.0, k2 -> 2.0, k3 -> 5.0, k4 -> 5.0)
      )
      bootstrapped should equal(expected)
    }
  }

}