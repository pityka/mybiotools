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

package mybiotools.pcanew

import org.scalatest.FunSuite

import mybiotools._
import mybiotools.mymatrix._
import mybiotools.pcanew._
import mybiotools.gwascommons._
import org.saddle._

class PCATestSuite extends FunSuite {

  test("Simple test") {
    val m = MyMatrix(Array(Array(1d, 2d), Array(2d, 1d)), Seq(Individual("1"), Individual("2")), false)
    val p = pcaFromDistanceMatrix(m)
    val pe = PCAResult(coordinates = Map(
      Individual("1") -> Vector(0.7071067811865475 * math.sqrt(0.15365092212534687), 0.7071067811865475 * math.sqrt(0.11701964434787852)),
      Individual("2") -> Vector(0.7071067811865475 * math.sqrt(0.15365092212534687), -0.7071067811865475 * math.sqrt(0.11701964434787852))
    ), eigenValues = List(0.15365092212534687, 0.11701964434787852))
    assertResult(pe) { p }
  }

  test("Simple test identity") {
    val f = Frame("2" -> Series("2" -> 1.0, "1" -> 0.0), "1" -> Series("1" -> 1.0, "2" -> 0.0))
    // println(f)
    val p = pcaFromGRM(f)
    val pe = PCAResult(Map("1" -> Vector(1.0, 0.0), "2" -> Vector(0.0, 1.0)), List(1.0, 1.0))
    assertResult(pe) { p }
  }

  test("Test saddle") {
    val frame = Frame("1" -> Series("1" -> 2.0, "2" -> 5.0, "3" -> 6.0, "4" -> 2.0), "2" -> Series("2" -> 2.0, "3" -> 3.0, "1" -> 4.0, "4" -> 3.0), "3" -> Series("1" -> -1.0, "2" -> 3.0, "3" -> 4.0, "4" -> 5.0))
    createPCAPlots(frame)
    // mybiotools.plots.show(createPCAPlots(frame))
  }

}