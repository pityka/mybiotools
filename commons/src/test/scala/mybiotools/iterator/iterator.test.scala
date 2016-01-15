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

package mybiotools.iterator

import org.scalatest.FunSpec
import org.scalatest.Matchers

class ForkIteratorSpec extends FunSpec with Matchers {

  class SumOdd extends Accumulator[Int] {
    var s = 0
    def predicate(i: Int) = i % 2 == 1
    def accumulate(i: Int) = {

      if (s <= 25) {
        s += i
        TakeMore
      } else Done
    }
  }

  class MultEven extends Accumulator[Int] {
    var s = 1
    def predicate(i: Int) = i % 2 == 0
    def accumulate(i: Int) = {

      if (s <= 25) {
        s *= i
        TakeMore
      } else Done
    }
  }

  describe("sum odd") {
    it("simple map with postprocessing of the iterator") {
      val list = 1 to 10 toList
      val iter = list iterator
      val summer = new SumOdd
      val mapper = new Mapper(List(summer))
      val iter2 = mapper.map(iter)
      val iter3 = iter2.filter(_ % 2 == 0)
      iter3.sum should equal(list.filter(_ % 2 == 0).sum)
      summer.s should equal(list.filter(_ % 2 == 1).sum)
    }
    it("simple map with postprocessing of the iterator 2") {
      val list = 1 to 30 toList
      val iter = list iterator
      val summer = new SumOdd
      val mapper = new Mapper(List(summer))
      val iter2 = mapper.map(iter)
      val iter3 = iter2.filter(_ % 2 == 0)
      iter3.sum should equal(list.filter(_ % 2 == 0).sum)
      summer.s should equal(36)
    }
    it("mapuntil with postprocessing of the iterator") {
      val list = 1 to 30 toList
      val iter = list iterator
      val summer = new SumOdd
      val mapper = new Mapper(List(summer))
      val iter2 = mapper.mapUntilDone(iter)
      val iter3 = iter2.filter(_ % 2 == 0)
      iter3.sum should equal(42)
      summer.s should equal(36)
    }

  }

  describe("sum odd and multiply even") {
    it("simple map with postprocessing of the iterator") {
      val list = 1 to 10 toList
      val iter = list iterator
      val summer = new SumOdd
      val mult = new MultEven
      val mapper = new Mapper(List(summer, mult))
      val iter2 = mapper.map(iter)
      val iter3 = iter2.filter(_ % 2 == 0)
      iter3.sum should equal(list.filter(_ % 2 == 0).sum)
      summer.s should equal(list.filter(_ % 2 == 1).sum)
      mult.s should equal(48)
    }
    it("simple map with postprocessing of the iterator 2") {
      val list = 1 to 30 toList
      val iter = list iterator
      val mult = new MultEven
      val summer = new SumOdd

      val mapper = new Mapper(List(summer, mult))

      val iter2 = mapper.map(iter)
      val iter3 = iter2.filter(_ % 2 == 0)
      iter3.sum should equal(list.filter(_ % 2 == 0).sum)
      summer.s should equal(36)
      mult.s should equal(48)
    }
    it("mapuntil with postprocessing of the iterator") {
      val list = 1 to 30 toList
      val iter = list iterator
      val mult = new MultEven
      val summer = new SumOdd

      val mapper = new Mapper(List(summer, mult))

      val iter2 = mapper.mapUntilDone(iter)
      val iter3 = iter2.filter(_ % 2 == 0)
      iter3.sum should equal(42)
      summer.s should equal(36)
      mult.s should equal(48)
    }

  }

}