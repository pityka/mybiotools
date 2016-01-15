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

object LexicographicalOrder {

  def lexicographicCompareLists[A](x: List[A], y: List[A])(implicit o: Ordering[A]): Int = {
    def loop(a: List[A], b: List[A]): Int = {
      if (a == Nil && b != Nil) -1
      else if (a != Nil && b == Nil) +1
      else if (a == Nil && b == Nil) 0
      else {
        val headA = a.head
        val headB = b.head
        if (o.lt(headA, headB)) -1
        else if (o.gt(headA, headB)) +1
        else loop(a.tail, b.tail)
      }
    }
    loop(x, y)
  }

  def lexicographicCompareListsOfSameSize[A](x: List[A], y: List[A])(implicit o: Ordering[A]): Int = {
    def loop(a: List[A], b: List[A]): Int = {
      if (a == Nil) 0
      else {
        val headA = a.head
        val headB = b.head
        if (o.lt(headA, headB)) -1
        else if (o.gt(headA, headB)) +1
        else loop(a.tail, b.tail)
      }
    }
    loop(x, y)
  }

  def lexicographicalOrderingOfSameSize[A](implicit o: Ordering[A]) = new scala.math.Ordering[List[A]] {
    def compare(x: List[A], y: List[A]) = lexicographicCompareListsOfSameSize(x, y)
  }

  def lexicographicalOrdering[A](implicit o: Ordering[A]) = new scala.math.Ordering[List[A]] {
    def compare(x: List[A], y: List[A]) = lexicographicCompareLists(x, y)
  }
}