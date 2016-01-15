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

//Produces an Iterator[Seq[A]] from an Iterator[A] by the continuous spans of the predicate.
final class SpanIterator[A](iter: Iterator[A], p: A => Boolean) extends Iterator[Seq[A]] {

  val buffer = collection.mutable.ArrayBuffer[A]()

  var currentPredicateValue: Boolean = _

  var firstRound = true

  var end = false

  val start = iter.hasNext

  var fwRead: A = _

  def hasNext = start && !end

  def next = {

    if (firstRound) {
      firstRound = false
      fwRead = iter.next
      currentPredicateValue = p(fwRead)
    }

    while (p(fwRead) == currentPredicateValue && iter.hasNext) {

      buffer.append(fwRead)

      fwRead = iter.next
    }

    if (!iter.hasNext) {
      if (p(fwRead) == currentPredicateValue) {
        buffer.append(fwRead)
        end = true
      }
    }

    currentPredicateValue = p(fwRead)

    val ret = buffer.toList

    buffer.clear

    ret

  }

}