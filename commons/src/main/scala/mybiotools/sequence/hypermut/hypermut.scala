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

package mybiotools.sequence

import scala.util.matching.Regex
import scala.util.matching.Regex.Match
import mybiotools.eq._
import scala.collection.mutable.ArrayBuffer

package object hypermut {

  def hyperMutationCount(ref: String, s: String, useApobec3FSpecificPattern: Boolean = false) = {

    assume(ref.size == s.size)
    val ancientState = "G"
    val mutatedState = "A"
    val downStream = if (useApobec3FSpecificPattern) "(A[^C])" else "([AG][^C])"
    val controlDownStream = "(([CT][ACGT])|([AG]C))"
    val pattern = (ancientState + downStream).r
    val controlPattern = (ancientState + controlDownStream).r

    var mutatedPositions = ArrayBuffer[Int]()
    var matchingContextPositions = ArrayBuffer[Int]()
    var controlMutatedPositions = ArrayBuffer[Int]()
    var matchingControlContextPositions = ArrayBuffer[Int]()

    var i = 0
    while (i < s.size - 2) {
      val matchd: Option[Match] = pattern.findFirstMatchIn(ref.substring(i, s.size))
      matchd.foreach { matchdata =>
        assert(matchdata.end - matchdata.start == 3)
        val start = matchdata.start + i
        if (start < s.size - 2) {
          if (s.substring(start, start + 3) === (mutatedState + ref.substring(start + 1, start + 3))) {
            mutatedPositions += start
            matchingContextPositions += start
          }
          if (s.substring(start, start + 3) === (ref.substring(start, start + 3))) {
            matchingContextPositions += start
          }

        }
      }
      i = matchd match {
        case Some(matchdata) => matchdata.start + 1 + i
        case None => s.size
      }
    }

    i = 0
    while (i < s.size - 2) {
      val matchd: Option[Match] = controlPattern.findFirstMatchIn(ref.substring(i, s.size))
      matchd.foreach { matchdata =>
        assert(matchdata.end - matchdata.start == 3)
        val start = matchdata.start + i
        if (start < s.size - 2) {
          if ((s.substring(start, start + 3) === (mutatedState + ref.substring(start + 1, start + 3)))) {
            controlMutatedPositions += start
            matchingControlContextPositions += start
          }
          if ((s.substring(start, start + 3) === (ref.substring(start, start + 3)))) {
            matchingControlContextPositions += start
          }

        }
      }
      i = matchd match {
        case Some(matchdata) => matchdata.start + 1 + i
        case None => s.size
      }
    }

    HypermutResult(mutatedPositions.toVector, matchingContextPositions.toVector, controlMutatedPositions.toVector, matchingControlContextPositions.toVector)
  }
}