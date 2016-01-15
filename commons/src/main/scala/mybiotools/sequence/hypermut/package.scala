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

package mybiotools.sequence.hypermut

import mybiotools._

case class HypermutResult(
    mutatedPositions: Vector[Int],
    matchingContextPositions: Vector[Int],
    controlMutatedPositions: Vector[Int],
    matchingControlContextPositions: Vector[Int]
) {
  def mutationEvents = mutatedPositions.size
  def possibleMutationEvents = matchingContextPositions.size
  def controlEvents = controlMutatedPositions.size
  def possibleControlEvents = matchingControlContextPositions.size

  def oddsRatio = (mutationEvents.toDouble / (possibleMutationEvents - mutationEvents)) / (controlEvents.toDouble / (possibleControlEvents - controlEvents))
  def rateRatio = (mutationEvents.toDouble / (possibleMutationEvents)) / (controlEvents.toDouble / (possibleControlEvents))
  def phi = pearsonsPhi(ContingencyTable2x2(mutationEvents, controlEvents, possibleMutationEvents - mutationEvents, possibleControlEvents - controlEvents))
}

