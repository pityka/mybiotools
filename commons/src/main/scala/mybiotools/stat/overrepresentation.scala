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

package mybiotools.stat

object OverRepresentation {

  def hypergeometricTest(total: Int, marked: Int, draws: Int, markedDraws: Int): Double =
    synchronized {
      jdistlib.HyperGeometric.cumulative(
        markedDraws.toDouble - 1,
        marked.toDouble,
        total - marked.toDouble,
        draws.toDouble,
        false,
        false
      )
    }

  def hypergeometricTestForDepletion(total: Int, marked: Int, draws: Int, markedDraws: Int): Double =
    synchronized {
      jdistlib.HyperGeometric.cumulative(
        markedDraws.toDouble - 1,
        marked.toDouble,
        total - marked.toDouble,
        draws.toDouble,
        true,
        false
      )
    }

  /** Tests whether targetset is overrepresented in aprioriset. */
  //   using the whole genome as background would be wrong. you should use the genes that go in and out of the part you care about as the background and hits. So if you do DEG, use only the genes you've tested as background (for example I usually exclude low expressed genes from DEG, and therefore also from the background). Additionally, you might want to consider limiting your analysis only to the genes that are annotated in your dataset so the intersection of your test and your annotation.
  // and finally, if you happen to get too many bad hits, let's say sets with really low sizes, you might want to consider adding a conservative pseudo-count, in the sense that you always assume there was one more hit, and you missed it. But you should not need this usually if you do the test right, meaning that you test what is the probability of getting EQUAL or more hits as you observe.
  def geneSet[T](numberOfAllGenes: Int, aprioriSet: Set[T], targetSet: Set[T]) =
    hypergeometricTest(numberOfAllGenes, aprioriSet.size, targetSet.size, (targetSet & aprioriSet).size)

  def geneSetForDepletion[T](numberOfAllGenes: Int, aprioriSet: Set[T], targetSet: Set[T]) =
    hypergeometricTestForDepletion(numberOfAllGenes, aprioriSet.size, targetSet.size, (targetSet & aprioriSet).size)

}