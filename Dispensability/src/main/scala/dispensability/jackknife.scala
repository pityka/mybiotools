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

package dispensability

object JackknifeProjection {
  def projectIndividualsToVariants[T <: AnnotationWithGene[T]](variants: Seq[VariantWithSumGenotype[T]], totalIndividuals: Int, projections: Seq[Int]): Seq[(Int, Seq[(Int, Int)])] = {

    def jackKnifeProjection(spectrum: Seq[(Int, Int)], n: Int, N: Int, order: Int, stopWhenDecreasing: Boolean) = {

      def Delta(N: Int, n: Int): Double = n to (N - 1) map (j => 1.0 / j.toDouble) sum

      def missing(N: Int) = {
        order match {

          case 3 => {
            val f1 = spectrum.filter(_._1 == 1).map(_._2).head
            val f2 = spectrum.filter(_._1 == 2).map(_._2).head
            val f3 = spectrum.filter(_._1 == 3).map(_._2).head
            val d1 = Delta(N, n)
            val d2 = math.pow(d1, 2)
            val d3 = math.pow(d1, 3)
            (d1 + d2 / 2 + d3 / 6) * f1 - (d3 + d2) * f2 + d3 * f3
          }

          case 2 => {
            val f1 = spectrum.filter(_._1 == 1).map(_._2).head
            val f2 = spectrum.filter(_._1 == 2).map(_._2).head
            val d1 = Delta(N, n)
            val d2 = math.pow(d1, 2)
            (d1 + d2 / 2.0) * f1 - (d2 * f2)
          }

        }
      }
      if (missing(N) < missing(N - 1) && stopWhenDecreasing) None
      else Some(missing(N) + spectrum.map(_._2).sum)
    }

    def siteFrequencySpectrum(variants: Seq[VariantWithSumGenotype[T]]): Seq[(Int, Int)] = {
      val sp = variants
        .map(v => v -> v.alleleCount)
        .groupBy(_._2).toSeq.map(x => x._1 -> x._2.size).toSeq
      // assert(sp.map(x => x._1 * x._2).sum == variants.size, sp.map(x => x._1 * x._2).sum.toString)
      sp
    }

    val spectrum = siteFrequencySpectrum(variants)
    val n = totalIndividuals
    projections.map { N =>
      N -> List(
        N -> scala.util.Try(jackKnifeProjection(spectrum, n, N, 3, true)).toOption.flatten
      // N -> jackKnifeProjection(spectrum, n, N, 2, true),
      // N -> Some(chao1987Projection(spectrum))
      ).filter(_._2.isDefined).map(x => x._1 -> x._2.get.toInt)
    }

  }

}