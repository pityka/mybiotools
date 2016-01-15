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

import org.saddle._
import com.apporiented.algorithm.clustering._
import collection.JavaConversions._

package object clustering {

  def breadth_traverse[T](cl: Cluster[T]): List[Cluster[T]] = {
    def loop(cl: List[Cluster[T]], acc: List[Cluster[T]]): List[Cluster[T]] = cl match {
      case Nil => acc.reverse
      case x :: xs => loop(xs ::: x.getChildren.toList, x :: acc)
    }

    loop(List(cl), Nil)

  }

  def traverse[T](root: Cluster[T]): Seq[T] =
    if (root.isLeaf) Seq(root.getName)
    else root.getChildren.flatMap(x => traverse(x))

  def print(root: Cluster[_]): String =
    if (root.isLeaf) root.getName.toString
    else "(" + root.getChildren.map(x => print(x)).mkString(",") + ")"

  def clusterFrameByRows[RX, CX](frame: Frame[RX, CX, Double])(distance: (Vec[Double], Vec[Double]) => Double)(implicit ct: reflect.ClassTag[RX]): Cluster[RX] = {
    val droppedRows = frame.squeeze.rdropNA
    val names: Array[RX] = droppedRows.rowIx.toSeq.toArray
    val rowseq = droppedRows.toRowSeq
    val distances = rowseq.map {
      case (rxi, rowi) =>
        rowseq.map {
          case (rxj, rowj) =>
            distance(rowi.toVec, rowj.toVec)
        }.toArray
    }.toArray
    new DefaultClusteringAlgorithm().performClustering[RX](
      distances,
      names.asInstanceOf[Array[RX with java.lang.Object]],
      new AverageLinkageStrategy()
    );

  }

  private val pearson = new org.apache.commons.math3.stat.correlation.PearsonsCorrelation

  val euclideanDistance = (s1: Vec[Double], s2: Vec[Double]) => {
    var i = 0
    var sum = 0.0
    while (i < s1.length) {
      val diff: Double = s1.raw(i) - s2.raw(i)
      sum += diff * diff
      i += 1
    }
    math.sqrt(sum)
  }
  val pearsonCorrelation = (s1: Vec[Double], s2: Vec[Double]) => pearson.correlation(s1.toSeq.toArray, s2.toSeq.toArray)

  def weightedEuclideanDistance(w: Vec[Double]) = (s1: Vec[Double], s2: Vec[Double]) => math.sqrt((w * (s1 - s2)) dot (s1 - s2))
}
