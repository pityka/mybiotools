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

package mybiotools.stringkeyedmatrix

import mybiotools.bit2seq._
import Bit2State._
import collection.generic.{ CanBuildFrom }

import mybiotools.stringstore.String8

/**
 * Rectangular matrix/table with string keys, with merging.
 *
 * Main purpose of this class is to store genotype data.
 */
class StringKeyedMatrix[D, Repr <: IndexedSeq[D]] private[stringkeyedmatrix] (
    private[stringkeyedmatrix] val dataMatrix: IndexedSeq[Repr],
    val keys1: Seq[String8],
    val keys2: Seq[String8]
) {

  assert(keys1.distinct == keys1, "keys1 are not uniq" + keys1.size + " vs " + keys1.distinct.size)
  assert(keys2.distinct == keys2, "keys2 are not uniq" + keys2.size + " vs " + keys2.distinct.size)
  assert(keys1.size == dataMatrix.size)

  for (i <- 0 to keys1.size - 1) assert(keys2.size == dataMatrix(i).size, keys2.size + " ne " + dataMatrix(i).size)

  def canEqual(other: Any): Boolean = other.isInstanceOf[StringKeyedMatrix[_, _]]

  override def hashCode: Int = 41 * (41 * (41 + keyIndices1.hashCode) + dataMatrix.hashCode) + keyIndices2.hashCode

  override def equals(that: Any): Boolean = that match {
    case t: StringKeyedMatrix[_, _] => (t canEqual this) && dataMatrix == t.dataMatrix && t.keyIndices1 == keyIndices1 && t.keyIndices2 == keyIndices2
    case _ => false
  }

  private val keyIndices1 = collection.immutable.HashMap[String8, Int](keys1.zipWithIndex: _*)

  private val keyIndices2 = collection.immutable.HashMap[String8, Int](keys2.zipWithIndex: _*)

  def dimension1 = keys1.size

  def dimension2 = keys2.size

  //TODO: this may kill down Bit2Buffer and store Bit2's in a vector.
  def transpose = new StringKeyedMatrix[D, IndexedSeq[D]](dataMatrix.transpose, keys2, keys1)

  def foreachDim2(s: String8)(f: D => Unit) {
    dataMatrix(keyIndices1(s)).foreach(x => f(x))
  }

  import org.saddle._
  import mybiotools.gwascommons._
  def toSaddleFrame(implicit st: org.saddle.ST[D]): Frame[String, String, D] = {
    val index1 = Index(keys1.map(_.value): _*)
    val index2 = Index(keys2.map(_.value): _*)
    Frame(dataMatrix.map(x => Series[String, D](Vec(x: _*), index1)), index2)
  }

  private def helperFilterArrayOnIndices[K, T <: IndexedSeq[K]](
    array: T,
    idxlist: Seq[Int]
  )(implicit cbf: CanBuildFrom[IndexedSeq[K], K, T]): T = {
    val builder = cbf(array)
    array.zipWithIndex.filter(x => idxlist.contains(x._2)).unzip._1.foreach { x =>
      builder += x
    }
    builder.result
  }

  def filterDim1(f: String => Boolean)(implicit cbf: CanBuildFrom[IndexedSeq[D], D, Repr]): StringKeyedMatrix[D, Repr] = {
    val (filteredKeys1, filteredIndices) = keys1.zipWithIndex filter (x => f(x._1)) unzip
    val filteredMatrix: IndexedSeq[Repr] = helperFilterArrayOnIndices[Repr, IndexedSeq[Repr]](dataMatrix, filteredIndices)
    new StringKeyedMatrix(filteredMatrix, filteredKeys1, keys2)
  }

  def filterDim2(f: String => Boolean)(implicit cbf: CanBuildFrom[IndexedSeq[D], D, Repr]): StringKeyedMatrix[D, Repr] = {
    val (filteredKeys2, filteredIndices) = keys2.zipWithIndex filter (x => f(x._1)) unzip
    val filteredMatrix: IndexedSeq[Repr] = dataMatrix.map((x: Repr) =>
      helperFilterArrayOnIndices[D, Repr](x, filteredIndices))
    new StringKeyedMatrix(filteredMatrix, keys1, filteredKeys2)
  }

  def row(s: String8) = dataMatrix(keyIndices1(s))

  def column(s: String8) = dataMatrix.map(_.apply(keyIndices2(s)))

  override def toString = "StringKeyedMatrix[" + (if (dataMatrix.size > 0) dataMatrix.head.asInstanceOf[AnyRef].getClass.getName) + "](" + dataMatrix.size + "," + keys1.toString + "," + keys2.toString + ")"

  def deepString = dataMatrix.toString

  def contains(k1: String8, k2: String8) = keyIndices1.contains(k1) && keyIndices2.contains(k2)

  def apply(k1: String8, k2: String8): D = {
    dataMatrix(keyIndices1(k1))(keyIndices2(k2))
  }

  def get(k1: String8, k2: String8): Option[D] = if (contains(k1, k2)) Some(apply(k1, k2)) else None

  def mapValues[U, To <: IndexedSeq[U]](f: D => U)(implicit manifest: Manifest[U], cbf: CanBuildFrom[Repr, U, To]): StringKeyedMatrix[U, To] = {
    val transformed = (for (i <- 0 to dimension1 - 1) yield {
      val b = cbf(dataMatrix.head)
      for (j <- 0 to dimension2 - 1) (b += f(dataMatrix(i)(j)))
      b.result
    }).toIndexedSeq
    new StringKeyedMatrix[U, To](transformed, keys1, keys2)
  }

  def merge[To <: IndexedSeq[D]](that: StringKeyedMatrix[D, _], missingValue: D)(collision: (D, D) => D)(implicit manifest: Manifest[D], cbf: CanBuildFrom[Repr, D, To]): StringKeyedMatrix[D, To] = {
    def helper(x1: Option[D], x2: Option[D]): Option[D] = (x1, x2) match {
      case (None, None) => None
      case (Some(y1), Some(y2)) => Some(collision(y1, y2))
      case (None, Some(y1)) => Some(y1)
      case (Some(y1), None) => Some(y1)
    }

    val mergedKeys1 = (keys1 ++ that.keys1).distinct.sorted
    val mergedKeys2 = (keys2 ++ that.keys2).distinct.sorted

    val array = (for (i <- mergedKeys1.par) yield {
      val b = cbf(dataMatrix.head)
      (for (j <- mergedKeys2) yield {
        val x = helper(get(i, j), that.get(i, j)) match {
          case Some(x) => x
          case None => missingValue
        }
        b += x
      })
      b.result
    }).toIndexedSeq

    new StringKeyedMatrix(array, mergedKeys1, mergedKeys2)
  }

}

/** Factories of `StringKeyedMatrix`. Provides method to read various genotype files. */
object StringKeyedMatrix {

  // def fromArray[D](  dataMatrix: Array[Array[D]],
  //                    keys1: scala.collection.immutable.IndexedSeq[String],
  //                    keys2: scala.collection.immutable.IndexedSeq[String] ): StringKeyedMatrix[D] = new StringKeyedMatrix( dataMatrix, keys1, keys2 )

  def fromMapofMaps[D](in: Map[String8, Map[String8, D]])(implicit manifest: Manifest[D]): StringKeyedMatrix[D, IndexedSeq[D]] = {
    val keys1 = in.keys.toSeq.sorted

    val keys2 = in.head._2.keys.toSeq.sorted
    in.foreach { x => assert(x._2.keys.toSeq.sorted == keys2) }

    val arrays = (for (i <- keys1) yield {
      (for (j <- keys2) yield in(i)(j)).toIndexedSeq
    }).toIndexedSeq

    new StringKeyedMatrix(arrays, keys1, keys2)
  }

  def fromMapofTuples[D](in: Map[Tuple2[String8, String8], D])(implicit manifest: Manifest[D]): StringKeyedMatrix[D, IndexedSeq[D]] = {
    val in2: Map[String8, Map[String8, D]] = in.groupBy(x => x._1._1).mapValues(x => x.groupBy(y => y._1._2).mapValues(y => y.values.head))
    fromMapofMaps(in2)
  }

  def fromUnsortedIterable[D](in: Iterable[Tuple3[String8, String8, D]])(implicit manifest: Manifest[D]): StringKeyedMatrix[D, IndexedSeq[D]] = {
    val m: Map[Tuple2[String8, String8], D] = in.map(x => ((x._1, x._2), x._3)).toMap //in.groupBy(x => (x._1, x._2)).mapValues(x => x.head._3)
    fromMapofTuples(m)
  }

  import scala.collection.mutable.Buffer
  import scala.collection.mutable.ArrayBuffer
  import scala.collection.mutable.WrappedArray
  /** sorted by first key then second key */
  def fromSortedIterator[D](in: Iterator[Tuple3[String8, String8, D]], dim2Size: Int)(implicit manifest: Manifest[D]): StringKeyedMatrix[D, WrappedArray[D]] = {
    var rowRead = 0
    val keyB1 = scala.collection.mutable.ArrayBuffer[String8]()
    val keyB2 = scala.collection.mutable.ArrayBuffer[String8]()

    var rowBuff = ArrayBuffer[D]()
    val arrayBuff = ArrayBuffer[ArrayBuffer[D]]()
    var dim1counter = 0
    var dim2counter = 0
    in.foreach { element =>

      if (dim2counter == 0) arrayBuff.append(rowBuff)

      arrayBuff(dim1counter).append(element._3)
      if (dim2counter == dim2Size - 1) {
        dim2counter = 0
        dim1counter += 1
        keyB1.append(element._1)
        rowBuff = ArrayBuffer[D]()
        if (keyB2.size < dim2Size) { keyB2.append(element._2) }
      } else {
        dim2counter += 1
        if (keyB2.size < dim2Size) { keyB2.append(element._2) }
      }
    }

    new StringKeyedMatrix((arrayBuff.map(x => genericWrapArray(x.toArray))), keyB1.toSeq, keyB2.toSeq)
  }

  /** sorted by first key then second key */
  def fromSortedIteratorOfBit2(in: Iterator[Tuple3[String8, String8, Bit2]], dim2Size: Int): StringKeyedMatrix[Bit2, Bit2Buffer] = {
    var rowRead = 0
    val keyB1 = scala.collection.mutable.ArrayBuffer[String8]()
    val keyB2 = scala.collection.mutable.ArrayBuffer[String8]()

    var rowBuff = Bit2Buffer()
    val arrayBuff = ArrayBuffer[Bit2Buffer]()
    var dim1counter = 0
    var dim2counter = 0
    in.foreach { element =>
      if (dim2counter == 0) arrayBuff.append(rowBuff)

      arrayBuff(dim1counter) += element._3
      if (dim2counter == dim2Size - 1) {
        dim2counter = 0
        dim1counter += 1
        keyB1.append(element._1)
        rowBuff = Bit2Buffer()
        if (keyB2.size < dim2Size) { keyB2.append(element._2) }
      } else {
        dim2counter += 1
        if (keyB2.size < dim2Size) { keyB2.append(element._2) }
      }
    }

    new StringKeyedMatrix((arrayBuff), keyB1.toSeq, keyB2.toSeq)
  }

  def fromSortedIteratorWithKeySeqs[D](in: Iterator[D], keys1: Seq[String8], keys2: Seq[String8])(implicit manifest: Manifest[D]): StringKeyedMatrix[D, WrappedArray[D]] = {

    val dim2Size = keys2.size
    var rowBuff = ArrayBuffer[D]()
    val arrayBuff = ArrayBuffer[ArrayBuffer[D]]()
    var dim1counter = 0
    var dim2counter = 0
    in.foreach { element =>
      if (dim2counter == 0) arrayBuff.append(rowBuff)

      arrayBuff(dim1counter).append(element)
      if (dim2counter == dim2Size - 1) {
        dim2counter = 0
        dim1counter += 1
        rowBuff = ArrayBuffer[D]()
      } else {
        dim2counter += 1
      }
    }
    new StringKeyedMatrix((arrayBuff.map(x => genericWrapArray(x.toArray))), keys1, keys2)
  }

  def fromSortedIteratorWithKeySeqsOfBit2(in: Iterator[Bit2], keys1: Seq[String8], keys2: Seq[String8]): StringKeyedMatrix[Bit2, Bit2Buffer] = {

    val dim2Size = keys2.size
    var rowBuff = Bit2Buffer()
    val arrayBuff = ArrayBuffer[Bit2Buffer]()
    var dim1counter = 0
    var dim2counter = 0
    in.foreach { element =>
      if (dim2counter == 0) arrayBuff.append(rowBuff)

      arrayBuff(dim1counter) += (element)
      if (dim2counter == dim2Size - 1) {
        dim2counter = 0
        dim1counter += 1
        rowBuff = Bit2Buffer()
      } else {
        dim2counter += 1
      }
    }
    new StringKeyedMatrix((arrayBuff), keys1, keys2)
  }

  // def fromArray[D]( dataMatrix: IndexedSeq[IndexedSeq[D]],
  //                   keys1: Seq[String],
  //                   keys2: Seq[String] )( implicit manifest: Manifest[D] ): StringKeyedMatrix[D] = {
  // 
  //   new StringKeyedMatrix( dataMatrix, keys1, keys2 )
  // }  

}