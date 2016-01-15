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

package mymatrix {

  object MyMatrix {

    /**
     * Factory
     *
     * @param d 2d data array
     * @param k ordered seq of keys
     * @param c Is this a symmetric matrix?
     * @tparam K type of keys
     * @tparam D data type
     */
    def apply[K <% Ordered[K], D](d: Array[Array[D]], k: Seq[K], c: Boolean = false): MyMatrix[K, D] = new MyMatrix(d, k, c)

  }

  /**
   * Square matrix of pairwise data. Representation is an `Array[Array[D]]`.
   *
   * 2D array with 1D keys. E.g. keys = (a,b,c), matrix = keys x keys.
   *
   * Immutable with the exception of `mapValuesInPlace` and `toArray`.
   *
   * @param data 2d data array
   * @param keys ordered seq of keys
   * @param copyLowerTriangleToUpper Is this a symmetric matrix?
   * @tparam K type of keys
   * @tparam D data type
   */
  class MyMatrix[K <% Ordered[K], D](
      private val data: Array[Array[D]],
      val keys: Seq[K],
      copyLowerTriangleToUpper: Boolean = false
  ) {
    assert(keys.size == data.size)

    private def arrayequal(that: Array[Array[D]]): Boolean = {
      if (that.size != size) false else {
        var b = true
        var i = 0
        while (i < size && b) {
          if (data(i).size != that(i).size) b = false else {
            var j = 0
            while (j < size && b) {
              if (data(i)(j) != that(i)(j)) b = false
              j += 1
            }
          }
          i += 1
        }
        b
      }
    }

    private val keymap = scala.collection.immutable.TreeMap[K, Int](keys.zipWithIndex: _*)

    /** Size of 1 side of the square matrix (size of keys). */
    val size = keys.size

    def canEqual(other: Any): Boolean = other.isInstanceOf[MyMatrix[_, _]]
    override def hashCode: Int = 41 * (41 + keymap.hashCode) + data.hashCode
    override def equals(that: Any): Boolean = that match {
      case t: MyMatrix[_, _] => (t canEqual this) && arrayequal(t.data.asInstanceOf[Array[Array[D]]]) && t.keys == keys
      case _ => false
    }
    override def toString = "MyMatrix(" + data.deep + "," + keys.toString + ")"

    if (copyLowerTriangleToUpper) {
      for (i <- 0 to size - 1; j <- 0 to size - 1; if (i < j)) {
        data(i)(j) = data(j)(i)
      }
    }

    def contains(k1: K) = keys.contains(k1)

    /** @return value associated with keys `k1` and `k2`. */
    def apply(k1: K, k2: K): D = {
      data(keymap(k1))(keymap(k2))
    }

    /** Maps values of the matrix and creates a new one. Deep "copy" of data! */
    def mapValues[U](f: D => U)(implicit manifest: Manifest[U]): MyMatrix[K, U] = {
      val copied = (for (i <- 0 to size - 1) yield {
        (for (j <- 0 to size - 1) yield f(data(i)(j))).toArray
      }).toArray
      new MyMatrix(copied, keys)
    }

    /** Map data values in place. No copy, overwrites mutable state. */
    def mapValuesInPlace[U](f: D => D): Unit = {
      for (i <- 0 to size - 1) {
        for (j <- 0 to size - 1) data(i)(j) = f(data(i)(j))
      }
    }

    /** Returns the underlying representation arrays. This class shares state with the returned array! */
    def toArray(implicit manifest: Manifest[D]): Array[Array[D]] = data

  }
}

/** Provides data structure for storing pairwise data (square matrix) */
package object mymatrix {

  /**
   * Calculates pairwise "distance" matrix
   * @tparam K type of key
   * @tparam D type of datapoints
   * @tparam U type of "distance" data
   * @param alignment Input data
   * @param f Distance function. No need to be a proper metric but needs to be commutative.
   */
  def computePairwiseMatrix[K, U, D](alignment: Map[K, D])(f: (D, D) => U)(implicit manifest: Manifest[U], ord: Ordering[K]): MyMatrix[K, U] = {
    val seqs = scala.collection.mutable.ListBuffer[K]()
    var ii = 0
    val ar: Array[Array[U]] = (for (i <- alignment) yield {
      ii += 1
      seqs append i._1
      var jj = 0
      (for (j <- alignment) yield {
        jj += 1
        if (ii >= jj) f(i._2, j._2)
        else null.asInstanceOf[U]
      }).toArray
    }).toArray

    MyMatrix(ar, seqs.toSeq, true)
  }
}