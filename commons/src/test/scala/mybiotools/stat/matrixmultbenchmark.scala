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
package stat
import LinearRegression._

import org.scalatest.FunSpec
import org.scalatest.Matchers
import mybiotools.gwascommons.Individual
import org.ejml.simple.SimpleMatrix
import org.ejml.ops.MatrixFeatures
import org.ejml.data.DenseMatrix64F
import org.ejml.ops.CommonOps
import org.ejml.alg.dense.mult.MatrixVectorMult
import org.ejml.ops.RandomMatrices

class MatrixMultBenchmark extends FunSpec with Matchers {

  def generateRandomMatrix(rows: Int, cols: Int) = {
    val rand = new java.util.Random(1)
    RandomMatrices.createRandom(rows, cols, rand)
  }
  def measure(n: Int)(f: => Unit) = {
    val values = ((1 to n).map { i =>
      val t1 = System.nanoTime
      f
      val t2 = System.nanoTime - t1
      t2
    }.drop((n * 0.9).toInt))
    values.sum / (values.size.toDouble * 1E9)
  }

  def run(rows: Int, cols: Int, print: String) {
    val matrix1 = generateRandomMatrix(rows, cols)
    val matrix2 = generateRandomMatrix(cols, rows)
    val matrix2T = SimpleMatrix.wrap(matrix2).transpose.getMatrix

    val ref = new DenseMatrix64F(rows, rows)
    org.ejml.alg.dense.mult.MatrixMatrixMult.mult_reorder(matrix1, matrix2, ref)
    val N = 100000

    {
      val ret = new DenseMatrix64F(rows, rows)
      val avg = measure(N) {
        org.ejml.alg.dense.mult.MatrixMatrixMult.mult_reorder(matrix1, matrix2, ret)
      }
      println(print + "ejml reorder: " + avg)
    }
    {
      val ret = new DenseMatrix64F(rows, rows)

      val avg = measure(N) {
        org.ejml.alg.dense.mult.MatrixMatrixMult.multTransB(matrix1, matrix2T, ret)
      }
      ret.toString should equal(ref.toString)

      println(print + "ejml reorder pretransposed: " + avg)
    }
    {
      val ret = new DenseMatrix64F(rows, rows)

      val avg = measure(N) {
        org.ejml.alg.dense.mult.MatrixMatrixMult.mult_small(matrix1, matrix2, ret)
      }
      ret.toString should equal(ref.toString)
      println(print + "ejml small: " + avg)
    }
    {
      val ret2 = new DenseMatrix64F(rows, rows)

      val blas = LinearRegression.BLAS.get
      val avg = measure(N) {
        blas.dgemm("N", "N", matrix2.numCols, matrix1.numRows, matrix2.numRows, 1.0, matrix2.getData, matrix2.numCols, matrix1.getData, matrix1.numCols, 0.0, ret2.getData, ret2.numRows)
      }
      ret2.toString should equal(ref.toString)
      println(print + "dgemm: " + avg)
    }

    {
      val ret2 = new DenseMatrix64F(rows, rows)

      val blas = LinearRegression.BLAS.get
      val avg = measure(N) {
        blas.dgemm("T", "N", matrix2T.numRows, matrix1.numRows, matrix2T.numCols, 1.0, matrix2T.getData, matrix2T.numCols, matrix1.getData, matrix1.numCols, 0.0, ret2.getData, ret2.numRows)
      }
      ret2.toString should equal(ref.toString)
      println(print + "dgemm pretransposed: " + avg)
    }

  }

  describe("benchmarks") {
    ignore("100x5") {
      run(100, 5, "100x5")
    }
    ignore("5x500") {
      run(5, 500, "5x500")
    }
    ignore("5x5000") {
      run(5, 5000, "5x5000")
    }
    ignore("100x100") {
      run(100, 100, "100x100")
    }
  }

}