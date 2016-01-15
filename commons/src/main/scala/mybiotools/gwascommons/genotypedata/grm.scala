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

package mybiotools.gwascommons.genotypedata

import org.ejml.data.DenseMatrix64F
import org.saddle._

import mybiotools.gwascommons._
import mybiotools.mapreduce._
import mybiotools.stringstore._
import java.io.File
import mybiotools._
import mybiotools.eq._
import scala.util._
import mybiotools.pcanew._

object GRM {

  /**
   * Calculates Genotype Relatedness Matrix.
   *
   * This implements the same algorithm as gcta in a simple form.
   * The getGRMFromAutosomes method implements the same faster.
   * This is here to for testing only as both passes the same tests.
   */
  private[genotypedata] def fromFrame[I, T](f: Frame[I, T, Double])(implicit st1: ST[T], o1: Ordering[T], st2: ST[I], o2: Ordering[I]): Frame[I, I, Double] = {

    val standardized: Frame[I, T, Double] = Frame(f.toColSeq.map {
      case (colname, col) =>
        val af = col.sum / (2 * col.length.toDouble)
        colname -> (
          if (af <= 1E-50 || 1.0 - af <= 1E-50) col.mapValues(x => 0.0)
          else ((col - 2 * af) / math.sqrt((2 * af * (1.0 - af))))
        ).proxyWith(col.map(x => x._1 -> 0.0))
    }: _*)

    val pairwisenonmissing: Frame[I, I, Double] = {
      val missing: Frame[I, T, Double] = f.map(x => (x._1, x._2, if (x._3.isNaN) 0.0 else 1.0))
      val m = missing.toMat
      Frame(m mult m.T, missing.rowIx, missing.rowIx)
    }

    val m = standardized.toMat
    Frame(m mult m.T, standardized.rowIx, standardized.rowIx) / pairwisenonmissing
  }

  def recursivelyDropSamples[T](f: Frame[T, T, Double], minimum: Double)(implicit st1: ST[T], o1: Ordering[T]): Frame[T, T, Double] = {

    def loop(f: Frame[T, T, Double]): Frame[T, T, Double] = {
      val rowswithmissing: Seq[(T, Int)] = f.toRowSeq.map(x => x._1 -> (x._2.toVec.countif(k => k < minimum))).filter(_._2 > 0).sortBy(_._2)
      rowswithmissing.lastOption match {
        case None => f
        case Some(r) => loop(f.row(f.rowIx.toSeq.filterNot(_ === r._1): _*).col(f.colIx.toSeq.filterNot(_ === r._1): _*))
      }
    }

    loop(f)

  }

  def read(trunk: String): Frame[Individual, Individual, Double] = openSource(trunk + ".grm.id") { sid =>
    openSource(trunk + ".grm.gz") { sgz =>
      read(sgz, sid)
    }
  }

  def read(grm: scala.io.Source, id: scala.io.Source): Frame[Individual, Individual, Double] = {
    val ids: IndexedSeq[Individual] = id.getLines.map { l =>
      val spl = fastSplitSetSeparator(l, Set(' ', '\t'))
      Individual(spl(0), spl(1))
    }.toIndexedSeq

    val empty = new DenseMatrix64F(ids.size, ids.size)
    grm.getLines.foreach { l =>
      val spl = fastSplitSetSeparator(l, Set(' ', '\t'))
      val i = spl(0).toInt - 1
      val j = spl(1).toInt - 1
      val v = spl(3).toDouble
      empty.set(i, j, v)
      empty.set(j, i, v)
    }
    val idx = Index(ids: _*)
    Frame(Mat(ids.size, ids.size, empty.getData), idx, idx)

  }

  def write(grm: Frame[Individual, Individual, Double], writerGRM: java.io.Writer, writerIDs: java.io.Writer): Unit = write(grm, writerGRM, writerIDs, (x: Individual) => x.toLine)

  def write[T](grm1: Frame[T, T, Double], writerGRM: java.io.Writer, writerIDs: java.io.Writer, toLine: T => String): Unit = {
    assert(grm1.rowIx.toSeq.toSet == grm1.colIx.toSeq.toSet)
    val grm = if (grm1.rowIx.toSeq != grm1.colIx.toSeq) grm1.sortedRIx.sortedCIx else grm1
    grm.rowIx.toSeq.foreach { ind =>
      writerIDs.write(toLine(ind) + "\n")
    }

    var i = 0
    var j = 0
    while (i < grm.rowIx.length) {
      while (j <= i) {
        writerGRM.write((i + 1) + "\t" + (j + 1) + "\t1\t" + grm.raw(i, j) + "\n")
        j += 1
      }
      j = 0
      i += 1
    }
  }

  def writeFastlmm(grm: Frame[Individual, Individual, Double], writer: java.io.Writer): Unit = {
    writer.write("var\t")
    writer.write(grm.rowIx.toSeq.map(_.toLine).mkString("\t") + "\n")
    grm.toRowSeq.foreach {
      case (rx, row) =>
        writer.write(rx.toLine + "\t")
        writer.write(row.toVec.toSeq.mkString("\t") + "\n")
    }
  }

  private class GRMMapReduce(n: Int, missingValue: Float) extends MapReduceScheme[Seq[(PDosageFileRowSummary, Array[Float])], (DenseMatrix64F, DenseMatrix64F, Int)] {

    val map = (group: Seq[(mybiotools.gwascommons.genotypedata.PDosageFileRowSummary, Array[Float])]) => {

      val m = group.size

      val wmatrix = new DenseMatrix64F(n, m)
      val missingmatrix = new DenseMatrix64F(n, m)
      org.ejml.ops.CommonOps.add(missingmatrix, 1.0)
      val missingpositions = Array.fill(n)(scala.collection.mutable.ArrayBuffer[Int]())
      val nmatrix = new DenseMatrix64F(n, n)
      val wwt = new DenseMatrix64F(n, n)

      group.zipWithIndex.foreach {
        case ((pdoserowsummary, array), j) =>
          val allele1Frequency = pdoserowsummary.sumAl1Dosage / (2.0 * pdoserowsummary.count)
          val a1ftwice = 2 * allele1Frequency
          val sd = math.sqrt(a1ftwice * (1.0 - allele1Frequency))
          var i = 0
          while (i < n) {
            val x_i = array(i)
            if (sd < 1E-50 || x_i.isNaN || x_i == missingValue) {
              if (x_i.isNaN || x_i == missingValue) {

                missingmatrix.set(i, j, 0.0)
                missingpositions(i).append(j)
              }
              wmatrix.set(i, j, 0.0)
            } else {
              wmatrix.set(i, j, (x_i - a1ftwice) / sd)
            }

            i += 1
          }
      }

      mybiotools.stat.LinearRegression.BLAS match {
        case Some(blas) => blas.dgemm("T", "N", wmatrix.getNumRows, wmatrix.getNumRows, wmatrix.getNumCols, 1.0, wmatrix.data, wmatrix.getNumCols, wmatrix.data, wmatrix.getNumCols, 0.0, wwt.data, wwt.getNumRows)
        case None => org.ejml.ops.CommonOps.multOuter(wmatrix, wwt)
      }

      var i = 0
      var j = 0
      var k = 0
      while (i < n) {
        val mMinusmissingpositionsISize = m - missingpositions(i).size
        while (j <= i) {
          var comm = 0
          val missingpositionsJSize = missingpositions(j).size
          val missingpositionsJ = missingpositions(j)
          while (k < missingpositionsJSize) {
            if (missingmatrix.get(i, missingpositionsJ(k)) == 1.0) {
              comm += 1
            }
            k += 1

          }
          nmatrix.set(i, j, mMinusmissingpositionsISize - comm)
          k = 0
          j += 1
        }
        j = 0
        i += 1
      }

      (wwt, nmatrix, m)

    }

    val reduce = (op1: (DenseMatrix64F, DenseMatrix64F, Int), op2: (DenseMatrix64F, DenseMatrix64F, Int)) => {
      org.ejml.ops.CommonOps.addEquals(op1._1, op2._1)
      org.ejml.ops.CommonOps.addEquals(op1._2, op2._2)
      (op1._1, op1._2, op1._3 + op2._3)
    }

  }

  def plotGRMToFile[T](grm: Frame[T, T, Double], file: java.io.File)(implicit ct: scala.reflect.ClassTag[T], ord: math.Ordering[T]) {
    mybiotools.writeBinaryToFile(
      file.getCanonicalPath,
      mybiotools.plots.renderToByteArray(
        plotGRM(grm, "GRM"),
        "image/png", 30.0
      )
    )
  }

  def plotGRM[T](grm: Frame[T, T, Double], main: String)(implicit ct: scala.reflect.ClassTag[T], ord: math.Ordering[T]) = {

    // this could be weighted by eigenvalues
    val sampleOrder = Index(clustering.traverse(clustering.clusterFrameByRows(grm)(clustering.euclideanDistance)): _*)

    val reorderedFrame = grm.reindex(rix = sampleOrder, cix = sampleOrder)

    val max = reorderedFrame.toMat.toVec.max.get
    val min = reorderedFrame.toMat.toVec.min.get
    val colormap = new de.erichseifert.gral.plots.colors.LinearGradient(new java.awt.Color(0, 0, 0), new java.awt.Color(0, 255, 0))
    colormap.setRange(min, max)

    mybiotools.plots.Raster.createArrayBackedRasterPlot(reorderedFrame, main = main, colormap = colormap)
  }

  def getPCA(
    data: SNPMajorIterators,
    numberOfAxes: Int,
    batchSize: Int,
    threads: Int
  ): PCAResult[Individual] =
    mybiotools.pcanew.pcaFromGRM(
      getGRMFromAutosomes(data, batchSize, threads)._1,
      numberOfAxes
    )

  def getGRMFromAutosomes(
    snpreaders: SNPMajorIterators,
    batchSize: Int,
    threads: Int
  ): (Frame[Individual, Individual, Double], Int) = getGRMFromAutosomes(snpreaders.snpIterator, snpreaders.individuals, snpreaders.missingValue, batchSize, threads)

  def getGRMFromAutosomes(
    dosages: Iterator[Tuple2[PDosageFileRowSummary, Array[Float]]],
    individuals: Seq[Individual],
    missingValue: Float,
    batchSize: Int,
    threads: Int
  ): (Frame[Individual, Individual, Double], Int) = {

    if (!dosages.hasNext) throw new RuntimeException("Empty iterator.")
    else {

      val n = individuals.size

      val iter: Iterator[List[(PDosageFileRowSummary, Array[Float])]] = dosages.grouped(batchSize).map(_.toList)

      MapReduceTraversal.traverse(iter, new GRMMapReduce(n, missingValue), threads, threads) match {
        case Success((matrix, nmatrix, snpcount)) => {
          org.ejml.ops.CommonOps.elementDiv(matrix, nmatrix)
          val t = new DenseMatrix64F(matrix.getNumRows, matrix.getNumCols)
          org.ejml.ops.CommonOps.transpose(matrix, t)
          org.ejml.ops.SpecializedOps.copyTriangle(t, matrix, true)

          val idx = Index(individuals: _*)
          Frame(Mat(individuals.size, individuals.size, matrix.getData), idx, idx) -> snpcount
        }
        case Failure(e) => throw new RuntimeException("Error in GRMMapReduce. ", e)
      }
    }

  }

  def getGRMWithFastlmmFromBed(bed: File, bim: File, fam: File, snps: Seq[String8], phenofile: File, phenoIndex: Int): File = {
    val tmpsubset = TempFile.createTempFile(".plink")
    val tmplist = TempFile.createTempFile(".plink")
    val tmpgrm = TempFile.createTempFile(".sim")
    mybiotools.writeToFile(tmplist.getAbsolutePath, snps.map(_.value).mkString("\n"))
    val cmd = "plink --bed " + bed.getAbsolutePath + " --bim " + bim.getAbsolutePath + " --fam " + fam.getAbsolutePath + " --make-bed --out " + tmpsubset.getAbsolutePath + " --noweb --extract " + tmplist.getAbsolutePath

    val (stdout, stderr, success) = execGetStreamsAndCode(cmd)

    if (!success) throw new RuntimeException("error in plink" + stdout.mkString("\n") + stderr.mkString("\n"))

    val cmdfastlmm = "fastlmmc -bfilesim " + tmpsubset.getAbsolutePath + " -simOut " + tmpgrm.getAbsolutePath + " -runGwasType NORUN -pheno " + phenofile.getAbsolutePath + " -mpheno " + phenoIndex

    val (stdout2, stderr2, success2) = execGetStreamsAndCode(cmdfastlmm)

    if (!success2) throw new RuntimeException("error in fastlmm" + stdout2.mkString("\n") + stderr2.mkString("\n"))

    tmpgrm

  }

  def getGRMWithFastlmmFromPDose(pdose: File, phenofile: File, phenoIndex: Int): File = {

    val tmpgrm = TempFile.createTempFile(".sim")

    val famfile = {
      val tmpfam = TempFile.createTempFile(".fam")
      writeToFile(
        tmpfam.getCanonicalPath,
        getIndividualsFromPDoseHeader(openSource(pdose.getCanonicalPath)(_.getLines.next)).map(_.toFamLine).mkString("\n")
      )
      tmpfam
    }

    val common = putBesides((pdose -> ".dat" :: famfile -> ".fam" :: Nil): _*)

    val cmdfastlmm = "fastlmmc -dfile1Sim " + common.getAbsolutePath + " -simOut " + tmpgrm.getAbsolutePath + " -runGwasType NORUN -pheno " + phenofile.getAbsolutePath + " -mpheno " + phenoIndex

    val (stdout2, stderr2, success2) = execGetStreamsAndCode(cmdfastlmm, unsuccessfulOnErrorStream = false)

    if (!success2) throw new RuntimeException("error in fastlmm" + stdout2.mkString("\n") + stderr2.mkString("\n"))

    tmpgrm

  }

}