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
import org.saddle.index._
import mybiotools.gwascommons._
import java.io.File

object SaddleFrameExtend {

  def joinSPreserveColIx[RX, CX, T](frame: Frame[RX, CX, T], other: Series[RX, T],
    newColIx: CX, how: JoinType = LeftJoin)(implicit ev1: ST[CX], ord1: Ordering[CX]): Frame[RX, CX, T] = {
    val resultingFrame = frame.joinS(other, how)
    val newColIndex = frame.colIx.concat(Index(newColIx))
    resultingFrame.setColIndex(newColIndex)
  }

  /** Removes those column names who are part of @param columns and are identical with any other column. */
  def distinctColumns[RX, CX, T](frame: Frame[RX, CX, T], columns: Seq[CX]): Seq[CX] = {

    @scala.annotation.tailrec
    def recurse(part1: Seq[CX], part2: Seq[CX]): Seq[CX] = {
      if (part1.size == 0) part2
      else {
        val head = part1.head
        val a1: Seq[T] = frame.firstCol(head).toSeq.map(_._2)

        val filtered = {

          val sameashead = part1.drop(1).filter { c2 =>
            columns.contains(c2) && a1 == frame.firstCol(c2).toSeq.map(_._2)
          }

          part1.drop(1).filterNot(x => sameashead.contains(x))

        }
        recurse(filtered, part2 :+ head)
      }
    }

    recurse(frame.colIx.toSeq, Nil)

  }

}

object FrameToPlink {

  def frameToMach(frame: Frame[Individual, String, Double]): (File, File) = {
    val mlinfo = TempFile.createTempFile(".mlinfo")
    val mldose = TempFile.createTempFile(".mldose")
    openFileWriter(mlinfo) { mlinfowriter =>
      openFileWriter(mldose) { mldosewriter =>
        mlinfowriter.write("SNP\tAl1\tAl2\tFreq1\tMAF\tAvgCall\tRsq\n")
        frame.toColSeq.foreach {
          case (snp, series) =>
            val seq = series.toVec.toSeq
            val nonmiss = seq.filter(x => !x.isNaN)
            // val frq = nonmiss.reduce(_ + _) / (nonmiss.size * 2)
            mlinfowriter.write(snp + "\tA\tT\t0.0\t0.0\t0.0\t1.0\n")
        }
        frame.toRowSeq.foreach {
          case (ind, series) =>
            val seq = series.toVec.toSeq
            mldosewriter.write(ind.FID + "->" + ind.IID + " DOSE " + seq.mkString(" ") + "\n")
        }
      }
    }
    (mldose, mlinfo)
  }

  def frameToPlink(frame: Frame[Individual, String, Double], missingValue: String): String = {
    val cols = frame.colIx.toSeq

    val rowseq: Seq[(String, String, Seq[Option[Double]])] = frame.toRowSeq.map {
      case (ind, series) =>
        (ind.FID.value, ind.IID.value, series.values.toSeq.map(_ match {
          case x if x.isNaN => None
          case x => Some(x)
        }))
    }.toSeq

    import mybiotools.tabular._
    import TabSerialization._
    val protocol = new PlinkProtocol(missingValue)
    import protocol._

    TableWriter.write(toTab((rowseq, Seq("FID", "IID") ++ cols.toSeq)))(Separator(' '))

  }

  def writeFrameToPlink(frame: Frame[Individual, String, Double], missingValue: String, writer: java.io.Writer): Unit = {

    val cols = frame.colIx.toSeq

    writer.write("FID IID" + cols.toSeq.mkString(" ", " ", "") + "\n")
    frame.toRowSeq.foreach {
      case (ind, series) =>
        writer.write(ind.FID.value + " " + ind.IID.value)
        val d = series.values.toSeq.map(_ match {
          case x if x.isNaN => missingValue
          case x => x.toString
        }).mkString(" ", " ", "")
        writer.write(d + "\n")
    }

  }

  def frameToPlinkNoHeader(frame: Frame[Individual, String, Double], missingValue: String): String = {
    val cols = frame.colIx.toSeq

    val rowseq: Seq[(String, String, Seq[Option[Double]])] = frame.toRowSeq.map {
      case (ind, series) =>
        (ind.FID.value, ind.IID.value, series.values.toSeq.map(_ match {
          case x if x.isNaN => None
          case x => Some(x)
        }))
    }.toSeq

    import mybiotools.tabular._
    import TabSerialization._
    val protocol = new PlinkProtocol(missingValue)
    import protocol._

    TableWriter.write(toTab(rowseq))(Separator(' '))

  }

}

