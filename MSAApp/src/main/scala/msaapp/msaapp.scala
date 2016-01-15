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

import mybiotools._
import mybiotools.sequence.alignment._
import mybiotools.sequence._
import java.io.File
import mybiotools.mapreduce.MapReduceTraversal._
import org.saddle._
import mybiotools.plots.ScatterPlot._
import mybiotools.eq._

object MSAApp extends App {

  val fasta = readFasta(scala.io.Source.stdin)

  val gapopen = args(0).toDouble
  val gapextend = args(1).toDouble
  val tranalign = args(2).toBoolean

  require(gapopen > 0.0, "gap penalty must be > 0")
  require(gapextend > 0.0, "gap penalty must be > 0")

  if (!tranalign) {

    val msa = GlobalPairwiseProfileAlignment.makeProfileAlignmentMultiStage((fasta).toSeq, GlobalPairwiseProfileAlignment.pdotp, gapopen, gapextend, 5)

    println(makeFastaString(msa, 80))
  } else {

    val aminoacidfasta = mybiotools.sequence.translateFasta(fasta, keepGapsInFrame = false)
    val aminoacidmsa = GlobalPairwiseProfileAlignment.makeProfileAlignmentMultiStage((aminoacidfasta).toSeq, GlobalPairwiseProfileAlignment.pdotp, gapopen, gapextend, 2)

    val alignednucleotide = padAlignment(
      aminoacidmsa.map {
      case (k, aa) =>
        k -> selectFromNucleotideAminoAcidPositions(
          fasta(k).filterNot(_ == '-').toUpperCase,
          aa.toUpperCase,
          (0 until aa.size).toSet
        )
    }.toMap,
      'N'
    )

    println(makeFastaString(alignednucleotide, 80))

  }

}