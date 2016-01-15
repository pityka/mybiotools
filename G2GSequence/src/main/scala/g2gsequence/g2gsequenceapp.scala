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

package g2gsequence

import mybiotools.config.Config.configInstance
import java.io.File
import scala.collection.JavaConversions._
import mybiotools.{ readFasta, FastaSequenceData }

object G2GSequenceApp extends App {

  case class InputFile(fasta: File, ref: String)
  case class InputFasta(file: File, fasta: FastaSequenceData, ref: String)

  val inputFiles: List[InputFile] = {
    val inputfiles = io.Source.fromFile(configInstance.getString("inputs")).getLines.map(x => x.split(" ")(0) -> new File(x.split(" ")(1)))
    inputfiles.map(f => InputFile(f._2, f._1)).toList
  }

  val mapSequenceKey = {
    val id: PartialFunction[String, String] = { case x => x }
    val f = configInstance.getString("keyMap")
    if (f == "") None else Some(io.Source.fromFile(f).getLines.map { l =>
      val spl = mybiotools.fastSplitSetSeparator(l, Set('\t'))
      spl(0) -> spl(1)
    }.toMap.orElse(id))
  }

  val minorAlleleCount = configInstance.getInt("minorAlleleCount")

  val minimumDataCount = configInstance.getInt("minimumDataCount")

  val fastas = inputFiles.map {
    case InputFile(file, ref) => {
      val ff = readFasta(io.Source.fromFile(file))
      InputFasta(file, mapSequenceKey.map(x => mybiotools.transformFastaHeaders(ff)(x)).getOrElse(ff), ref)
    }

  }

  assert(fastas.map(_.ref).distinct.size == fastas.size, "Unique references needed")
  assert(fastas.forall(x => x.fasta.contains(x.ref)), "Alignments should contain the reference " + fastas.filter(x => !x.fasta.contains(x.ref)).map(_.ref))

  val includedSequences: Set[String] = {
    val file = configInstance.getString("includedSequences")
    if (file == "-") fastas.map(_.fasta.keys.toSet).reduce(_ ++ _).toList else io.Source.fromFile(file).getLines.toList
  }.filterNot(s => fastas.map(_.ref).contains(s)).toSet

  val forbiddenLetters = configInstance.getStringList("forbiddenLetters").toList

  val outfile = configInstance.getString("out")

  fastas.foreach {
    case InputFasta(file, fasta, ref) =>
      println(s" Positions with at least 95% coverage: $file : " + mybiotools.fastaCoverage(mybiotools.maskToSequenceKey(fasta, ref).filter(x => includedSequences.contains(x._1))).filter(_._2 >= 0.95).size)
      println("Sequences overlapping with filter: " + fasta.filter(x => includedSequences.contains(x._1)).size)
  }

  val called = fastas.map {
    case InputFasta(file, fasta, ref) =>
      file -> createBinaryPhenotypesWhereVariable(
        alignment = mybiotools.maskToSequenceKey(fasta, ref),
        referenceKey = ref,
        minimumDataCount = math.min(minimumDataCount, fasta.filter(x => includedSequences.contains(x._1)).size),
        minorAlleleCount = minorAlleleCount,
        includedSequenceKeys = includedSequences.toList,
        includePositionFilter0Based = None,
        excludePositionFilter0Based = None,
        forbiddenLetters = forbiddenLetters
      )
  }

  val variableSites = called.map(x => x._1 -> x._2._2)

  val mutations = called.map(_._2._1).reduce(_ ++ _)

  called.foreach {
    case (file, (mutations, variableSites)) =>
      println(file.getAbsolutePath + " : Number of variable positions in alignment: " + variableSites.size)
      println(file.getAbsolutePath + " : Number of mutation variables generated: " + mutations.size)
  }

  mybiotools.writeToFile(
    outfile + ".pheno.txt",
    formatPhenotypesForPlink(mutations, includedSequences.toList)
  )

  val tfamtped = formatPhenotypesForTFamTPed(mutations, includedSequences.toList)
  mybiotools.writeToFile(outfile + ".pheno.tped", tfamtped._2)
  mybiotools.writeToFile(outfile + ".pheno.tfam", tfamtped._1)

}
