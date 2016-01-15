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

import mybiotools.gwascommons._
import mybiotools._
import associationresults._
import mybiotools.config.Config.configInstance
import java.io.File

object PlotManhattanApp extends App {

  val inputFile = configInstance.getString("input")

  val inputSource =
    if (inputFile == "-") io.Source.stdin
    else io.Source.fromFile(inputFile)

  val maxChr = configInstance.getInt("maxChr")

  val yLimit = configInstance.getDouble("yLimit") match {
    case -1 => None
    case x => Some(x)
  }

  val bonferroni = configInstance.getDouble("significanceLine") match {
    case -1 => None
    case x => Some(x)
  }

  val outputFile = configInstance.getString("output")

  val header = configInstance.getBoolean("headerLine")

  val externalMap = configInstance.getString("map")

  val scaleFactor = configInstance.getInt("scaleFactor")

  val genomicMapFile: Option[File] = if (externalMap == "-") None else
    Some(new File(externalMap))

  val pcolumnname = configInstance.getString("header.P")

  val snpcolumnname = configInstance.getString("header.SNP")

  val chr = configInstance.getString("header.CHR") match {
    case "-" => None
    case x => Some(x)
  }

  val pos = configInstance.getString("header.BP") match {
    case "-" => None
    case x => Some(x)
  }

  val test = configInstance.getString("header.TEST") match {
    case "-" => None
    case x => Some(x)
  }

  val nmiss = configInstance.getString("header.NMISS") match {
    case "-" => None
    case x => Some(x)
  }

  val effect = configInstance.getString("header.BETA") match {
    case "-" => None
    case x => Some(x)
  }

  val allele = configInstance.getString("header.A1") match {
    case "-" => None
    case x => Some(x)
  }

  val se = configInstance.getString("header.SE") match {
    case "-" => None
    case x => Some(x)
  }

  val maf = configInstance.getString("header.MAF") match {
    case "-" => None
    case x => Some(x)
  }

  val filterChr = configInstance.getString("chr") match {
    case "-" => None
    case x => Some(x.toInt)
  }

  val filterToBp = configInstance.getString("tobp") match {
    case "-" => None
    case x => Some(x.toInt)
  }

  val filterFromBp = configInstance.getString("frombp") match {
    case "-" => None
    case x => Some(x.toInt)
  }

  val writeOnlyJoinedData = configInstance.getBoolean("writeOnlyJoinedData")

  val associter = (if (header)
    readAssociationResultsFromFileWithHeader(
    inputSource,
    genomicMapFile,
    pcolumnname,
    snpcolumnname,
    chr,
    pos,
    effectheader = effect,
    alleleheader = allele,
    nonMissheader = nmiss,
    effectErrorHeader = se,
    testHeader = test,
    frqHeader = maf,
    phenoHeader = None
  )
  else
    readAssociationResultsFromFileWithoutHeader(
      inputSource.getLines,
      genomicMapFile,
      snpcolumnname.toInt,
      pcolumnname.toInt,
      chr.map(_.toInt),
      pos.map(_.toInt),
      effectcolumn = effect.map(_.toInt),
      allelecolumn = allele.map(_.toInt),
      nonMissColumn = nmiss.map(_.toInt),
      errorColumn = se.map(_.toInt),
      testColumn = test.map(_.toInt),
      frqColumn = maf.map(_.toInt),
      phenoColumn = None
    ))
    .filter(assoc => filterChr.map { (chr: Int) =>
      chromosomeNumberFromString(assoc.genomicLocation.chromosome) == chr &&
        filterToBp.map(to => assoc.genomicLocation.basePairPosition <= to).getOrElse(true) &&
        filterFromBp.map(from => assoc.genomicLocation.basePairPosition >= from).getOrElse(true)
    }.getOrElse(true))

  if (writeOnlyJoinedData) {
    openFileWriter(new java.io.File(outputFile)) { writer =>
      associter.foreach { a =>
        writer.write(a.toLine)
        writer.write('\n')
      }

    }
  } else {
    val plot = mybiotools.plots.ManhattanPlot.plotToPNG(associter, bonferroni, yLimit, maxChr, scale = scaleFactor)
    mybiotools.writeBinaryToFile(outputFile, plot)
  }

}