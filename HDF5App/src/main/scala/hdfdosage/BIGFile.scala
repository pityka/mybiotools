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

package hdfdosage

import java.io.{ File, BufferedOutputStream, DataOutputStream, ByteArrayOutputStream, Writer, FileOutputStream, RandomAccessFile }

import java.nio.{ ByteBuffer, ByteOrder }
import mybiotools.gwascommons.{ GenomicMap, GenomicLocation, Individual }
import mybiotools.gwascommons.genotypedata.PDosageFileRowSummary
import mybiotools.stringstore.String8

object BIGFile {

  def convertIntToLittleEndian(i: Int): Int = {
    ByteBuffer.allocate(4)
      .order(ByteOrder.BIG_ENDIAN).putInt(i)
      .order(ByteOrder.LITTLE_ENDIAN)
      .getInt(0);
  }

  def convertIntFromLittleEndian(i: Int): Int = {
    ByteBuffer.allocate(4)
      .order(ByteOrder.LITTLE_ENDIAN).putInt(i)
      .order(ByteOrder.BIG_ENDIAN)
      .getInt(0);
  }

  def convertFloatToLittleEndian(i: Float): Float = {
    ByteBuffer.allocate(4)
      .order(ByteOrder.BIG_ENDIAN).putFloat(i)
      .order(ByteOrder.LITTLE_ENDIAN).getFloat(0)
  }

  def convertFloatFromLittleEndian(i: Float): Float = {
    ByteBuffer.allocate(4)
      .order(ByteOrder.LITTLE_ENDIAN).putFloat(i)
      .order(ByteOrder.BIG_ENDIAN).getFloat(0)
  }

  def writeBIG(
    bigfile: File,
    famFile: File,
    bimWriter: Writer,
    mergedIndividuals: Seq[Individual],
    dosageLineIterator: Iterator[Tuple2[PDosageFileRowSummary, Array[Float]]],
    genomicMap: GenomicMap,
    recodeToRecessive: Boolean = false,
    snpfilter: PDosageFileRowSummary => Boolean = (x => true)
  ): Unit = {

    val numberOfIndividuals = mergedIndividuals.size

    // write .fam
    mybiotools.writeToFile(
      famFile.getCanonicalPath,
      mergedIndividuals.map(_.toFamLine).mkString("\n")
    )

    val dataOS = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(bigfile)))

    val rgBuf = Array[Byte](
      'b', 'd',
      '0', '1', '0', '0',
      0x1a,
      1
    ) ++ {
        val bs = new ByteArrayOutputStream()
        val stream = new DataOutputStream(bs)
        List(0, numberOfIndividuals, 0, 0, 0, 0).map(convertIntToLittleEndian)
          .foreach(x => stream.writeInt(x))
        bs.toByteArray
      }

    dataOS.write(rgBuf, 0, rgBuf.size)

    var snpswritten = 0
    dosageLineIterator.foreach { tup =>
      val dosageLine = tup._2
      val snpName = tup._1.snpName
      val al1 = tup._1.al1
      val al2 = tup._1.al2
      var individualswritten = 0
      dosageLine.foreach { dosage =>
        val x = if (dosage == MissingValue) Float.NaN else (if (recodeToRecessive) mybiotools.gwascommons.gwas.GWAS.recodeGenotype(dosage, mybiotools.gwascommons.gwas.Recessive).toFloat else dosage)
        dataOS.writeFloat(convertFloatToLittleEndian(x))
        individualswritten += 1
      }
      assert(individualswritten == numberOfIndividuals)
      snpswritten += 1

      // write .bim
      val location = genomicMap.get(snpName)
      val chr = location.map(_.chromosome).getOrElse(".")
      val bp = location.map(_.basePairPosition.toString).getOrElse(".")
      bimWriter.write(chr + " " + snpName + " 0 " + bp + " " + al1 + " " + al2)
      bimWriter.write('\n')

    }

    dataOS.close
    bimWriter.close

    // Reopen the file and write the number of snps written.
    val rf = new RandomAccessFile(bigfile, "rw")
    rf.seek(8)
    rf.writeInt(convertIntToLittleEndian(snpswritten))
    rf.close

  }

  // def writeBIGIndividualMajor(bigfile: File,
  //   famFile: File,
  //   bimWriter: Writer,
  //   mergedIndividuals: IndexedSeq[Individual],
  //   dosageLineIterator: Iterator[Tuple2[PDosageFileRowSummary, Array[Float]]],
  //   snpfilter: PDosageFileRowSummary => Boolean = (x => true)): Unit = {

  //   // write .fam
  //   mybiotools.writeToFile(famFile.getCanonicalPath,
  //     mergedIndividuals.map(_.toLine).mkString("\n"))

  //   val dataOS = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(bigfile)))

  //   // transpose the matrix
  //   val lineArray = dosageLineIterator.toIndexedSeq

  //   val snpswritten = lineArray.size

  //   val rgBuf = Array[Byte](
  //     'b', 'd',
  //     '0', '1', '0', '0',
  //     0x1a,
  //     01 // mode snpmajor
  //   ) ++ {
  //       val bs = new ByteArrayOutputStream()
  //       val stream = new DataOutputStream(bs)
  //       stream.writeInt(snpswritten)
  //       stream.writeInt(mergedIndividuals.size)
  //       stream.writeInt(0)
  //       stream.writeInt(0)
  //       stream.writeInt(0)
  //       stream.writeInt(0)
  //       bs.toByteArray
  //     }

  //   dataOS.write(rgBuf, 0, rgBuf.size)

  //   for (
  //     indIdx <- 0 to mergedIndividuals.size - 1;
  //     snpIdx <- 0 to snpswritten - 1
  //   ) {
  //     val dosage = lineArray(snpIdx)._2.apply(indIdx) match {
  //       case x if x == MissingValue => Float.NaN
  //       case x => x
  //     }
  //     dataOS.writeFloat(dosage)
  //   }
  //   dataOS.close

  //   // write .bim
  //   lineArray.foreach { tup =>
  //     val snpName = tup._1.snpName
  //     val al1 = tup._1.al1
  //     val al2 = tup._1.al2
  //     bimWriter.write(snpName + " " + al1 + " " + al2)
  //     bimWriter.write('\n')

  //   }
  //   bimWriter.close

  // }

}