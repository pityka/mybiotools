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

import mybiotools.gwascommons._
import mybiotools._
import java.io.File

object DosageWriter {

  def writePDose(writer: java.io.Writer, mergedIndividuals: Seq[Individual], snpDataIterator: Iterator[(PDosageFileRowSummary, Array[Float])], recodeToRecessive: Boolean): Unit = {
    writer.write("SNP A1 A2 ")
    writer.write(mergedIndividuals.map(_.toLine).mkString(" "))
    writer.write('\n')

    // val decimalFormatter = new java.text.DecimalFormat("0.0#")
    val sb = new java.lang.StringBuffer(4)

    snpDataIterator.foreach { tuple =>
      val arrayOfDosages = tuple._2
      val snpName = tuple._1.snpName
      val a1 = tuple._1.al1
      val a2 = tuple._1.al2
      writer.write(snpName + " " + a1 + " " + a2)
      arrayOfDosages.foreach { dosage =>
        writer.write(' ')
        formatDouble2DecimalTo(sb, if (recodeToRecessive) mybiotools.gwascommons.gwas.GWAS.recodeGenotype(dosage, mybiotools.gwascommons.gwas.Recessive) else dosage)
        writer.write(sb.toString)
        sb.setLength(0)
      }
      writer.write('\n')
    }

  }
}