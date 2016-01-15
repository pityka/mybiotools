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

package dispensability

import mybiotools.config.Config.configInstance
import mybiotools._
import mybiotools.eq._
import java.io.File
import htsjdk.variant.variantcontext.writer.VariantContextWriterFactory
import htsjdk.samtools.SAMSequenceDictionary
import htsjdk.samtools.SAMSequenceRecord
import scala.collection.JavaConversions._
import htsjdk.variant.vcf.{ VCFHeader, VCFHeaderLine }
import htsjdk.variant.variantcontext.VariantContextBuilder
import htsjdk.variant.variantcontext.GenotypeBuilder
import htsjdk.variant.variantcontext._
import htsjdk.variant.vcf._
import mybiotools.stringstore._
import mybiotools.gwascommons._
import vcfhelpers._

object MakeNoGTFile extends App {
  implicit private val codec = scala.io.Codec("ISO-8859-1")
  val files = openSource(args(0))(_.getLines.toList)
  val out = new java.io.File(args(1))

  openZippedFileWriter(out) { writer =>

    files.foreach { file =>
      openSource(file)(
        _.getLines
          .filterNot(_.head === '#')
          .map(line => fastSplit1WideSeparatorIterator(line, '\t').take(7).toVector)
          .filter(_(6) === "PASS")
          .foreach { splitted =>
            val cleanedchr = if (splitted(0).startsWith("chr") || splitted(0).startsWith("Chr")) splitted(0).drop(3) else splitted(0)
            writer.write(List(cleanedchr, splitted(1), ".", splitted(3), splitted(4), "100", splitted(6), ".").mkString("\t") + "\n")
          }
      )
    }
  }

}