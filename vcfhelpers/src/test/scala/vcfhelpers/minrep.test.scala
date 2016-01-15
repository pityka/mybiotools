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
package vcfhelpers

import mybiotools.gwascommons._
import java.io.File
import org.scalatest.FunSpec
import org.scalatest.Matchers
import collection.JavaConversions._

class MinRepSpec extends FunSpec with Matchers {

  describe("Minimal Representation") {
    it("examples from http://www.cureffi.org/2014/04/24/converting-genetic-variants-to-their-minimal-representation/") {
      minimalRepresentation("CTCC", "<SDFSDF>", 1001) should equal(MinimalRepresentation("CTCC", "<SDFSDF>", 1001))
      minimalRepresentation("CTCC", "CCC", 1001) should equal(MinimalRepresentation("CT", "C", 1001))
      minimalRepresentation("CTCC", "C", 1001) should equal(MinimalRepresentation("CTCC", "C", 1001))
      minimalRepresentation("CTCC", "CCCC", 1001) should equal(MinimalRepresentation("T", "C", 1002))
    }
    it("Antonio's file") {
      val file = getClass.getResource("/").getPath + "/minrep.txt"
      val regexp = """\d+\:(\d+)\_([ATGC]+)\>([ATGC]+)\s\d+\:(\d+)\_([ATGC]+)\>([ATGC]+)""".r
      // 10:100147021_AG>AGG	10:100147021_A>AG
      mybiotools.openSource(file)(_.getLines.foreach { line =>
        val regexp(pos1, ref1, alt1, pos2, ref2, alt2) = line
        minimalRepresentation(ref1, alt1, pos1.toInt) should equal(MinimalRepresentation(ref2, alt2, pos2.toInt))
      })
    }
    it("Antonio's file 2") {
      val file = getClass.getResource("/").getPath + "/merged.nogt.vcf_uniq.txt_MinRep_correspondences.txt"
      val regexp = """\d+\:(\d+)\_([ATNGC]+)\>([ATGCN]+)\s\d+\:(\d+)\_([NATGC]+)\>([ATGCN]+)""".r
      // 10:100147021_AG>AGG  10:100147021_A>AG
      mybiotools.openSource(file)(_.getLines.foreach { line =>
        val regexp(pos1, ref1, alt1, pos2, ref2, alt2) = line
        minimalRepresentation(ref1, alt1, pos1.toInt) should equal(MinimalRepresentation(ref2, alt2, pos2.toInt))
        minimalRepresentation(ref2, alt2, pos2.toInt) should equal(MinimalRepresentation(ref2, alt2, pos2.toInt))
      })
    }

  }
}