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

import mybiotools.{ fastSplitSeparatorIterator }
import mybiotools.stringstore._
import mybiotools.gwascommons.genotypedata.PDosageFileRowSummary

object PDosageFileRowSummaryHDFHelpers {

  def toDTO(pdfrs: PDosageFileRowSummary) = {
    import pdfrs._
    new SNPDTO(snpName, al1, al2, sumAl1Dosage, sumAl2Dosage, count, maf)
  }

  def fromSNPDTO(dto: SNPDTO, totalCount: Int): PDosageFileRowSummary = PDosageFileRowSummary(StringStore(new String(dto.snpName).trim), StringStore(new String(dto.allele1).trim), StringStore(new String(dto.allele2).trim), dto.sumAl1Dosage, dto.sumAl2Dosage, dto.count, dto.maf, totalCount - dto.count)

  def fromSNPDTOold(dto: SNPDTOold, totalCount: Int): PDosageFileRowSummary = PDosageFileRowSummary(StringStore(new String(dto.snpName).trim), StringStore(new String(dto.allele1).trim), StringStore(new String(dto.allele2).trim), dto.sumAl1Dosage, dto.sumAl2Dosage, dto.count, dto.maf, totalCount - dto.count)

}