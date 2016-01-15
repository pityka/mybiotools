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
package gwascommons

import gwascommons._
import org.scalatest.FunSpec
import org.scalatest.Matchers

class GenotypesSpec extends FunSpec with Matchers {
  import gwascommons._

  describe("Genotypes") {
    it("fsf") {
      import DiscreteGenotypes._
      Heterozygous.isHet should equal(true)
      Heterozygous.isHomVar should equal(false)
      Heterozygous.isHomRef should equal(false)
      Heterozygous.noCall should equal(false)
      Heterozygous.variantAlleleDosage should equal(1.0)

      HomozygousRef.isHet should equal(false)
      HomozygousRef.isHomVar should equal(false)
      HomozygousRef.isHomRef should equal(true)
      HomozygousRef.noCall should equal(false)
      HomozygousRef.variantAlleleDosage should equal(0.0)

      HomozygousVar.isHet should equal(false)
      HomozygousVar.isHomVar should equal(true)
      HomozygousVar.isHomRef should equal(false)
      HomozygousVar.noCall should equal(false)
      HomozygousVar.variantAlleleDosage should equal(2.0)

      Missing.isHet should equal(false)
      Missing.isHomVar should equal(false)
      Missing.isHomRef should equal(false)
      Missing.noCall should equal(true)
      evaluating(Missing.variantAlleleDosage) should produce[RuntimeException]
    }
  }

  describe("Alleles") {

    it("Adenine is snp and purine") {
      Allele_A_REF.snp should equal(true)
      Allele_A_VAR.snp should equal(true)
      Allele_A_REF.purine should equal(true)
      Allele_A_VAR.purine should equal(true)
      Allele_A_REF.pirimidine should equal(false)
      Allele_A_VAR.pirimidine should equal(false)

    }

    it("Guanine is snp and purine") {
      Allele_G_REF.snp should equal(true)
      Allele_G_VAR.snp should equal(true)
      Allele_G_REF.purine should equal(true)
      Allele_G_VAR.purine should equal(true)
      Allele_G_REF.pirimidine should equal(false)
      Allele_G_VAR.pirimidine should equal(false)

    }

    it("Thymine is snp and pirimidine") {
      Allele_T_REF.snp should equal(true)
      Allele_T_VAR.snp should equal(true)
      Allele_T_REF.purine should equal(false)
      Allele_T_VAR.purine should equal(false)
      Allele_T_REF.pirimidine should equal(true)
      Allele_T_VAR.pirimidine should equal(true)

    }

    it("Cytosine is snp and pirimidine") {
      Allele_C_REF.snp should equal(true)
      Allele_C_VAR.snp should equal(true)
      Allele_C_REF.purine should equal(false)
      Allele_C_VAR.purine should equal(false)
      Allele_C_REF.pirimidine should equal(true)
      Allele_C_VAR.pirimidine should equal(true)

    }

    it("Anything is snp and not pirimidine and not purine") {
      Allele_N_REF.snp should equal(true)
      Allele_N_VAR.snp should equal(true)
      Allele_N_REF.purine should equal(false)
      Allele_N_VAR.purine should equal(false)
      Allele_N_REF.pirimidine should equal(false)
      Allele_N_VAR.pirimidine should equal(false)

    }

    it("Insertion is NOT snp and not pirimidine and not purine") {
      Allele_INSERT_REF.snp should equal(false)
      Allele_INSERT_VAR.snp should equal(false)
      Allele_INSERT_REF.purine should equal(false)
      Allele_INSERT_VAR.purine should equal(false)
      Allele_INSERT_REF.pirimidine should equal(false)
      Allele_INSERT_VAR.pirimidine should equal(false)

    }

    it("Not called allele is snp and not pirimidine and not purine") {
      Allele_NoCall.snp should equal(true)
      Allele_NoCall.purine should equal(false)
      Allele_NoCall.pirimidine should equal(false)

    }

    it("Self mutations should be false") {
      Allele_C_REF transversion Allele_C_REF should equal(false)
      Allele_T_REF transversion Allele_T_REF should equal(false)
      Allele_G_REF transversion Allele_G_REF should equal(false)
      Allele_A_REF transversion Allele_A_REF should equal(false)

      Allele_C_VAR transversion Allele_C_REF should equal(false)
      Allele_T_VAR transversion Allele_T_REF should equal(false)
      Allele_G_VAR transversion Allele_G_REF should equal(false)
      Allele_A_VAR transversion Allele_A_REF should equal(false)

      Allele_C_REF transversion Allele_C_VAR should equal(false)
      Allele_T_REF transversion Allele_T_VAR should equal(false)
      Allele_G_REF transversion Allele_G_VAR should equal(false)
      Allele_A_REF transversion Allele_A_VAR should equal(false)

      Allele_C_VAR transversion Allele_C_VAR should equal(false)
      Allele_T_VAR transversion Allele_T_VAR should equal(false)
      Allele_G_VAR transversion Allele_G_VAR should equal(false)
      Allele_A_VAR transversion Allele_A_VAR should equal(false)

      Allele_C_REF transition Allele_C_REF should equal(false)
      Allele_T_REF transition Allele_T_REF should equal(false)
      Allele_G_REF transition Allele_G_REF should equal(false)
      Allele_A_REF transition Allele_A_REF should equal(false)

      Allele_C_VAR transition Allele_C_REF should equal(false)
      Allele_T_VAR transition Allele_T_REF should equal(false)
      Allele_G_VAR transition Allele_G_REF should equal(false)
      Allele_A_VAR transition Allele_A_REF should equal(false)

      Allele_C_REF transition Allele_C_VAR should equal(false)
      Allele_T_REF transition Allele_T_VAR should equal(false)
      Allele_G_REF transition Allele_G_VAR should equal(false)
      Allele_A_REF transition Allele_A_VAR should equal(false)

      Allele_C_VAR transition Allele_C_VAR should equal(false)
      Allele_T_VAR transition Allele_T_VAR should equal(false)
      Allele_G_VAR transition Allele_G_VAR should equal(false)
      Allele_A_VAR transition Allele_A_VAR should equal(false)
    }

    it("Transitions") {
      Allele_C_REF transition Allele_T_REF should equal(true)
      Allele_T_REF transition Allele_C_REF should equal(true)
      Allele_G_REF transition Allele_A_REF should equal(true)
      Allele_A_REF transition Allele_G_REF should equal(true)

      Allele_C_REF transition Allele_A_REF should equal(false)
      Allele_C_REF transition Allele_G_REF should equal(false)
      Allele_T_REF transition Allele_A_REF should equal(false)
      Allele_T_REF transition Allele_G_REF should equal(false)
      Allele_G_REF transition Allele_T_REF should equal(false)
      Allele_G_REF transition Allele_C_REF should equal(false)
      Allele_A_REF transition Allele_T_REF should equal(false)
      Allele_A_REF transition Allele_C_REF should equal(false)
    }

    it("Transversions") {
      Allele_C_REF transversion Allele_T_REF should equal(false)
      Allele_T_REF transversion Allele_C_REF should equal(false)
      Allele_G_REF transversion Allele_A_REF should equal(false)
      Allele_A_REF transversion Allele_G_REF should equal(false)

      Allele_C_REF transversion Allele_A_REF should equal(true)
      Allele_C_REF transversion Allele_G_REF should equal(true)
      Allele_T_REF transversion Allele_A_REF should equal(true)
      Allele_T_REF transversion Allele_G_REF should equal(true)
      Allele_G_REF transversion Allele_T_REF should equal(true)
      Allele_G_REF transversion Allele_C_REF should equal(true)
      Allele_A_REF transversion Allele_T_REF should equal(true)
      Allele_A_REF transversion Allele_C_REF should equal(true)
    }

  }

  describe("AllelicContext") {

    case class AC(alleles: Seq[AlleleWithReference]) extends AllelicContextWithReference

    it("with A -> G is snp and not indel") {
      val ac = AC(Seq(Allele_A_REF, Allele_G_VAR))
      ac.indel should equal(false)
      ac.snp should equal(true)
      ac.reference should equal(Allele_A_REF)
    }

  }

  describe("GenotypeLike") {
    case class GT(allele1: AlleleWithReference, allele2: AlleleWithReference) extends GenotypeLikeWithReference

    it("GT(AREF,AREF) is homref") {
      GT(Allele_A_REF, Allele_A_REF).isHomRef should equal(true)
      GT(Allele_A_REF, Allele_A_REF).noCall should equal(false)
      GT(Allele_A_REF, Allele_A_REF).isHet should equal(false)
      GT(Allele_A_REF, Allele_A_REF).isHomVar should equal(false)

      GT(Allele_T_REF, Allele_T_REF).isHomRef should equal(true)
      GT(Allele_T_REF, Allele_T_REF).noCall should equal(false)
      GT(Allele_T_REF, Allele_T_REF).isHet should equal(false)
      GT(Allele_T_REF, Allele_T_REF).isHomVar should equal(false)

      GT(Allele_C_REF, Allele_C_REF).isHomRef should equal(true)
      GT(Allele_C_REF, Allele_C_REF).noCall should equal(false)
      GT(Allele_C_REF, Allele_C_REF).isHet should equal(false)
      GT(Allele_C_REF, Allele_C_REF).isHomVar should equal(false)

      GT(Allele_G_REF, Allele_G_REF).isHomRef should equal(true)
      GT(Allele_G_REF, Allele_G_REF).noCall should equal(false)
      GT(Allele_G_REF, Allele_G_REF).isHet should equal(false)
      GT(Allele_G_REF, Allele_G_REF).isHomVar should equal(false)

      //
      GT(Allele_G_REF, Allele_T_REF).isHomRef should equal(false)
      GT(Allele_G_REF, Allele_T_REF).noCall should equal(false)
      GT(Allele_G_REF, Allele_T_REF).isHet should equal(true)
      GT(Allele_G_REF, Allele_T_REF).isHomVar should equal(false)

      GT(Allele_T_REF, Allele_A_REF).isHomRef should equal(false)
      GT(Allele_T_REF, Allele_A_REF).noCall should equal(false)
      GT(Allele_T_REF, Allele_A_REF).isHet should equal(true)
      GT(Allele_T_REF, Allele_A_REF).isHomVar should equal(false)

      GT(Allele_C_REF, Allele_A_REF).isHomRef should equal(false)
      GT(Allele_C_REF, Allele_A_REF).noCall should equal(false)
      GT(Allele_C_REF, Allele_A_REF).isHet should equal(true)
      GT(Allele_C_REF, Allele_A_REF).isHomVar should equal(false)

      GT(Allele_A_REF, Allele_T_REF).isHomRef should equal(false)
      GT(Allele_A_REF, Allele_T_REF).noCall should equal(false)
      GT(Allele_A_REF, Allele_T_REF).isHet should equal(true)
      GT(Allele_A_REF, Allele_T_REF).isHomVar should equal(false)

      //
      GT(Allele_G_VAR, Allele_G_VAR).isHomRef should equal(false)
      GT(Allele_G_VAR, Allele_G_VAR).noCall should equal(false)
      GT(Allele_G_VAR, Allele_G_VAR).isHet should equal(false)
      GT(Allele_G_VAR, Allele_G_VAR).isHomVar should equal(true)

      GT(Allele_T_VAR, Allele_T_VAR).isHomRef should equal(false)
      GT(Allele_T_VAR, Allele_T_VAR).noCall should equal(false)
      GT(Allele_T_VAR, Allele_T_VAR).isHet should equal(false)
      GT(Allele_T_VAR, Allele_T_VAR).isHomVar should equal(true)

      GT(Allele_C_VAR, Allele_C_VAR).isHomRef should equal(false)
      GT(Allele_C_VAR, Allele_C_VAR).noCall should equal(false)
      GT(Allele_C_VAR, Allele_C_VAR).isHet should equal(false)
      GT(Allele_C_VAR, Allele_C_VAR).isHomVar should equal(true)

      GT(Allele_A_VAR, Allele_A_VAR).isHomRef should equal(false)
      GT(Allele_A_VAR, Allele_A_VAR).noCall should equal(false)
      GT(Allele_A_VAR, Allele_A_VAR).isHet should equal(false)
      GT(Allele_A_VAR, Allele_A_VAR).isHomVar should equal(true)

      //
      GT(Allele_A_VAR, Allele_T_VAR).isHomRef should equal(false)
      GT(Allele_A_VAR, Allele_T_VAR).noCall should equal(false)
      GT(Allele_A_VAR, Allele_T_VAR).isHet should equal(true)
      GT(Allele_A_VAR, Allele_T_VAR).isHomVar should equal(false)

      GT(Allele_A_VAR, Allele_C_VAR).isHomRef should equal(false)
      GT(Allele_A_VAR, Allele_C_VAR).noCall should equal(false)
      GT(Allele_A_VAR, Allele_C_VAR).isHet should equal(true)
      GT(Allele_A_VAR, Allele_C_VAR).isHomVar should equal(false)

      GT(Allele_A_VAR, Allele_G_VAR).isHomRef should equal(false)
      GT(Allele_A_VAR, Allele_G_VAR).noCall should equal(false)
      GT(Allele_A_VAR, Allele_G_VAR).isHet should equal(true)
      GT(Allele_A_VAR, Allele_G_VAR).isHomVar should equal(false)

      //
      GT(Allele_T_VAR, Allele_C_VAR).isHomRef should equal(false)
      GT(Allele_T_VAR, Allele_C_VAR).noCall should equal(false)
      GT(Allele_T_VAR, Allele_C_VAR).isHet should equal(true)
      GT(Allele_T_VAR, Allele_C_VAR).isHomVar should equal(false)

      GT(Allele_C_VAR, Allele_T_VAR).isHomRef should equal(false)
      GT(Allele_C_VAR, Allele_T_VAR).noCall should equal(false)
      GT(Allele_C_VAR, Allele_T_VAR).isHet should equal(true)
      GT(Allele_C_VAR, Allele_T_VAR).isHomVar should equal(false)

      GT(Allele_G_VAR, Allele_A_VAR).isHomRef should equal(false)
      GT(Allele_G_VAR, Allele_A_VAR).noCall should equal(false)
      GT(Allele_G_VAR, Allele_A_VAR).isHet should equal(true)
      GT(Allele_G_VAR, Allele_A_VAR).isHomVar should equal(false)
    }
  }

}