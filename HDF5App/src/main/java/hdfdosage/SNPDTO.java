// /*
// * The MIT License
// *
// * Copyright (c) 2015 ECOLE POLYTECHNIQUE FEDERALE DE LAUSANNE, Switzerland,
// * Group Fellay
// *
// * Permission is hereby granted, free of charge, to any person obtaining
// * a copy of this software and associated documentation files (the "Software"),
// * to deal in the Software without restriction, including without limitation
// * the rights to use, copy, modify, merge, publish, distribute, sublicense,
// * and/or sell copies of the Software, and to permit persons to whom the Software
// * is furnished to do so, subject to the following conditions:
// *
// * The above copyright notice and this permission notice shall be included in all
// * copies or substantial portions of the Software.
// *
// * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// * SOFTWARE.
// */
//
// package hdfdosage;
//
// import ch.systemsx.cisd.hdf5.CompoundElement;
//
// import mybiotools.stringstore.String8;
//
// public class SNPDTO{
//
// 	@CompoundElement(dimensions={40})
// 	byte[] snpName;
//
// 	@CompoundElement(dimensions={7})
// 	byte[] allele1;
//
// 	@CompoundElement(dimensions={7})
// 	byte[] allele2;
//
// 	@CompoundElement(dimensions={1})
// 	float sumAl1Dosage;
//
// 	@CompoundElement(dimensions={1})
// 	float sumAl2Dosage;
//
// 	@CompoundElement(dimensions={1})
// 	int count;
//
// 	@CompoundElement(dimensions={1})
// 	float maf;
//
//
//
//
// 	public SNPDTO() {
//
// 	}
//
// 	public SNPDTO(String8 snpNamep, String8 al1, String8 al2, float sumal1dosagep, float sumal2dosagep, int countp, float mafp) {
// 		snpName = snpNamep.chars();
//
// 		allele1 = al1.chars();
// 		if (allele1.length > 7) {
// 			allele1 = java.util.Arrays.copyOfRange(allele1,0,7);
// 		}
// 		allele2 = al2.chars();
// 		if (allele2.length > 7) {
// 			allele2 = java.util.Arrays.copyOfRange(allele2,0,7);
// 		}
// 		sumAl1Dosage = sumal1dosagep;
// 		sumAl2Dosage = sumal2dosagep;
// 		count = countp;
// 		maf = mafp;
// 	}
//
// 	public String toString() {
// 		return new String(snpName) + new String(allele1) + new String(allele2);
// 	}
//
// }
