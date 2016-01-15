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
import mybiotools.gwascommons._
import mybiotools.gwascommons.genotypedata._

object SNP2HLABestAllele extends App {

  val gprobf = args(0)

  val famf = args(1)

  openSource(gprobf) { gprob =>
    openSource(famf) { fam =>
      val map: Map[Individual, HLAReport] = readBeagleGprobs(gprob, fam)

      println("FID IID A1 A1_prob A2 A2_prob B1 B1_prob B2 B2_prob C1 C1_prob C2 C2_prob DRB1_1 DRB1_1_prob DRB1_2 DRB1_2_prob DQA1_1 DQA1_1_prob DQA1_2 DQA1_2_prob DQB1_1 DQB1_1_prob DQB1_2 DQB1_2_prob DPA1_1 DPA1_1_prob DPA1_2 DPA1_2_prob DPB1_1 DPB1_1_prob DPB1_2 DPB1_2_prob")
      map.foreach {
        case (ind, report) =>
          println(s"${ind.FID} ${ind.IID} ${report.A1._1} ${report.A1._2} ${report.A2._1} ${report.A2._2} ${report.B1._1} ${report.B1._2} ${report.B2._1} ${report.B2._2} ${report.C1._1} ${report.C1._2} ${report.C2._1} ${report.C2._2} ${report.DRB1_1._1} ${report.DRB1_1._2} ${report.DRB1_2._1} ${report.DRB1_2._2} ${report.DQA1_1._1} ${report.DQA1_1._2} ${report.DQA1_2._1} ${report.DQA1_2._2} ${report.DQB1_1._1} ${report.DQB1_1._2} ${report.DQB1_2._1} ${report.DQB1_2._2} ${report.DPA1_1._1} ${report.DPA1_1._2} ${report.DPA1_2._1} ${report.DPA1_2._2} ${report.DPB1_1._1} ${report.DPB1_1._2} ${report.DPB1_2._1} ${report.DPB1_2._2} ")
      }

    }
  }

}
