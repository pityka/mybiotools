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

package rnaseqalign.analysis

import rnaseqalign.tasks._

import org.scalatest._

import mybiotools.tasks._

import org.scalatest.FunSpec
import org.scalatest.Matchers
import mybiotools.gwascommons._
import mybiotools.plots._
import mybiotools.saddlehelpers._

class EnrichmentPlotSuite extends FunSuite with Matchers {

  ignore("1") {
    mybiotools.plots.show(EnrichmentPlot.createPlot(List(
      DataForEnrichmentPlot(1E-2, 3, "A"),
      DataForEnrichmentPlot(1E-2, 3, "B"),
      DataForEnrichmentPlot(1E-2, -13, "C"),
      DataForEnrichmentPlot(1E-3, 2, "C"),
      DataForEnrichmentPlot(1E-5, 2, "D"),
      DataForEnrichmentPlot(1E-3, 2, "D"),
      DataForEnrichmentPlot(1E-10, 2, "D")
    )))
  }

}