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
import mybiotools.saddlehelpers._
import org.saddle._

class DetailedPlotSuite extends FunSuite with Matchers {

  ignore("1") {
    mybiotools.plots.show(
      detailedGeneExpressionPlot(
        counts = Frame("s11" -> Series("g1" -> 10d, "g2" -> 11d), "s12" -> Series("g1" -> 20d, "g2" -> 11d), "s21" -> Series("g1" -> 30d, "g2" -> 11d), "s22" -> Series("g1" -> 40d, "g2" -> 11d)),
        primaryDimensionReverse = Map("p1" -> Set("s12", "s11"), "p2" -> Set("s22", "s21")).toSeq,
        secondaryDimensionReverse = Map("s1" -> Set("s21", "s11"), "s2" -> Set("s12", "s22")).toSeq,
        geneGroups = Map("g1" -> "0", "g3" -> "2"),
        main = "",
        ylab = ""
      )
    )
  }

}