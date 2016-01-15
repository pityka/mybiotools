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
package stat
import PoissonRegression._

import org.scalatest.FunSpec
import org.scalatest.Matchers
import mybiotools.gwascommons.Individual
import org.saddle._

class PoissonRegressionSpec extends FunSpec with Matchers {

  describe("R example") {

    val counts = List(18, 17, 15, 20, 10, 20, 25, 13, 12).map(_.toDouble)
    val treatment = List(1, 1, 1, 2, 2, 2, 3, 3, 3).map(_.toDouble)
    val outcome = List(1, 2, 3, 1, 2, 3, 1, 2, 3).map(_.toDouble)

    val result = poissonRegression(Frame("outcome" -> Series(outcome: _*), "treatment" -> Series(treatment: _*)), List("treatment", "outcome"), Series(counts: _*), MeanImpute, 50, 1E-10).asInstanceOf[PoissonRegressionResult]

    // println(result.table)

    it("should have the right estimates") {
      math.abs(result.covariates("outcome")._1.slope - (-1.607e-01)) should be < 0.001
      math.abs(result.covariates("treatment")._1.slope - (2.159e-14)) should be < 0.001
      math.abs(result.intercept._1.slope - 3.126e+00) should be < 0.001
      math.abs(result.covariates("outcome")._1.sd - (1.006e-01)) should be < 0.001
      math.abs(result.covariates("treatment")._1.sd - (1.000e-01)) should be < 0.001

      math.abs(result.intercept._1.sd - (2.881e-01)) should be < 0.001
    }
    // it("should have the right r2") {
    //   math.abs(result.r2 - 0.07308) should be < 0.01
    // }
    ignore("should have the right loglikelihood") {
      math.abs(result.logLikelihood.L - (-24.82407)) should be < 0.001
      math.abs(result.logLikelihood.df - (3)) should be < 0.001 // R tells 3: 2 for the betas and 1 for the sigma
    }
    it("df") {
      math.abs(result.df - (3)) should be < 0.001
    }
    // counts <- c(18,17,15,20,10,20,25,13,12)
    //    outcome <- c(1 ,2, 3, 1, 2, 3, 1, 2, 3)
    //    treatment <- c(1,1,1,2,2,2,3,3,3)
    //    print(d.AD <- data.frame(treatment, outcome, counts))
    //    glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
    //    summary(glm.D93)

    //     Call:
    //     glm(formula = counts ~ outcome + treatment, family = poisson())
    //     
    //     Deviance Residuals: 
    //         Min       1Q   Median       3Q      Max  
    //     -1.7331  -0.5666   0.1165   0.2449   1.4854  
    //     
    //     Coefficients:
    //                   Estimate Std. Error z value Pr(>|z|)    
    //     (Intercept)  3.126e+00  2.881e-01  10.853   <2e-16 ***
    //     outcome     -1.607e-01  1.006e-01  -1.597     0.11    
    //     treatment    2.159e-14  1.000e-01   0.000     1.00    
    //     ---
    //     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    //     
    //     (Dispersion parameter for poisson family taken to be 1)
    //     
    //         Null deviance: 10.581  on 8  degrees of freedom
    //     Residual deviance:  8.016  on 6  degrees of freedom
    //     AIC: 55.648
    //     
    //     Number of Fisher Scoring iterations: 4
  }

}