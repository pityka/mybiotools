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

package mybiotools.stat

import org.saddle._
import mybiotools.gwascommons._
import org.apache.commons.math3.util.FastMath
import org.apache.commons.math3.random.{ RandomGenerator, RandomDataGenerator, Well44497b }
import org.ejml.data.DenseMatrix64F
import org.ejml.simple.SimpleMatrix
import org.ejml.factory.DecompositionFactory

object GlobalTest {

  sealed trait MethodOfWeightedChiSquare
  case object Imhof extends MethodOfWeightedChiSquare
  case object Liu extends MethodOfWeightedChiSquare

  private def toEjml(m: Mat[Double]) = SimpleMatrix.wrap(DenseMatrix64F.wrap(m.numRows, m.numCols, m.contents))

  private def invertPD(m: Mat[Double]) = Mat(m.numRows, m.numCols, LinearRegression.invertPositiveDefinite(toEjml(m)).get.getMatrix.getData)

  sealed trait WeightingScheme {
    def withWeightedColumns(d: Mat[Double]): Mat[Double]
  }

  object InverseVariance extends WeightingScheme {
    def withWeightedColumns(m: Mat[Double]) = Mat(m.cols.map(c => c * 1.0 / c.variance): _*)
  }

  /* If genotype data is coded as 0,1,2 then this is what SKAT does */
  case class BetaPDFAtHalfMean(a: Double, b: Double) extends WeightingScheme {
    def withWeightedColumns(m: Mat[Double]) = {
      Mat(m.cols.map { col =>
        val mean = col.mean
        val w = jdistlib.Beta.density(mean / 2, a, b, false)
        col * w * w
      }: _*)
    }
  }

  private def autoLogistic(s: Vec[Double]): Vec[Boolean] = {
    val values = s.toSeq.distinct
    assert(values.size <= 2, s"Series supposed to have two states. Values: $values, $s ")
    val yesvalue = values.sorted.last
    val novalue = values.sorted.head
    s.map(_ == yesvalue)
  }

  /**
   * Global test as in Goeman 2011 for linear outcomes
   *
   * P-value is evaluated with Imhof's method.
   */
  def test(
    data: Frame[Individual, String, Double],
    phenoName: String,
    phenoScale: PhenotypeScale,
    covariateNames: Seq[String],
    snpNames: Seq[String],
    weightingScheme: WeightingScheme,
    methodOfPValue: MethodOfWeightedChiSquare
  ): (Double, Double) = {

    val index = data.rowIx

    val nullMatrix = Mat(vec.ones(index.length) +: data.col(covariateNames: _*).reindexRow(index).toMat.cols: _*)

    val design = Mat(vec.ones(index.length) +: weightingScheme.withWeightedColumns(data.col(covariateNames ++ snpNames: _*).reindexRow(index).toMat).cols: _*)

    val y = data.firstCol(phenoName).toVec
    val (scaledS, lambdas) = phenoScale match {
      case Linear => test_linear(design, nullMatrix, y)
      case Logistic => test_logistic(design, nullMatrix, autoLogistic(y))
    }

    val p = methodOfPValue match {
      case Imhof => linCombChiSquaresCumulativeImhof(lambdas, 0.0)
      case Liu => linCombChiSquaresCumulativeLiu2009(lambdas, 0.0)
    }

    if (p.isNaN) {
      println(s"ERROR P NAN \n design: $design, null:$nullMatrix, y:$y")
    }

    if (p < 1E-6) {
      println(s"LOW P: \n ${lambdas.toSeq} \n $scaledS")
    }

    (scaledS, p)

  }

  /**
   * Global test as in Goeman 2011 for linear outcomes
   *
   * P-value is evaluated with Imhof's method.
   */
  def test_linear(
    design: Mat[Double],
    condition: Mat[Double],
    outcome: Vec[Double]
  ): (Double, Vec[Double]) = {
    val n = outcome.length
    val H = condition mult invertPD(condition.T mult condition) mult condition.T
    val I = Mat.ident(H.numRows)
    val ImH = I - H
    val sigma2 = (outcome.T mult ImH mult outcome).raw(0, 0) / n
    val expected = (H mult outcome).col(0)
    val diff = outcome - expected

    val S = ((diff.T mult design) mult (design.T mult diff)).raw(0, 0) / (n * sigma2)

    val A = (ImH mult design) mult (design.T mult ImH)

    val scale = VarianceComponentModel.trace(design mult design.T mult ImH)

    val lambdas = try {
      quadraticRatio(A, ImH, S)
    } catch {
      case e => throw new RuntimeException(s"Error in quadraticRatio, \n$outcome, \n$design,\n $condition", e)
    }

    S / scale -> lambdas

  }

  def test_logistic(
    design: Mat[Double],
    condition: Mat[Double],
    outcome: Vec[Boolean]
  ): (Double, Vec[Double]) = {
    val n = outcome.length

    val nullEstimates: Vec[Double] = {
      val names = 1 until condition.numCols map (_.toString)
      val lr = LogisticRegression.logisticRegressionFullData(condition, outcome, names, 50, 1E-6, None).right.toOption.get.asInstanceOf[LogisticRegressionResult]
      Vec(0 until condition.numCols map (i => lr.covariate(i).get._1.slope): _*)
    }

    val expected = Vec(condition.rows.map(v => logistic((v.T mult nullEstimates).raw(0, 0))): _*)

    val diff = outcome.map(x => if (x) 1.0 else 0.0) - expected

    val diagW = expected * (expected - 1) * -1

    val diagD = Vec(design.rows.map(v => (v.T mult v).raw(0, 0)): _*)

    val S = ((diff.T mult design) mult (design.T mult diff)).raw(0, 0) /
      (diff.zipMap(diagD)(_ * _).T mult diff).raw(0, 0)

    val H = {
      val xt = condition.T
      val xtwxInvert = {
        val sm = LinearRegression.invertPositiveDefinite(LogisticRegression.calculateXTwX(toEjml(xt), diagW.toSeq.toIndexedSeq, toEjml(condition))).toOption.get
        Mat(sm.numRows, sm.numCols, sm.getMatrix.getData)
      }
      val wx = Mat(condition.cols.zipWithIndex.map(x => x._1 * diagW.raw(x._2)): _*)
      wx mult xtwxInvert mult xt
    }

    val I = Mat.ident(H.numRows)
    val ImH = I - H
    val A = ImH.T mult design mult design.T mult ImH
    val B = Mat(ImH.T.cols.zipWithIndex.map(x => x._1 * diagD.raw(x._2)): _*) mult ImH

    val lambdas = quadraticRatio(A, B, S)

    S / (n - condition.numCols) -> lambdas

  }

  def quadraticRatio(a: Mat[Double], b: Mat[Double], q: Double): Vec[Double] = {
    val c = a - (b * q)
    val evd = DecompositionFactory.eig(c.numRows, false, true)
    val densmat = DenseMatrix64F.wrap(c.numRows, c.numCols, c.contents)
    val success = evd.decompose(densmat)
    if (!success) throw new RuntimeException("EVD Decomposition failed. \n" + q + "\n" + c.toString + "\n" + a.toString + "\n" + b.toString)
    val lambdas = Vec((0 until c.numRows map (i => evd.getEigenvalue(i).real)): _*).filter(_ != 0.0)

    lambdas
  }

  def positiveLinCombChiSquaresCumulativeLiu2009(lambdas: Vec[Double], q: Double): Double =
    positiveLinCombChiSquaresCumulativeLiu2009(lambdas, lambdas.map(i => 1.0), lambdas.map(i => 0.0), q)

  /**
   * P(Q(X) > t) is approximated with P( Chisq(df,ncp) > (t-muQ)/sigmaQ * sigmaX + muX))
   *
   * This is a linear transformation of a non centra chi square
   * ((X - muX)/sigmaX)*sigmaQ + muQ = X*sigmaQ/sigmaX - muX*sigmaQ/sigmaX + muQ
   *  = a * X + b, a = sigmaQ/sigmaX, b = - muX*sigmaQ/sigmaX + muQ
   */
  case class LiuResult(muQ: Double, sigmaQ: Double, muX: Double, sigmaX: Double, df: Double, ncp: Double) {
    def cumulative(q: Double) = {
      val tStar = (q - muQ) / sigmaQ

      jdistlib.NonCentralChiSquare.cumulative(tStar * sigmaX + muX, df, ncp, false, false)
    }
    def density(a: Double) = {
      val tStar = (a - muQ) / sigmaQ

      jdistlib.NonCentralChiSquare.density(tStar * sigmaX + muX, df, ncp, false) * math.abs(sigmaX / sigmaQ)
    }
    def mean = ((df + ncp) / sigmaX) * sigmaQ - (muX / sigmaX) * sigmaQ + muQ
    def variance: Double = 2 * (df + 2 * ncp) * (sigmaQ / sigmaX) * (sigmaQ / sigmaX)

  }

  def positiveLinCombChiSquaresCumulativeLiu2009(
    lambdas: Vec[Double],
    dfs: Vec[Double],
    ncps: Vec[Double],
    q: Double
  ) =
    positiveLinCombChiSquaresLiu2009(lambdas, dfs, ncps).cumulative(q)

  def linCombChiSquaresCumulativeLiu2009(
    lambdas: Vec[Double],
    q: Double
  ): Double = {

    def findpoints(
      f: Double => Double,
      lowerbound: Double,
      thr: Double,
      start: Double,
      tiny: Double
    ) = {

      val base = 2

      def p(x: Double) = {
        f(x) < thr
      }

      // val tiny = math.sqrt(math.ulp(1.0))

      def step(i: Double) = math.max(lowerbound, start) + tiny * math.pow(base, i)

      var i = 1

      while (!p(step(i)) && step(i) < Double.PositiveInfinity) {
        i += 1
      }
      (step(i), step(i - 1) - math.max(lowerbound, start))
    }

    val pos = lambdas.filter(_ > 0.0)
    val neg = lambdas.filter(_ < 0.0).map(_ * (-1))

    if (neg.length == 0) positiveLinCombChiSquaresCumulativeLiu2009(pos, q)
    else {
      val lrpos = positiveLinCombChiSquaresLiu2009(pos, pos.map(i => 1.0), pos.map(i => 0.0))
      val lrneg = positiveLinCombChiSquaresLiu2009(neg, neg.map(i => 1.0), neg.map(i => 0.0))

      val (posMax, posWidth) = findpoints(
        (x: Double) => lrpos.density(x),
        math.sqrt(math.ulp(1.0)),
        1E-80,
        lrpos.mean,
        math.sqrt(lrpos.variance)
      )
      val (negMax, negWidth) = findpoints(
        (x: Double) => lrneg.density(x),
        math.sqrt(math.ulp(1.0)),
        1E-80,
        lrneg.mean,
        math.sqrt(lrneg.variance)
      )

      def convolutedPDF(z: Double) = {
        // As the chi square has 0 density at x<=0 we integrate from 0 to infinity
        // instead of infinity we use the point where the pdf vanishes

        val shiftednegmax = negMax + z

        val max = math.min(shiftednegmax, posMax)
        val width = math.min(lrneg.variance, lrpos.variance)

        val startCycle = math.min(math.max(5, (math.log(max / (width / 2)) / math.log(2d)).toInt), 20)

        val r = (NumericalIntegration.iterative(
          (x: Double) => {
            lrpos.density(x) * lrneg.density(x - z)
          },
          math.sqrt(math.ulp(1.0)),
          max,
          1E-2,
          1E-8,
          startCycle,
          startCycle + 10,
          NewtonCotes.Booles
        ))._1

        r

      }

      val (convMax, _) = findpoints(
        (x: Double) => convolutedPDF(x),
        q,
        1E-40,
        lrpos.mean - lrneg.mean,
        math.sqrt(lrpos.variance + lrneg.variance)
      )
      val convWidth = math.sqrt(lrpos.variance + lrneg.variance) * 10

      val startCycle = math.min(20, math.max(5, (math.log(math.abs(convWidth - q) / (convWidth / 2)) / math.log(2d)).toInt))

      val r = NumericalIntegration.iterative(
        (x: Double) => {
          convolutedPDF(x)
        },
        q,
        convMax,
        1E-2,
        1E-4,
        startCycle,
        startCycle + 10,
        NewtonCotes.Booles
      )._1

      math.min(r, 1.0)

    }

  }

  // http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.421.5737&rep=rep1&type=pdf
  // http://dx.doi.org/10.1016/j.csda.2008.11.025
  def positiveLinCombChiSquaresLiu2009(lambdas: Vec[Double], dfs: Vec[Double], ncps: Vec[Double]): LiuResult = {
    assert(lambdas.toSeq.forall(_ > 0.0))
    var c1 = 0.0
    var c2 = 0.0
    var c3 = 0.0
    var c4 = 0.0

    {
      var i = 0
      val n = lambdas.length
      while (i < n) {
        val l = lambdas.raw(i)
        val df = dfs.raw(i)
        val ncp = ncps.raw(i)

        val l2 = l * l
        val l3 = l2 * l
        c1 += l * df
        c2 += l2 * df
        c3 += l3 * df
        c4 += l3 * l * df
        i += 1
      }
    }

    {
      var i = 0
      val n = lambdas.length
      while (i < n) {
        val l = lambdas.raw(i)
        val df = dfs.raw(i)
        val ncp = ncps.raw(i)

        val l2 = l * l
        val l3 = l2 * l
        c1 += l * ncp
        c2 += l2 * 2 * ncp
        c3 += l3 * 3 * ncp
        c4 += l3 * l * 4 * ncp
        i += 1
      }
    }

    val k1 = 1 * c1
    val k2 = 2 * c2
    val k3 = 6 * c3
    val k4 = 48 * c4

    val s1 = c3 / (math.pow(c2, 1.5))
    val s2 = c4 / (c2 * c2)

    val a = if (s1 * s1 > s2) 1d / (s1 - math.sqrt(s1 * s1 - s2)) else 1 / s1

    val delta = math.max(0.0, s1 * a * a * a - a * a)
    val l = a * a - 2 * delta

    assert(delta >= 0)
    assert(l >= 0)
    assert(a >= 1 / s1)
    assert(s2 >= 0)
    val muX = l + delta
    val sigmaX = math.sqrt(2) * a

    LiuResult(c1, math.sqrt(k2), muX, sigmaX, l, delta)

  }

  /**
   * Imhof's method of linear combination of chi squares
   *
   * This code assumes ncp=0,df=1 for each
   * If needed write out the rest of the terms from:
   * http://www.jstor.org/stable/pdf/2332763.pdf?acceptTC=true, eq 3.2
   */
  def linCombChiSquaresCumulativeImhof(
    lambdas1: Vec[Double],
    q: Double,
    errorBound: Double = 5E-9
  ) = {

    val lambdas = lambdas1.filter(l => math.abs(l) > 1E-6).contents
    val lsum = lambdas1.filter(l => math.abs(l) > 1E-6).sum

    def theta(u: Double) = {
      var sum = 0.0
      var i = 0
      val n = lambdas.size
      while (i < n) {
        sum += FastMath.atan(lambdas(i) * u)
        i += 1
      }

      0.5 * (sum - q * u)
    }

    def rho(u: Double) = {
      var sum = 0.0
      var i = 0
      val n = lambdas.size
      while (i < n) {
        val l = lambdas(i)
        sum += math.log(1 + l * l * u * u)
        i += 1
      }
      math.exp(0.25 * sum)
    }

    def integrand(u: Double) = (if (u == 0.0) 0.5 * lsum - 0.5 * q else math.sin(theta(u)) / (u * rho(u))) / math.Pi

    def getErrorBound(u: Double) = {
      var prod = 0.0
      var i = 0
      val m = lambdas.size
      while (i < m) {
        prod += math.log(math.abs(lambdas(i)))
        i += 1
      }

      math.exp(-1 * math.log(math.Pi * m * 0.5) - 0.5 * m * math.log(u) - prod * 0.5)
    }

    // Without change of variables:
    def integrate(max: Double, startCycle: Int) = {
      val (integral, cycle) = NumericalIntegration.iterative(
        integrand,
        0.0,
        max,
        1E-8,
        1E-16,
        startCycle,
        64,
        NewtonCotes.Trapezoid
      )
      (0.5 + integral, cycle)
    }

    var end = 1E0
    var e = getErrorBound(end)
    while (e <= errorBound) {
      end *= 0.5
      e = getErrorBound(end)
    }

    val cycle1 = {
      // linearize theta at 0 as a, this is the period length of sin(a*x)
      val periodAt0 = (2 * math.Pi) / (lambdas.map(math.abs).sum * 0.5)
      val periods = end / periodAt0
      (math.log(periods * 100) / math.log(2d)).toInt + 1
    }

    val (i0, _) = integrate(end, cycle1)
    var b = false
    while ((e > errorBound && (i0 < 0 || e / i0 > 1E-4))) {
      end *= 2
      e = getErrorBound(end)
      b = true
    }

    val r = if (b) {
      val cycle2 = {
        // linearize theta at 0 as a, this is the period length of sin(a*x)
        val periodAt0 = (2 * math.Pi) / (lambdas.map(math.abs).sum * 0.5)
        val periods = end / periodAt0
        (math.log(periods * 100) / math.log(2d)).toInt + 1
      }
      val (i1, _) = integrate(end, cycle2)
      // println(end + " " + e + " " + i1 + " " + e / i1 + " " + cycle2)
      i1 + e
    } else {
      // println(end + " " + e + " " + i0 + " " + e / i0 + " " + cycle1)
      i0 + e
    }

    if (r.isNaN) {
      println("NAN: " + lambdas1.toSeq + " " + q)
    }

    r

  }

}