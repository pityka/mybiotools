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

import mybiotools.mymatrix._
import org.ejml.simple.SimpleEVD
import org.ejml.data.DenseMatrix64F
import org.ejml.simple.SimpleMatrix
import org.ejml.factory.DecompositionFactory
import org.ejml.ops.EigenOps
import mybiotools.gwascommons._
import de.erichseifert.gral.plots.XYPlot
import org.ejml.ops._
import org.saddle._
import mybiotools.plots.ScatterPlot.Label
import mybiotools.plots.ScatterPlot._

import org.saddle._

package object pcanew {

  def brokenStickUsefulEigenValues(eigs: Seq[Double]): Seq[Double] = {
    val sorted = eigs.sortBy(x => -1 * x)
    val expected = 1 to eigs.size map (k => (for (i <- k to eigs.size) yield (1.0 / i)).sum)
    sorted zip expected filter (x => x._1 > x._2) map (_._1)
  }

  def pcaFromDistanceMatrix(matrix: MyMatrix[Individual, Double]): PCAResult[Individual] = {
    val expmat = matrix.mapValues(x => scala.math.exp(-2 * x))
    val evd = DecompositionFactory.eig(matrix.size, true, true)
    val densmat = new DenseMatrix64F(expmat.toArray)
    // println("----")
    //    println(densmat.toString)
    //    println("####")
    val success = evd.decompose(densmat)
    if (!success) throw new RuntimeException("EVD Decomposition failed.")

    val numberOfEigenVectorsRetrieved = matrix.size
    val eigvals = scala.collection.mutable.ListBuffer[Double]()
    val evecs = (for (i <- 0 to numberOfEigenVectorsRetrieved - 1) yield {
      val evec = evd.getEigenVector(i)

      // scale to unit length, then scale by sqrt(|eigenvalue|)
      val factor = math.sqrt(math.abs(evd.getEigenvalue(i).real)) / org.ejml.ops.NormOps.normP2(evec)
      ((for (j <- 0 to matrix.size - 1) yield { evec.get(j, 0) * factor }) -> evd.getEigenvalue(i).real)
    }).sortBy { x => -1 * x._2 }.map { x => eigvals append x._2; x._1 }.transpose.zipWithIndex.map { pair => matrix.keys(pair._2) -> pair._1 }.toMap
    PCAResult(evecs, eigvals.toSeq)
  }

  def pcaFromMatrix(matrix: MyMatrix[Individual, Double]): PCAResult[Individual] = {
    val evd = DecompositionFactory.eig(matrix.size, true, true)
    val densmat = new DenseMatrix64F(matrix.toArray)
    // println("----")
    //    println(densmat.toString)
    //    println("####")
    val success = evd.decompose(densmat)
    if (!success) throw new RuntimeException("EVD Decomposition failed.")

    val numberOfEigenVectorsRetrieved = matrix.size
    val eigvals = scala.collection.mutable.ListBuffer[Double]()
    val evecs = (for (i <- 0 to numberOfEigenVectorsRetrieved - 1) yield {
      val evec = evd.getEigenVector(i)

      val factor = math.sqrt(math.abs(evd.getEigenvalue(i).real)) / org.ejml.ops.NormOps.normP2(evec)

      ((for (j <- 0 to matrix.size - 1) yield { evec.get(j, 0) * factor }) -> evd.getEigenvalue(i).real)
    }).sortBy { x => -1 * x._2 }.map { x => eigvals append x._2; x._1 }.transpose.zipWithIndex.map { pair => matrix.keys(pair._2) -> pair._1 }.toMap
    PCAResult(evecs, eigvals.toSeq)
  }

  def pcaFromGRM[RX](matrix1: Frame[RX, RX, Double])(implicit o: Ordering[RX], st: ST[RX]): PCAResult[RX] = pcaFromGRM(matrix1, matrix1.numRows)

  def pcaFromGRM[RX](matrix1: Frame[RX, RX, Double], axes: Int)(implicit o: Ordering[RX], st: ST[RX]): PCAResult[RX] = {
    val matrix = matrix1.sortedRIx.sortedCIx
    val evd = DecompositionFactory.eig(matrix.numRows, true, true)
    val densmat = DenseMatrix64F.wrap(matrix.numRows, matrix.numCols, matrix.toMat.contents)
    // println("----")
    //    println(densmat.toString)
    //    println("####")
    val success = evd.decompose(densmat)
    if (!success) throw new RuntimeException("EVD Decomposition failed.")

    val eigvalsWithIndex = (0 until matrix.numRows map (i => evd.getEigenvalue(i).real) zipWithIndex).sortBy(_._1).reverse.take(axes)
    val evecs = eigvalsWithIndex.map {
      case (eval, idx) =>
        val evec = evd.getEigenVector(idx)
        val factor = math.sqrt(math.abs(eval)) / org.ejml.ops.NormOps.normP2(evec)

        (for (j <- 0 until matrix.numRows) yield { evec.get(j, 0) * factor })
    }.transpose.zipWithIndex.map { pair => matrix.rowIx.raw(pair._2) -> pair._1 }.toMap
    PCAResult(evecs, eigvalsWithIndex.map(_._1))

  }

  def makePositiveByClipping[RX](matrix1: Frame[RX, RX, Double])(implicit o: Ordering[RX], st: ST[RX]): Frame[RX, RX, Double] = {
    val matrix = matrix1.sortedRIx.sortedCIx
    val m = makePositiveByClipping(matrix.toMat)
    Frame(m, matrix.rowIx, matrix.colIx)
  }

  def makePositiveByClipping(matrix: Mat[Double]): Mat[Double] = {

    val evd = DecompositionFactory.eig(matrix.numRows, true, true)
    val densmat = DenseMatrix64F.wrap(matrix.numRows, matrix.numCols, matrix.toMat.contents)

    val success = evd.decompose(densmat)
    if (!success) throw new RuntimeException("EVD Decomposition failed.")

    val numberOfEigenVectorsRetrieved = evd.getNumberOfEigenvalues
    val d = SimpleMatrix.wrap(EigenOps.createMatrixD(evd))
    0 until d.numRows foreach { i =>
      if (d.get(i, i) < 0) {
        d.set(i, i, 0)
      }
    }
    val v = SimpleMatrix.wrap(EigenOps.createMatrixV(evd))
    val vinv = v.invert
    val m = v mult d mult vinv

    Mat(m.numRows, m.numCols, m.getMatrix.getData)

  }

  def pcaFromSaddle(mat: Mat[Double], maxAxes: Int) = {

    val returnedAxes = math.min(maxAxes, mat.numCols)

    val centered = Mat(mat.cols.map(x => x - x.mean): _*)

    val centeredDM = DenseMatrix64F.wrap(centered.numRows, centered.numCols, centered.contents)

    // ejml's notation: X = U W VT
    val svd = DecompositionFactory.svd(centered.numRows, centered.numCols, false, true, false);
    if (!svd.decompose(centeredDM))
      throw new RuntimeException("SVD failed");

    val V = svd.getV(null, false);
    val W = svd.getW(null);

    // Singular values are in an arbitrary order initially
    SingularOps.descendingOrder(null, false, W, V, false);

    val sing = {
      val nonzero = new SimpleMatrix(W).extractDiag.getMatrix.getData.take(returnedAxes)
      val zero = new DenseMatrix64F(returnedAxes, 1)
      System.arraycopy(nonzero, 0, zero.getData, 0, nonzero.size)
      zero.getData
    }

    val scorematrix = new DenseMatrix64F(mat.numRows, returnedAxes);

    val truncatedV = CommonOps.extract(V, 0, V.getNumRows, 0, returnedAxes) //V.reshape(V.getNumRows, returnedAxes, false);

    // score matrix could be computed by U*W which is quicker since W is diagonal
    CommonOps.mult(centeredDM, truncatedV, scorematrix)

    val eigenVectors = Mat(truncatedV.getNumRows, truncatedV.getNumCols, truncatedV.getData)
    // this factor scaled the eigenvector to sqrt(eval) length
    val eigenVectorFactors = Vec(sing.map(math.abs)) / Vec(eigenVectors.cols.map(v => math.sqrt(v dot v)): _*)

    (Mat(scorematrix.getNumRows, scorematrix.getNumCols, scorematrix.getData), Vec(sing).map(x => x * x), eigenVectors, Mat(eigenVectors.cols.zipWithIndex.map(x => x._1 * eigenVectorFactors.raw(x._2)): _*))
  }

  def pcaFromFrame[RX, CX](frame: Frame[RX, CX, Double], maxAxes: Int)(implicit st1: ST[RX], o1: Ordering[RX], st2: ST[CX], o2: Ordering[CX]): PCAResultFrames[RX, CX] = {
    val (projected, eigenvalues, eigenvectors, loadings) = pcaFromSaddle(frame.toMat, maxAxes)
    PCAResultFrames(
      Frame(mat = projected, rowIx = frame.rowIx, colIx = Index(0 until projected.numCols: _*)),
      eigenvalues,
      Frame(mat = eigenvectors, rowIx = frame.colIx, colIx = Index(0 until eigenvectors.numCols: _*)),
      Frame(mat = loadings, rowIx = frame.colIx, colIx = Index(0 until loadings.numCols: _*))
    )
  }

  def createPCAPlots[RX, CX](
    frame: Frame[RX, CX, Double],
    maxAxes: Int = 4,
    title: String = "",
    labels: Map[mybiotools.plots.ScatterPlot.Label, Seq[RX]] = Map[mybiotools.plots.ScatterPlot.Label, Seq[RX]]()
  )(implicit st1: ST[RX], o1: Ordering[RX], st2: ST[CX], o2: Ordering[CX]): (de.erichseifert.gral.graphics.Drawable, mybiotools.pcanew.PCAResultFrames[RX, CX]) = {
    val pcaresult = pcaFromFrame(frame, maxAxes)
    (createPCAPlots(pcaresult, frame, maxAxes, title, labels), pcaresult)
  }

  def createPCAPlots[RX, CX](
    pcaresult: PCAResultFrames[RX, CX],
    frame: Frame[RX, CX, Double],
    maxAxes: Int,
    title: String,
    labels: Map[mybiotools.plots.ScatterPlot.Label, Seq[RX]]
  )(implicit st1: ST[RX], o1: Ordering[RX], st2: ST[CX], o2: Ordering[CX]): de.erichseifert.gral.graphics.Drawable = {
    import de.erichseifert.gral.graphics._

    val PCAResultFrames(projected, eigenvalues, eigenvectors, loadingmatrix) = pcaresult
    val sum = eigenvalues.sum
    val scaled = eigenvalues.map(_ / sum)
    val pcaplots =
      (0 until math.min(maxAxes, projected.numCols)).combinations(2).map { list =>
        val c1 = projected.firstCol(list.head)
        val c2 = projected.firstCol(list(1))
        val zipped: Seq[(Label, IndexedSeq[D2vD3])] = c1.joinMap(c2, how = index.InnerJoin)((x, y) =>
          (x, y)).map(x =>
          (x._1, (labels.filter(y =>
            y._2.contains(x._1))
            .headOption.map(_._1).getOrElse(Label("?", java.awt.Color.black)), seq2(Vector(x._2))))).toVec.toSeq.filter(_ != null)
        val ev1 = scaled(list.head)
        val ev2 = scaled(list(1))
        mybiotools.plots.ScatterPlot.createScatterPlotFromMultiple(
          data = zipped,
          main = "PCA " + title,
          xlab = "pc" + (list.head + 1) + ", " + formatDouble(ev1.get(0) * 100.0) + "% variance",
          ylab = "pc" + (list(1) + 1) + ", " + formatDouble(ev2.get(0) * 100.0) + "% variance"

        )
      }.toList

    val screeplot = {
      mybiotools.plots.ScatterPlot.createScatterPlot(
        data = scaled.toSeq.sortBy(x => -1.0 * x).zipWithIndex.map(x => (x._1, x._2.toDouble).swap),
        main = "Scree",
        xlab = "rank",
        ylab = "scaled eigenvalues"
      )
    }

    val loadingplots = (0 until math.min(maxAxes, loadingmatrix.numCols)).toList flatMap {
      (idx: Int) =>
        val loadings = loadingmatrix.firstCol(idx).toVec
        val sd = math.sqrt(loadings.variance)
        val mean = loadings.mean
        val outliers = loadingmatrix.firstCol(idx).filter(x => math.abs(x - mean) > 3 * sd).toSeq.map(_._1)
        val sorted = loadingmatrix.firstCol(idx).sorted

        val histogram = mybiotools.plots.HistogramPlot.createHistogramPlot(
          data = loadings.toSeq,
          breaks = -1, main = "PC" + (idx + 1) + " loadings", xlab = ("first 15 outliers: " + outliers.take(15).grouped(5).map(_.mkString(",")).mkString("\n"))
        )

        val heatmap: DrawableContainer = {
          val cxx = (sorted.head(50).toSeq.map(_._1) ++ sorted.tail(50).toSeq.map(_._1)).distinct

          val selected = frame.col(cxx: _*)

          val variableOrder = Index(clustering.traverse(clustering.clusterFrameByRows(selected.T)(clustering.euclideanDistance)): _*)

          // this could be weighted by eigenvalues
          // val sampleOrder = Index(clustering.traverse(clustering.clusterFrameByRows(projected)(clustering.euclideanDistance)): _*)
          val sampleOrder = Index(projected.firstCol(idx).toSeq.sortBy(_._2).map(_._1): _*)

          val reorderedFrame = selected.reindex(rix = sampleOrder, cix = variableOrder)

          val colormap = mybiotools.plots.Raster.defaultColorMap(reorderedFrame.min.min.get, reorderedFrame.max.max.get)

          mybiotools.plots.Raster.createRasterPlot(reorderedFrame.T, main = "top 100 correlated variable with PC" + (idx + 1) + " (value from dataspace)", colormap = colormap)
        }

        histogram :: heatmap :: Nil

    }

    val plots = ((pcaplots) :+ screeplot) ::: loadingplots

    (mybiotools.plots.compositeTable(plots, 3))

  }

  def plotPCAResultToFile[T](pca: PCAResult[T], axisX: Int, axisY: Int, file: java.io.File, color: Map[T, java.awt.Color] = Map[T, java.awt.Color](), labels: Map[T, String] = Map[T, String]()) {
    mybiotools.writeBinaryToFile(
      file.getCanonicalPath,
      mybiotools.plots.renderToByteArray(
        plotPCAResult(pca, axisX, axisY, color, labels),
        "image/png", 1.0
      )
    )
  }

  def plotPCAResult[T](pca: PCAResult[T], axisX: Int, axisY: Int, colors: Map[T, java.awt.Color] = Map(), labels: Map[T, String] = Map()): XYPlot = {

    import de.erichseifert.gral.data.DataSeries;
    import de.erichseifert.gral.data.DataTable;
    import de.erichseifert.gral.data.DataSource;
    import de.erichseifert.gral.data.filters.Convolution;
    import de.erichseifert.gral.data.filters.Filter;
    import de.erichseifert.gral.data.filters.Kernel;
    import de.erichseifert.gral.data.filters.KernelUtils;
    import de.erichseifert.gral.data.filters.Median;
    import de.erichseifert.gral.plots.Plot;
    import de.erichseifert.gral.plots.XYPlot;
    import de.erichseifert.gral.plots.legends.Legend;
    import de.erichseifert.gral.plots.lines.DefaultLineRenderer2D;
    import de.erichseifert.gral.ui.InteractivePanel;
    import de.erichseifert.gral.util.GraphicsUtils;
    import de.erichseifert.gral.util.Insets2D;
    import de.erichseifert.gral.plots.PlotArea
    import de.erichseifert.gral.util.Orientation;
    import de.erichseifert.gral.plots.points._
    import de.erichseifert.gral.plots.lines._
    import de.erichseifert.gral.plots.axes._
    import de.erichseifert.gral.plots.XYPlot.XYPlotArea2D
    import de.erichseifert.gral.graphics.AbstractDrawable
    import de.erichseifert.gral.util.Location
    import java.awt._;
    import scala.runtime.RichInt
    import scala.runtime.RichFloat
    import de.erichseifert.gral.graphics.DrawableContainer
    import de.erichseifert.gral.graphics.TableLayout
    import de.erichseifert.gral.plots.colors.QuasiRandomColors

    import de.erichseifert.gral.graphics.EdgeLayout

    import mybiotools.SummaryStat
    import mybiotools.plots._
    import mybiotools.gwascommons._
    import mybiotools.gwascommons.associationresults._
    import mybiotools.plots._

    import scala.collection.JavaConversions._

    val datatables = {
      val defaultColor = new java.awt.Color(205, 55, 0)
      val cols: Map[java.awt.Color, DataTable] = (colors.toSeq.map(_._2).distinct.map(c => c -> new DataTable(classOf[java.lang.Double], classOf[java.lang.Double])) :+ (defaultColor -> new DataTable(classOf[java.lang.Double], classOf[java.lang.Double]))).toMap
      pca.coordinates.foreach {
        case (ind, values) =>
          val col = colors.get(ind).getOrElse(defaultColor)
          val x = values(axisX - 1)
          val y = values(axisY - 1)
          cols(col).add(x, y)
          cols(col).setName(labels.get(ind).getOrElse("default"))

      }

      cols.toSeq

    }

    val alldata: Iterable[(Double, Double)] = pca.coordinates.values.map(list => (list(axisX - 1), list(axisY - 1)))

    val xmin = alldata.map(_._1).min
    val xmax = alldata.map(_._1).max
    val ymin = alldata.map(_._2).min
    val ymax = alldata.map(_._2).max

    val plot = new XYPlot(datatables.map(_._2): _*)
    plot.setSetting(Plot.TITLE, s"Axes: " + axisX + " - " + axisY + " , #: " + pca.coordinates.size)

    // Customization
    datatables.foreach {
      case (color, datatable) =>
        val pointrenderer = plot.getPointRenderer(datatable)

        pointrenderer.setSetting(PointRenderer.SHAPE, new java.awt.geom.Ellipse2D.Double(0, 0, 5, 5))

        pointrenderer.setSetting(PointRenderer.COLOR, color)

    }

    val legend = plot.getLegend()
    legend.clear()
    datatables.foreach {
      case (_, dt) =>
        legend.add(dt)
    }
    plot.setSetting(Plot.LEGEND, true)
    plot.setSetting(Plot.LEGEND_DISTANCE, 1)
    plot.setSetting(Plot.LEGEND_LOCATION, Location.EAST)
    plot.setInsets(new Insets2D.Double(100.0, 100.0, 100.0, 200.0));

    val plotarea = plot.getPlotArea

    plotarea.setSetting(PlotArea.BORDER, new BasicStroke(1.0f))

    val axis_X = plot.getAxis(XYPlot.AXIS_X)
    val axis_Y = plot.getAxis(XYPlot.AXIS_Y)

    axis_X.setMin(xmin - 0.05 * (xmax - xmin + 1))
    axis_X.setMax(xmax + 0.05 * (xmax - xmin + 1))
    axis_Y.setMin(ymin - 0.05 * (ymax - ymin + 1))
    axis_Y.setMax(ymax + 0.05 * (ymax - ymin + 1))

    val rendererY = plot.getAxisRenderer(XYPlot.AXIS_Y);
    val rendererX = plot.getAxisRenderer(XYPlot.AXIS_X);
    rendererX.setSetting(AxisRenderer.LABEL, "PC " + axisX + " - eigenvalue: " + "%.2f".format(pca.eigenValues(axisX - 1) / pca.eigenValues.sum))

    rendererY.setSetting(AxisRenderer.LABEL, "PC " + axisY + " - eigenvalue: " + "%.2f".format(pca.eigenValues(axisY - 1) / pca.eigenValues.sum))

    rendererY.setSetting(AxisRenderer.INTERSECTION, -Double.MaxValue);
    rendererX.setSetting(AxisRenderer.INTERSECTION, -Double.MaxValue);

    val fontsmall = new java.awt.Font(Font.SANS_SERIF, Font.PLAIN, 12)

    rendererX.setSetting(AxisRenderer.LABEL_FONT, fontsmall)
    rendererY.setSetting(AxisRenderer.LABEL_FONT, fontsmall)
    rendererX.setSetting(AxisRenderer.TICKS_FONT, fontsmall)
    rendererY.setSetting(AxisRenderer.TICKS_FONT, fontsmall)

    plot.setInsets(new Insets2D.Double(100.0, 100.0, 100.0, 100.0));

    val bounds = new java.awt.geom.Rectangle2D.Double(0.0, 0.0, 700.0, 700.0)
    plot.setBounds(bounds)

    plot
  }

}