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
// package mybiotools.workflows
//
// import mybiotools._
// import mybiotools.gwascommons._
// import mybiotools.gwascommons.associationresults._
// import gwascommons.genotypedata._
// import gwascommons.gwas._
// import gwascommons.gwas.GWAS._
// import mybiotools.config.Config.configInstance
// import mybiotools.stat.LinearRegression.readPlinkCovarFile
// import collection.JavaConversions._
// import java.io.File
// import _root_.ch.systemsx.cisd.hdf5._
// import hdfdosage.HDFDosageIterator
// import mybiotools.stringstore._
// import scala.concurrent.Future
// import akka.actor.{ ActorRef, Actor, ActorRefFactory, Props }
// import akka.actor.Actor._
// import mybiotools.tasks._
// import java.io.File
// import scala.concurrent.duration._
// import mybiotools.eq._
// import mybiotools.iterator._
//
// case class Plots(manh: SharedFile, qq: SharedFile) extends ResultWithSharedFiles(manh, qq)
// case class ListOfPlots(list: List[Plots]) extends ResultWithSharedFiles(list.flatMap(x => x.manh :: x.qq :: Nil): _*)
//
// case class HeaderPositions(
//   snpcolumn: Int,
//   pcolumn: Int,
//   chrcolumn: Option[Int],
//   bpcolumn: Option[Int],
//   effectcolumn: Option[Int],
//   allelecolumn: Option[Int],
//   nonMissColumn: Option[Int],
//   errorColumn: Option[Int],
//   testColumn: Option[Int],
//   frqColumn: Option[Int],
//   phenoColumn: Option[Int]
// ) extends Result
//
// case class PlotTaskInput(
//   assocFile: Option[Set[SharedFile]],
//   expectedFiles: Option[Int],
//   maxPValue: Option[Double],
//   phenotypes: Option[List[Option[String]]],
//   header: Option[HeaderPositions],
//   outname: Option[Option[String]]
// )
//     extends Prerequisitive[PlotTaskInput] {
//   def ready = expectedFiles.isDefined &&
//     maxPValue.isDefined &&
//     phenotypes.isDefined &&
//     header.isDefined &&
//     outname.isDefined &&
//     assocFile.isDefined &&
//     assocFile.get.size === expectedFiles.get
// }
// object PlotTaskInput {
//
//   def apply(expectedFiles: Int, header: HeaderPositions, maxPValue: Double, phenotypes: List[Option[String]], outname: Option[String]): PlotTaskInput = {
//
//     PlotTaskInput(
//       assocFile = None, header = Some(header), maxPValue = Some(maxPValue), phenotypes = Some(phenotypes), outname = Some(outname), expectedFiles = Some(expectedFiles)
//     )
//   }
//
//   def updatePlotTaskInput: UpdatePrerequisitive[PlotTaskInput] = {
//     // case (self, m: SharedAssocFile) => self.copy(assocFile = Some(m.fileNamep))
//     case (self, i: CatFilesOutput) => self.copy(assocFile = Some((self.assocFile.map(xs => xs + i.file).getOrElse(Set(i.file)))))
//   }
// }
// object plotGWAS {
//
//   def apply(
//     in: PlotTaskInput,
//     memory: Int,
//     update: UpdatePrerequisitive[PlotTaskInput] = PlotTaskInput.updatePlotTaskInput orElse identity[PlotTaskInput]
//   )(implicit components: TaskSystemComponents) =
//     newTask(
//       in, update, CPUMemoryRequest(cpu = 1, memory = memory)
//     ) {
//       case (PlotTaskInput(Some(assocFile), _, Some(maxPValue), Some(phenotypes), Some(header), Some(outname)), ce) =>
//         import ce._
//
//         val tests = Set(
//           StringStore("ADD"),
//           StringStore("DOM"),
//           StringStore("REC"),
//           StringStore("HETADV")
//         )
//
//         def concatiter(l: Vector[File]): Iterator[String] = new Iterator[String] {
//           var fi = -1
//
//           val n = l.size
//
//           var open: Option[(scala.io.Source, Iterator[String])] = None
//           def hasNext = {
//             val b = (fi < n - 1 || open.map(_._2.hasNext).getOrElse(false))
//             if (b) take
//             else if (open.isDefined) open.get._1.close
//             b
//           }
//           def next = open.get._2.next
//
//           def take = if (open.isEmpty || !open.get._2.hasNext) {
//             open.foreach(_._1.close)
//             fi += 1
//             val s = mybiotools.createSource(l(fi))
//             open = Some(s, s.getLines)
//           }
//         }
//
//         val accumulators = phenotypes.map { ph =>
//           val ph8 = ph.map(x => StringStore(x))
//           ph -> filterAndCopy[AssociationResult] {
//             _ match {
//               case x: HasPhenotypeName => ph.isEmpty || x.phenotypeName === ph8.get
//               case _ => true
//             }
//           }
//         }
//         val mapper = new Mapper(accumulators.map(_._2))
//
//         val iter =
//           gwascommons.associationresults.readAssociationResultsFromFileWithoutHeader(
//             concatiter(assocFile.toVector.map(_.localFile).filter(f => openSource(f)(_.getLines.hasNext))),
//             None,
//             snpcolumn = header.snpcolumn,
//             pcolumn = header.pcolumn,
//             chrcolumn = header.chrcolumn,
//             bpcolumn = header.bpcolumn,
//             effectcolumn = header.effectcolumn,
//             allelecolumn = header.allelecolumn,
//             nonMissColumn = header.nonMissColumn,
//             errorColumn = header.errorColumn,
//             testColumn = header.testColumn,
//             frqColumn = header.frqColumn,
//             phenoColumn = header.phenoColumn
//           ).filter {
//               case x: HasTest => tests.contains(x.test)
//               case _ => true
//             }.filter(_.pValue < maxPValue)
//
//         mapper.map(iter).exhaust
//
//         ListOfPlots(accumulators.map {
//           case (ph, acc) =>
//             val manhattan = TempFile.createTempFile(".manh.png")
//             val qqplot = TempFile.createTempFile(".qq.png")
//             writeBinaryToFile(manhattan.getCanonicalPath, plots.ManhattanPlot.plotToPNG(acc.buffer.iterator))
//             plots.pngToFile(qqplot, plots.QQPlot.plot(acc.buffer.iterator.map(_.pValue), maximum = Some(maxPValue), disablePruning = false))
//
//             Plots(manh = SharedFile(manhattan, name = outname.getOrElse(ph.getOrElse(assocFile.head.name)).replaceAll("[^a-zA-Z0-9.-]", "_") + ".manh.png"), qq = SharedFile(qqplot, name = outname.getOrElse(ph.getOrElse(assocFile.head.name)).replaceAll("[^a-zA-Z0-9.-]", "_") + ".qq.png"))
//
//         })
//
//     }
//
// }
//
