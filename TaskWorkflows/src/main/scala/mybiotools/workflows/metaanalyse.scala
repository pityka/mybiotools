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
// import mybiotools.tasks._
// import java.io.File
// import akka.actor.{ ActorRefFactory }
// import scala.util.Try
// import mybiotools.stat.ZMetaCombination._
// import mybiotools.sharedfiletypes._
// import mybiotools.sharedfiletypes.SharedFileSets._
// import mybiotools.eq._
//
// case class MetaAnalyseOutput(file: SharedFile) extends ResultWithSharedFiles(file)
//
// case class Groups(divisor: Int, remainder: Int)
// object Groups {
//   def makeN(n: Int) = {
//     assert(n >= 1)
//     for (i <- 0 until n) yield Groups(n, i)
//   }
// }
//
// case class MetaAnalyseInput(
//     files: Option[Set[SharedFile]],
//     expectedFileCount: Option[Int],
//     outname: Option[String],
//     groups: Option[Groups]
// ) extends SimplePrerequisitive[MetaAnalyseInput] {
//   override def ready = {
//
//     (expectedFileCount.isDefined && files.isDefined && outname.isDefined && files.get.size === expectedFileCount.get && groups.isDefined)
//
//   }
// }
//
// object MetaAnalyseInput {
//
//   def apply(n: Int, outname: String, groups: Groups): MetaAnalyseInput = {
//
//     MetaAnalyseInput(
//       expectedFileCount = Some(n),
//       outname = Some(outname),
//       files = None,
//       groups = Some(groups)
//     )
//   }
//   def applySharedFiles(files: Set[SharedFile], outname: String, groups: Groups): MetaAnalyseInput = {
//     MetaAnalyseInput(
//       expectedFileCount = Some(files.size),
//       outname = Some(outname),
//       files = Some(files),
//       groups = Some(groups)
//     )
//   }
//
//   def updateMetaAnalyseInput: UpdatePrerequisitive[MetaAnalyseInput] = {
//     case (self, i: TracksOutput) => self.copy(files = if (self.files.isDefined) Some((self.files.get + i.tests).toSet) else Some(Set(i.tests)))
//     case (self, i: PlinkAssocFile) => {
//       self.copy(files = if (self.files.isDefined) Some((self.files.get + i.file).toSet) else Some(Set(i.file)))
//     }
//     case (self, i: PoeOutput) => self.copy(files = if (self.files.isDefined) Some((self.files.get + i.tests).toSet) else Some(Set(i.tests)))
//     case (self, i: JoinHumanGenesOutput) =>
//       self.copy(files = if (self.files.isDefined) Some((self.files.get + i.tests).toSet) else Some(Set(i.tests)))
//   }
// }
//
// trait MetaAnalysisStub[B, K, C] {
//
//   private def combineP(l: Iterable[File], divisor: Int, remainder: Int)(extract: File => Iterator[(K, B)])(combine: Seq[B] => C): File = {
//
//     val tmpFile = TempFile.createTempFile("combinedp.gz")
//
//     openZippedFileWriter(tmpFile) { writer =>
//
//       val testsMaps: List[Iterator[(K, B)]] =
//         l.map(x => extract(x)
//           .filter(x => (scala.math.abs(x._1.toString.hashCode) % divisor) == remainder)).toList
//
//       val combined: Iterator[(K, C)] = collectStreamsAndMap(testsMaps)((k, vs) => k -> combine(vs))
//
//       combined.foreach {
//         case (key, p) =>
//           val keystring = key match {
//             case x: Product => x.productIterator.toList.mkString(" ")
//             case x => x.toString
//           }
//           val pstring = p match {
//             case x: Product => x.productIterator.toList.mkString(" ")
//             case x => x.toString
//           }
//           writer.write(keystring + " " + pstring)
//           writer.write("\n")
//       }
//
//     }
//
//     tmpFile
//
//   }
//
//   def extract(f: File): Iterator[(K, B)]
//
//   def combine(s: Seq[B]): C
//
//   def runStub(in: (MetaAnalyseInput, ComputationEnvironment)) = in match {
//     case (MetaAnalyseInput(Some(files), _, Some(outname), Some(Groups(divisor, remainder))), ce) =>
//
//       import ce._
//       val combined = combineP(files.toSeq.sortBy(_.name).map(_.file), divisor, remainder)(extract _)(combine _)
//
//       MetaAnalyseOutput(SharedFile(combined, name = outname + ".gz", canMoveAway = true))
//
//   }
// }
//
// object metaAnalyiseTracks extends MetaAnalysisStub[Double, (String, String, String), Double] {
//
//   def extract(file: File) = scala.io.Source.fromFile(file.getCanonicalPath).getLines.map { line =>
//     val spl = fastSplitSetSeparator(line, Set(' ', '\t'))
//     (spl(0), spl(1), spl(3)) -> Try(spl(2).toDouble).toOption
//   }.filter(_._2.isDefined).map(x => x._1 -> x._2.get)
//
//   def combine(s: Seq[Double]) = stat.FishersCombination.apply(s)
//
//   def apply(
//     in: MetaAnalyseInput,
//     update: UpdatePrerequisitive[MetaAnalyseInput] = MetaAnalyseInput.updateMetaAnalyseInput orElse identity[MetaAnalyseInput],
//     memory: Int
//   )(implicit components: TaskSystemComponents) =
//     newTask(
//       in, update, CPUMemoryRequest(cpu = 1, memory = memory)
//     ) { (in, ce) =>
//       runStub((in, ce))
//     }
// }
//
// object metaAnalyiseTracksWithGenes extends MetaAnalysisStub[Double, (String, String, String, String, String, String), Double] {
//
//   def extract(file: File) = scala.io.Source.fromFile(file.getCanonicalPath).getLines.map { line =>
//     val spl = fastSplitSetSeparator(line, Set(' ', '\t'))
//     (spl(0), spl(1), spl(3), spl(4), spl(5), spl(6)) -> Try(spl(2).toDouble).toOption
//   }.filter(_._2.isDefined).map(x => x._1 -> x._2.get)
//
//   def combine(s: Seq[Double]) = stat.FishersCombination.apply(s)
//
//   def apply(
//     in: MetaAnalyseInput,
//     update: UpdatePrerequisitive[MetaAnalyseInput] = MetaAnalyseInput.updateMetaAnalyseInput orElse identity[MetaAnalyseInput],
//     memory: Int
//   )(implicit components: TaskSystemComponents) =
//     newTask(
//       in, update, CPUMemoryRequest(cpu = 1, memory = memory)
//     ) { (in, ce) =>
//       runStub((in, ce))
//     }
// }
//
// object metaAnalyseGWAS extends MetaAnalysisStub[(Double, Double, String), (String, String, String, String, String), (Double, Double, Double, String)] {
//
//   def extract(file: File) = {
//     rethrow("Error during field extraction in " + file.getAbsolutePath) {
//       val lines = createSourceFromZippedFile(file.getCanonicalPath).getLines
//       val header = fastSplitSetSeparator(lines.next, Set(' ', '\t'))
//       val chridx = header.indexOf("CHR")
//       val snpidx = header.indexOf("SNP")
//       val bpidx = header.indexOf("BP")
//       val a1idx = header.indexOf("A1")
//       val testidx = header.indexOf("TEST")
//       val betaidx = header.indexOf("BETA")
//       val phenoidx = header.indexOf("PHENO")
//       val seidx = header.indexOf("SE")
//       val intermarker = header.indexOf("INTER_MARKER")
//       val intermodel = header.indexOf("INTER_MODEL")
//
//       if (chridx < 0 || snpidx < 0 || bpidx < 0 || testidx < 0 || phenoidx < 0) {
//         println(file)
//         println(header)
//       }
//
//       lines.map { line =>
//         val spl = fastSplitSetSeparator(line, Set(' ', '\t'))
//         (
//           spl(chridx),
//           spl(snpidx),
//           spl(bpidx),
//           spl(testidx) + (if (intermodel >= 0) spl(intermodel) else ""),
//           spl(phenoidx)
//         ) -> Try((
//             spl(betaidx).toDouble,
//             spl(seidx).toDouble,
//             spl(a1idx)
//           )).toOption
//       }.filter(_._2.isDefined).map(x => x._1 -> x._2.get)
//     }
//
//   }
//
//   def combine(betaSEAllele: Seq[(Double, Double, String)]) = {
//     val byAllele: Map[String, Iterable[(Double, Double, String)]] = betaSEAllele.groupBy(_._3)
//     if (byAllele.size == 1) {
//       val (w, s) = inverseVarianceWeightedSumWithSD(betaSEAllele.map(x => (x._1, x._2)))
//       val z = w / s
//       (zTest2Sided(z), w, s, byAllele.keys.head)
//     } else if (byAllele.size == 2) {
//       val s = byAllele.toSeq
//       val first = s.head
//       val last = s.last
//       val lastreversed = last._2.map(x => (x._1 * -1.0, x._2))
//       val firstAllele = first._1
//       val firstbetase = first._2.map(x => (x._1, x._2))
//       val (weightedMean, sdOfWeightedMean) = inverseVarianceWeightedSumWithSD(firstbetase ++ lastreversed)
//       val z = weightedMean / sdOfWeightedMean
//       (zTest2Sided(z), weightedMean, sdOfWeightedMean, firstAllele)
//     } else (Double.NaN, Double.NaN, Double.NaN, "?")
//
//   }
//
//   def apply(
//     in: MetaAnalyseInput,
//     update: UpdatePrerequisitive[MetaAnalyseInput] = MetaAnalyseInput.updateMetaAnalyseInput orElse identity[MetaAnalyseInput],
//     memory: Int
//   )(implicit components: TaskSystemComponents) =
//     newTask(
//       in, update, CPUMemoryRequest(cpu = 1, memory = memory)
//     ) { (in, ce) =>
//       runStub((in, ce))
//     }
// }
//
// object metaAnalysePOE extends MetaAnalysisStub[(Double, Double), (String, String, String, String, String), Double] {
//
//   def extract(file: File) = io.Source.fromFile(file.getCanonicalPath).getLines.map { line =>
//     val spl = fastSplitSetSeparator(line, Set(' ', '\t'))
//     (spl(0), spl(1), spl(2), spl(3), spl(4)) -> Try((spl(8).toDouble, spl(9).toDouble)).toOption
//   }.filter(_._2.isDefined).map(x => x._1 -> x._2.get)
//
//   def combine(s: Seq[(Double, Double)]) = stat.ZMetaCombination.zTest1sided_upper(stat.ZMetaCombination.inverseVarianceWeightedSumZ(s))
//
//   def apply(
//     in: MetaAnalyseInput,
//     update: UpdatePrerequisitive[MetaAnalyseInput] = MetaAnalyseInput.updateMetaAnalyseInput orElse identity[MetaAnalyseInput],
//     memory: Int
//   )(implicit components: TaskSystemComponents) =
//     newTask(
//       in, update, CPUMemoryRequest(cpu = 1, memory = memory)
//     ) { (in, ce) =>
//       runStub((in, ce))
//     }
// }
//
