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

// package mybiotools.workflows

// import mybiotools._
// import mybiotools.tasks._
// import java.io.File
// import akka.actor.{ ActorRefFactory }
// import com.typesafe.config.{ Config, ConfigFactory }
// import collection.JavaConversions._
// import mybiotools.gwascommons._
// import mybiotools.gwascommons.genotypedata._
// import mybiotools.stringstore._
// import scala.concurrent._
// import ExecutionContext.Implicits.global
// import hdfdosage.FileSets

// import hdfdosage.{ DosageFileFormat, HDFDosage, PDose, PGenotypeProbabilities, BIGSNPMajor, TPed }

// sealed trait FastlmmKernelSNPs extends Result
// case object UseAll extends FastlmmKernelSNPs
// case class UseRandom(n: Int) extends FastlmmKernelSNPs
// case class UseSpecific(snps: Seq[String8]) extends FastlmmKernelSNPs

// case class FastlmmInput(
//     outname: Option[String],
//     parameters: Option[String],
//     tpedfiles: Option[List[TPedFileSet]],
//     pdose: Option[List[SharedFile]],
//     hdfdosage: Option[List[SharedFile]],
//     pgeno: Option[List[SharedFile]],
//     vcf: Option[List[SharedFile]],
//     expectedfiles: Option[Int],
//     covariateFile: Option[PlinkCovariateFile],
//     phenoname: Option[String],
//     covariateNames: Option[Seq[String]],
//     kernelSNPs: Option[FastlmmKernelSNPs],
//     kernel: Option[FastlmmGRMFile]
// ) extends Prerequisitive[FastlmmInput] {

//   def ready = outname.isDefined &&
//     parameters.isDefined &&
//     (
//       (tpedfiles.isDefined && expectedfiles.isDefined && tpedfiles.get.size == expectedfiles.get) ||
//       (hdfdosage.isDefined && expectedfiles.isDefined && hdfdosage.get.size == expectedfiles.get) ||
//       (pgeno.isDefined && expectedfiles.isDefined && pgeno.get.size == expectedfiles.get) ||
//       (vcf.isDefined && expectedfiles.isDefined && vcf.get.size == expectedfiles.get) ||
//       (pdose.isDefined && expectedfiles.isDefined && pdose.get.size == expectedfiles.get)
//     ) &&
//       covariateFile.isDefined &&
//       phenoname.isDefined &&
//       covariateNames.isDefined &&
//       (kernel.isDefined || kernelSNPs.isDefined)

// }

// case class FastlmmOutput(
//   tests: SharedFile, log: SharedFile, kernel: SharedFile
// ) extends ResultWithSharedFiles(tests, log, kernel)

// object FastlmmInput {

//   def apply(
//     outname: String,
//     parameters: String,
//     covariateFile: File,
//     phenoname: String,
//     covariateNames: Seq[String],
//     expectedfiles: Int
//   )(implicit components: TaskSystemComponents): FastlmmInput = {

//     FastlmmInput(
//       outname = Some(outname),
//       parameters = Some(parameters),
//       tpedfiles = None,
//       pdose = None,
//       vcf = None,
//       pgeno = None,
//       hdfdosage = None,
//       expectedfiles = Some(expectedfiles),
//       covariateFile = Some(PlinkCovariateFile(SharedFile(covariateFile))),
//       phenoname = Some(phenoname),
//       covariateNames = Some(covariateNames),
//       kernelSNPs = None,
//       kernel = None
//     )
//   }

//   def apply(
//     outname: String,
//     parameters: String,
//     phenoname: String,
//     covariateNames: Seq[String],
//     expectedfiles: Int
//   )(implicit components: TaskSystemComponents): FastlmmInput = {

//     FastlmmInput(
//       outname = Some(outname),
//       parameters = Some(parameters),
//       tpedfiles = None,
//       pdose = None,
//       vcf = None,
//       pgeno = None,
//       hdfdosage = None,
//       expectedfiles = Some(expectedfiles),
//       covariateFile = None,
//       phenoname = Some(phenoname),
//       covariateNames = Some(covariateNames),
//       kernelSNPs = None,
//       kernel = None
//     )
//   }

//   private def updatePDoseList(input: FastlmmInput, pdose: PDoseFile): FastlmmInput = {
//     val updated = input.pdose match {
//       case None => Some(List(pdose.file))
//       case Some(xs) => Some(pdose.file :: xs)
//     }
//     input.copy(pdose = updated)

//   }

//   private def updateTpedList(input: FastlmmInput, pdose: TPedFileSet): FastlmmInput = {
//     val updated = input.tpedfiles match {
//       case None => Some(List(pdose))
//       case Some(xs) => Some(pdose :: xs)
//     }
//     input.copy(tpedfiles = updated)

//   }
//   private def updateHDFDosageList(input: FastlmmInput, file: SharedFile): FastlmmInput = {
//     val updated = input.hdfdosage match {
//       case None => Some(List(file))
//       case Some(xs) => Some(file :: xs)
//     }
//     input.copy(hdfdosage = updated)
//   }
//   private def updatePGenoList(input: FastlmmInput, file: SharedFile): FastlmmInput = {
//     val updated = input.pgeno match {
//       case None => Some(List(file))
//       case Some(xs) => Some(file :: xs)
//     }
//     input.copy(pgeno = updated)
//   }
//   private def updateVCFList(input: FastlmmInput, file: SharedFile): FastlmmInput = {
//     val updated = input.vcf match {
//       case None => Some(List(file))
//       case Some(xs) => Some(file :: xs)
//     }
//     input.copy(vcf = updated)
//   }

//   def update: UpdatePrerequisitive[FastlmmInput] = {
//     case (self, plinkc: PlinkCovariateFile) => self.copy(covariateFile = Some(plinkc))
//     case (self, m: FastlmmKernelSNPs) => self.copy(kernelSNPs = Some(m))
//     case (self, m: FastlmmGRMFile) => self.copy(kernel = Some(m))
//     case (self, m: GRMFile) => self.copy(kernel = Some(m.fastlmm))
//     case (self, m: TPedFileSet) => updateTpedList(self, m)
//     case (self, m: PDoseFile) => updatePDoseList(self, m)
//     case (self, m: PGenoFile) => updatePGenoList(self, m.file)
//     case (self, m: VCFFile) => updateVCFList(self, m.file)
//   }

// }

// object fastlmmtask {
//   def apply(
//     in: FastlmmInput,
//     cpu: Int = 1,
//     memory: Int = 20000,
//     update: UpdatePrerequisitive[FastlmmInput] = FastlmmInput.update orElse identity[FastlmmInput]
//   )(implicit ce: TaskSystemComponents): ProxyTaskActorRef =
//     newTask(
//       in, update, CPUMemoryRequest(cpu = cpu, memory = memory)
//     ) {

//       case (
//         FastlmmInput(
//           Some(outname),
//           Some(parameters),
//           tpedfiles,
//           pdose,
//           hdfdosage,
//           pgeno,
//           vcf,
//           _,
//           Some(covariateFile),
//           Some(phenoname),
//           Some(covariateNames),
//           kernelSNPs,
//           kernel), ce) =>
//         import ce._
//         implicit val components = ce.toTaskSystemComponents

//         def newconcatiter(maf: Double) = {
//           val filesets: List[FileSets.GenotypeFileSet] =
//             if (pdose.isDefined) pdose.get.map(pd => FileSets.PDose(file = pd.file, missingValue = -9f, fam = None))
//             else if (pgeno.isDefined) pgeno.get.map(pd => FileSets.PGenotypeProbabilities(file = pd.file, missingValue = -9f, fam = None))
//             else if (vcf.isDefined) vcf.get.map(pd => FileSets.VCFFile(pd.file))
//             else hdfdosage.get.map(pd => FileSets.HDFDosage(pd.file))

//           SNPMajorReaders.concatenate(filesets.iterator.map(fs => FileSets.getIteratorFromFileSet(fs, minimumMaf = maf, 1.0, subset = Full, Set()))).get

//         }

//         val (phenofile, phenoindex, cleanedCovarfile) = {
//           val individualsingenotypefile = newconcatiter(0.0).individuals

//           val covariate = openSource(covariateFile.file.file.getAbsolutePath)(source => mybiotools.stat.LinearRegression.readPlinkCovarFile(source, "-9")).col((phenoname +: covariateNames): _*).row(individualsingenotypefile: _*).rfilter(x => !x.hasNA)

//           val phenofile = {
//             val tmp = TempFile.createTempFile("pheno")
//             mybiotools.writeToFile(
//               tmp.getAbsolutePath,
//               FrameToPlink.frameToPlink(covariate.col(phenoname), "-9")
//             )
//             tmp
//           }
//           val covarfile = {
//             val cov = covariate.col(covariateNames: _*)
//             if (cov.numRows > 0 && cov.numCols > 0) {
//               val tmp = TempFile.createTempFile("covar")
//               mybiotools.writeToFile(
//                 tmp.getAbsolutePath,
//                 FrameToPlink.frameToPlinkNoHeader(cov, "-9")
//               )
//               Some(tmp)
//             } else None
//           }
//           (phenofile, 1, covarfile)
//         }

//         // get the kernel
//         val simfile: File = kernel.get.file.file

//         val futureResult: Future[FastlmmOutput] = {
//           val n = newconcatiter(0.0).snpIterator.size
//           val chunksize = 250000
//           if (n > chunksize) {
//             val chunks = {

//               (for (i <- 0 to n by chunksize) yield FileSubSet(fromIdx = i, toIdx = (if (i + chunksize <= n) i + chunksize else n))).filter(x => x.toIdx - x.fromIdx > 0)
//             }
//             Future.sequence(chunks.zipWithIndex.map {
//               case (chunk, chunkidx) =>
//                 val filtered = newconcatiter(0.0).filterByIndex(chunk)
//                 val file = TempFile.createTempFile(".pdose")
//                 openFileWriter(file) { writer =>
//                   DosageWriter.writePDose(writer, filtered.individuals, filtered.snpIterator, false)
//                 }

//                 val pdoseinput = PDoseFile(SharedFile(file, name = outname + ".chunk." + chunkidx, canMoveAway = true))
//                 val fastlmminput = FastlmmInput(
//                   outname = outname + "." + chunkidx,
//                   parameters = parameters,
//                   phenoname = phenoname,
//                   covariateNames = covariateNames,
//                   expectedfiles = 1
//                 )

//                 val task = fastlmmtask(fastlmminput)

//                 task <~ pdoseinput
//                 task <~ FastlmmGRMFile(SharedFile(simfile, name = outname + ".fastlmm", canMoveAway = false))
//                 task <~ covariateFile

//                 task.?[FastlmmOutput]

//             }).map { listofFastlmOutputs =>
//               val cattedout = TempFile.createTempFile(".catted")
//               val cattedlog = TempFile.createTempFile(".catted")
//               mybiotools.cat(listofFastlmOutputs.map(_.tests.file), cattedout, 1)
//               mybiotools.cat(listofFastlmOutputs.map(_.log.file), cattedlog, 0)
//               FastlmmOutput(
//                 tests = SharedFile(cattedout, name = outname + ".tests", canMoveAway = true),
//                 log = SharedFile(cattedlog, name = outname + ".logs", canMoveAway = true),
//                 kernel = SharedFile(simfile, name = outname + ".sim", canMoveAway = false)
//               )
//             }
//           } else {
//             // if chunk is small enough
//             val file = {
//               val nonfiltered = newconcatiter(0.0)
//               val file = TempFile.createTempFile(".pdose")
//               openFileWriter(file) { writer =>
//                 DosageWriter.writePDose(writer, nonfiltered.individuals, nonfiltered.snpIterator, false)
//               }
//               file
//             }
//             val (tests, log) = runFastlmm(file, phenofile, cleanedCovarfile, phenoindex, simfile)
//             Future {
//               FastlmmOutput(
//                 tests = SharedFile(tests, name = outname + ".tests", canMoveAway = true),
//                 log = SharedFile(log, name = outname + ".logs", canMoveAway = true),
//                 kernel = SharedFile(simfile, name = outname + ".sim", canMoveAway = false)
//               )
//             }
//           }
//         }

//         LauncherActor.block(CPUMemoryRequest(resourceAllocated.cpu, resourceAllocated.memory)) {
//           Await.result(futureResult, scala.concurrent.duration.Duration.Inf)
//         }
//     }

// }

