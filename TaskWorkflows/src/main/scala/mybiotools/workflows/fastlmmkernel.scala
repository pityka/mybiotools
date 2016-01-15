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

// object fastlmmkerneltask {
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
//             val tmp = TempFile.createTempFile("covar")
//             mybiotools.writeToFile(
//               tmp.getAbsolutePath,
//               FrameToPlink.frameToPlinkNoHeader(covariate.col(covariateNames: _*), "-9")
//             )
//             tmp
//           }
//           (phenofile, 1, covarfile)
//         }

//         // get the kernel
//         val simfile: File = if (kernel.isDefined) kernel.get.file.file
//         else kernelSNPs.get match {
//           case UseAll => {

//             val snpmajorreader = newconcatiter(0.01)

//             {
//               val (mat, snpcount) = GRM.getGRMFromAutosomes(snpmajorreader, batchSize = 10000, threads = resourceAllocated.cpu)
//               val tmp = TempFile.createTempFile(".sim")
//               openFileWriter(tmp) { writer =>
//                 GRM.writeFastlmm(mat, writer)
//               }
//               tmp
//             }

//           }
//           case UseRandom(n) => {
//             val max = newconcatiter(0.01).snpIterator.size
//             val rand = new scala.util.Random(1)
//             def dice: Boolean = {
//               rand.nextDouble <= (n / max.toDouble)
//             }
//             val filtered = newconcatiter(0.1).filter(x => rand.nextDouble <= (n / max.toDouble))

//             val tmp = TempFile.createTempFile(".pdose")
//             openFileWriter(tmp) { writer =>
//               DosageWriter.writePDose(writer, filtered.individuals, filtered.snpIterator, false)
//             }

//             GRM.getGRMWithFastlmmFromPDose(tmp, phenofile, phenoindex)

//           }
//           case UseSpecific(list) => {

//             val set = list.toSet

//             val filtered = newconcatiter(0.0).filter(x => set.contains(x.snpName))

//             val tmp = TempFile.createTempFile(".pdose")
//             openFileWriter(tmp) { writer =>
//               DosageWriter.writePDose(writer, filtered.individuals, filtered.snpIterator, false)
//             }

//             GRM.getGRMWithFastlmmFromPDose(tmp, phenofile, phenoindex)
//           }
//         }

//         FastlmmGRMFile(SharedFile(simfile))

//     }

// }

