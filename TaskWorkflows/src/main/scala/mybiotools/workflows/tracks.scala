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

package mybiotools.workflows

import mybiotools._
import mybiotools.tasks._
import java.io.File
import akka.actor.{ ActorRefFactory }
import com.typesafe.config.{ Config, ConfigFactory, ConfigRenderOptions }
import collection.JavaConversions._

import hdfdosage.{ DosageFileFormat, HDFDosage, PDose, PGenotypeProbabilities, BIGSNPMajor, TPed }

case class PlinkCovariateFile(file: SharedFile) extends ResultWithSharedFiles(file)

case class TracksParameters(
  config: String,
  obligatoryCovariateNames: List[String]
) extends Result

object TracksParameters {
  def apply(c: Config, obligCov: List[String]): TracksParameters = TracksParameters(c.root.render(ConfigRenderOptions.defaults().setComments(false).setFormatted(false)), obligCov)
}

case class TracksInput(
    outname: Option[String],
    parameters: Option[TracksParameters],
    inputlist: Option[DosageToolInputFiles],
    genomicMap: Option[BimFile],
    covariateFile: Option[PlinkCovariateFile],
    jarFile: Option[JarFile]
) extends SimplePrerequisitive[TracksInput] {
  override def persistent = this.copy(jarFile = None)
}

case class TracksOutput(
  tests: SharedFile, qqplots: Seq[SharedFile], supportFilesZipped: SharedFile
) extends ResultWithSharedFiles((qqplots ++ Seq(tests, supportFilesZipped)): _*)

object TracksInput {
  def empty = TracksInput(None, None, None, None, None, None)

  def apply(
    files: Seq[File],
    parameters: TracksParameters,
    genomicMap: File,
    outname: String,
    covariateFile: Option[File],
    jar: File
  )(implicit components: TaskSystemComponents): TracksInput = {

    TracksInput(
      inputlist = Some(DosageToolInputFiles(files.map(s => SharedFile(s)))),
      outname = Some(outname),
      parameters = Some(parameters),
      genomicMap = Some(BimFile(SharedFile(genomicMap))),
      covariateFile = covariateFile.map(x => PlinkCovariateFile(SharedFile(x))),
      jarFile = Some(JarFile(SharedFile(jar)))
    )
  }

  def fromSharedFiles(
    files: Seq[SharedFile],
    parameters: TracksParameters,
    genomicMap: File,
    outname: String,
    covariateFile: Option[File],
    jar: File
  )(implicit components: TaskSystemComponents): TracksInput = {

    TracksInput(
      inputlist = Some(DosageToolInputFiles(files)),
      outname = Some(outname),
      parameters = Some(parameters),
      genomicMap = Some(BimFile(SharedFile(genomicMap))),
      covariateFile = covariateFile.map(x => PlinkCovariateFile(SharedFile(x))),
      jarFile = Some(JarFile(SharedFile(jar)))
    )
  }

  def apply(
    list: File,
    parameters: TracksParameters,
    genomicMap: File,
    outname: String,
    covariateFile: Option[File],
    jar: File
  )(implicit components: TaskSystemComponents): TracksInput = openSource(list.getCanonicalPath) { source =>
    apply(source.getLines.map(x => new File(x)).toList, parameters, genomicMap, outname, covariateFile, jar)
  }

  def updatePlinkCovar: UpdatePrerequisitive[TracksInput] = {
    case (self, plinkc: PlinkCovariateFile) => self.copy(covariateFile = Some(plinkc))
  }

}

object trackstask {
  def apply(
    in: TracksInput,
    memory: Int,
    update: UpdatePrerequisitive[TracksInput] = TracksInput.updatePlinkCovar orElse identity[TracksInput]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 8, memory = memory)
    ) {

      case (
        TracksInput(
          Some(outname),
          Some(parameters),
          Some(inputfiles),
          Some(genomicMap),
          Some(covariateFile),
          Some(jarFile)), ce) =>
        import ce._

        val tracksconfig = ConfigFactory.parseString(parameters.config)

        val phenotypenames = tracksconfig.getStringList("tracks.phenotype").grouped(2).map(_.head)

        val tests = scala.util.Try(tracksconfig.getStringList("tracks.tests").toList).toOption.getOrElse(List("SKAT"))

        {

          val workdir = {
            val folder = new File(TempFile.createTempFile("tracks").getParent)
            val x = new File(folder, outname + "workdir")
            x.mkdir
            x
          }

          val output = new File(workdir, outname)

          val inputlist = {
            val f = TempFile.createTempFile("filelist")
            writeToFile(f.getCanonicalPath, inputfiles.elems.map(_.localFile.getCanonicalPath).mkString("\n"))
            f.getCanonicalPath
          }

          val numcpu = resourceAllocated.cpu

          val fileServiceExtendedFolders = (new File(mybiotools.config.Config.configInstance.getString("tasks.fileServiceBaseFolder")).getAbsolutePath :: mybiotools.config.Config.configInstance.getStringList("tasks.fileServiceExtendedFolders").toList.map(f => new File(f).getAbsolutePath)).mkString(",")

          val mergedConfig = {

            val environmentDependentConfig = ConfigFactory.parseString(s"""
        |tracks {
        |  
        | testGenotypeBedTrunks = "${inputlist}"
        |
        |  allmap = "${genomicMap.file.file}"
        |
        |  covariateFile = "${covariateFile.file.file}"   
        |
        |  workerThreads = $numcpu
        |
        |  out = "${output.getCanonicalPath}"
        |
        |}
        |tasks.fileServiceBaseFolder = "${workdir.getCanonicalPath}"
        |tasks.fileServiceExtendedFolders = [$fileServiceExtendedFolders]
        |tasks.cacheEnabled = false
        |hosts.RAM=${resourceAllocated.memory}
        |hosts.hostname="${actorsystem.settings.config.getString("akka.remote.netty.tcp.hostname")}"
        |hosts.remoteCacheAddress=none
        |hosts.numCPU = $numcpu
        """.stripMargin)
            val configFromParameter = ConfigFactory.parseString(parameters.config)
            val merged = environmentDependentConfig.withFallback(configFromParameter)
            val covarnames = (merged.getStringList("tracks.covarnames").toList ++ parameters.obligatoryCovariateNames).distinct
            merged.withValue(
              "tracks.covarnames",
              ConfigFactory.parseString(s"""tracks.covarnames = [${covarnames.mkString(",")}]""").getValue("tracks.covarnames")
            )
          }

          val configFile = output.getCanonicalPath + ".config"

          writeToFile(configFile, mergedConfig.root.render)

          val (stdout, stderr, succ) = execGetStreamsAndCodeWithLog(s"java -Djava.io.tmpdir=${System.getProperty("java.io.tmpdir")} -Dconfig.file=$configFile -Xmx${(ce.resourceAllocated.memory / 1.2).toInt}m -cp ${jarFile.file.file} tracks.apps.MultiVariateTestApp", unsuccessfulOnErrorStream = false)

          if (!succ) throw new RuntimeException("error in tracks pipeline\n stdout:" + stdout.mkString("\n stderr:\n ") + stderr.mkString("\n end stderr \n"))

          val qqplots = phenotypenames.flatMap { ph =>
            tests.map { test =>
              val f = new File(output.getCanonicalPath + s".$ph.$test.qq.png")
              SharedFile(f, name = outname + s".$ph.$test.qq.png")
            }
          }.toList

          val testsFile = SharedFile(new File(output.getCanonicalPath + ".tests"), name = outname + ".tests")

          val supportFile = {
            val files = new File(output.getParent).listFiles.filter(_.getName.startsWith(outname)).filterNot(_.getName.endsWith("qq.png"))
            val tmp = TempFile.createTempFile("tar")
            val cmd = s"tar czf ${tmp.getCanonicalPath} ${files.map(_.getCanonicalPath).mkString(" ")} "
            execGetStreamsAndCode(cmd)
            SharedFile(tmp, name = outname + ".supports.tar.gz")
          }

          TracksOutput(testsFile, qqplots, supportFile)
        }
    }

}

