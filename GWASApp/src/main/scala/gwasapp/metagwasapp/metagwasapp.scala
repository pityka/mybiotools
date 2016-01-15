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

package gwasapp.metagwasapp

import mybiotools._
import mybiotools.gwascommons._
import gwascommons.genotypedata._
import gwascommons.gwas._
import gwascommons.gwas.GWAS._
import mybiotools.config.Config.configInstance
import mybiotools.stat.LinearRegression.readPlinkCovarFile
import collection.JavaConversions._
import java.io.File
import _root_.ch.systemsx.cisd.hdf5._
import hdfdosage.HDFDosageIterator
import mybiotools.stringstore._
import mybiotools.tasks._
import scala.concurrent.Future
import mybiotools.saddlehelpers._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util._
import com.typesafe.config._
import org.saddle._
import java.io.File

import hdfdosage.FileSets
import hdfdosage.FileSets._
import mybiotools.workflows._
import mybiotools.stat._
import mybiotools.gwascommons.associationresults._
import mybiotools.eq._
import gwasapp._
import akka.actor._
import mybiotools.sharedfiletypes._

case class MetaGWASAppConfig(subconfigs: Map[String, GWASAppConfig])
  extends ResultWithSharedFiles(subconfigs.map(_._2).flatMap(_.files).toSeq: _*)
  with Serializable

object MetaGWASAppConfig {
  def apply(config: Config)(implicit fs: TaskSystemComponents): MetaGWASAppConfig =
    MetaGWASAppConfig(
      subconfigs = config.getObject("gwases").map {
      case (name, configvalue) =>
        configvalue match {
          case c: ConfigObject => Some(name -> GWASAppConfig(c.toConfig))
          case _ => None
        }
    }.filter(_.isDefined).map(_.get).toMap
    )

}

case class MetaEnvironmentConfig(
  metaAnalyiseMemory: Int,
  gwasMemory: Int,
  metaGroupCount: Int,
  gwasenv: GWASEnvironmentConfig
)

object MetaEnvironmentConfig {

  def apply(config: Config): MetaEnvironmentConfig =
    MetaEnvironmentConfig(
      metaGroupCount = config.getInt("chunks"),
      metaAnalyiseMemory = config.getInt("metaRAM"),
      gwasMemory = config.getInt("gwasRAM"),
      gwasenv = GWASEnvironmentConfig(config.getConfig("gwasenv"))
    )
}

object MetaGWASApp extends App {

  val taskSystem = mybiotools.tasks.defaultTaskSystem

  if (taskSystem.hostConfig.myRole == mybiotools.tasks.MASTER) {
    val config = MetaGWASAppConfig(configInstance.getConfig("metagwas"))(taskSystem.components)
    val log = taskSystem.getLogger(this)
    val env = MetaEnvironmentConfig(configInstance.getConfig("metagwas"))

    val runner = new MetaGWASRunner(config, env, taskSystem.components)
    try {
      runner.run
      log.info("Finished.")
    } finally {
      taskSystem.shutdown
      Thread.sleep(5000)
    }
  }
}

class MetaGWASRunner(config: MetaGWASAppConfig, env: MetaEnvironmentConfig, taskSystem: mybiotools.tasks.TaskSystemComponents) {
  import env._

  def run: CatFilesOutput = {

    import config._
    implicit val components = taskSystem
    implicit val ec = mybiotools.concurrent.newExecutionContext("MetaGWAS", 20)
    val log = createLogger(this)

    val concatenateTask = {
      val input = CatFilesInput(metaGroupCount, s"combinedP.gwas.concatenated")
      catFiles(input)
    }

    val gwasMetaAnalizers =
      Groups.makeN(metaGroupCount).map { group =>
        val input = MetaAnalyseInput(
          None,
          Some(subconfigs.size),
          Some(s"combinedP.gwas.${group.remainder}"),
          Some(group)
        )
        val t = metaAnalyseGWAS(input, memory = metaAnalyiseMemory)
        t ~> concatenateTask
        t
      }

    subconfigs.foreach {
      case (name, conf) =>
        val task = gwastask(GWASAppInput(Some(name), Some(conf), Some(gwasenv)), gwasMemory)
        gwasMetaAnalizers.foreach { meta =>
          task ~> meta
        }
    }

    Await.result(concatenateTask.?[CatFilesOutput], 168 hours)
  }

}