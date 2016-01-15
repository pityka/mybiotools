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

package settest.metasettest

import settest._
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
import akka.actor._
import mybiotools.sharedfiletypes._
import mybiotools.sharedfiletypes.SharedFileSets._

case class MetaSetTestConfig(subconfigs: Map[String, SetTestConfig]) extends ResultWithSharedFiles(subconfigs.flatMap(_._2.files).toSeq: _*) with Serializable

object MetaSetTestConfig {

  def apply(config: Config)(implicit fs: TaskSystemComponents): MetaSetTestConfig = MetaSetTestConfig(config.getObject("sub").map {
    case (name, configvalue) =>
      configvalue match {
        case c: ConfigObject => Some(name -> SetTestConfig(c.toConfig))
        case _ => None
      }
  }.filter(_.isDefined).map(_.get).toMap)

}

case class MetaSetEnv(
  settestRAM: Int,
  metaAnalyiseMemory: Int,
  metaGroupCount: Int
) extends Serializable

object MetaSetTestEnv {

  def apply(config: Config): MetaSetEnv = MetaSetEnv(
    settestRAM = config.getInt("settestRAM"),
    metaGroupCount = config.getInt("chunks"),
    metaAnalyiseMemory = config.getInt("metaRAM")
  )
}

object MetaSetTestApp extends App {

  val taskSystem = mybiotools.tasks.defaultTaskSystem

  if (taskSystem.hostConfig.myRole == mybiotools.tasks.MASTER) {
    val config = MetaSetTestConfig(configInstance.getConfig("metasettest"))(taskSystem.components)
    val env = MetaSetTestEnv(configInstance.getConfig("metasettest"))
    val log = taskSystem.getLogger(this)

    val runner = new MetaSetTestRunner(config, env, taskSystem.components)
    try {
      runner.run
      log.info("Finished.")
    } finally {
      taskSystem.shutdown
      Thread.sleep(5000)
    }
  }
}

class MetaSetTestRunner(config: MetaSetTestConfig, env: MetaSetEnv, taskSystem: mybiotools.tasks.TaskSystemComponents) {
  import env._

  val updateMetaAnalyseInputWithSetTest: UpdatePrerequisitive[MetaAnalyseInput] = {
    case (self, i: SetTestResultFiles) if i.tests.isDefined => self.copy(files = if (self.files.isDefined) Some((self.files.get + i.tests.get).toSet) else Some(Set(i.tests.get)))
  }

  def run: CatFilesOutput = {

    import config._
    implicit val components = taskSystem
    implicit val ec = mybiotools.concurrent.newExecutionContext("MetaSetTets", 20)
    val log = createLogger(this)

    val concatenateTask = {
      val input = CatFilesInput(metaGroupCount, s"combinedP.settest.concatenated")
      catFiles(input)
    }

    val metaAnalizers =
      Groups.makeN(metaGroupCount).map { group =>
        val input = MetaAnalyseInput(
          None,
          Some(subconfigs.size),
          Some(s"combinedP.settest.${group.remainder}"),
          Some(group)
        )
        val t = metaAnalyiseTracks(input, memory = metaAnalyiseMemory, update = updateMetaAnalyseInputWithSetTest)
        t ~> concatenateTask
        t
      }

    subconfigs.foreach {
      case (name, conf) =>
        val task = settesttask(SetTestInput(Some(name), Some(conf)), settestRAM)
        metaAnalizers.foreach { meta =>
          task ~> meta
        }
    }

    Await.result(concatenateTask.?[CatFilesOutput], 168 hours)
  }

}