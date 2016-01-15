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

case class MetaSetTestInput(
    outname: Option[String],
    config: Option[MetaSetTestConfig],
    env: Option[MetaSetEnv]
) extends SimplePrerequisitive[MetaSetTestInput] {
  override def persistent = this.copy(env = None)
}

object metasettesttask {
  def apply(
    in: MetaSetTestInput,
    memory: Int,
    update: UpdatePrerequisitive[MetaSetTestInput] = identity[MetaSetTestInput]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 0, memory = memory)
    ) {

      case (
        MetaSetTestInput(
          Some(outname),
          Some(config),
          Some(env)), ce) =>

        val tscopy = ce.components.childPrefix("metasettest-" + outname)
        (new MetaSetTestRunner(config, env, tscopy).run)

    }

}

