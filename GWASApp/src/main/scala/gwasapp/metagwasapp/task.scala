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
import mybiotools.tasks._
import java.io.File
import akka.actor._
import gwasapp._
import com.typesafe.config._
import collection.JavaConversions._
import mybiotools.sharedfiletypes._
import mybiotools.sharedfiletypes.SharedFileSets._

import hdfdosage.{ DosageFileFormat, HDFDosage, PDose, PGenotypeProbabilities, BIGSNPMajor, TPed }

case class MetaGWASAppInput(
    outname: Option[String],
    config: Option[MetaGWASAppConfig],
    env: Option[MetaEnvironmentConfig]
) extends SimplePrerequisitive[MetaGWASAppInput] {
  override def persistent = this.copy(env = None)
}

object metagwastask {
  def apply(
    in: MetaGWASAppInput,
    memory: Int,
    update: UpdatePrerequisitive[MetaGWASAppInput] = identity[MetaGWASAppInput]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = 0, memory = memory)
    ) {

      case (
        MetaGWASAppInput(
          Some(outname),
          Some(config),
          Some(env)), ce) =>

        val tscopy = ce.components.childPrefix("metagwas-" + outname)
        (new MetaGWASRunner(config, env, tscopy).run)

    }

}

