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

package genotyper
import mybiotools.config.Config.configInstance
import mybiotools.workflows._
import java.io.File
import scala.collection.JavaConversions._

import mybiotools.tasks._
import java.io.File
import mybiotools._

import scala.collection.JavaConversions._
import htsjdk.samtools.util.Iso8601Date
import htsjdk.samtools._
import scala.sys.process._
import scala.concurrent._
import scala.concurrent.duration._
import java.util.concurrent.Executors
import htsjdk.samtools.cram.ref.ReferenceSource
import genotyper.tasks._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Crams2BamsApp extends App {
  htsjdk.samtools.util.Log.setGlobalLogLevel(htsjdk.samtools.util.Log.LogLevel.WARNING)

  val ts = defaultTaskSystem
  if (ts.hostConfig.myRole == MASTER) {
    val log = ts.getLogger(this)
    implicit val components = ts.components

    val ref = new File(configInstance.getString("cram2bam.ref"))

    val crams = mybiotools.openSource(configInstance.getString("cram2bam.crams"))(_.getLines.toList).map(x => new File(x))

    val samplenames: List[(String, Seq[File])] = crams.groupBy(x => AddReadGroup.getReadGroupCRAM(x, ref).head.sm).toList

    val future: Future[Seq[BamFile]] = Future.sequence(
      samplenames.map {
        case (sm, list) =>
          log.info(s"""Merging $list into ${sm + ".bam"} """)
          mergecram(MergeCramInput(list.toList, ref, sm + ".bam")).?[BamFile]
      }
    )

    Await.ready(future, 168 hours)

    ts.shutdown

  }

}