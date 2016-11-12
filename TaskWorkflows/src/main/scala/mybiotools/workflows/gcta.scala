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
import com.typesafe.config.{ Config, ConfigFactory }
import collection.JavaConversions._
import mybiotools.gwascommons.gcta._

// import hdfdosage.{ DosageFileFormat, HDFDosage, PDose, PGenotypeProbabilities, BIGSNPMajor, TPed }

case class GRMGZPair(grmgz: SharedFile, grmid: SharedFile, name: String) extends ResultWithSharedFiles(grmgz, grmid)

case class GCTAInput(
    outname: Option[String],
    extraArgs: Option[String],
    grms: Option[Set[GRMGZPair]],
    covarFile: Option[Option[SharedFile]],
    qCovarFile: Option[Option[SharedFile]],
    phenoFile: Option[SharedFile],
    expectedGRMs: Option[Int]
) extends Prerequisitive[GCTAInput] {
  def ready = outname.isDefined &&
    extraArgs.isDefined &&
    grms.isDefined &&
    covarFile.isDefined &&
    qCovarFile.isDefined &&
    phenoFile.isDefined &&
    expectedGRMs.isDefined &&
    expectedGRMs.get == grms.get.size
}

case class GCTAOutput(
  hsq: Option[SharedFile], result: Option[GCTAResult], log: SharedFile
) extends ResultWithSharedFiles(hsq.toList :+ log: _*)

object GCTAInput {

  def apply(
    grms: Set[(File, File, String)],
    extraArgs: String,
    outname: String,
    covarFile: Option[File],
    qCovarFile: Option[File],
    phenoFile: File
  )(implicit components: TaskSystemComponents): GCTAInput = {

    GCTAInput(
      outname = Some(outname),
      extraArgs = Some(extraArgs),
      grms = Some(grms.map(f => GRMGZPair(SharedFile(f._1), SharedFile(f._2), f._3))),
      covarFile = Some(covarFile.map(x => SharedFile(x))),
      qCovarFile = Some(qCovarFile.map(x => SharedFile(x))),
      phenoFile = Some(SharedFile(phenoFile)),
      expectedGRMs = Some(grms.size)
    )
  }

  def apply(
    extraArgs: String,
    outname: String,
    covarFile: Option[File],
    qCovarFile: Option[File],
    phenoFile: File,
    expectedGRMs: Int
  )(implicit components: TaskSystemComponents): GCTAInput = {

    GCTAInput(
      outname = Some(outname),
      extraArgs = Some(extraArgs),
      covarFile = Some(covarFile.map(x => SharedFile(x))),
      qCovarFile = Some(qCovarFile.map(x => SharedFile(x))),
      phenoFile = Some(SharedFile(phenoFile)),
      expectedGRMs = Some(expectedGRMs),
      grms = None
    )
  }

  private def updateGRMs(s: GCTAInput, x: GRMFile) = s.grms match {
    case None => s.copy(grms = Some(Set(GRMGZPair(x.gctagz, x.gctaid, x.gctagz.name))))
    case Some(set) => s.copy(grms = Some(set + GRMGZPair(x.gctagz, x.gctaid, x.gctagz.name)))
  }

  def update: UpdatePrerequisitive[GCTAInput] = {
    case (self, grm: GRMFile) => updateGRMs(self, grm)
  }

}

object gctatask {
  def apply(
    in: GCTAInput,
    cpu: Int = 1,
    memory: Int = 2000,
    update: UpdatePrerequisitive[GCTAInput] = GCTAInput.update orElse identity[GCTAInput]
  )(implicit ce: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = cpu, memory = memory)
    ) {

      case (
        GCTAInput(
          Some(outname),
          Some(args),
          Some(grms),
          Some(covarFile),
          Some(qCovarFile),
          Some(phenoFile),
          _
          ), ce) =>
        import ce._

        {

          if (grms.size == 0) {
            val l = TempFile.createTempFile(".log")
            writeToFile(l.getAbsolutePath, "No GRM provided.")
            GCTAOutput(None, None, log = SharedFile(l, canMoveAway = true, name = outname + ".log"))
          } else {

            val (mgrmlist, grmnames) = {
              val trunks: Seq[(String, File)] = grms.toSeq.map {
                case GRMGZPair(grmsh, idsh, name) =>
                  name -> putBesides(List(grmsh.file -> ".grm.gz", idsh.file -> ".grm.id"): _*)
              }
              val tmp = mybiotools.TempFile.createTempFile("mgrm.list")
              mybiotools.writeToFile(tmp.getAbsolutePath, trunks.map(_._2.getAbsolutePath).mkString("\n"))
              tmp -> trunks.map(_._1)
            }

            def replaceName(f: File): File = {
              val tmp = TempFile.createTempFile(".hsq")
              writeToFile(
                tmp.getAbsolutePath,
                openSource(f.getAbsolutePath) { s =>
                  val string = s.mkString
                  grmnames.zipWithIndex.foldLeft(string) { case (s, (name, index)) => s.replaceAllLiterally(s"V(G${index + 1})", "V(" + name + ")") }
                }
              )
              tmp
            }

            val covarstring = covarFile.map(c => s" --covar ${c.file.getAbsolutePath} ").getOrElse("")
            val qcovarstring = qCovarFile.map(c => s" --qcovar ${c.file.getAbsolutePath} ").getOrElse("")

            val out = TempFile.createTempFile("").getAbsolutePath
            val logfile = out + ".log"

            val threads = resourceAllocated.cpu

            val cmd = s"gcta64 --reml --mgrm-gz ${mgrmlist.getAbsolutePath} --pheno ${phenoFile.file.getAbsolutePath} $covarstring $qcovarstring --out $out --thread-num $threads  $args "

            val (stdout, stderr, success) = mybiotools.execGetStreamsAndCode(cmd, unsuccessfulOnErrorStream = true)

            mybiotools.writeToFile(logfile, (stdout ++ stderr).mkString("\n"))

            if (success) {
              val hsq = replaceName(new File(out + ".hsq"))
              val lines = scala.io.Source.fromFile(hsq).getLines.toIndexedSeq.drop(1)
              mgrmlist.delete

              GCTAOutput(
                hsq = Some(SharedFile(hsq, canMoveAway = true, name = outname + ".hsq")),
                result = Some(GCTAResult.fromHSQLinesWithoutHeader(lines)),
                log = SharedFile(new File(logfile), canMoveAway = true, name = outname + ".log")
              )

            } else {
              val (stdout, stderr, success) = mybiotools.execGetStreamsAndCode(cmd + " --reml-alg 2  ", unsuccessfulOnErrorStream = true)

              mybiotools.appendToFile(logfile, (stdout ++ stderr).mkString("\n"))
              if (success) {
                val hsq = replaceName(new File(out + ".hsq"))
                val lines = scala.io.Source.fromFile(hsq).getLines.toIndexedSeq.drop(1)
                mgrmlist.delete

                GCTAOutput(
                  hsq = Some(SharedFile(hsq, canMoveAway = true, name = outname + ".hsq")),
                  result = Some(GCTAResult.fromHSQLinesWithoutHeader(lines)),
                  log = SharedFile(new File(logfile), canMoveAway = true, name = outname + ".log")
                )
              } else {
                mgrmlist.delete

                log.warning("gcta error\n" + stdout.mkString("\n") + stderr.mkString("\n"))
                GCTAOutput(None, None, log = SharedFile(new File(logfile), canMoveAway = true, name = outname + ".log"))
              }
            }
          }
        }

    }

}
