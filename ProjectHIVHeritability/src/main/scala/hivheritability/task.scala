package hivheritability

import mybiotools.workflows

import mybiotools._
import mybiotools.tasks._
import java.io.File
import akka.actor.{ ActorRefFactory }
import scala.util.Try
import mybiotools.stat.ZMetaCombination._
import hivheritability._
import hdfdosage.FileSets._
import com.typesafe.config.ConfigFactory
import mybiotools.gwascommons._
import scala.collection.JavaConversions._
import org.saddle._
import org.saddle.scalar._
import mybiotools.workflows.{ MakeBedOut, BimFile, BedFile, FamFile }
import mybiotools.gwascommons.genotypedata._

case class HeritabilityOutput(file: SharedFile) extends ResultWithSharedFiles(file)

case class HeritabilityInput(
  covariateFile: Option[SharedFile],
  bootstrapCount: Option[Int],
  randomizationCount: Option[Int],
  downsamples: Option[Seq[Int]],
  bed: Option[SharedFile],
  bim: Option[SharedFile],
  fam: Option[SharedFile],
  ldthresholds: Option[List[Double]],
  ldPruneKeepSnps: Option[Set[String]],
  conditionOnSnps: Option[Set[String]],
  genomicMap: Option[SharedFile],
  outname: Option[String],
  covariateNames: Option[Seq[String]],
  phenotypeName: Option[String],
  minimumMAF: Option[Double]
) extends SimplePrerequisitive[HeritabilityInput]

object HeritabilityInput {

  def updateHeritabilityInput: UpdatePrerequisitive[HeritabilityInput] = {
    case (self, i: MakeBedOut) => self.copy(bed = Some(i.bed.file), bim = Some(i.bim.file), fam = Some(i.fam.file))
  }
}

object heritabilityTask {

  def apply(
    in: HeritabilityInput,
    cpu: Int,
    memory: Int,
    update: UpdatePrerequisitive[HeritabilityInput] = identity[HeritabilityInput]
  )(implicit components: TaskSystemComponents) =
    newTask(
      in, update, CPUMemoryRequest(cpu = cpu, memory = memory)
    ) {

      case (HeritabilityInput(
        Some(covariateFile),
        Some(bootstrapCountp),
        Some(randomizationCountp),
        Some(downsamplesp),
        Some(bed),
        Some(bim),
        Some(fam),
        Some(ldthresholdsp),
        Some(prunekeepsnps),
        Some(conditionOnSnps),
        Some(gmap),
        Some(outname),
        Some(covariateNames),
        Some(phenotypeName),
        Some(minimumMAF)
        ),
        ce) =>
        import ce._

        val workdir = {
          val folder = new File(TempFile.createTempFile("heritability").getParent)
          val x = new File(folder, outname + "workdir")
          x.mkdir
          x
        }

        val fullCovariateFile: Frame[Individual, String, Double] = openSource((covariateFile.file.getAbsolutePath))(source => mybiotools.stat.LinearRegression.readPlinkCovarFile(source, "-9"))
        val indorder: Seq[Individual] = fullCovariateFile.rowIx.toSeq

        val phenoFile = {
          val pheno: Series[Individual, Double] = fullCovariateFile.firstCol(phenotypeName)

          val phFile = TempFile.createTempFile(".ph")
          openFileWriter(phFile) { writer =>
            indorder.foreach { ind =>
              val value = pheno.first(ind) match {
                case NA => "NA"
                case Value(x) if x.isNaN => "NA"
                case Value(x) => x.toString
              }
              writer.write(ind.FID + " " + ind.IID + " " + value + "\n")
            }
          }
          phFile

        }

        val snpsInBedToKeep: Set[String] = openSource(bim.file)(s => getBimEntriesWithAllelesWithoutLocation(s).filter(x => prunekeepsnps.contains(x._1.value) || conditionOnSnps.contains(x._1.value)).map(_._1.value).toSet)

        val plinkCommand = s"plink --bed ${bed.file.getAbsolutePath} --bim ${bim.file.getAbsolutePath} --fam ${fam.file.getAbsolutePath} --prune  --geno 0.01 --maf ${minimumMAF} --make-bed --out ${workdir.getAbsolutePath}/common.nonmiss --noweb --pheno ${phenoFile.getAbsolutePath} "

        val (stdout, stderr, success) = execGetStreamsAndCodeWithLog(plinkCommand)

        if (!success) throw new RuntimeException(
          stdout.mkString("\n") +
            stderr.mkString("\n")
        )

        val covFile = {

          val covs = if (!conditionOnSnps.isEmpty) {
            val conditionSnpsFrame: Frame[Individual, String, Double] = {
              val bed = new File(workdir.getAbsolutePath + "/common.nonmiss.bed")
              val bim = new File(workdir.getAbsolutePath + "/common.nonmiss.bim")
              val fam = new File(workdir.getAbsolutePath + "/common.nonmiss.fam")
              openFileSet(hdfdosage.FileSets.BedFile(bed, bim, fam), 0.0, Full, conditionOnSnps)(_.toLocusIteratorWithGenomicMap(Map()).toFrame.mapColIndex(_.value))
            }
            fullCovariateFile.col(covariateNames: _*).rconcat(conditionSnpsFrame).rdropNA
          } else fullCovariateFile.col(covariateNames: _*)

          val covFile = TempFile.createTempFile(".cov")
          openFileWriter(covFile) { writer =>
            indorder.foreach { ind =>
              val cov = covs.first(ind).toVec.toSeq
              if (!cov.isEmpty) {
                writer.write(ind.FID + " " + ind.IID + " " + cov.mkString(" ") + "\n")
              }

            }
          }
          covFile
        }

        val trunkToHeritability: String = if (snpsInBedToKeep.size > 0) {

          val plinkCommandExtractKeep = s"plink --bed ${bed.file.getAbsolutePath} --bim ${bim.file.getAbsolutePath} --fam ${fam.file.getAbsolutePath} --snps ${snpsInBedToKeep.mkString(",")}  --make-bed --out ${workdir.getAbsolutePath}/keepLDPruneExtracted --noweb --pheno ${phenoFile.getAbsolutePath} --prune "

          val plinkCommandMerge = s"plink --bfile ${workdir.getAbsolutePath}/common.nonmiss --bmerge ${workdir.getAbsolutePath}/keepLDPruneExtracted.bed ${workdir.getAbsolutePath}/keepLDPruneExtracted.bim ${workdir.getAbsolutePath}/keepLDPruneExtracted.fam   --make-bed --out ${workdir.getAbsolutePath}/common.nonmiss.withKeepLDPrune --noweb  "

          val plinkCommandSort = s"plink --bfile ${workdir.getAbsolutePath}/common.nonmiss.withKeepLDPrune --make-bed --noweb --out ${workdir.getAbsolutePath}/common.nonmiss.withKeepLDPrune.sorted"

          val (stdout2, stderr2, success2) = execGetStreamsAndCodeWithLog(plinkCommandExtractKeep)

          val (stdout3, stderr3, success3) = execGetStreamsAndCodeWithLog(plinkCommandMerge)

          val (stdout4, stderr4, success4) = execGetStreamsAndCodeWithLog(plinkCommandSort)

          if (!success4) throw new RuntimeException(
            stdout.mkString("\n") +
              stderr.mkString("\n") +
              stdout2.mkString("\n") +
              stderr2.mkString("\n") +
              stdout3.mkString("\n") +
              stderr3.mkString("\n") +
              stdout4.mkString("\n") +
              stderr4.mkString("\n")
          )

          s"${workdir.getAbsolutePath}/common.nonmiss.withKeepLDPrune.sorted"
        } else s"${workdir.getAbsolutePath}/common.nonmiss"

        val heritabilityconfig = new HIVHeritabilityConfig {
          def phenotypeFile = phenoFile
          def covariateFile = None
          def qCovariateFile = Some(covFile)
          def grmgztrunks: Map[String, File] = Map()
          def treeFiles: Map[String, File] = Map()

          def bootstrapCount = bootstrapCountp
          def randomizationCount = randomizationCountp
          def downsamples = downsamplesp
          def outTrunk = workdir.getAbsolutePath + "/heribility."
          def outGroupNames = Set()
          def outgroupaminoacidFiles: Map[String, java.io.File] = Map()
          def outgroupnucleotideFiles: Map[String, java.io.File] = Map()
          def runraxml: Boolean = false
          def scaleTree = false
          def useCorrelationMatrix = false
          def checkPositiveDefinite = false
          def listKernels = false
          def subcohortFiles: Map[String, File] = Map()
          def genotypeBedBimFamFiles: Map[String, (File, Option[File])] = Map()
          def genotypeFilesForLDPruning: Map[String, GenotypeFileSet] = Map(outname -> hdfdosage.FileSets.BedFile(new File(trunkToHeritability + ".bed"), new File(trunkToHeritability + ".bim"), new File(trunkToHeritability + ".fam")))
          def genotypeFilesForLASSO = None
          def gctaGRMMemory = resourceAllocated.memory
          def javaGRMMemory = resourceAllocated.memory
          def subcohortMixAndDownSample = None
          def nosubcohortruns = false
          def gctaMaxIterations = 3000
          def ldthresholds = ldthresholdsp
          def ldPruneKeepSnps = prunekeepsnps
          def ldPruneAwaySnps = conditionOnSnps
          def genomicMap = Some(gmap.file)
          def grmThreads = resourceAllocated.cpu
          def grmBatchSize = 1000
          def sequenceAlignments = None
          def sequenceLDPruneThreshold = 1.0
          def lanlepitopefile = None
          def g2gbestperaafile = None
          def h2withlasso = false
          def g2gpthreshold = 1.0
          def onlyCurrentG2GPositions = false

          def grmgroups: Map[String, Set[String]] = ldthresholdsp.map { l => "model1" -> Set(outname + ".ld" + l) }.toMap
        }

        val taskSystem = {
          val lconf = new mybiotools.tasks.LocalConfiguration(resourceAllocated.cpu, resourceAllocated.memory)

          val fileServiceExtendedFolders = (new File(mybiotools.config.Config.configInstance.getString("tasks.fileServiceBaseFolder")).getAbsolutePath :: mybiotools.config.Config.configInstance.getStringList("tasks.fileServiceExtendedFolders").toList.map(f => new File(f).getAbsolutePath)).mkString(",")

          val configstring = s"""
          |tasks.fileServiceBaseFolder = "${workdir.getCanonicalPath}/"
          |tasks.fileServiceExtendedFolders = [$fileServiceExtendedFolders]
          |tasks.cacheEnabled = false
          """.stripMargin
          mybiotools.tasks.customTaskSystem(lconf, ConfigFactory.parseString(configstring))
        }
        val hivher = new HIVHeritabilityRun(heritabilityconfig, taskSystem)

        hivher.run

        taskSystem.shutdown

        val tar = {
          val files = workdir.listFiles
          val tmp = TempFile.createTempFile("tar")
          val cmd = s"tar czf ${tmp.getCanonicalPath} ${files.map(_.getCanonicalPath).mkString(" ")} "
          execGetStreamsAndCode(cmd)
          SharedFile(tmp, name = outname + ".tar.gz", canMoveAway = true)
        }
        HeritabilityOutput(tar)

    }
}
