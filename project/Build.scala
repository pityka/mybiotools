import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._
import ReflectPlugin._
import com.lihaoyi.workbench.Plugin._
import spray.revolver.AppProcess
import spray.revolver.RevolverPlugin.Revolver
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin



object BuildSettings {
  val buildScalaVersion = "2.11.7"

  val scalaVersionRegex = "(\\d+)\\.(\\d+).*".r

  val gitHeadCommitSha = settingKey[String]("current git commit SHA")

  val gitHeadCommitShaTask = TaskKey[Unit]("githeadcommit", "current git commit SHA")


  



val extraSettings = 
  sbtassembly.Plugin.assemblySettings  ++ 
    com.typesafe.sbt.SbtScalariform.scalariformSettings ++
    ReflectPlugin.allSettings ++
    Seq (    
    updateOptions := updateOptions.value.withConsolidatedResolution(true),    
    scalaVersion := buildScalaVersion,
    gitHeadCommitShaTask in ThisBuild <<= gitHeadCommitSha in ThisBuild map println,
    gitHeadCommitSha in ThisBuild := scala.util.Try(
      Process("git rev-parse --short HEAD").lines.head
      ).toOption.map(x => x).getOrElse(
      sbt.IO.read(file("GITCOMMITHASH")).trim
      ))

  val buildSettings = Defaults.defaultSettings ++ 
    extraSettings ++ Seq(
          scalacOptions in Compile <++= scalaVersion map { sv =>
      val l : Seq[String] = sv match {
        case scalaVersionRegex(major, minor) if (major == "2" && minor.toInt >= 10)   => Seq("-deprecation","-unchecked","-feature","-language:implicitConversions","-language:postfixOps","-language:reflectiveCalls","-language:existentials","-Xmax-classfile-name","254")
        case _ => Seq( "-deprecation", "-unchecked" )
      }
      l
    },
    scalacOptions in Test := Seq(),
    scalacOptions in Test <++= scalaVersion map { sv =>
      sv match {
        case scalaVersionRegex(major, minor) if (major == "2" && minor.toInt >= 10)   => Seq("-unchecked","-feature","-language:implicitConversions","-language:postfixOps","-language:reflectiveCalls","-language:existentials","-Xmax-classfile-name","254")
        case _ => Seq(  "-unchecked" )
      }
      
    },
    mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
      {
        case x if x.matches("META-INF/*") => MergeStrategy.first
        case x if x.matches("README") => MergeStrategy.concat
        case x => old(x)
      }
    },
    compile <<= (compile in Compile) dependsOn (compile in Test),
    resolvers ++= Resolvers.allres
  ) 
}

object Resolvers {

  // val myCloudBees = "Cloudbees Private" at "https://repository-pityka.forge.cloudbees.com/snapshot/"

  val b =  "GuiceyFruite" at "http://guiceyfruit.googlecode.com/svn/repo/releases/"

  val c =  "Twitter" at "http://maven.twttr.com/"
  
  val scalatools = "Scala Tools" at "http://scala-tools.org/repo-releases"
  
  val typesafe = "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

  val freehep = "Freehep" at "http://java.freehep.org/maven2"

  val apache_snapshots = "Apache Releases" at "https://repository.apache.org/content/repositories/releases/"

  val localmaven = "Local Maven" at Path.userHome.asFile.toURI.toURL + ".m2/repository"

  val sonatype = Seq(
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

  val allres = Seq ( b, c,typesafe,freehep,apache_snapshots,localmaven) ++ sonatype 
}

object Dependencies {
  val scalatest = "org.scalatest" %% "scalatest" % "2.1.5" % "test" 

  val typesafeconfig = "com.typesafe" % "config" % "1.2.0"

  val akka_actor = "com.typesafe.akka" %% "akka-actor" % "2.3.14"

  val akka_remote = "com.typesafe.akka" %% "akka-remote" % "2.3.14"

  val akka_agent = "com.typesafe.akka" %% "akka-agent" % "2.3.14"

  val akka_testkit = "com.typesafe.akka" %% "akka-testkit" % "2.3.14" % "test"

  val breeze =  Seq("org.scalanlp" %% "breeze" % "0.11.2")

  val akkadeps = Seq(akka_actor,akka_remote,akka_testkit,akka_agent)

  val htsjdk =  "com.github.samtools" % "htsjdk" % "1.140"


  val ganymed =  "ch.ethz.ganymed" % "ganymed-ssh2" % "261"
 
  val colt = "colt" % "colt" % "1.2.0"

  val saddle = "org.scala-saddle" %% "saddle-core" % "1.3.4"  exclude("com.googlecode.efficient-java-matrix-library", "ejml")

  val hierarchicalclustering = "com.apporiented" %% "hierarchical-clustering" % "1.0-pityu5"

  // saddle needs this
  val jodaconvert = "org.joda" % "joda-convert" % "1.2"

  val awssdk =  "com.amazonaws" % "aws-java-sdk" % "1.5.4"

  val scalacheck = "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"
  
  val guava = "com.google.guava" % "guava" % "18.0"

  val googlecollections = "com.google.collections" % "google-collections" % "1.0"
  
  val ejml = "org.ejml" % "all" % "0.28"

  val commonsMath3 = "org.apache.commons" % "commons-math3" % "3.4.1"

  val commonsLang = "commons-lang" % "commons-lang" % "2.6"

  val commonsIO = "commons-io" % "commons-io" % "2.3"

  val netlibjavacore = "com.github.fommil.netlib" % "core" % "1.1.2" 

  val netlibjavaosx = "com.github.fommil.netlib" % "netlib-native_system-osx-x86_64" % "1.1" classifier "natives"

  val netlibjavalinuxgcc = "com.github.fommil.netlib" % "netlib-native_system-linux-x86_64" % "1.1" classifier "natives"

  // val leveldb=  "org.iq80.leveldb" % "leveldb" % "0.7"
      
     val jline = "jline" % "jline" % "0.9.92"
     
     val junit = "junit" % "junit" % "4.8.1" % "test"
  

    val gral = "de.erichseifert.gral" %% "gral" % "0.9-SNAPSHOT-pityu6b9"

    val vectorgraphics2d = "de.erichseifert.gral" %% "vectorgraphics2d" % "0.9.1-pityu1b4"
  
  val commonDeps = Seq(htsjdk,guava,ejml,scalatest,scalacheck,typesafeconfig,commonsMath3,gral,vectorgraphics2d,colt,saddle,jodaconvert,
    "com.google.code.findbugs" % "jsr305" % "1.3.+",hierarchicalclustering,akka_actor,netlibjavacore,netlibjavalinuxgcc,netlibjavaosx)
 
  
  val alldeps = Seq(scalatest,guava,ejml,junit,typesafeconfig
    ) ++ commonDeps

  
    
}



object MyBuild extends Build
{
  import Resolvers._
  import Dependencies._
  import BuildSettings._

  lazy val Commons = Project(
    id = "commons",
    base = file("commons/"),
    settings = buildSettings ++ fmppTemplate ++ Seq(resolvers := allres, libraryDependencies ++= commonDeps,libraryDependencies := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      libraryDependencies.value :+ "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"
    case _ =>
      libraryDependencies.value
  }
} )
    )

  lazy val root = Project(
    "root",
    file("."),
    settings = buildSettings ++ Seq (resolvers := allres, libraryDependencies ++= alldeps) ) 
    .dependsOn(Commons,HDFApp) 

  lazy val aggregator = Project("aggregate",file("aggregate"),settings=buildSettings)
    .aggregate(Commons,HDFApp,Tasks,SetTestApp,GWASApp,PlotQQApp,PlotManhattanApp,FDRApp,TranAlignApp,TranslateFastaApp,ZmetaApp,MaskBedToBimApp,GrepFastaApp,DiscriminateToFilesApp,TaskWorkflows,TasksSharedFileTypes,MSAApp,ViralShortReadApp,GenotypePipeline,G2GSequence,Dummify,RNASeqAlignPipeline,Dispensability,MeasureBedApp)


  lazy val HDFApp = Project(
    "HDF5App",
    file("HDF5App"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= commonDeps ++ Seq(commonsIO,commonsLang,akka_actor,akka_remote)  ) 
  ) dependsOn (Commons,Tasks,vcfhelpers)  


  lazy val SetTestApp = Project(
    "SetTestApp",
    file("SetTestApp"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= (commonDeps) )
  ) dependsOn (Commons,Tasks,HDFApp,vcfhelpers,TaskWorkflows)

  
  lazy val Tasks = Project(
    "Tasks",
    file("Tasks"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= commonDeps ++ akkadeps ++ Seq(awssdk,ganymed) )
  ) dependsOn (Commons,TasksMonitorWebSharedJvm) 

  lazy val TasksMonitorWebClient = (project in file("TasksMonitorWebClient")).settings(extraSettings++workbenchSettings:_*).settings(
    name := "TasksMonitorWebClient",
      bootSnippet := "example.ScalaJSExample().main();",
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.8.0",   
    "com.lihaoyi" %%% "upickle" % "0.2.8",
    "com.lihaoyi" %%% "autowire" % "0.2.5",
    "com.lihaoyi" %%% "scalatags" % "0.5.2",
    "com.lihaoyi" %%% "scalarx" % "0.2.8"
  )
  ).enablePlugins(ScalaJSPlugin).
  dependsOn(TasksMonitorWebSharedJs)

  lazy val TasksMonitorWebShared = (crossProject.crossType(CrossType.Pure) in file("TasksMonitorWebShared")).
  settings(extraSettings:_*).settings(libraryDependencies ++= Seq(
    "com.lihaoyi" %%% "upickle" % "0.2.8",
    "com.lihaoyi" %%% "autowire" % "0.2.5",
    "com.lihaoyi" %%% "scalatags" % "0.5.2"
  ))

lazy val TasksMonitorWebSharedJvm = TasksMonitorWebShared.jvm
lazy val TasksMonitorWebSharedJs = TasksMonitorWebShared.js

lazy val TasksMonitorWebServer = (project in file("TasksMonitorWebServer")).settings(extraSettings++Revolver.settings:_*).settings(
  name := "TasksMonitorWebServer",
  libraryDependencies ++= Seq(
    "io.spray" %% "spray-can" % "1.3.3",
    "io.spray" %% "spray-routing" % "1.3.3",
    "org.webjars" % "uikit" % "2.20.3"
  ) ++ akkadeps ++ Seq(
    "com.lihaoyi" %%% "upickle" % "0.2.8",
    "com.lihaoyi" %%% "autowire" % "0.2.5",
    "com.lihaoyi" %%% "scalatags" % "0.5.2"
  )
).settings(
  (resources in Compile) += {
    (fastOptJS in (TasksMonitorWebClient, Compile)).value
    (artifactPath in (TasksMonitorWebClient, Compile, fastOptJS)).value
  }).aggregate(TasksMonitorWebClient).
  dependsOn(TasksMonitorWebSharedJvm,Tasks)
 
  lazy val TranAlignApp = Project(
    "TranAlign",
    file("TranAlign"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= commonDeps )
  ) dependsOn (Commons) 

  lazy val PlotQQApp = Project(
    "PlotQQApp",
    file("PlotQQApp"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= commonDeps )
  ) dependsOn (Commons)

  lazy val MeasureBedApp = Project(
    "MeasureBedApp",
    file("MeasureBedApp"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= commonDeps )
  ) dependsOn (Commons)

  lazy val VCFMinimalRepApp = Project(
    "VCFMinimalRepApp",
    file("VCFMinimalRepApp"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= commonDeps )
  ) dependsOn (Commons,vcfhelpers)

  lazy val FDRApp = Project(
    "FDRApp",
    file("FDRApp"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= commonDeps )
  ) dependsOn (Commons)
  
  lazy val PlotManhattanApp = Project(
    "PlotManhattanApp",
    file("PlotManhattanApp"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= commonDeps )
  ) dependsOn (Commons) 

  lazy val MaskBedToBimApp = Project(
    "MaskBedToBimApp",
    file("MaskBedToBimApp"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= commonDeps )
  ) dependsOn (Commons) 

  lazy val ZmetaApp = Project(
    "ZmetaApp",
    file("ZmetaApp"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= commonDeps )
  ) dependsOn (Commons) 

  
   lazy val GWASApp = Project(
    "GWASApp",
    file("GWASApp"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= (commonDeps ++ Seq(akka_agent)))
  ) dependsOn (TaskWorkflows)

  
  //  lazy val FastlmmWrapper = Project(
  //   "FastlmmWrapper",
  //   file("FastlmmWrapper"),
  //   settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= commonDeps )
  // ) dependsOn (TaskWorkflows)

  lazy val TranslateFastaApp = Project(
    "TranslateFastaApp",
    file("TranslateFastaApp"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= commonDeps )
  ) dependsOn (Commons) 

  lazy val GrepFastaApp = Project(
    "GrepFastaApp",
    file("GrepFastaApp"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= commonDeps )
  ) dependsOn (Commons) 

    lazy val SNP2HLABestAllele = Project(
    "SNP2HLABestAllele",
    file("SNP2HLABestAllele"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= commonDeps )
  ) dependsOn (Commons) 

  

  lazy val DiscriminateToFilesApp = Project(
    "DiscriminateToFilesApp",
    file("DiscriminateToFilesApp"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= commonDeps )
  ) dependsOn (Commons) 

  lazy val WorkerNode: Project = Project(
    "WorkerNode",
    file("WorkerNode"),
    settings = buildSettings ++ Seq (resolvers := allres, libraryDependencies ++= alldeps)
  ) dependsOn (root)

  lazy val ImputationCommon = Project(
    id = "imputation-common",
    base = file("imputation-common/"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= alldeps )
    ) dependsOn(  HDFApp)

  lazy val TaskWorkflows = Project(
    id = "TaskWorkflows",
    base = file("TaskWorkflows/"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= alldeps )
    )
  .dependsOn(ImputationCommon,vcfhelpers,TasksSharedFileTypes) 

   lazy val TasksSharedFileTypes = Project(
    id = "TasksSharedFileTypes",
    base = file("TasksSharedFileTypes/"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= alldeps )
    )
  .dependsOn(Tasks,HDFApp) 




  lazy val MSAApp = Project(
    id = "MSAApp",
    base = file("MSAApp/"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= alldeps )
    ) dependsOn( Commons)

  lazy val ViralShortReadApp = Project(
    id = "ViralShortReadApp",
    base = file("ViralShortReadApp/"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= alldeps )
    ) dependsOn( TaskWorkflows,GenotypePipeline)

  lazy val Dispensability = Project(
    id = "Dispensability",
    base = file("Dispensability/"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= alldeps )
    ) dependsOn( Tasks,Commons,vcfhelpers)
  
  lazy val GenotypePipeline = Project(
    id = "GenotypePipeline",
    base = file("GenotypePipeline/"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= alldeps )
    ) dependsOn( RNASeqAlignPipeline)

  lazy val RNASeqAlignPipeline = Project(
    id = "RNASeqAlignPipeline",
    base = file("RNASeqAlignPipeline/"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= alldeps   )
    ) dependsOn( TaskWorkflows)

  lazy val G2GSequence = Project(
    id = "G2GSequence",
    base = file("G2GSequence/"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= alldeps )
    ) dependsOn( Commons)

  lazy val Dummify = Project(
    id = "Dummify",
    base = file("Dummify/"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= alldeps )
    ) dependsOn( Commons)

  lazy val vcfhelpers = Project(
    id = "vcfhelpers",
    base = file("vcfhelpers/"),
    settings = buildSettings ++ Seq(resolvers := allres, libraryDependencies ++= commonDeps ++ akkadeps  )
    ) dependsOn( Commons)
  
 
lazy val fmpp = TaskKey[Seq[File]]("fmpp")
  lazy val fmppOptions = SettingKey[Seq[String]]("fmpp-options")
  lazy val fmppConfig = config("fmpp")

  lazy val fmppTemplate = fmppConfig(Compile) ++ templateBase
  lazy val templateBase = Seq(
    libraryDependencies += "net.sourceforge.fmpp" % "fmpp" % "0.9.14" % fmppConfig.name,
    ivyConfigurations += fmppConfig,
    fmppOptions := "--ignore-temporary-files" :: Nil,
    fullClasspath in fmppConfig <<= update map { _ select configurationFilter(fmppConfig.name) map Attributed.blank }
  )

  def fmppConfig(c: Configuration): Seq[Setting[_]] = inConfig(c)(Seq(
    sourceGenerators <+= fmpp,
    fmpp <<= fmppTask,
    mappings in packageSrc <<= (managedSources, sourceManaged) map { (srcs, base) => srcs x relativeTo(base) },
    // includeFilter in managedSources := "*Protocols.scala" || "*Protocol.scala",
    // excludeFilter in unmanagedSources := "*Protocols.scala" || "*Protocol.scala",
    sources <<= managedSources
    // sources <<= unmanagedSources
  ))

  lazy val fmppTask =
    (fullClasspath in fmppConfig, runner in fmpp, unmanagedSources, sourceDirectory, includeFilter in unmanagedSources, sourceManaged, fmppOptions, streams) map { (cp, r, sources, srcRoot, filter, output, args, s) =>
      IO.delete(output)
      val arguments = "-U" +: "all" +: "-S" +: srcRoot.getAbsolutePath +: "-O" +: output.getAbsolutePath +: (args ++ sources.getPaths.filter(p => p.endsWith("Protocols.scala") || p.endsWith("Protocol.scala")))
      toError(r.run("fmpp.tools.CommandLine", cp.files, arguments, s.log))
      IO.copyDirectory(srcRoot,output)
      (output ** filter).get
  }
  // // Declare a project with ID 'sub1' in directory 'a'.
  // // Declare a classpath dependency on sub2 in the 'test' configuration.
  // lazy val sub1: Project = Project("sub1", file("a")) dependsOn(sub2 % "test")
  // 
  // // Declare a project with ID 'sub2' in directory 'b'.
  // // Declare a configuration dependency on the root project.
  // lazy val sub2 = Project("sub2", file("b"), delegates = root :: Nil)
}