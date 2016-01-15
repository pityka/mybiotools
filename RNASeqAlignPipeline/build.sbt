import AssemblyKeys._ // put this at the top of the file
import BuildSettings._
import com.typesafe.sbt.SbtNativePackager._
import NativePackagerKeys._

scalariformSettings

seq(sbtassembly.Plugin.assemblySettings: _*)

name := "RNASeqAlignPipeline"

organization := "pityu"

version := "0.0.51"+gitHeadCommitSha.value

seq(ReflectPlugin.allSettings:_*)
	
reflectPackage	:= "rnaseqalign"
	
reflectClass	:= "Reflected"

sourceGenerators in Compile <+= reflect map identity

scalariformSettings

parallelExecution in Test := false

mainClass in assembly := Some("rnaseqalign.RNASeqAlignApp")

mainClass in Compile := Some("rnaseqalign.RNASeqAlignApp")

