import AssemblyKeys._ // put this at the top of the file
import BuildSettings._
import com.typesafe.sbt.SbtNativePackager._
import NativePackagerKeys._


scalariformSettings

seq(sbtassembly.Plugin.assemblySettings: _*)

name := "GenotypePipeline"

organization := "pityu"

version := "0.0.47"+gitHeadCommitSha.value

seq(ReflectPlugin.allSettings:_*)
	
reflectPackage	:= "genotyper"
	
reflectClass	:= "Reflected"

sourceGenerators in Compile <+= reflect map identity

scalariformSettings

parallelExecution in Test := false

mainClass in assembly := Some("genotyper.GenotypingApp")

mainClass in Compile := Some("genotyper.GenotypingApp")

