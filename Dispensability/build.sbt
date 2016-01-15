import AssemblyKeys._ // put this at the top of the file
import com.typesafe.sbt.SbtNativePackager._
import NativePackagerKeys._
import BuildSettings._

scalariformSettings

seq(sbtassembly.Plugin.assemblySettings: _*)

name := "Dispensability"

organization := "pityu"

version := gitHeadCommitSha.value

seq(ReflectPlugin.allSettings:_*)
	
reflectPackage	:= "dispensability"
	
reflectClass	:= "Reflected"

sourceGenerators in Compile <+= reflect map identity

scalariformSettings

parallelExecution in Test := false

mainClass in assembly := Some("dispensability.DispensabilityScoreApp")

mainClass in Compile := Some("dispensability.DispensabilityScoreApp")
