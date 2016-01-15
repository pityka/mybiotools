import AssemblyKeys._ // put this at the top of the file
import BuildSettings._
import com.typesafe.sbt.SbtNativePackager._
import NativePackagerKeys._

scalariformSettings

seq(sbtassembly.Plugin.assemblySettings: _*)

name := "ViralShortReadApp"

organization := "pityu"

version := "0.0.1"+gitHeadCommitSha.value

seq(ReflectPlugin.allSettings:_*)
	
reflectPackage	:= "viralshortread"
	
reflectClass	:= "Reflected"

sourceGenerators in Compile <+= reflect map identity

scalariformSettings

parallelExecution in Test := false

mainClass in assembly := Some("viralshortread.ViralShortReadApp")

mainClass in Compile := Some("viralshortread.ViralShortReadApp")

