import AssemblyKeys._ // put this at the top of the file
import com.typesafe.sbt.SbtNativePackager._
import NativePackagerKeys._

scalariformSettings

seq(sbtassembly.Plugin.assemblySettings: _*)

name := "G2GSequence"

organization := "pityu"

version := "0.0.1"

seq(ReflectPlugin.allSettings:_*)
	
reflectPackage	:= "g2gsequence"
	
reflectClass	:= "Reflected"

sourceGenerators in Compile <+= reflect map identity

scalariformSettings

parallelExecution in Test := false

mainClass in Compile := Some("g2gsequence.G2GSequenceApp")
