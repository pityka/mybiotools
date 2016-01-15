import AssemblyKeys._ // put this at the top of the file
import com.typesafe.sbt.SbtNativePackager._
import NativePackagerKeys._

scalariformSettings

seq(sbtassembly.Plugin.assemblySettings: _*)

name := "Dummify"

organization := "pityu"

version := "0.0.1"

seq(ReflectPlugin.allSettings:_*)
	
reflectPackage	:= "dummify"
	
reflectClass	:= "Reflected"

sourceGenerators in Compile <+= reflect map identity

scalariformSettings

parallelExecution in Test := false

mainClass in Compile := Some("dummify.Dummify")
