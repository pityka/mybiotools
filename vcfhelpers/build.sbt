import AssemblyKeys._ // put this at the top of the file

scalariformSettings

seq(sbtassembly.Plugin.assemblySettings: _*)

scalariformSettings

parallelExecution in Test := false

name := "vcfhelpers"

organization := "pityu"

version := "0.0.4"