import AssemblyKeys._ // put this at the top of the file

scalariformSettings



seq(sbtassembly.Plugin.assemblySettings: _*)

name := "TaskWorkflows"

organization := "pityu"

version := "0.0.5-SNAPSHOT"