import AssemblyKeys._ // put this at the top of the file
import BuildSettings._




name := "GWASApp"

organization := "pityu"


version := "3.6.1-b1"+gitHeadCommitSha.value



seq(sbtassembly.Plugin.assemblySettings: _*)

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case x if x.matches("META-INF/*") => MergeStrategy.first
    case x if x.matches("README") => MergeStrategy.concat
    case x => old(x)
  }
}

scalariformSettings

seq(ReflectPlugin.allSettings:_*)
	
reflectPackage	:= "gwasapp"
	
reflectClass	:= "Reflected"

sourceGenerators in Compile <+= reflect map identity

mainClass in assembly := Some("gwasapp.GWASApp")

mainClass in Compile := Some("gwasapp.GWASApp")

