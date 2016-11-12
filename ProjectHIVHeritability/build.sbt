import AssemblyKeys._ // put this at the top of the file
import BuildSettings._



name := "HIVHeritability"

organization := "pityu"


version := "0.1.56-"+gitHeadCommitSha.value



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

reflectPackage	:= "hivheritability"

reflectClass	:= "Reflected"

sourceGenerators in Compile <+= reflect map identity
