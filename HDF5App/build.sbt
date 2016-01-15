import AssemblyKeys._ // put this at the top of the file
import BuildSettings._


name := "dosagetool"

organization := "pityu"

version := "17.0.2-b1"+gitHeadCommitSha.value

// unmanagedBase <<= baseDirectory { base => base / "../lib" }


crossScalaVersions := Seq("2.9.2", "2.9.1")



scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

seq(sbtassembly.Plugin.assemblySettings: _*)

mainClass in assembly := Some("hdfdosage.HDF5DosageApplication")

mainClass in Compile := Some("hdfdosage.HDF5DosageApplication")

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case "META-INF/NOTICE.txt" => MergeStrategy.concat
    case "META-INF/LICENSE.txt" => MergeStrategy.concat
    case x => old(x)
  }
}

scalariformSettings

seq(ReflectPlugin.allSettings:_*)
	
reflectPackage	:= "hdfdosage"
	
reflectClass	:= "Reflected"
	
sourceGenerators in Compile <+= reflect map identity