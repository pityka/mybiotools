import AssemblyKeys._ // put this at the top of the file




name := "ZmetaApp"

organization := "pityu"

version := "1.0.0"


scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

seq(sbtassembly.Plugin.assemblySettings: _*)

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case x if x.matches("META-INF/*") => MergeStrategy.first
    case x => old(x)
  }
}

scalariformSettings