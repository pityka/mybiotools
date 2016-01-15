import AssemblyKeys._ // put this at the top of the file




name := "FDRApp"

organization := "pityu"

version := "1.0.1"


seq(sbtassembly.Plugin.assemblySettings: _*)

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case x if x.matches("META-INF/*") => MergeStrategy.first
    case x => old(x)
  }
}