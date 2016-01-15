import AssemblyKeys._ // put this at the top of the file





scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

seq(sbtassembly.Plugin.assemblySettings: _*)

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case x if x.matches("META-INF/.*") => MergeStrategy.concat
    case x => old(x)
  }
}