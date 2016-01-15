import AssemblyKeys._ // put this at the top of the file
import BuildSettings._



name := "SNP2HLABestAllele"

organization := "pityu"

version := gitHeadCommitSha.value


scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

seq(sbtassembly.Plugin.assemblySettings: _*)

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case x if x.matches("META-INF/*") => MergeStrategy.first
    case x => old(x)
  }
}