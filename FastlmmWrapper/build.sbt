import AssemblyKeys._ // put this at the top of the file




name := "FastlmmWrapper"

organization := "pityu"


version := "0.0.0"



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
	
reflectPackage	:= "fastlmmwrapper"
	
reflectClass	:= "Reflected"

sourceGenerators in Compile <+= reflect map identity
