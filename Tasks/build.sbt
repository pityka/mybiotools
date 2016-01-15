import AssemblyKeys._ // put this at the top of the file

name := "tasks"

organization := "pityu"

version := "12.0.1-b1"

seq(sbtassembly.Plugin.assemblySettings: _*)

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case "META-INF/NOTICE.txt" => MergeStrategy.concat
    case "META-INF/LICENSE.txt" => MergeStrategy.concat
    case x => old(x)
  }
}

scalariformSettings

seq(ReflectPlugin.allSettings:_*)
	
reflectPackage	:= "mybiotools.tasks"
	
reflectClass	:= "Reflected"
	
sourceGenerators in Compile <+= reflect map identity


