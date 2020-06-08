scalaVersion := "2.13.2"
val commonSettings = Seq(scalaVersion := "2.13.2")

val compilerScala = (project in file("compiler-scala"))
  .settings(commonSettings)

val libNative = (project in file("lib-native"))
  .settings(commonSettings)

val lib = (project in file("lib"))
  .settings(commonSettings)

val repl = (project in file("repl"))
  .settings(commonSettings)
  .dependsOn(compilerScala)
  .dependsOn(libNative)
  .dependsOn(lib)

val test = (project in file("test"))
  .settings(commonSettings)
  .dependsOn(compilerScala)
  .dependsOn(libNative)
  .dependsOn(lib)
  .dependsOn(repl)
