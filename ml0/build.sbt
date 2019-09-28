scalaVersion := "2.12.10"
val commonSettings = Seq(scalaVersion := "2.12.10")

val compilerScala = (project in file("compiler-scala"))
  .settings(commonSettings)

val libNative = (project in file("lib-native"))
  .settings(commonSettings)

val lib = (project in file("lib"))
  .settings(commonSettings)

val test = (project in file("test"))
  .settings(commonSettings)
  .dependsOn(compilerScala)
  .dependsOn(libNative)
  .dependsOn(lib)

val repl = (project in file("repl"))
  .settings(commonSettings)
  .dependsOn(compilerScala)
  .dependsOn(libNative)
  .dependsOn(lib)
