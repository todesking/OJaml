scalaVersion := "2.12.8"

val compilerScala = project in file("compiler-scala")

val libNative = project in file("lib-native")

val test = (project in file("test"))
  .dependsOn(compilerScala)
  .dependsOn(libNative)
