scalaVersion := "2.12.8"

val compilerScala = project in file("compiler-scala")

val libNative = project in file("lib-native")

val lib = project in file("lib")

val test = (project in file("test"))
  .dependsOn(compilerScala)
  .dependsOn(libNative)
  .dependsOn(lib)
