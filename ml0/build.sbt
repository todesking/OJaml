scalaVersion := "2.12.8"

val compilerScala = project in file("compiler-scala")

val test = (project in file("test"))
  .dependsOn(compilerScala)
