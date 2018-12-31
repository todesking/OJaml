package com.todesking.ojaml.ml0.test

import org.scalatest.FunSpec
import java.nio.file.Path
import java.nio.file.Files

import scala.collection.JavaConverters._

class Main extends FunSpec {
  init()

  def init(): Unit = {
    val baseDir = new java.io.File(getClass.getClassLoader.getResource("test").toURI).toPath

    listFiles(baseDir).foreach(registerTest(baseDir, _))
  }

  def listFiles(p: Path): Seq[Path] =
    Files.list(p).collect(java.util.stream.Collectors.toList[Path]).asScala

  private[this] def registerTest(base: Path, p: Path): Unit = {
    if (Files.isDirectory(p)) {
      describe(base.relativize(p).toString) {
        listFiles(p).foreach(registerTest(p, _))
      }
    } else if (p.getFileName.toString.endsWith(".ml0")) {
      it(base.relativize(p).toString) {
        test(p)
      }
    } else {
      it(base.relativize(p).toString) {
        fail(s"Unknown test file: $p")
      }
    }
  }

  private[this] def test(p: Path): Unit = {
    val outDir = Files.createTempDirectory("ojaml-test")
    val c = new com.todesking.ojaml.ml0.compiler.scala.Compiler(outDir)

    val reName = """(.+)\.ml0""".r
    val className = "test.ml0." + p.getFileName() match { case `reName`(name) => name }

    val reExpected = """\(\* ([\w.]+): ([\w.]+) = (.+) \*\)""".r
    val expects = Files.readAllLines(p).asScala.filter(_.startsWith("(* ")).map {
      case `reExpected`(name, tpe, value) => (name, tpe, value)
    }

    try {
      c.compile(Seq(p))
      val cl = new java.net.URLClassLoader(Array(outDir.toUri.toURL))
      val klass = cl.loadClass(className)
      expects.foreach {
        case (fieldName, fieldTypeName, value) =>
          val field = klass.getField(fieldName)
          val result = field.get(null)
          assert(fieldTypeName == field.getType.getName)
          assert(value == s"$result")
      }
    } finally {
      // rm outDir
    }
  }
}
