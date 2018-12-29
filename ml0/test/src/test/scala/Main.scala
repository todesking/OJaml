package com.todesking.ojaml.ml0.test

import org.scalatest.FunSpec
import java.nio.file.Path
import java.nio.file.Files

import scala.collection.JavaConverters._

class Main extends FunSpec {
  init()

  def init(): Unit = {
    val baseDir = new java.io.File(getClass.getClassLoader.getResource("test").toURI).toPath

    listFiles(baseDir).foreach(registerTest)
  }

  def listFiles(p: Path): Seq[Path] =
    Files.list(p).collect(java.util.stream.Collectors.toList[Path]).asScala

  private[this] def registerTest(p: Path): Unit = {
    if (Files.isDirectory(p)) {
      describe(p.toString) {
        listFiles(p).foreach(registerTest)
      }
    } else if (p.getFileName.toString.endsWith(".ml0")) {
      it(p.toString) {
        test(p)
      }
    } else {
      it(p.toString) {
        fail(s"Unknown test file: $p")
      }
    }
  }

  private[this] def test(p: Path): Unit = {
    val outDir = Files.createTempDirectory("ojaml-test")
    val c = new com.todesking.ojaml.ml0.compiler.scala.Compiler(outDir)

    val reName = """(.+)\.ml0""".r
    val className = "test.ml0." + p.getFileName() match { case `reName`(name) => name }

    val reExpected = """\(\* expected: (.+) \*\)""".r
    val expected = Files.readAllLines(p).asScala.collect {
      case `reExpected`(e) => e
    }.headOption getOrElse { throw new RuntimeException(s"Expected value not specified in $p") }

    try {
      c.compile(Seq(p))
      val cl = new java.net.URLClassLoader(Array(outDir.toUri.toURL))
      val klass = cl.loadClass(className)
      val field = klass.getField("result")
      val result = field.get(null)
      assert(expected == result.toString)
    } finally {
      // rm outDir
    }
  }
}
