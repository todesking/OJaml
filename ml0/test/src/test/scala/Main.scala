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
    import com.todesking.ojaml.ml0.compiler.{ scala => scala_compiler }
    val outDir = Files.createTempDirectory("ojaml-test")
    val c = new scala_compiler.Compiler(outDir)

    val reName = """(.+)\.ml0""".r
    val className = "test.ml0." + p.getFileName() match { case `reName`(name) => name }

    val lines = Files.readAllLines(p).asScala

    val reError = """^(\s*\(\*\s*\^).*""".r
    val expectedErrors = lines.zipWithIndex.collect {
      case (reError(error), i) =>
        (i + 1, error.length + 1)
    }

    val reExpected = """^\s*\(\* ([\w.]+): ([\w.]+) = (.+) \*\)\s*$""".r
    val expects = lines.collect {
      case `reExpected`(name, tpe, value) => (name, tpe, value)
    }

    assert(expectedErrors.isEmpty || expects.isEmpty)
    if (expectedErrors.nonEmpty) {
      val content = scala_compiler.FileContent(p, lines.mkString("\n"))
      val result = c.compileContents(Seq(content))
      val errors = result.errors.map { e =>
        (e.line, e.col) -> e
      }.toMap
      assert(errors.size == result.errors.size)
      val unexpected = errors.keySet -- expectedErrors
      val notHappend = expectedErrors -- errors.keySet
      unexpected.foreach {
        case pos =>
          val e = errors(pos)
          println(s"${e.path}:${e.line}:${e.col} [Unexpected] ${e.message}")
      }
      notHappend.foreach {
        case (l, c) =>
          println(s"$p:$l:$c Error expected but not happend")
      }
      assert(Set() == notHappend)
      assert(unexpected.isEmpty)
    } else {
      try {
        val content = scala_compiler.FileContent(p, lines.mkString("\n"))
        val result = c.compileContents(Seq(content))
        assert(Seq() == result.errors)
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
}
