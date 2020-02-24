package com.todesking.ojaml.ml0.test

import org.scalatest.FunSpec
import java.nio.file.Path
import java.nio.file.Files

import com.todesking.ojaml.ml0.compiler.scala.ModuleRef
import com.todesking.ojaml.ml0.compiler.scala.Result
import com.todesking.ojaml.ml0.compiler.scala.Type
import com.todesking.ojaml.ml0.compiler.scala.VarRef

import scala.collection.JavaConverters._
import com.todesking.ojaml.ml0.compiler.scala.Javalizer

class Main extends FunSpec {
  private[this] val classLoader = getClass.getClassLoader
  init()

  def init(): Unit = {
    val baseDir = new java.io.File(classLoader.getResource("test").toURI).toPath

    listFiles(baseDir).foreach(registerTest(baseDir, _))
  }

  def listFiles(p: Path): Seq[Path] =
    Files.list(p).collect(java.util.stream.Collectors.toList[Path]).asScala

  private[this] def registerTest(base: Path, p: Path): Unit = {
    if (Files.isDirectory(p)) {
      if (p.getFileName.toString.endsWith(".ml0")) {
        it(base.relativize(p).toString) {
          test(base.relativize(p).toString, listFiles(p))
        }
      } else {
        describe(base.relativize(p).toString) {
          listFiles(p).foreach(registerTest(p, _))
        }
      }
    } else if (p.getFileName.toString.endsWith(".ml0")) {
      it(base.relativize(p).toString) {
        test(base.relativize(p).toString, Seq(p))
      }
    } else {
      it(base.relativize(p).toString) {
        fail(s"Unknown test file: $p")
      }
    }
  }

  private[this] def test(testName: String, paths: Seq[Path]): Unit = {
    import TestMain.Target
    import TestMain.Assertion
    import com.todesking.ojaml.ml0.compiler.{ scala => scala_compiler }

    lazy val predefPath = new java.io.File(classLoader.getResource("lib/Predef.ml0").toURI).toPath
    lazy val predefContent = scala_compiler.FileContent(predefPath, Files.readAllLines(predefPath).asScala.mkString("\n"))

    val targets = paths.sortBy(_.toString).map(Target.from)
    val predef = targets.exists(_.predef)
    val debugPrint = targets.exists(_.debugPrint)
    val pending = targets.exists(_.pending)
    if (pending) this.pending

    val expectedErrors = targets.flatMap { t =>
      t.expectedErrors.map { case (l, c) => (t.path.toString, l, c) }
    }.toSet
    val assertions = targets.flatMap(_.assertions)

    val testContents = targets.map { t => scala_compiler.FileContent(t.path, t.content) }
    val contents = if (predef) predefContent +: testContents else testContents

    val outDir = Files.createTempDirectory("ojaml-test")
    val cl = this.getClass.getClassLoader
    val compiler = new scala_compiler.Compiler(outDir, cl, debugPrint)

    assert(expectedErrors.isEmpty || assertions.isEmpty)

    def validateErrors(result: Seq[Result.Message]): Unit = {
      val errors = result.map { e =>
        if (e.pos == null) ("(nopos)", 0, 0) -> e
        else (e.pos.location, e.pos.line, e.pos.col) -> e
      }.toMap
      assert(errors.size == result.size)
      val unexpected = errors.keySet -- expectedErrors
      val notHappend = expectedErrors -- errors.keySet
      unexpected.foreach {
        case pos =>
          val e = errors(pos)
          println(s"${e.pos} [Unexpected] ${e.message}")
      }
      notHappend.foreach {
        case (p, l, c) =>
          println(s"$p:$l:$c Error expected but not happend")
      }
      if (unexpected.nonEmpty || notHappend.nonEmpty) {
        fail("Unexpected/Missing errors: See error log")
      }
    }

    def validateRuntime(env: Map[VarRef.ModuleMember, Type]): Unit = {
      try {
        try {
          val cl = new java.net.URLClassLoader(Array(outDir.toUri.toURL), this.getClass.getClassLoader)

          // make sure all classes are valid
          targets.flatMap(_.classNames).foreach { cn => cl.loadClass(cn) }

          assertions.foreach {
            case Assertion(ref, klassName, fieldName, typeName, value) =>
              val klass = cl.loadClass(klassName)
              val field = klass.getField(fieldName)
              val actual = field.get(null)
              if (typeName != "*") {
                val tpe = env(VarRef.ModuleMember(ref, fieldName))
                assert(typeName == tpe.toString)
                assert(tpe.jtype.hname == field.getType.getName, s"at $fieldName")
              }
              if (value != "*") {
                assert(value == s"$actual", s"at $fieldName")
              }
          }
        } catch {
          case e: LinkageError => throw new RuntimeException(e)
        } finally {
          // rm outDir
        }
      } catch {
        case e: Throwable =>
          println(s"Test failed at runtime($testName): outDir = $outDir")
          throw e
      }
    }

    compiler.typeContents(contents).fold({ errors =>
      validateErrors(errors)
    }, {
      case (env, trees) =>
        validateErrors(Seq())
        trees.flatMap(compiler.javalizePhase).foreach(compiler.emit)
        validateRuntime(env)
    })
  }
}

object TestMain {
  case class Assertion(
    ref: ModuleRef,
    klass: String,
    field: String,
    tpe: String,
    value: String)
  case class Target(
    path: Path,
    content: String,
    expectedErrors: Set[(Int, Int)],
    assertions: Seq[Assertion],
    classNames: Seq[String],
    predef: Boolean,
    pending: Boolean,
    debugPrint: Boolean)

  object Target {
    def from(path: Path): Target = {
      val lines = Files.readAllLines(path).asScala

      val reModuleName = """module\s+([^\s]+)""".r
      val classNames = lines.collect { case `reModuleName`(name) => s"test.ml0.$name" }

      val defaultClassName = "test.ml0." + path.getFileName.toString.split("\\.")(0)

      val pending = lines.contains("(* pending *)")
      val debugPrint = lines.contains("(* debug *)")
      val predef = lines.contains("(* using: Predef *)")

      val reError = """^(\s*\(\*\s*\^).*""".r
      val expectedErrors = lines.zipWithIndex.collect {
        case (reError(error), i) =>
          val line = i // line = i +1(1-origin) -1(above line)
          val col = error.length
          (line, col)
      }.toSet

      val reAssertion = """^\s*\(\* ([\w.]+): ([^=]+?)= (.+) \*\)\s*$""".r
      val assertions = lines.collect {
        case `reAssertion`(name, rawTpe, value) =>
          val tpe = rawTpe.init
          if (name.contains(".")) {
            val className = "test.ml0." + name.split("\\.").init.mkString(".")
            val n = name.split("\\.").last
            Assertion(ModuleRef.fromFullName(className), className, n, tpe, value)
          } else {
            Assertion(ModuleRef.fromFullName(defaultClassName), defaultClassName, name, tpe, value)
          }
      }
      Target(
        path,
        content = lines.mkString("\n"),
        expectedErrors = expectedErrors,
        assertions = assertions,
        classNames = classNames,
        predef = predef,
        pending = pending,
        debugPrint = debugPrint)
    }
  }
}
