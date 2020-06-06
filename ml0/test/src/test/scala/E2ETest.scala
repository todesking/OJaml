package com.todesking.ojaml.ml0.test

import org.scalatest.FunSpec
import java.nio.file.Path
import java.nio.file.Files

import com.todesking.ojaml.ml0.compiler.scala.ModuleRef
import com.todesking.ojaml.ml0.compiler.scala.Result
import com.todesking.ojaml.ml0.compiler.scala.Type
import com.todesking.ojaml.ml0.compiler.{ scala => scala_compiler }

import scala.collection.JavaConverters._
import com.todesking.ojaml.ml0.compiler.scala.Javalizer
import java.util.stream.Collectors
import com.todesking.ojaml.ml0.compiler.scala.MemberRef

class E2ETest extends FunSpec {
  private[this] val classLoader = getClass.getClassLoader
  init()

  def init(): Unit = {
    val baseDir = new java.io.File(classLoader.getResource("test").toURI).toPath

    listFiles(baseDir).foreach(registerTest(baseDir, _))
  }

  val emptyEnv = scala_compiler.Compiler.newEnv(classLoader)
  val predefDir = Files.createTempDirectory("ojaml-test-predef")
  lazy val predefEnv = {
    val predefContent = scala_compiler.Source.readResource(classLoader, "lib/Predef.ml0")

    val compiler = new scala_compiler.Compiler(false)
    val emitter = new scala_compiler.Emitter(predefDir)

    compiler.compile(emptyEnv, Seq(predefContent)).fold { errors =>
      throw new AssertionError(s"Predef compilation error: ${errors.mkString("\n")}")
    } {
      case (env, trees) =>
        trees.foreach(emitter.emit(_))
        env
    }
  }

  def listFiles0(p: Path): Seq[Path] = {
    val stream = Files.list(p)
    try {
      stream
        .collect(java.util.stream.Collectors.toList[Path])
        .asScala
    } finally {
      stream.close()
    }
  }
  def listFiles(p: Path): Seq[Path] =
    listFiles0(p)
      .sortBy { path =>
        val grouped =
          if (Files.isDirectory(path) && !path.toString.endsWith(".ml0")) 1
          else 0
        (grouped, path.toString)
      }

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
    import E2ETest.Target
    import E2ETest.Assertion

    val targets = paths.sortBy(_.toString).map(Target.from)
    val usePredef = targets.exists(_.predef)
    val debugPrint = targets.exists(_.debugPrint)
    val pending = targets.exists(_.pending)
    if (pending) this.pending

    val expectedErrors = targets.flatMap { t =>
      t.expectedErrors.map { case (l, c) => (t.path.toString, l, c) }
    }.toSet
    val assertions = targets.flatMap(_.assertions)
    assert(expectedErrors.isEmpty || assertions.isEmpty)

    val contents = targets.map { t => scala_compiler.Source(t.path.toString, t.content) }

    val outDir = Files.createTempDirectory("ojaml-test")
    val compiler = new scala_compiler.Compiler(debugPrint)
    val emitter = new scala_compiler.Emitter(outDir)

    val env = if (usePredef) predefEnv else emptyEnv

    val runtimeClasspath = (
      if (usePredef) Array(predefDir, outDir)
      else Array(outDir)).map(_.toUri.toURL)

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

    def validateRuntime(env: Map[MemberRef, Type]): Unit = {
      try {
        try {
          val cl = new java.net.URLClassLoader(runtimeClasspath, this.classLoader)

          // make sure all classes are valid
          targets.flatMap(_.classNames).foreach { cn => cl.loadClass(cn) }

          assertions.foreach {
            case Assertion(ref, klassName, fieldName, typeName, value) =>
              val klass = cl.loadClass(klassName)
              val field = klass.getField(fieldName)
              val actual = field.get(null)
              if (typeName != "*") {
                val tpe = env(ref.memberRef(fieldName))
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

    compiler.compile(env, contents).fold { errors =>
      validateErrors(errors)
    } {
      case (env, trees) =>
        validateErrors(Seq())
        trees.foreach(emitter.emit)
        validateRuntime(env.typeEnv.types)
    }

    rmr(outDir) // skipped if test failed
  }

  private[this] def rmr(path: Path): Unit = {
    if (Files.isDirectory(path)) {
      listFiles0(path).foreach { sub => rmr(sub) }
    }
    Files.delete(path)
  }
}

object E2ETest {
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
