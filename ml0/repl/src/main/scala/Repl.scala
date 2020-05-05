package com.todesking.ojaml.ml0

import com.todesking.ojaml.ml0.compiler.{ scala => ojaml }

import ojaml.util.Syntax._
import com.todesking.ojaml.ml0.compiler.scala.Javalizer
import java.io.Closeable
import java.nio.file.Files
import java.nio.file.Path
import java.util.stream.Collectors
import scala.collection.JavaConverters._
import com.todesking.ojaml.ml0.compiler.scala.Pos
import com.todesking.ojaml.ml0.compiler.scala.Classpath
import com.todesking.ojaml.ml0.compiler.scala.Import
import com.todesking.ojaml.ml0.compiler.scala.Source
import com.todesking.ojaml.ml0.compiler.scala.RawAST

class Repl extends Closeable {
  import ojaml.{ RawAST => RT, TypedAST => TT }
  import Repl.Input
  import Repl.Result

  val replFileName = "<REPL>"
  val fakePos = Pos(replFileName, 1, 1)
  lazy val tmpDir = Files.createTempDirectory("ojaml-repl")
  lazy val targetClassLoader = new java.net.URLClassLoader(Array(tmpDir.toUri.toURL), this.getClass.getClassLoader)

  private[this] def newCompiler(debugPrint: Boolean) =
    new ojaml.Compiler(debugPrint)

  private[this] def emitter = new ojaml.Emitter(tmpDir)

  private[this] var compiler = newCompiler(false)
  private[this] var env = ojaml.Compiler.newEnv(getClass.getClassLoader)

  private[this] var imports: Seq[ojaml.Import] = Seq()
  private[this] var nextIndex = 0

  private[this] def mkName(s: String) = ojaml.Name(fakePos, s)

  private[this] def mkQName(s: String) = {
    ojaml.QName(s.split("\\.", -1).map(mkName))
  }

  def evalPredef(): Unit = {
    val predefContent = Source.readResource(getClass.getClassLoader, "lib/Predef.ml0")
    compiler.typeContents(env, Seq(predefContent)).fold { errors =>
      println("Compile error in Predef:")
      errors.foreach { e =>
        println(s"${e.pos} ${e.message}")
      }
    } {
      case (newEnv, trees) =>
        val predefImports = trees.flatMap { tree =>
          tree.body.collect {
            case TT.TLet(pos, name, tpe, expr) =>
              (tree.moduleRef, name.value)
          }
        }.map {
          case (module, name) =>
            val qname = mkQName(s"${module.fullName}.$name")
            Import.Single(qname, None)
        }
        trees
          .flatMap(compiler.javalizePhase(_))
          .foreach(emitter.emit(_))
        this.env = newEnv
        this.imports ++= predefImports
    }
  }

  private[this] def input(tree: ojaml.RawAST.Term): Input = {
    import ojaml.{ RawAST => RT }
    tree match {
      case tree @ RT.TLet(_, name, tname, expr) => Input.Let(name.value, tree)
      case tree @ RT.Data(_, name, tvars, ctors) => Input.Data(ctors.map(_._1.value), tree)
      case tree @ RT.TExpr(_, expr) => Input.Expr(s"res$nextIndex", expr)
    }
  }

  private[this] def parse(code: String): Either[Result, Input] = {
    val parser = new ojaml.Parser(replFileName)
    val exprName = s"res$nextIndex"
    parser.parseTerm(code).toEither match {
      case Left(_) =>
        parser.parseExpr(code).toEither match {
          case Left(msgs) =>
            Left(Result.CompileError(msgs.head.message))
          case Right(expr) =>
            Right(Input.Expr(exprName, expr))
        }
      case Right(term) => Right(input(term))
    }
  }

  private[this] def compile(statement: RT.Term): Either[Result, (ojaml.Compiler.Env, TT.Module)] = {
    val program = RT.Program(
      fakePos,
      mkQName("ojaml.repl"),
      imports,
      Seq(
        RT.Module(
          fakePos,
          mkName(s"Repl_$nextIndex"),
          Seq(statement))))
    val result =
      for {
        x <- compiler.namePhase(env.nameEnv, program)
        (nenv, Seq(namedTree)) = x
        x <- compiler.typePhase(env.typeEnv, namedTree)
        (tenv, typedTree) = x
      } yield (env.copy(nameEnv = nenv, typeEnv = tenv), typedTree)
    result.toEither.swap.map { errors =>
      Result.CompileError(errors.mkString(", "))
    }.swap
  }

  private[this] def evalClass(name: String): Either[Result, Class[_]] =
    try {
      Right(targetClassLoader.loadClass(name))
    } catch {
      case e: Throwable => Left(Result.RuntimeError(e))
    }

  private[this] def readValue(klass: Class[_], tree: TT.Module): Result = {
    val (name, tpe) =
      tree match {
        case ojaml.TypedAST.Module(_, _, _, Seq(ojaml.TypedAST.TLet(_, n, t, _))) =>
          (n, t)
        case unk => throw new AssertionError(s"TLet expected: $unk")
      }
    val isUnit = tpe == ojaml.Type.Unit
    if (isUnit) Result.Empty
    else {
      val fieldName = emitter.escape(name.value)
      val field = klass.getField(fieldName)
      val value = field.get(null)
      Result.Value(name.value, value, tpe.toString)
    }
  }

  private[this] def evalRuntime(in: Input, tree: TT.Module): Result = {
    evalClass(tree.moduleRef.fullName).map { klass =>
      in match {
        case Input.Data(_, _) => Result.Empty
        case Input.Let(name, _) => readValue(klass, tree)
        case Input.Expr(name, _) => readValue(klass, tree)
      }
    }.merge
  }

  def eval(code: String): Result = {
    parse(code).flatMap { in =>
      val (statement, names) = in match {
        case Input.Data(cnames, tree) => (tree, cnames)
        case Input.Expr(name, tree) => (RT.TLet(fakePos, mkName(name), None, tree), Seq(name))
        case Input.Let(name, tree) => (tree, Seq(name))
      }
      compile(statement).map {
        case (newEnv, tree) =>
          val j = new Javalizer
          compiler.javalizePhase(tree).foreach(emitter.emit)
          this.env = newEnv
          this.imports = this.imports ++ names.map { name =>
            ojaml.Import.Single(mkQName(s"${tree.moduleRef.fullName}.$name"), None)
          }
          nextIndex += 1
          evalRuntime(in, tree)
      }
    }.merge
  }

  def setDebugPrint(x: Boolean): Unit = {
    compiler = newCompiler(x)
  }

  override def close(): Unit = {
    rmr(tmpDir)
  }

  private[this] def rmr(path: Path): Unit = {
    if (Files.isDirectory(path)) {
      Files.list(path)
        .collect(Collectors.toList())
        .asScala
        .foreach { sub => rmr(sub) }
    }
    Files.delete(path)
  }
}

object Repl {
  def main(args: Array[String]): Unit = {
    val repl = new Repl()

    println("OJaml REPL")
    println(s"tmp dir = ${repl.tmpDir}")

    try {
      repl.evalPredef()
      loop(repl)
    } finally {
      repl.close()
    }
  }

  sealed abstract class Result
  object Result {
    case object Empty extends Result
    case class Value(bindingName: String, value: Any, tpe: String) extends Result
    case class CompileError(msg: String) extends Result
    case class RuntimeError(exception: Throwable) extends Result
  }

  sealed abstract class Input
  object Input {
    case class Expr(name: String, tree: ojaml.RawAST.Expr) extends Input
    case class Let(name: String, tree: ojaml.RawAST.TLet) extends Input
    case class Data(ctors: Seq[String], tree: ojaml.RawAST.Data) extends Input
  }

  @scala.annotation.tailrec
  def loop(repl: Repl): Unit = {
    val prompt = "ojaml> "
    val line = scala.io.StdIn.readLine(prompt)
    line match {
      case null => // EOF
      case "" =>
        loop(repl)
      case cmd if cmd.head == ':' => cmd.tail match {
        case "q" | "exit" =>
        case "debug on" =>
          repl.setDebugPrint(true)
          loop(repl)
        case "debug off" =>
          repl.setDebugPrint(false)
          loop(repl)
        case unk =>
          println(s"Unknown command.")
          loop(repl)
      }
      case code =>
        repl.eval(code) match {
          case Result.Empty =>
          case Result.Value(name, value, tpe) =>
            println(s"$name: $tpe = $value")
          case Result.CompileError(msg) =>
            println(s"Compile error: $msg")
          case Result.RuntimeError(th) =>
            th.printStackTrace()
        }
        loop(repl)
    }
  }
}
