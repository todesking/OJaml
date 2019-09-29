package com.todesking.ojaml.ml0

import com.todesking.ojaml.ml0.compiler.{ scala => ojaml }

import ojaml.util.Syntax._
import com.todesking.ojaml.ml0.compiler.scala.PackageEnv

class Repl {
  import ojaml.{ RawAST => RT, TypedAST => TT }
  import Repl.Input
  import Repl.Result

  val replFileName = "<REPL>"
  lazy val tmpDir = java.nio.file.Files.createTempDirectory("ojaml-repl")
  lazy val targetClassLoader = new java.net.URLClassLoader(Array(tmpDir.toUri.toURL), this.getClass.getClassLoader)

  private[this] def newCompiler(debugPrint: Boolean) =
    new ojaml.Compiler(tmpDir, getClass.getClassLoader, debugPrint)

  private[this] var compiler = newCompiler(false)
  private[this] var packageEnv = ojaml.PackageEnv(compiler.classRepo)
  private[this] var moduleEnv = Map.empty[ojaml.VarRef.ModuleMember, ojaml.Type]

  private[this] val predefImports =
    Seq("+", "-", "*", "/", "%", "==", "<=", ">=", ">", "<", "&", "|")
      .map { name => mkQName(s"com.todesking.ojaml.ml0.lib.Predef.$name") }
      .map(ojaml.Import.apply)

  private[this] var imports: Seq[ojaml.Import] = Seq()
  private[this] var nextIndex = 0

  private[this] def mkName(s: String) = {
    val name = ojaml.Name(s)
    fakePos(name)
  }

  private[this] def mkQName(s: String) = {
    fakePos(ojaml.QName(s.split("\\.", -1).map(mkName)))
  }

  private[this] def fakePos[A <: ojaml.HasPos](a: A): A = {
    a.fillPos(replFileName, 1, 1)
    a
  }

  def evalPredef(): Unit = {
    val src = {
      val is = getClass.getClassLoader.getResourceAsStream("lib/Predef.ml0")
      val s = new java.util.Scanner(is)
      var lines = Seq.empty[String]
      while (s.hasNextLine()) {
        lines :+= s.nextLine()
      }
      lines.mkString("\n")
    }
    val predefContent = ojaml.FileContent(java.nio.file.Paths.get("(Predef)"), src)
    val result =
      for {
        rawAst <- compiler.parsePhase(predefContent)
        x1 <- compiler.namePhase(packageEnv, rawAst)
        (newPEnv, namedAsts) = x1
        x2 <- namedAsts.mapWithContextEC(moduleEnv) { (menv, tree) =>
          compiler.typePhase(menv, tree)
        }
        (newMEnv, typedAst) = x2
      } yield (newPEnv, newMEnv, typedAst)
    result.fold({ errors =>
      errors.foreach { e =>
        println(e)
      }
    }, {
      case (newPEnv, newMEnv, trees) =>
        trees.foreach(compiler.assemble)
        this.packageEnv = newPEnv
        this.moduleEnv = newMEnv
        this.imports ++= predefImports
    })
  }

  private[this] def input(tree: ojaml.RawAST.Term): Input = {
    import ojaml.{ RawAST => RT }
    tree match {
      case tree @ RT.TLet(name, expr) => Input.Let(name.value, tree)
      case tree @ RT.Data(name, ctors) => Input.Data(name.value, ctors.map(_._1.value), tree)
      case expr: RT.Expr => Input.Expr(s"res$nextIndex", expr)
    }
  }

  private[this] def parse(code: String): Either[Result, Input] = {
    val parser = new ojaml.Parser(replFileName)
    parser.parseTerm(code).toEither match {
      case Left(msgs) => Left(Result.CompileError(msgs.head.message))
      case Right(rawTree) => Right(input(rawTree))
    }
  }

  private[this] def compile(statement: RT.Term): Either[Result, (PackageEnv, ojaml.Compiler.ModuleEnv, TT.Module)] = {
    val program = RT.Program(
      mkQName("ojaml.repl"),
      imports,
      Seq(
        RT.Module(
          mkName(s"Repl_$nextIndex"),
          Seq(statement))))
    val result =
      for {
        x1 <- compiler.namePhase(packageEnv, program)
        (newPEnv, Seq(namedTree)) = x1
        x2 <- compiler.typePhase(moduleEnv, namedTree)
        (newMEnv, typedTree) = x2
      } yield (newPEnv, newMEnv, typedTree)
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
        case ojaml.TypedAST.Module(_, _, Seq(ojaml.TypedAST.TLet(n, t, _))) =>
          (n, t)
        case unk => throw new AssertionError(s"TLet expected: $unk")
      }
    val isUnit = tpe == ojaml.Type.Unit
    if (isUnit) Result.Empty
    else {
      val fieldName = compiler.assembler.escape(name.value)
      val field = klass.getField(fieldName)
      val value = field.get(null)
      Result.Value(name.value, value, tpe.toString)
    }
  }

  private[this] def evalRuntime(in: Input, tree: TT.Module): Either[Result, Result] = {
    evalClass(tree.moduleRef.fullName).map { klass =>
      in match {
        case Input.Data(_, _, _) => Result.Empty
        case Input.Let(name, _) => readValue(klass, tree)
        case Input.Expr(name, _) => readValue(klass, tree)
      }
    }
  }

  def eval(code: String): Result = {
    parse(code).flatMap { in =>
      val (statement, names) = in match {
        case Input.Data(name, cnames, tree) => (tree, name +: cnames)
        case Input.Expr(name, tree) => (RT.TLet(mkName(name), tree), Seq(name))
        case Input.Let(name, tree) => (tree, Seq(name))
      }
      compile(statement).flatMap {
        case (newPEnv, newMEnv, tree) =>
          compiler.assemble(tree)
          this.packageEnv = newPEnv
          this.moduleEnv = newMEnv
          this.imports = this.imports ++ names.map { name =>
            ojaml.Import(mkQName(s"${tree.moduleRef.fullName}.$name"))
          }
          nextIndex += 1
          evalRuntime(in, tree)
      }
    }.merge
  }

  def setDebugPrint(x: Boolean): Unit = {
    compiler = newCompiler(x)
  }
}

object Repl {
  def main(args: Array[String]): Unit = {
    val repl = new Repl()

    println("OJaml REPL")
    println(s"tmp dir = ${repl.tmpDir}")

    repl.evalPredef()
    loop(repl)
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
    case class Data(name: String, ctors: Seq[String], tree: ojaml.RawAST.Data) extends Input
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
