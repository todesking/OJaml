package com.todesking.ojaml.ml0

import com.todesking.ojaml.ml0.compiler.{ scala => ojaml }

import ojaml.util.Syntax._

class Repl {
  import Repl.Result

  val replFileName = "<REPL>"
  lazy val tmpDir = java.nio.file.Files.createTempDirectory("ojaml-repl")
  lazy val targetClassLoader = new java.net.URLClassLoader(Array(tmpDir.toUri.toURL), this.getClass.getClassLoader)

  private[this] def newCompiler(debugPrint: Boolean) =
    new ojaml.Compiler(tmpDir, getClass.getClassLoader, debugPrint)

  private[this] var compiler = newCompiler(false)
  private[this] var packageEnv = ojaml.PackageEnv(compiler.classRepo)
  private[this] var moduleEnv = Map.empty[ojaml.VarRef.ModuleMember, ojaml.Type]

  private[this] var imports: Seq[ojaml.Import] = Seq()
  private[this] def nextIndex = imports.size

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

  def eval(code: String): Result = {
    val parser = new ojaml.Parser(replFileName)
    parser.parseTerm(code) match {
      case parser.NoSuccess(msg, _) =>
        Result.CompileError(msg)
      case parser.Success(rawTree, _) =>
        import ojaml.{ RawAST => RT }
        val (isExpr, bindingName, letTree) = rawTree match {
          case RT.TLet(name, expr) => (false, name.value, rawTree)
          case expr: RT.Expr =>
            val name = s"res$nextIndex"
            (true, name, RT.TLet(mkName(name), expr))
        }
        val program = RT.Program(
          mkQName("ojaml.repl"),
          imports,
          Seq(
            RT.Module(
              mkName(s"Repl_$nextIndex"),
              Seq(letTree))))
        val compileResult =
          for {
            x1 <- compiler.namePhase(packageEnv, program)
            (newPEnv, Seq(namedTree)) = x1
            x2 <- compiler.typePhase(moduleEnv, namedTree)
            (newMEnv, typedTree) = x2
          } yield (newPEnv, newMEnv, typedTree)

        compileResult.fold({ errors =>
          Result.CompileError(errors.mkString(", "))
        }, {
          case (newPEnv, newMEnv, tree) =>
            compiler.assemble(tree)
            this.packageEnv = newPEnv
            this.moduleEnv = newMEnv
            this.imports :+= ojaml.Import(mkQName(s"${tree.moduleRef.fullName}.$bindingName"))
            val tpe =
              tree match {
                case ojaml.TypedAST.Module(_, _, Seq(ojaml.TypedAST.TLet(_, t, _))) =>
                  t
                case unk => throw new AssertionError(s"TLet expected: $unk")
              }
            val isUnit = tpe == ojaml.Type.Unit
            try {
              val klass = targetClassLoader.loadClass(tree.moduleRef.fullName)
              if (isExpr && !isUnit) {
                val fieldName = compiler.assembler.escape(bindingName)
                val field = klass.getField(fieldName)
                val value = field.get(null)
                Result.Value(bindingName, value, tpe.toString)
              } else {
                Result.Empty
              }
            } catch {
              case e: Throwable =>
                Result.RuntimeError(e)
            }
        })
    }
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

    loop(repl)
  }

  sealed abstract class Result
  object Result {
    case object Empty extends Result
    case class Value(bindingName: String, value: Any, tpe: String) extends Result
    case class CompileError(msg: String) extends Result
    case class RuntimeError(exception: Throwable) extends Result
  }

  @scala.annotation.tailrec
  def loop(repl: Repl): Unit = {
    val prompt = "ojaml> "
    val line = scala.io.StdIn.readLine(prompt)
    line match {
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
