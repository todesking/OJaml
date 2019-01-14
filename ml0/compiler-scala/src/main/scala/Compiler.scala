package com.todesking.ojaml.ml0.compiler.scala

import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files

import Compiler.{ Result, Error }
import Util.MapWithContext
import Util.MapE

class Compiler(baseDir: Path, cl: ClassLoader, debugPrint: Boolean = false) {
  import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => RT, TypedAST => TT }

  val classRepo = new ClassRepo(cl)
  val assembler = new Assembler(baseDir)

  def compile(files: Seq[Path]): Result =
    compileContents(files.map(FileContent.read))

  def moduleVars(t: TypedAST.Struct): Map[VarRef.Module, Type] = t.body.flatMap {
    case TypedAST.TLet(name, tpe, _) =>
      Seq(VarRef.Module(t.moduleRef, name.value) -> tpe)
    case other =>
      Seq()
  }.toMap

  def compileContents(files: Seq[FileContent]): Result = {
    files.mapWithContextE(Map.empty[VarRef.Module, Type])(typing)
      .map { trees =>
        trees.foreach(assembler.emit(_))
      }.fold({ l => Result(l) }, {r => Result(Seq()) })
  }

  def typing(moduleVars: Map[VarRef.Module, Type], file: FileContent): Either[Seq[Error], (Map[VarRef.Module, Type], TypedAST.Struct)] = {
    val parser = new Parser(file.path.toString)
    parser.parse(file.content) match {
      case parser.NoSuccess(msg, next) =>
        val line = next.pos.line
        val col = next.pos.column
        Left(Seq(Error(Pos(file.path.toString, line, col), s"Parse error: $msg\n${next.pos.longString}")))
      case parser.Success(ast, _) =>
        val namer = new Namer(classRepo)
        namer.appProgram(ast).flatMap { namedTrees =>
          if (debugPrint) {
            println("Phase: Namer")
            namedTrees.foreach { nt =>
              println(AST.pretty(nt))
            }
          }
          namedTrees.mapE { nt =>
            val typer = new Typer(classRepo)
            typer.appStruct(nt).map { typed =>
              if (debugPrint) {
                println("Phase: Typer")
                println(AST.pretty(typed))
              }
            }
          }
        }
    }
  }
}

object Compiler {
  case class Error(pos: Pos, message: String)
  case class Result(errors: Seq[Error]) {
    def isSuccess = errors.isEmpty
  }
}

