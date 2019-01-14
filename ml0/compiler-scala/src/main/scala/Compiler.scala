package com.todesking.ojaml.ml0.compiler.scala

import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files

import Compiler.{ Result, Error }
import Util.MapE

class Compiler(baseDir: Path, cl: ClassLoader, debugPrint: Boolean = false) {
  import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => RT, TypedAST => TT }

  val classRepo = new ClassRepo(cl)

  def compile(files: Seq[Path]): Result =
    compileContents(files.map(FileContent.read))

  def compileContents(files: Seq[FileContent]): Result = {
    files.foreach { f =>
      val es = compile1(f)
      if (es.nonEmpty) return Result(es)
    }
    Result(Seq())
  }

  def compile1(file: FileContent): Seq[Error] = {
    val parser = new Parser(file.path.toString)
    parser.parse(file.content) match {
      case parser.NoSuccess(msg, next) =>
        val line = next.pos.line
        val col = next.pos.column
        Seq(Error(Pos(file.path.toString, line, col), s"Parse error: $msg\n${next.pos.longString}"))
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
              val assembler = new Assembler(baseDir)
              assembler.emit(typed)
              Seq.empty[Error]
            }
          }.map(_.flatten)
        }.fold(l => l, r => r)
    }
  }
}

object Compiler {
  case class Error(pos: Pos, message: String)
  case class Result(errors: Seq[Error]) {
    def isSuccess = errors.isEmpty
  }
}

