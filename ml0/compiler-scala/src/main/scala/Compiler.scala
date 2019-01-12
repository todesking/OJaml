package com.todesking.ojaml.ml0.compiler.scala

import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files

import Compiler.{ Result, Error }

class Compiler(baseDir: Path, cl: ClassLoader) {
  import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => RT, TypedAST => TT }

  lazy val typer = new Typer(new ClassRepo(cl))
  lazy val assembler = new Assembler(baseDir)

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
        typer.typeProgram(ast).fold(l => l, { r => assembler.emit(r); Seq() })
    }
  }
}

object Compiler {
  case class Error(pos: Pos, message: String)
  case class Result(errors: Seq[Error]) {
    def isSuccess = errors.isEmpty
  }
}

