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

  def compile(files: Seq[Path]): Seq[Error] =
    compileContents(files.map(FileContent.read))

  def extractMV(t: TypedAST.Struct): Map[VarRef.ModuleMember, Type] = t.body.flatMap {
    case TypedAST.TLet(name, tpe, _) =>
      Seq(VarRef.ModuleMember(t.moduleRef, name.value) -> tpe)
    case other =>
      Seq()
  }.toMap

  def compileContents(files: Seq[FileContent]): Seq[Error] = {
    files.mapWithContextE(Map.empty[VarRef.ModuleMember, Type])(typing)
      .map(_.flatten)
      .map { trees =>
        trees.foreach(assembler.emit(_))
        Seq.empty[Error]
      }.merge
  }

  type ModuleEnv = Map[VarRef.ModuleMember, Type]

  def typing(penv: PackageEnv, moduleVars: ModuleEnv, file: FileContent): Result[(ModuleEnv, Seq[TypedAST.Struct])] = {
    val parser = new Parser(file.path.toString)
    parser.parse(file.content) match {
      case parser.NoSuccess(msg, next) =>
        val line = next.pos.line
        val col = next.pos.column
        Left(Seq(Error(Pos(file.path.toString, line, col), s"Parse error: $msg\n${next.pos.longString}")))
      case parser.Success(ast, _) =>
        val namer = new Namer(classRepo, moduleVars.keys.map { mv => mv.module -> })
        namer.appProgram(ast).flatMap { namedTrees =>
          if (debugPrint) {
            println("Phase: Namer")
            namedTrees.foreach { nt =>
              println(AST.pretty(nt))
            }
          }
          namedTrees.mapWithContextE(moduleVars) { (mvs, nt) =>
            val typer = new Typer(classRepo, mvs)
            typer.appStruct(nt).map { typed =>
              if (debugPrint) {
                println("Phase: Typer")
                println(AST.pretty(typed))
              }
              (mvs, typed)
            }
          }
        }.map { trees =>
          val mvs = moduleVars ++ trees.flatMap(extractMV)
          (mvs, trees)
        }
    }
  }
}

object Compiler {
  case class Error(pos: Pos, message: String)
  type Result[A] = Either[Seq[Error], A]
}

