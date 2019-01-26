package com.todesking.ojaml.ml0.compiler.scala

import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files

import Compiler.{ Result, Error }
import Util.SeqSyntax

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
    val penv = PackageEnv(classRepo)
    files.mapWithContextE((penv, Map.empty[VarRef.ModuleMember, Type])) {
      case ((pe, me), f) =>
        typing(pe, me, f).map { case (p, m, t) => ((p, m), t) }
    }
      .map(_.flatten)
      .map { trees =>
        trees.foreach(assembler.emit(_))
        Seq.empty[Error]
      }.merge
  }

  type ModuleEnv = Map[VarRef.ModuleMember, Type]

  def typing(penv: PackageEnv, moduleVars: ModuleEnv, file: FileContent): Result[(PackageEnv, ModuleEnv, Seq[TypedAST.Struct])] = {
    val parser = new Parser(file.path.toString)
    parser.parse(file.content) match {
      case parser.NoSuccess(msg, next) =>
        val line = next.pos.line
        val col = next.pos.column
        Left(Seq(Error(Pos(file.path.toString, line, col), s"Parse error: $msg\n${next.pos.longString}")))
      case parser.Success(ast, _) =>
        val namer = new Namer(penv)
        namer.appProgram(ast).flatMap {
          case (pe, namedTrees) =>
            if (debugPrint) {
              println("Phase: Namer")
              namedTrees.foreach { nt =>
                println(AST.pretty(nt))
              }
              println(pe.pretty)
            }
            namedTrees.foldLeftE(moduleVars) { (mvs, nt) =>
              val typer = new Typer(classRepo, mvs)
              typer.appStruct(nt).map { typed =>
                val newMvs = mvs ++ Typer.moduleVarsOf(typed)
                if (debugPrint) {
                  println("Phase: Typer")
                  println(AST.pretty(typed))
                  println("Module members")
                  newMvs.toSeq
                    .map { case (k, v) => s"${k.module.fullName}.${k.name}" -> v }
                    .sortBy(_._1)
                    .foreach {
                      case (k, v) =>
                        println(s"- $k: $v")
                    }
                }
                (newMvs, typed)
              }
            }.map { case (mvs, typed) => (pe, mvs, typed) }
        }.map {
          case (pe, mvs, typed) =>
            (pe, mvs, typed)
        }
    }
  }
}

object Compiler {
  case class Error(pos: Pos, message: String)
  type Result[A] = Either[Seq[Error], A]
}

