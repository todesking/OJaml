package com.todesking.ojaml.ml0.compiler.scala

import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files

import util.Syntax._

// Facade of compile phases
class Compiler(baseDir: Path, cl: ClassLoader, debugPrint: Boolean = false) {
  import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => RT, TypedAST => TT }
  import Compiler.ModuleEnv

  val classRepo = new ClassRepo(cl)
  val assembler = new Assembler(baseDir)

  def compileFiles(files: Seq[Path]): Seq[Result.Message] =
    compileContents(files.map(FileContent.read))

  def compileContents(files: Seq[FileContent]): Seq[Result.Message] = {
    typeContents(files)
      .map {
        case (_, trees) =>
          trees.foreach(assembler.emit)
          Seq.empty[Result.Message]
      }.merge
  }

  def assemble(tree: TypedAST.Module) =
    assembler.emit(tree)

  def parsePhase(file: FileContent): Result[RawAST.Program] = {
    val parser = new Parser(file.path.toString)
    val result = parser.parse(file.content)
    if (debugPrint) {
      result.foreach { ast =>
        println("Phase: Parer")
        println(RawAST.pretty(ast))
      }
    }
    result
  }

  def namePhase(penv: PackageEnv, tree: RawAST.Program): Result[(PackageEnv, Seq[NamedAST.Module])] = {
    val namer = new Namer(penv)
    namer.appProgram(tree).map {
      case (pe, namedTrees) =>
        if (debugPrint) {
          println("Phase: Namer")
          namedTrees.foreach { nt =>
            println(AST.pretty(nt))
          }
          println(pe.pretty)
        }
        (pe, namedTrees)
    }
  }

  def typePhase(moduleVars: ModuleEnv, tree: NamedAST.Module): Result[(ModuleEnv, TypedAST.Module)] = {
    val typer = new Typer(classRepo, moduleVars)
    typer.appModule(tree).map { typed =>
      val newMvs = moduleVars ++ Typer.moduleVarsOf(typed)
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
  }

  def typeContents(files: Seq[FileContent]): Result[(Map[VarRef.ModuleMember, Type], Seq[TypedAST.Module])] = {
    val penv = PackageEnv(classRepo)
    files.mapWithContextEC((penv, Map.empty[VarRef.ModuleMember, Type])) {
      case ((pe, me), f) =>
        typeContent(pe, me, f).map { case (p, m, t) => ((p, m), t) }
    }
      .map { case ((p, c), treess) => (c, treess.flatten) }
  }

  def typeContent(penv: PackageEnv, moduleVars: ModuleEnv, file: FileContent): Result[(PackageEnv, ModuleEnv, Seq[TypedAST.Module])] = {
    for {
      rawTree <- parsePhase(file)
      x1 <- namePhase(penv, rawTree)
      (pe, namedTrees) = x1
      x2 <- namedTrees.mapWithContextEC(moduleVars) { (mvs, nt) =>
        typePhase(mvs, nt)
      }
      (mvs, typed) = x2
    } yield (pe, mvs, typed)
  }
}

object Compiler {
  type ModuleEnv = Map[VarRef.ModuleMember, Type]
}

