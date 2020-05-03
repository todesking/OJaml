package com.todesking.ojaml.ml0.compiler.scala

import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files

import util.Syntax._

// Facade of compile phases
class Compiler(debugPrint: Boolean = false) {
  import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => RT, TypedAST => TT }
  import Compiler.Env
  import Compiler.TypeEnv

  val namer = new Namer()

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
    namer.appProgram(tree, penv).map {
      case (pe, namedTrees) =>
        if (debugPrint) {
          println("Phase: Namer")
          namedTrees.foreach { nt =>
            println(NamedAST.pretty(nt))
          }
          println(pe.pretty)
        }
        (pe, namedTrees)
    }
  }

  def typePhase(env: TypeEnv, tree: NamedAST.Module): Result[(TypeEnv, TypedAST.Module)] = {
    val typer = new Typer(env.classpath, env.types)
    typer.appModule(tree).map { typed =>
      val newEnv = env.addTypes(Typer.moduleVarsOf(typed))
      if (debugPrint) {
        println("Phase: Typer")
        println(TypedAST.pretty(typed))
        println("Module members")
        newEnv.types.toSeq
          .map { case (k, v) => s"$k" -> v }
          .sortBy(_._1)
          .foreach {
            case (k, v) =>
              println(s"- $k: $v")
          }
      }
      (newEnv, typed)
    }
  }

  def javalizePhase(module: TypedAST.Module): Seq[JAST.ClassDef] = {
    val javalizer = new Javalizer
    val klasses = javalizer.apply(module)
    if (debugPrint) {
      println("Phase: Javalizer")
      klasses.foreach { k =>
        println(JAST.pretty(k))
      }
    }
    klasses
  }

  def typeContents(env: Env, files: Seq[FileContent]): Result[(Env, Seq[TypedAST.Module])] = {
    files.mapWithContextEC(env) {
      case (env, file) =>
        typeContent(env, file)
    }
      .map { case (env, treess) => (env, treess.flatten) }
  }

  def typeContent(env: Env, file: FileContent): Result[(Env, Seq[TypedAST.Module])] = {
    for {
      rawTree <- parsePhase(file)
      x1 <- namePhase(env.nameEnv, rawTree)
      (nenv, namedTrees) = x1
      x2 <- namedTrees.mapWithContextEC(env.typeEnv) { (tenv, tree) =>
        typePhase(tenv, tree)
      }
      (tenv, typed) = x2
    } yield (env.copy(nameEnv = nenv, typeEnv = tenv), typed)
  }

  def compile(env: Env, files: Seq[FileContent]): Result[(Env, Seq[JAST.ClassDef])] =
    typeContents(env, files)
      .map { case (env, trees) => (env, trees.flatMap(javalizePhase(_))) }

}

object Compiler {
  case class TypeEnv(classpath: Classpath, types: Map[VarRef.ModuleMember, Type]) {
    def addTypes(xs: Map[VarRef.ModuleMember, Type]): TypeEnv =
      copy(types = types ++ xs)
  }

  case class Env(classpath: Classpath, nameEnv: PackageEnv, typeEnv: TypeEnv)

  def newEnv(cl: ClassLoader) = {
    val cp = new Classpath(cl)
    Env(cp, new PackageEnv(cp), TypeEnv(cp, Map()))
  }
}

