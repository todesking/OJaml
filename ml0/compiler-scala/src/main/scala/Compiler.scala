package com.todesking.ojaml.ml0.compiler.scala

import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files

import util.Syntax._

// Facade of compile phases
class Compiler(debugPrint: Boolean = false) {
  import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => RT, TypedAST => TT }
  import Compiler.Env

  val namer = new Namer()

  def parsePhase(file: Source): Result[RawAST.Program] = {
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

  def namePhase(penv: NameEnv, tree: RawAST.Program): Result[(NameEnv, Seq[NamedAST.Module])] = {
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
    val typer = new Typer
    typer.appModule(env, tree).map { typed =>
      val newEnv = env.addTypes(Typer.memberTypes(typed))
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

  def typeContents(env: Env, files: Seq[Source]): Result[(Env, Seq[TypedAST.Module])] = {
    files.mapWithContextEC(env) {
      case (env, file) =>
        typeContent(env, file)
    }
      .map { case (env, treess) => (env, treess.flatten) }
  }

  def typeContent(env: Env, file: Source): Result[(Env, Seq[TypedAST.Module])] = {
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

  def compile(env: Env, files: Seq[Source]): Result[(Env, Seq[JAST.ClassDef])] =
    typeContents(env, files)
      .map { case (env, trees) => (env, trees.flatMap(javalizePhase(_))) }

}

object Compiler {
  case class Env(classpath: Classpath, nameEnv: NameEnv, typeEnv: TypeEnv)

  def newEnv(cl: ClassLoader) = {
    val cp = new Classpath(cl)
    val primitivesModule = PackageRef.Root.packageRef("ojaml").moduleRef("Primitives")
    val nameEnv = NameEnv.defaultEnv(cp)
    Env(cp, nameEnv, TypeEnv(cp, Map()))
  }
}

