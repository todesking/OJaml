package com.todesking.ojaml.ml0.compiler.scala

import Result.Error
import Result.error

import com.todesking.ojaml.ml0.compiler.scala.{ NamedAST => NT, TypedAST => TT }

import util.Syntax._

class Typer(classRepo: ClassRepo, moduleVars: Map[VarRef.ModuleMember, Type]) {
  import Typer.Ctx

  def appModule(s: NT.Module): Result[TT.Module] = {
    val init = Typer.Ctx(s.moduleRef, classRepo).bindModuleValues(moduleVars)
    val (_, typed) =
      s.body.foldLeft((init, Seq.empty[Result[TT.Term]])) {
        case ((c, a), t) =>
          appTerm(c, t).fold({ l =>
            (c, a :+ Left(l))
          }, {
            case (cc, tt) =>
              (cc, a :+ Right(tt))
          })
      }
    validate(typed).map(TT.Module(s.pkg, s.name, _))
  }

  private[this] def validate[A](xs: Seq[Result[A]]): Result[Seq[A]] = {
    val rights = xs.collect { case Right(x) => x }
    if (rights.size == xs.size) Right(rights)
    else Left(xs.collect { case Left(x) => x }.flatten)
  }

  def appTerm(ctx: Ctx, t: NT.Term): Result[(Ctx, TT.Term)] = t match {
    case NT.TLet(name, expr) =>
      for {
        e <- appExpr(ctx, expr)
        c <- ctx.bindModuleValue(name, e.tpe)
      } yield {
        (c, TT.TLet(name, e.tpe, e))
      }
    case e: NT.Expr =>
      appExpr(ctx, e).map { te => (ctx, te) }
  }

  def appExpr(ctx: Ctx, expr: NT.Expr): Result[TT.Expr] = expr match {
    case NT.Ref(ref) => ref match {
      case ref @ VarRef.ModuleMember(m, name) =>
        Right(TT.ModuleVarRef(m, name, ctx.typeOf(ref)))
      case ref @ VarRef.Local(depth, index) =>
        Right(TT.LocalRef(depth, index, ctx.typeOf(ref)))
    }
    case NT.LitInt(v) => Right(TT.LitInt(v))
    case NT.LitBool(v) => Right(TT.LitBool(v))
    case NT.LitString(v) => Right(TT.LitString(v))
    case NT.If(cond, th, el) =>
      for {
        e0 <- appExpr(ctx, cond)
        e1 <- appExpr(ctx, th)
        e2 <- appExpr(ctx, el)
        ret <- if (e0.tpe != Type.Bool) {
          error(cond.pos, s"Condition must be bool")
        } else if (e1.tpe != e2.tpe) {
          error(expr.pos, s"Then clause has thpe ${e1.tpe} but else clause has type ${e2.tpe}")
        } else {
          Right(TT.If(e0, e1, e2, e1.tpe))
        }
      } yield ret
    case NT.Fun(param, tpe, body) =>
      appExpr(ctx.bindLocal(param, tpe), body)
        .map(TT.Fun(tpe, _))
    case NT.ELet(ref, value, body) =>
      appExpr(ctx, value).flatMap { v =>
        val fun = NT.Fun(ref, v.tpe, body)
        fun.fillPos(expr.pos)
        appExpr(ctx, fun).map {
          case f @ TT.Fun(argType, b) =>
            assert(argType == v.tpe)
            TT.App(f, v, f.tpe.r)
          case unk =>
            throw new AssertionError(s"$unk")
        }
      }
    case NT.ELetRec(bindings, body) =>
      val c = bindings.foldLeft(ctx) {
        case (c, (r, t, e)) =>
          c.bindLocal(r, t)
      }

      validate(bindings.map {
        case (ref, tpe, e) =>
          appExpr(c, e).flatMap {
            case te: TT.Fun =>
              if (te.tpe != tpe) error(e.pos, s"Expression type ${te.tpe} is not compatible to $tpe")
              else Right(te)
            case _ =>
              throw new AssertionError()
          }
      }).flatMap { bs =>
        appExpr(c, body).map { tb =>
          TT.LetRec(bs, tb)
        }
      }
    case NT.App(f, x) =>
      for {
        tf <- appExpr(ctx, f)
        tx <- appExpr(ctx, x)
        ret <- tf.tpe match {
          case Type.Fun(l, r) =>
            if (l == tx.tpe)
              Right(TT.App(tf, tx, r))
            else
              error(x.pos, s"Argument type mismatch: $l required but ${tx.tpe} found")
          case t =>
            error(f.pos, s"Applying non-function type $t")
        }
      } yield ret
    case NT.JCallStatic(klassRef, name, args) =>
      validate(args.map(appExpr(ctx, _))).flatMap { targs =>
        val klass = ctx.findClass(klassRef).get // TODO
        klass.findStaticMethod(name.value, targs.map(_.tpe))
          .toResult(name.pos, s"Static method ${Type.prettyMethod(name.value, targs.map(_.tpe))} is not found in class ${klass.ref.fullName}")
          .map(TT.JCallStatic(_, targs))
      }
    case NT.JCallInstance(expr, name, args) =>
      for {
        targs <- validate(args.map(appExpr(ctx, _)))
        receiver <- appExpr(ctx, expr)
        klass <- ctx.findClass(receiver.tpe.boxed.ref)
          .toResult(expr.pos, s"Class ${receiver.tpe.boxed.ref.fullName} not found. Check classpath.")
        method <- klass.findInstanceMethod(name.value, targs.map(_.tpe))
          .toResult(name.pos, s"Instance method ${Type.prettyMethod(name.value, targs.map(_.tpe))} is not found in class ${receiver.tpe.boxed.ref.fullName}")
      } yield TT.JCallInstance(method, receiver, targs)
  }
}
object Typer {
  def moduleVarsOf(s: TypedAST.Module): Map[VarRef.ModuleMember, Type] = s.body.collect {
    case TypedAST.TLet(name, tpe, _) =>
      VarRef.ModuleMember(s.moduleRef, name.value) -> tpe
  }.toMap

  case class Ctx(
    currentModule: ModuleRef,
    repo: ClassRepo,
    typeTable: Map[VarRef, Type] = Map()) {

    def typeOf(ref: VarRef): Type =
      typeTable(ref) // If lookup failed, that means a bug.

    def findClass(ref: ClassRef): Option[ClassSig] = repo.find(ref)

    def bindModuleValue(name: Name, tpe: Type): Result[Ctx] = {
      val ref = VarRef.ModuleMember(currentModule, name.value)
      if (typeTable.contains(ref)) error(name.pos, s"Member $name is already defined")
      else Right(copy(typeTable = typeTable + (ref -> tpe)))
    }

    def bindModuleValues(mvs: Map[VarRef.ModuleMember, Type]): Ctx =
      copy(typeTable = typeTable ++ mvs)

    def bindLocal(ref: VarRef.Local, tpe: Type): Ctx =
      copy(typeTable = typeTable + (ref -> tpe))
  }

}
