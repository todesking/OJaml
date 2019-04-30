package com.todesking.ojaml.ml0.compiler.scala

import Result.Error
import Result.error

import com.todesking.ojaml.ml0.compiler.scala.{ NamedAST => NT, TypedAST => TT }

import util.Syntax._

class Typer(classRepo: ClassRepo, moduleVars: Map[VarRef.ModuleMember, Type]) {
  import Typer.Ctx
  import Typer.Subst

  private[this] var nextVarId = 1
  private[this] def freshVar() = {
    val v = Type.Var(nextVarId)
    nextVarId += 1
    v
  }

  def appModule(s: NT.Module): Result[TT.Module] = {
    val env = Typer.Ctx(s.moduleRef, classRepo).bindModuleValues(moduleVars)
    s.body
      .mapWithContext(env) {
        case (e, t) =>
          appTerm(e, t).fold({ l =>
            (e, Left(l))
          }, { case (ee, tt) => (ee, Right(tt)) })

      }
      .validated
      .map(TT.Module(s.pkg, s.name, _))
  }

  def appTerm(ctx: Ctx, t: NT.Term): Result[(Ctx, TT.Term)] = t match {
    case NT.TLet(name, expr) =>
      for {
        e <- appExpr(ctx, expr).map(_._2)
        c <- ctx.bindModuleValue(name, e.tpe)
      } yield {
        (c, TT.TLet(name, e.tpe, e))
      }
    case e: NT.Expr =>
      appExpr(ctx, e).map { case (_, te) => (ctx, te) }
  }

  private[this] def ok(s: Subst, e: TT.Expr): Result[(Subst, TT.Expr)] =
    Right((s, e))
  private[this] def ok0(e: TT.Expr): Result[(Subst, TT.Expr)] =
    ok(Subst.empty, e)

  def appExpr(ctx: Ctx, expr: NT.Expr): Result[(Subst, TT.Expr)] = expr match {
    case NT.Ref(ref) => ref match {
      case ref @ VarRef.ModuleMember(m, name) =>
        ok0(TT.ModuleVarRef(m, name, ctx.typeOf(ref)))
      case ref @ VarRef.Local(depth, index) =>
        ok0(TT.LocalRef(depth, index, ctx.typeOf(ref)))
    }
    case NT.LitInt(v) => ok0(TT.LitInt(v))
    case NT.LitBool(v) => ok0(TT.LitBool(v))
    case NT.LitString(v) => ok0(TT.LitString(v))
    case NT.If(cond, th, el) =>
      for {
        x0 <- appExpr(ctx, cond)
        (s0, e0) = x0
        x1 <- appExpr(ctx, th)
        (s1, e1) = x1
        x2 <- appExpr(ctx, el)
        (s2, e2) = x2
        s <- (s0 ++ s1 ++ s2 + (e0.tpe -> Type.Bool) + (e1.tpe -> e2.tpe)).unify(expr.pos)
      } yield (s, TT.If(e0, e1, e2, s.app(e1.tpe)))
    case NT.Fun(param, tpe, body) =>
      val t = tpe getOrElse freshVar()
      appExpr(ctx.bindLocal(param, t), body)
        .map { case (s, b) => (s, TT.Fun(s.app(t), b)) }
    case NT.ELet(ref, value, body) =>
      for {
        x0 <- appExpr(ctx, value)
        (s0, e0) = x0
        x1 <- appExpr(ctx.bindLocal(ref, e0.tpe), body)
        (s1, e1) = x1
        s <- (s0 ++ s1).unify(expr.pos)
      } yield (s, TT.App(TT.Fun(e0.tpe, e1), e0, e1.tpe))
    case NT.ELetRec(bindings, body) =>
      val bs = bindings.map {
        case (ref, tpe, fun) =>
          (ref, tpe getOrElse freshVar(), fun)
      }
      val c = bs.foldLeft(ctx) {
        case (c, (ref, tpe, fun)) =>
          c.bindLocal(ref, tpe)
      }

      val typed =
        bs.map {
          case (ref, tpe, fun) =>
            appExpr(c, fun).map {
              case (subst, fun: TT.Fun) => (subst, fun)
              case (subst, fun) => throw new AssertionError(s"$fun")
            }
        }.validated

      for {
        ts <- typed
        x <- appExpr(c, body)
        (sb, b) = x
        s <- (ts.map(_._1) :+ sb).reduce(_ ++ _).unify(expr.pos)
      } yield (s, TT.LetRec(ts.map(_._2), b))
    case NT.App(f, x) =>
      val a1 = freshVar()
      val a2 = freshVar()
      for {
        xf <- appExpr(ctx, f)
        (sf, tf) = xf
        xx <- appExpr(ctx, x)
        (sx, tx) = xx
        s <- (sf ++ sx + (Type.Fun(a1, a2) -> tf.tpe) + (a1 -> tx.tpe)).unify(expr.pos)
      } yield (s, TT.App(tf, tx, s.app(a2)))
    case NT.JCallStatic(klassRef, name, args) =>
      for {
        klass <- ctx.findClass(klassRef).toResult(expr.pos, s"Class not found: $klassRef")
        targs <- args.map(appExpr(ctx, _)).validated
        s <- (Subst.empty +: targs.map(_._1)).reduce(_ ++ _).unify(expr.pos)
        method <- klass.findStaticMethod(name.value, targs.map(_._2.tpe))
          .toResult(name.pos, s"Can't resolve static method ${Type.prettyMethod(name.value, targs.map(_._2.tpe))} in class ${klass.ref.fullName}")
      } yield (s, TT.JCallStatic(method, targs.map(_._2)))
    case NT.JCallInstance(expr, name, args) =>
      for {
        targs <- args.map(appExpr(ctx, _)).validated
        x <- appExpr(ctx, expr)
        (s1, e1) = x
        klass <- ctx.findClass(e1.tpe)
          .toResult(expr.pos, s"Class ${e1.tpe} not found. Check classpath.")
        argTypes = targs.map(_._2.tpe)
        klassName = e1.tpe.boxed.ref.fullName
        method <- klass.findInstanceMethod(name.value, targs.map(_._2.tpe))
          .toResult(name.pos, s"Can't resolve instance method ${Type.prettyMethod(name.value, argTypes)} in class $klassName")
      } yield (s1, TT.JCallInstance(method, e1, targs.map(_._2)))
  }
}
object Typer {
  def moduleVarsOf(s: TypedAST.Module): Map[VarRef.ModuleMember, Type] = s.body.collect {
    case TypedAST.TLet(name, tpe, _) =>
      VarRef.ModuleMember(s.moduleRef, name.value) -> tpe
  }.toMap

  class Subst(val items: List[(Type, Type)]) {
    def app(t: Type): Type = app0(t, items)

    private[this] def app0(t: Type, items: List[(Type, Type)]) =
      items.foldLeft(t) {
        case (t0, (a: Type.Var, t)) =>
          t0.substitute(a, t)
        case (t0, (t, a: Type.Var)) =>
          t0.substitute(a, t)
        case (t, (l, r)) =>
          throw new AssertionError(s"$t, $l -> $r")
      }

    def +(rhs: (Type, Type)): Subst = rhs match {
      case (from, to) => new Subst((from -> to) :: items)
    }

    def ++(rhs: Subst): Subst =
      new Subst(items ++ rhs.items)

    def dot(a: Type.Var, t: Type): Subst =
      new Subst((a -> app(t)) :: items)

    def unify(pos: Pos): Result[Subst] = unify0(items, pos)
    private[this] def unify0(items: List[(Type, Type)], pos: Pos): Result[Subst] =
      items match {
        case Nil => Right(Subst.empty)
        case (l, r) :: xs if l == r =>
          unify0(xs, pos)
        case (a: Type.Var, t) :: xs =>
          if (t.freeTypeVariables.contains(a)) {
            Result.error(pos, s"Unify failed: $a and $t")
          } else {
            unify0(xs.map { case (l, r) => l.substitute(a, t) -> r.substitute(a, t) }, pos)
              .map { s => s.dot(a, t) }
          }
        case (t, a: Type.Var) :: xs =>
          unify0((a -> t) :: xs, pos)
        case (Type.Fun(l1, r1), Type.Fun(l2, r2)) :: xs =>
          unify0((l1 -> l2) :: (r1 -> r2) :: xs, pos)
        case (l, r) :: xs =>
          Result.error(pos, s"Unify failed: $l and $r")
      }
  }
  object Subst {
    def apply(items: (Type, Type)*) = new Subst(items.toList)
    val empty = new Subst(Nil)
  }

  case class Ctx(
    currentModule: ModuleRef,
    repo: ClassRepo,
    gamma: Map[VarRef, Type] = Map()) {

    def typeOf(ref: VarRef): Type =
      gamma(ref) // If lookup failed, that means a bug.

    def findClass(tpe: Type): Option[ClassSig] = tpe match {
      case v: Type.Var => None
      case t => repo.find(t.boxed.ref)
    }
    def findClass(ref: ClassRef): Option[ClassSig] =
      repo.find(ref)

    def bindModuleValue(name: Name, tpe: Type): Result[Ctx] = {
      val ref = VarRef.ModuleMember(currentModule, name.value)
      if (gamma.contains(ref)) error(name.pos, s"Member $name is already defined")
      else Right(copy(gamma = gamma + (ref -> tpe)))
    }

    def bindModuleValues(mvs: Map[VarRef.ModuleMember, Type]): Ctx =
      copy(gamma = gamma ++ mvs)

    def bindLocal(ref: VarRef.Local, tpe: Type): Ctx =
      copy(gamma = gamma + (ref -> tpe))
  }

}
