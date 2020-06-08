package com.todesking.ojaml.ml0.compiler.scala

import Result.error

import com.todesking.ojaml.ml0.compiler.scala.{ NamedAST => NT, TypedAST => TT }

import util.Syntax._

class Typer {
  import Typer.Ctx
  import Typer.Subst

  private[this] var nextVarId = 1
  private[this] def freshVar() = {
    val v = Type.Var(nextVarId)
    nextVarId += 1
    v
  }

  def appModule(env: TypeEnv, s: NT.Module): Result[TT.Module] = {
    val ctx = Typer.newCtx(s.moduleRef, env)
    s.body
      .mapWithContextEC(ctx) {
        case (e, t) =>
          appTerm(e, t)
      }
      .map(_._2.flatten)
      .map(TT.Module(s.pos, s.pkg, s.name, _))
  }

  private[this] def eval(ctx: Ctx, expr: NT.Expr, hint: Option[Type]): Result[TT.Expr] = {
    for {
      x <- appExpr(ctx, expr)
      (s, tree) = x
      s <- hint.fold(s) { t => s + (t -> tree.tpe) }.unify(expr.pos)
    } yield {
      def tabs(e: TT.Expr) = {
        val tvs = e.tpe.freeTypeVariables.toSeq.sortBy(_.id)
        if (tvs.isEmpty) e
        else TT.TAbs(expr.pos, tvs, e, Type.Abs(tvs, e.tpe))
      }
      subst(s, tree) match {
        case expr @ (TT.Fun(_, _, _, _) | TT.RefMember(_, _, _)) =>
          tabs(expr)
        case x =>
          // TODO: Is this really safe??
          tabs(x)
      }
    }
  }
  def appTerm(ctx: Ctx, t: NT.Term): Result[(Ctx, Seq[TT.Term])] = t match {
    case NT.TLet(pos, name, tpe, expr) =>
      for {
        e <- eval(ctx, expr, tpe)
        ctx <- ctx.bindMember(name, e.tpe)
      } yield {
        assertNoFVs(e, Set())
        val e2 = reindex(Subst.empty, 1, e)
        (ctx, Seq(TT.TLet(pos, name, e2.tpe, Some(e2))))
      }
    case NT.TLetRec(pos, bindings) =>
      for {
        x <- appLetRecBindings(ctx, pos, bindings) { (ctx, name, tpe) => ctx.setMember(name, tpe) }
        (s, bs) = x
        c = bs.foldLeft(ctx) {
          case (c, (name, tpe, expr)) =>
            c.setMember(name.value, tpe)
        }
      } yield {
        // TODO: bind members
        (c, Seq(TT.TLetRec(pos, bs)))
      }
    case NT.Data(pos, name, tvars, ctors) =>
      def wrap(t: Type) = if (tvars.isEmpty) t else Type.Abs(tvars, t)
      val data = Type.Data(ctx.currentModule, name.value, tvars)
      val ctorDefs = ctors.map {
        case (ctorName, ts) =>
          TT.DataCtor(
            ctorName.pos,
            ctorName.value,
            ts,
            s"${ctorName.value}$$check",
            ts.zipWithIndex.map { case (_, i) => s"${ctorName.value}$$get$i" })
      }
      val members = ctorDefs.flatMap {
        case TT.DataCtor(pos, ctorName, params, checkerName, extractorNames) =>
          Seq(
            TT.TLet(pos, Name(pos, ctorName), wrap(params.foldRight(data: Type)(Type.Fun.apply)), None),
            TT.TLet(pos, Name(pos, checkerName), wrap(Type.Fun(data, Type.Bool)), None)) ++ extractorNames.zip(params).map {
              case (extractorName, t) =>
                TT.TLet(pos, Name(pos, extractorName), wrap(Type.Fun(data, t)), None)
            }
      }
      for {
        ctx <- members.foldLeftE(ctx) {
          case (ctx, TT.TLet(pos, name, tpe, body)) =>
            ctx.bindMember(name, tpe)
        }
      } yield {
        (ctx, members :+ TT.Data(pos, name, tvars, ctorDefs))
      }
    case NT.TExpr(pos, e) =>
      eval(ctx, e, None).map { e => (ctx, Seq(TT.TExpr(pos, e))) }
  }

  private[this] def ok(s: Subst, e: TT.Expr): Result[(Subst, TT.Expr)] =
    Result.ok((s, e))
  private[this] def ok0(e: TT.Expr): Result[(Subst, TT.Expr)] =
    ok(Subst.empty, e)

  private[this] def subst(s: Subst, tree: TT.Expr): TT.Expr = tree match {
    case _: TT.Lit => tree
    case TT.RefMember(pos, member, t) => TT.RefMember(pos, member, s.app(t))
    case TT.RefLocal(pos, name, t) => TT.RefLocal(pos, name, s.app(t))
    case TT.ELetRec(pos, vs, b) => TT.ELetRec(
      pos,
      vs.map { case (name, tpe, e) => (name, s.app(tpe), subst(s, e)) }
        .map { case (name, tpe, f: TT.Fun) => (name, tpe, f) case _ => throw new AssertionError() },
      subst(s, b))
    case TT.If(pos, c, th, el, t) =>
      TT.If(pos, subst(s, c), subst(s, th), subst(s, el), s.app(t))
    case TT.App(pos, f, x, t) =>
      TT.App(pos, subst(s, f), subst(s, x), s.app(t))
    case TT.Fun(pos, p, b, t) =>
      TT.Fun(pos, p, subst(s, b), s.app(t))
    case TT.TAbs(pos, ps, e, t) =>
      TT.TAbs(pos, ps, subst(s -- ps, e), s.app(t).asInstanceOf[Type.Abs])
    case TT.JCallStatic(pos, m, a) =>
      TT.JCallStatic(pos, m, a.map(subst(s, _)))
    case TT.JCallInstance(pos, m, r, a) =>
      TT.JCallInstance(pos, m, subst(s, r), a.map(subst(s, _)))
    case TT.MatchError(pos, tpe) =>
      TT.MatchError(pos, s.app(tpe))
  }

  private[this] def reindex(s: Subst, nextIndex: Int, tree: TT.Expr): TT.Expr = tree match {
    case _: TT.Lit => tree
    case TT.RefMember(pos, member, t) => TT.RefMember(pos, member, s.app(t))
    case TT.RefLocal(pos, name, t) => TT.RefLocal(pos, name, s.app(t))
    case TT.ELetRec(pos, vs, b) => TT.ELetRec(
      pos,
      vs.map { case (name, tpe, e) => (name, s.app(tpe), reindex(s, nextIndex, e)) }
        .map { case (name, tpe, f: TT.Fun) => (name, tpe, f) case _ => throw new AssertionError() },
      reindex(s, nextIndex, b))
    case TT.If(pos, c, th, el, t) =>
      TT.If(pos, reindex(s, nextIndex, c), reindex(s, nextIndex, th), reindex(s, nextIndex, el), s.app(t))
    case TT.App(pos, f, x, t) =>
      TT.App(pos, reindex(s, nextIndex, f), reindex(s, nextIndex, x), s.app(t))
    case TT.Fun(pos, p, b, t) =>
      TT.Fun(pos, p, reindex(s, nextIndex, b), s.app(t))
    case TT.TAbs(pos, ps, e, t) =>
      val s2 = s -- ps ++ Subst(ps.sortBy(_.id).zipWithIndex.map { case (v, i) => (v, Type.Var(nextIndex + i)) }: _*)
      TT.TAbs(pos, ps, reindex(s2, nextIndex + ps.size, e), t match { case Type.Abs(xs, b) => Type.Abs(xs.map(s2.app(_).asInstanceOf[Type.Var]), s2.app(b)) })
    case TT.JCallStatic(pos, m, a) =>
      TT.JCallStatic(pos, m, a.map(reindex(s, nextIndex, _)))
    case TT.JCallInstance(pos, m, r, a) =>
      TT.JCallInstance(pos, m, reindex(s, nextIndex, r), a.map(reindex(s, nextIndex, _)))
    case TT.MatchError(pos, t) =>
      TT.MatchError(pos, s.app(t))
  }

  private[this] def assertNoFVs1(tree: TT.Expr, nonFrees: Set[Type.Var]) =
    assert(
      (tree.tpe.freeTypeVariables -- nonFrees).isEmpty,
      s"$tree: ${tree.tpe} has free type variables: ${tree.tpe.freeTypeVariables.mkString(", ")}")

  private[this] def assertNoFVs(tree: TT.Expr, nonFrees: Set[Type.Var]): Unit = {
    assertNoFVs1(tree, nonFrees)
    tree match {
      case _: TT.Lit =>
      case TT.RefMember(pos, m, t) =>
      case TT.RefLocal(pos, n, t) =>
      case TT.ELetRec(pos, vs, b) =>
        vs.foreach {
          case (name, tpe, e) =>
            tpe match {
              case Type.Abs(params, t) =>
                assertNoFVs(e, nonFrees ++ params)
              case _ =>
                assertNoFVs(e, nonFrees)
            }
        }
        assertNoFVs(b, nonFrees)
      case TT.If(pos, c, th, el, t) =>
        assertNoFVs(c, nonFrees)
        assertNoFVs(th, nonFrees)
        assertNoFVs(el, nonFrees)
      case TT.App(pos, f, x, t) =>
        assertNoFVs(f, nonFrees)
        assertNoFVs(x, nonFrees)
      case TT.Fun(pos, p, b, t) =>
        assertNoFVs(b, nonFrees)
      case TT.TAbs(pos, ps, e, t) =>
        assertNoFVs(e, nonFrees ++ ps)
      case TT.JCallStatic(pos, m, a) =>
        a.foreach(assertNoFVs(_, nonFrees))
      case TT.JCallInstance(pos, m, r, a) =>
        a.foreach(assertNoFVs(_, nonFrees))
        assertNoFVs(r, nonFrees)
      case TT.MatchError(pos, t) =>
    }
  }

  private[this] def tappFresh(t: Type): Type = t match {
    case Type.Abs(params, body) =>
      val s = new Subst(params.map { p => p -> freshVar() }.toList)
      s.app(body)
    case t => t
  }
  def appExpr(ctx: Ctx, expr: NT.Expr): Result[(Subst, TT.Expr)] = expr match {
    case NT.RefMember(pos, member) =>
      ok0(TT.RefMember(pos, member, tappFresh(ctx.memberTypeOf(member))))
    case NT.RefLocal(pos, name) =>
      ok0(TT.RefLocal(pos, name, tappFresh(ctx.localTypeOf(name))))
    case NT.Lit(pos, v) => ok0(TT.Lit(pos, v))
    case NT.If(pos, cond, th, el) =>
      for {
        x0 <- appExpr(ctx, cond)
        (s00, e0) = x0
        s0 <- (s00 + (e0.tpe -> Type.Bool)).unify(cond.pos)
        x1 <- appExpr(ctx, th)
        (s1, e1) = x1
        x2 <- appExpr(ctx, el)
        (s20, e2) = x2
        s2 <- (s20 + (e2.tpe -> e1.tpe)).unify(el.pos)
        s <- (s0 ++ s1 ++ s2).unify(expr.pos)
      } yield (s, TT.If(pos, e0, e1, e2, s.app(e1.tpe)))
    case NT.Fun(pos, param, tpe, body) =>
      val t = tpe getOrElse freshVar()
      for {
        x <- appExpr(ctx.bindLocal(param, t), body)
        (s1, b) = x
        s = s1
      } yield (s, TT.Fun(pos, param, b, Type.Fun(s.app(t), b.tpe)))
    case NT.ELet(pos, name, value, body) =>
      val varIdStart = nextVarId
      for {
        x0 <- appExpr(ctx, value)
        (s0, e00) = x0
        e0 = e00 match {
          case TT.Fun(pos, param, body, funType) =>
            val params = funType.freeTypeVariables
              .filter(_.id >= varIdStart)
              .toSeq
              .sortBy(_.id)
            val tpe = Type.Abs(params, funType)
            TT.TAbs(pos, params, TT.Fun(pos, param, body, funType), tpe)
          case x => x
        }
        x1 <- appExpr(ctx.bindLocal(name, e0.tpe), body)
        (s1, e1) = x1
        s <- (s0 ++ s1).unify(body.pos)
      } yield (s, TT.App(pos, TT.Fun(pos, name, e1, Type.Fun(e0.tpe, e1.tpe)), e0, e1.tpe))
    case NT.ELetRec(pos, bindings, body) =>
      for {
        x <- appLetRecBindings(ctx, pos, bindings) { (c, name, tpe) => c.bindLocal(name, tpe) }
        (s, bs) = x
        c = bs.foldLeft(ctx) {
          case (c, (name, tpe, expr)) =>
            c.bindLocal(name.value, tpe)
        }
        x <- appExpr(c, body)
        (sb, b) = x
        s <- (s ++ sb).unify(expr.pos)
      } yield (s, TT.ELetRec(pos, bs, b))
    case NT.App(pos, f, x) =>
      val a1 = freshVar()
      val a2 = freshVar()
      for {
        xf <- appExpr(ctx, f)
        (sf, tf) = xf
        sf <- (sf + (Type.Fun(a1, a2) -> tf.tpe)).unify(f.pos)
        xx <- appExpr(ctx, x)
        (sx, tx) = xx
        s <- (sf ++ sx + (a1 -> tx.tpe)).unify(x.pos)
      } yield (s, TT.App(pos, tf, tx, s.app(a2)))
    case NT.JCallStatic(pos, klassRef, name, args) =>
      for {
        klass <- ctx.findClass(klassRef).toResult(expr.pos, s"Class not found: $klassRef")
        targs <- args.map(appExpr(ctx, _)).validated
        s <- (Subst.empty +: targs.map(_._1)).reduce(_ ++ _).unify(expr.pos)
        method <- klass.findStaticMethod(name.value, targs.map(_._2.tpe.jtype))
          .toResult(name.pos, s"Can't resolve static method ${Type.prettyMethod(name.value, targs.map(_._2.tpe))} in class ${klass.ref.fullName}")
      } yield (s, TT.JCallStatic(pos, method, targs.map(_._2)))
    case NT.JCallInstance(pos, expr, name, args) =>
      for {
        targs <- args.map(appExpr(ctx, _)).validated
        x <- appExpr(ctx, expr)
        (s1, e1) = x
        klass <- ctx.findClass(e1.tpe)
          .toResult(expr.pos, s"Class ${e1.tpe} not found. Check classpath.")
        argTypes = targs.map(_._2.tpe)
        klassName = e1.tpe.jtype.boxed.jname
        method <- klass.findInstanceMethod(name.value, targs.map(_._2.tpe.jtype))
          .toResult(name.pos, s"Can't resolve instance method ${Type.prettyMethod(name.value, argTypes)} in class $klassName")
      } yield (s1, TT.JCallInstance(pos, method, e1, targs.map(_._2)))
    case NT.MatchError(pos) =>
      ok0(TT.MatchError(pos, freshVar()))
  }

  private[this] def appLetRecBindings(
    ctx: Ctx,
    pos: Pos,
    bindings: Seq[(Name, Option[Type], NT.Expr)])(register: (Ctx, String, Type) => Ctx): Result[(Subst, Seq[(Name, Type, TT.Expr)])] = {
    val varIdStart = nextVarId
    val names = bindings.map(_._1)
    val types = bindings.map { case (_, tpe, _) => tpe getOrElse freshVar() }
    val exprs = bindings.map(_._3)

    val c = names.zip(types).foldLeft(ctx) {
      case (c, (name, tpe)) =>
        register(c, name.value, tpe)
    }

    val typed =
      types.zip(exprs).map {
        case (tpe, fun) =>
          appExpr(c, fun).map {
            case (subst, tfun: TT.Fun) => (subst + (tpe -> tfun.tpe), tfun)
            case (subst, tfun) => throw new AssertionError(s"$tfun")
          }
      }.validated

    for {
      ts <- typed
      s <- ts.map(_._1).reduce(_ ++ _).unify(pos)
      ts2 = ts.map {
        case (_, tree) =>
          subst(s, tree)
      }
    } yield (
      s,
      names.zip(types).zip(ts2).map {
        case ((name, tpe), expr) =>
          val tpe2 = s.app(tpe)
          val params = tpe2
            .freeTypeVariables
            .filter(_.id >= varIdStart)
            .toSeq
            .sortBy(_.id)
          val abs = Type.Abs(params, tpe2)
          (name, abs, expr)
      })
  }
}

object Typer {
  def memberTypes(s: TypedAST.Module): Map[MemberRef, Type] = s.body.flatMap {
    case TypedAST.TLet(pos, name, tpe, _) =>
      Seq(MemberRef(s.moduleRef, name.value) -> tpe)
    case TypedAST.TLetRec(pos, bindings) =>
      bindings.map {
        case (n, t, e) => s.moduleRef.memberRef(n.value) -> t
      }
    case _ =>
      Seq()
  }.toMap

  // TODO: add unified flag
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

    // don't call this method unless unified
    def --(items: Seq[Type.Var]) = {
      val x = items.toSet
      new Subst(this.items.filterNot {
        case (l: Type.Var, r) => x.contains(l)
        case _ => throw new IllegalStateException()
      })
    }

    def dot(a: Type.Var, t: Type): Subst =
      new Subst((a -> app(t)) :: items)

    def unify(pos: Pos): Result[Subst] = unify0(items, pos)
    private[this] def unify0(items: List[(Type, Type)], pos: Pos): Result[Subst] =
      items match {
        case Nil => Result.ok(Subst.empty)
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
        case (Type.Data(m1, n1, a1), Type.Data(m2, n2, a2)) :: xs if m1 == m2 && n1 == n2 =>
          unify0(a1.zip(a2).foldLeft(xs) { (l, x) => x :: l }, pos)
        case (Type.Fun(l1, r1), Type.Fun(l2, r2)) :: xs =>
          unify0((l1 -> l2) :: (r1 -> r2) :: xs, pos)
        case (l, r) :: xs =>
          Result.error(pos, s"Unify failed: $l and $r")
      }

    def pretty: String =
      items.map { case (from, to) => s"$from --> $to" }.mkString("\n")
  }
  object Subst {
    def apply(items: (Type, Type)*) = new Subst(items.toList)
    val empty = new Subst(Nil)
  }

  case class Ctx(
    currentModule: ModuleRef,
    repo: Classpath,
    memberTypes: Map[MemberRef, Type] = Map(),
    localTypes: Map[String, Type] = Map()) {

    def localTypeOf(name: String): Type =
      localTypes(name)

    def memberTypeOf(member: MemberRef): Type =
      memberTypes(member)

    def findClass(tpe: Type): Option[ClassSig] = tpe match {
      case v: Type.Var => None
      case t =>
        // TODO: Support array
        repo.find(ClassRef.fromInternalName(t.jtype.boxed.jname))
    }

    def findClass(ref: ClassRef): Option[ClassSig] =
      repo.find(ref)

    def bindMember(name: Name, tpe: Type): Result[Ctx] = {
      val ref = currentModule.memberRef(name.value)
      if (memberTypes.contains(ref)) error(name.pos, s"Member $name is already defined")
      else Result.ok(copy(memberTypes = memberTypes + (ref -> tpe)))
    }
    def setMember(name: String, tpe: Type): Ctx =
      copy(memberTypes = memberTypes + (currentModule.memberRef(name) -> tpe))

    def bindMembers(mts: Map[MemberRef, Type]): Ctx =
      copy(memberTypes = memberTypes ++ mts)

    def bindLocal(name: String, tpe: Type): Ctx =
      copy(localTypes = localTypes + (name -> tpe))
  }

  def newCtx(module: ModuleRef, env: TypeEnv) =
    Ctx(module, env.classpath).bindMembers(env.types)

}
