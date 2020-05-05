package com.todesking.ojaml.ml0.compiler.scala

import Result.ok
import Result.error
import Result.validate
import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => RT, NamedAST => NT }
import Namer.Ctx

import util.Syntax._

import NameEnv.Ref

class Namer() {

  def appProgram(p: RT.Program, env: NameEnv): Result[(NameEnv, Seq[NT.Module])] =
    p.items.mapWithContextEC(env) { (env, x) =>
      appModule(p.pkg, p.imports, env, x).map {
        case (pe, named) =>
          (pe, named)
      }
    }

  private[this] def appModule(pkg: QName, imports: Seq[Import], env: NameEnv, s: RT.Module): Result[(NameEnv, NT.Module)] = {
    val currentModule = ModuleRef(PackageRef.fromInternalName(pkg.internalName), s.name.value)
    if(env.exists(currentModule.pkg, currentModule.name))
      return error(s.name.pos, s"${currentModule.fullName} already defined")
    val init =Ctx(env.addModule(currentModule), currentModule)
      .copy(imports = Map(
        "bool" -> NameEnv.Ref.Member(PackageRef.root("ojaml").moduleRef("Primitives"), "Bool"),
        "int" -> NameEnv.Ref.Member(PackageRef.root("ojaml").moduleRef("Primitives"), "Int"),
      ))
    imports.foldLeft(Result.ok(init)) { (c, i) =>
      c.flatMap(_.addImport(i))
    }.flatMap { ctx =>
      s.body.mapWithContextEC(ctx) {
        case (c, t) =>
          appTerm(c, t)
      }.map {
        case (c, ts) =>
          (c.env, NT.Module(s.pos, pkg, s.name, ts))
      }
    }
  }

  private[this] def appTerm(ctx: Ctx, t: RT.Term): Result[(Ctx, NT.Term)] = t match {
    case RT.TLet(pos, name, tname, expr) =>
      for {
        e <- appExpr(ctx, expr)
        t <- tname.map { tn => ctx.resolveType(tn).map(Some(_)) }
          .fold[Result[Option[Type]]] { ok(None) } { x => x }
        c <- ctx.tlet(name)
      } yield {
        (c.addModuleMember(name.value), NT.TLet(pos, name, t, e))
      }
    case RT.Data(pos, name, tparams, ctors) =>
      for {
        ctx <- ctx.addDataType(name, tparams)
        tvars = tparams.zipWithIndex.map(_._2).map(Type.Var(_))
        boundCtx = tparams.zip(tvars).foldLeft(ctx) {
          case (c, (param, tv)) => c.bindType(param.value, tv)
        }
        resolvedCtors <- ctors.map {
          case (name, params) =>
            params.map { tname => boundCtx.resolveType(tname) }.validated.map { ts => (name, ts) }
        }.validated
        ctx <- resolvedCtors
          .foldLeftE(ctx) { case (c, (n, ts)) => c.addCtor(n.value, ts.size) }
      } yield (ctx, NT.Data(pos, name, tvars, resolvedCtors))
    case RT.TExpr(pos, expr) =>
      for {
        e <- appExpr(ctx, expr)
      } yield (ctx, NT.TExpr(pos, e))
  }

  private[this] def appExpr(ctx: Ctx, expr: RT.Expr): Result[NT.Expr] = expr match {
    case RT.Ref(pos, name) =>
      ctx.resolveValue(name).map(NT.Ref(pos, _))
    case RT.LitInt(pos, v) => ok(NT.LitInt(pos, v))
    case RT.LitBool(pos, v) => ok(NT.LitBool(pos, v))
    case RT.LitString(pos, v) => ok(NT.LitString(pos, v))
    case RT.If(pos, cond, th, el) =>
      for {
        e0 <- appExpr(ctx, cond)
        e1 <- appExpr(ctx, th)
        e2 <- appExpr(ctx, el)
      } yield NT.If(pos, e0, e1, e2)
    case RT.Fun(pos, name, tpeName, body) =>
      for {
        tpe <- tpeName.mapResult(ctx.resolveType)
        (ref, c) = ctx.bindLocal(name.value)
        tbody <- appExpr(c, body)
      } yield {
        NT.Fun(pos, ref, tpe, tbody)
      }
    case RT.ELetRec(pos, bs, body) =>
      for {
        x <- ctx.bindLocals(bs.map(_._1))
        (refs, c) = x
        ts <- validate(bs.map(_._2).map(_.mapResult(c.resolveType)))
        vs <- validate(bs.map(_._3).map(appExpr(c, _))).map(_.map(_.asInstanceOf[NT.Fun]))
        b <- appExpr(c, body)
      } yield NT.ELetRec(pos, refs.zip(ts).zip(vs).map { case ((r, t), v) => (r, t, v) }, b)
    case RT.ELet(pos, name, value, body) =>
      for {
        v <- appExpr(ctx, value)
        (ref, c) = ctx.bindLocal(name.value)
        b <- appExpr(c, body)
      } yield {
        NT.ELet(pos, ref, v, b)
      }
    case RT.App(pos, f, x) =>
      for {
        e0 <- appExpr(ctx, f)
        e1 <- appExpr(ctx, x)
      } yield NT.App(pos, e0, e1)
    case RT.JCall(pos, expr, name, args, isStatic) =>
      validate(args.map(appExpr(ctx, _))).flatMap { args =>
        if (isStatic) {
          expr match {
            case RT.Ref(pos, qname) =>
              ctx.resolveSimpleType(qname).flatMap {
                case Type.Klass(klass) =>
                  ok(NT.JCallStatic(pos, klass, name, args))
                case tpe =>
                  error(pos, "Static call: Class name required")
              }
            case expr =>
              error(expr.pos, "Static method call: Class name required")
          }
        } else {
          appExpr(ctx, expr).map { receiver =>
            NT.JCallInstance(pos, receiver, name, args)
          }
        }
      }
    case root @ RT.Match(pos, expr, clauses) =>
      for {
        e <- appExpr(ctx, expr)
        (eVar, c) = ctx.bindLocal(s"$$it")
        body <- appClauses(c, root.pos, eVar, clauses)
      } yield NT.ELet(pos, eVar, e, body)
  }

  private[this] def appClauses(ctx: Ctx, pos: Pos, eVar: VarRef, clauses: Seq[RT.Clause]): Result[NT.Expr] = {
    clauses.map {
      case RT.Clause(pos, pat, body) =>
        for {
          x <- appPat(ctx, pat, NT.Ref(pos, eVar))
          (checker, extractors) = x
          body <- appClauseBody(ctx, extractors, body)
        } yield (checker, body)
    }.validated.map {
      _.foldRight(NT.MatchError(pos): NT.Expr) {
        case ((checker, body), next) =>
          NT.If(checker.pos, checker, body, next)
      }
    }
  }

  private[this] def appClauseBody(ctx: Ctx, extractors: Seq[(String, NT.Expr)], body: RT.Expr): Result[NT.Expr] = {
    val (c2, refs) = extractors.mapWithContextC(ctx) {
      case (c, (name, _)) =>
        val (c2, r) = c.bindLocal(name)
        (r, c2)
    }
    appExpr(c2, body).map { b =>
      val fun = refs.foldRight(b) { case (ref, e) => NT.Fun(e.pos, ref, None, e) }
      val app = extractors.foldLeft(fun) { case (f, (_, e)) => NT.App(e.pos, f, e) }
      app
    }
  }

  private[this] def appPat(ctx: Ctx, pat: RT.Pat, target: NT.Expr): Result[(NT.Expr, Seq[(String, NT.Expr)])] = pat match {
    case RT.Pat.PAny(pos) =>
      ok((NT.LitBool(pat.pos, true), Nil))
    case RT.Pat.Capture(pos, name) =>
      ok((NT.LitBool(pat.pos, true), Seq(name -> target)))
    case RT.Pat.Ctor(pos, name, args) =>
      // TODO: Support qname
      for {
        x <- ctx.resolveCtor(QName(Seq(Name(pos, name))))
        (module, ctorName, arity) = x
        _ <-
          if(args.size == arity) ok(())
          else error(pos, s"Ctor $ctorName has arity $arity but argument size is ${args.size}")
        checkerRef = VarRef.ModuleMember(module, ctx.patCheckerName(ctorName))
        extractorRefs = args.zipWithIndex.map {
          case (_, i) =>
            VarRef.ModuleMember(module, ctx.patExtractorName(ctorName, i))
        }
        rootChecker = NT.App(pat.pos, NT.Ref(pat.pos, checkerRef), target)
        subPatterns <-
          extractorRefs.zip(args).map {
            case (extractor, arg) =>
              appPat(ctx, arg, NT.App(arg.pos, NT.Ref(arg.pos, extractor), target))
          }.validated
      } yield {
              val checker = subPatterns.map(_._1).foldLeft(rootChecker: NT.Expr) {
                case (l, r) =>
                  NT.If(r.pos, l, r, NT.LitBool(r.pos, false))
              }
              val extractor = subPatterns.flatMap(_._2)
              (checker, extractor)
      }
  }
}

object Namer {
  case class Ctx(
    env: NameEnv,
    currentModule: ModuleRef,
    imports: Map[String, Ref] = Map(),
    values: Map[String, VarRef] = Map(),
    stack: List[Ctx] = Nil,
    localTypes: Map[String, Type] = Map()) {
    val depth: Int = stack.size

    def tlet(name: Name): Result[Ctx] = {
      require(stack.isEmpty)
      if(env.valueExists(currentModule, name.value))
        error(name.pos, s"Name $name is already defined in ${currentModule.name}")
      else
        ok(copy(env = env.addValueMember(currentModule, name.value)))
    }

    def resolveRef(qname: QName): Result[Ref] =
      resolveRootRef(qname.parts.head).flatMap { root =>
        resolveRef(root, qname.parts.tail.toList)
      }

    private[this] def resolveRef(root: Ref, parts: List[Name]): Result[Ref] = parts match {
      case Nil => ok(root)
      case x :: xs =>
        env.findRef(root, x.value)
          .toResult(x.pos, s"Name `$x` is not found in $root")
          .flatMap { ref =>
            resolveRef(ref, xs)
          }
    }

    private[this] def resolveRootRef(name: Name): Result[Ref] =
      (
        env.findRef(Ref.Module(currentModule), name.value)
        orElse imports.get(name.value)
        orElse env.findRef(Ref.Package(currentModule.pkg), name.value)
        orElse env.findRef(name.value)
      ).toResult(name.pos, s"Name $name not found")

    def resolveSimpleType(qname: QName): Result[Type] = (
      (if(qname.size == 1) localTypes.get(qname.head.value).map(ok(_)) else None)
      getOrElse resolveRef(qname)
        .flatMap { ref => env.findType(ref).toResult(qname.pos, s"$qname is not a type") }
    )

    def resolveType(tname: TypeName): Result[Type] = tname match {
      case TypeName.Atom(pos, n) =>
        resolveSimpleType(n)
      case TypeName.Fun(l, r) =>
        for {
          tl <- resolveType(l)
          tr <- resolveType(r)
        } yield Type.Fun(tl, tr)
      case TypeName.App(pos, n, ns) =>
        for {
          t <- resolveType(n)
          args <- ns.map(resolveType).validated
          applied <- t match {
            case Type.Abs(params, body) =>
              if (params.size != args.size)
                error(pos, s"Type scheme $n has arity ${params.size} but argument length is ${args.size}")
              else
                ok(params.zip(args).foldLeft(body) { case (t, (p, a)) => t.substitute(p, a) })
            case other =>
              error(pos, s"Type $n can't take type parameters")
          }
        } yield applied
    }

    def resolveValue(qname: QName): Result[VarRef] = (
      (if(qname.size == 1) values.get(qname.head.value).map(ok(_)) else None)
      getOrElse resolveRef(qname).flatMap {
        case Ref.Member(module, name) =>
          if(env.valueExists(module, name))
            ok(VarRef.ModuleMember(module, name))
          else
            error(qname.lastPartPos, s"Member $module.$name is not a value")
        case ref =>
          error(qname.lastPartPos, s"$ref is not a value")
      }
    )

    def bindLocal(name: String): (VarRef.Local, Ctx) = {
      val ref = VarRef.Local(depth + 1, 0)
      (ref, copy(
        values  = values + (name -> ref),
        stack = this :: stack))
    }

    def bindLocals(names: Seq[Name]): Result[(Seq[VarRef.Local], Ctx)] = {
      names.foldLeftE(Set.empty[String]) { (ns, n) =>
        if(ns.contains(n.value)) error(n.pos, s"Name conflict: $n")
        else ok(ns + n.value)
      }.map { _ =>
        val refs = names.zipWithIndex.map { case (_, i) => VarRef.Local(depth + 1, i + 1) }
        val c = copy(
          values = values ++ names.map(_.value).zip(refs),
          stack = this :: stack)
        (refs, c)
      }
    }

    def addModuleMember(name: String): Ctx = {
      require(stack.isEmpty)
      copy(env = env.addValueMember(currentModule, name))
    }

    def addImport(i: Import): Result[Ctx] = i.flatten.foldLeftE(this) {
      case (ctx, Import.Single(qname, alias)) =>
        ctx.resolveRef(qname).map { ref =>
          ctx.addImport(ref, alias.map(_.value) getOrElse env.nameOf(ref))
        }
    }

    def addImport(ref: Ref, name: String): Ctx =
      copy(imports = imports + (name -> ref))

    def addDataType(name: Name, params: Seq[Name]): Result[Ctx] = {
      if(env.typeExists(currentModule, name.value)) {
        error(name.pos, s"Type ${name.value} is already defined")
      } else {
        val tpe =
          if (params.isEmpty) Type.Data(currentModule, name.value, Seq())
          else {
            val tvars = params.zipWithIndex.map(_._2).map(Type.Var(_))
            Type.Abs(tvars, Type.Data(currentModule, name.value, tvars))
          }
        val c = copy(
          env = env.addTypeMember(currentModule, name.value, tpe))
        ok(c)
      }
    }
    def bindType(name: String, tpe: Type): Ctx =
      copy(localTypes = localTypes + (name -> tpe))

    def patCheckerName(name: String) = s"${name}$$check"
    def patExtractorName(name: String, i: Int) = s"$name$$get$i"

    def addCtor(name: String, arity: Int): Result[Ctx] = {
      require(stack.isEmpty)
      ok(copy(env = env.addCtorMember(currentModule, name, arity)))
    }

    def resolveCtor(qname: QName): Result[(ModuleRef, String, Int)] =
      resolveRef(qname).flatMap { ref =>
        env.findCtor(ref)
          .toResult(qname.lastPartPos, s"${qname.last} is not a constructor")
      }
  }
}
