package com.todesking.ojaml.ml0.compiler.scala

import Result.ok
import Result.error
import Result.validate
import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => RT, NamedAST => NT }
import Namer.Ctx
import Namer.ValueLike

import util.Syntax._

class Namer() {

  def appProgram(p: RT.Program, packageEnv: NameEnv): Result[(NameEnv, Seq[NT.Module])] =
    p.items.mapWithContextEC(packageEnv) { (penv, x) =>
      appModule(p.pkg, p.imports, penv, x).map {
        case (pe, named) =>
          (pe, named)
      }
    }

  private[this] def appModule(pkg: QName, imports: Seq[Import], penv: NameEnv, s: RT.Module): Result[(NameEnv, NT.Module)] = {
    val currentModule = ModuleRef(PackageRef.fromInternalName(pkg.internalName), s.name.value)
    if (penv.memberExists(currentModule.pkg, currentModule.name))
      return error(s.name.pos, s"${currentModule.fullName} already defined")
    val init = Ctx(penv.addModule(currentModule), currentModule)
    imports.foldLeft(Result.ok(init)) { (c, i) =>
      c.flatMap(_.addImport(i))
    }.flatMap { ctx =>
      s.body.mapWithContextEC(ctx) {
        case (c, t) =>
          appTerm(c, t)
      }.map {
        case (c, ts) =>
          (c.penv, NT.Module(s.pos, pkg, s.name, ts))
      }
    }
  }

  private[this] def appTerm(ctx: Ctx, t: RT.Term): Result[(Ctx, NT.Term)] = t match {
    case RT.TLet(pos, name, expr) =>
      for {
        e <- appExpr(ctx, expr)
        c <- ctx.tlet(name)
      } yield {
        (c.addModuleMember(name.value), NT.TLet(pos, name, e))
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
            params.map { tname => boundCtx.findType(tname) }.validated.map { ts => (name, ts) }
        }.validated
        ctx2 <- resolvedCtors
          .foldLeftE(ctx) { case (c, (n, ts)) => c.addCtor(name.value, tvars, n.value, ts) }
      } yield (ctx2, NT.Data(pos, name, tvars, resolvedCtors))
    case RT.TExpr(pos, expr) =>
      for {
        e <- appExpr(ctx, expr)
      } yield (ctx, NT.TExpr(pos, e))
  }

  private[this] def appExpr(ctx: Ctx, expr: RT.Expr): Result[NT.Expr] = expr match {
    case RT.Ref(pos, name) =>
      valueLike(ctx, expr)
        .flatMap(_.toValue(name.pos, s"${name.value} is not a value"))
        .map(NT.Ref(pos, _))
    case RT.Prop(pos, e, n) =>
      valueLike(ctx, expr)
        .flatMap(_.toValue(n.pos, s"${n.value} is not a value"))
        .map(NT.Ref(pos, _))
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
        tpe <- tpeName.mapResult(ctx.findType)
        (ref, c) = ctx.bindLocal(name.value)
        tbody <- appExpr(c, body)
      } yield {
        NT.Fun(pos, ref, tpe, tbody)
      }
    case RT.ELetRec(pos, bs, body) =>
      for {
        x <- ctx.bindLocals(bs.map(_._1))
        (refs, c) = x
        ts <- validate(bs.map(_._2).map(_.mapResult(c.findType)))
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
      validate(args.map(appExpr(ctx, _))).flatMap { targs =>
        if (isStatic) {
          valueLike(ctx, expr).flatMap {
            case ValueLike.TopLevel(ref) => ref match {
              case PackageMember.Class(ref) =>
                ok(NT.JCallStatic(pos, ref, name, targs))
              case PackageMember.Module(ref) =>
                error(name.pos, "Module is not a class")
              case PackageMember.Package(ref) =>
                error(name.pos, "Package is not a class")
            }
            case ValueLike.Value(ref, _) =>
              error(name.pos, "Value is not a class")
          }
        } else {
          appExpr(ctx, expr).map { receiver =>
            NT.JCallInstance(pos, receiver, name, targs)
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
      ctx.locateCtor(Name(pos, name))
        .flatMap {
          case (module, ctorName) =>
            val checkerRef = VarRef.ModuleMember(module, ctx.patCheckerName(ctorName))
            val extractorRefs = args.zipWithIndex.map {
              case (_, i) =>
                VarRef.ModuleMember(module, ctx.patExtractorName(ctorName, i))
            }

            val rootChecker = NT.App(pat.pos, NT.Ref(pat.pos, checkerRef), target)
            extractorRefs.zip(args).map {
              case (extractor, arg) =>
                appPat(ctx, arg, NT.App(arg.pos, NT.Ref(arg.pos, extractor), target))
            }.validated.map { subPatterns =>
              val checker = subPatterns.map(_._1).foldLeft(rootChecker: NT.Expr) {
                case (l, r) =>
                  NT.If(r.pos, l, r, NT.LitBool(r.pos, false))
              }
              val extractor = subPatterns.flatMap(_._2)
              (checker, extractor)
            }
        }
  }

  private[this] def valueLike(ctx: Ctx, expr: RT.Expr): Result[ValueLike] = expr match {
    case RT.Ref(pos, name) =>
      ctx.findValue(name.value)
        .toResult(name.pos, s"Value ${name.value} is not found")
    case RT.Prop(pos, e, n) =>
      valueLike(ctx, e).flatMap {
        case ValueLike.TopLevel(ref) => ref match {
          case PackageMember.Class(c) =>
            error(n.pos, s"Property not supported for class")
          case PackageMember.Package(p) =>
            ctx.findPackageMember(p, n.value)
              .map(ValueLike.TopLevel)
              .toResult(n.pos, s"Member ${n.value} is not found in package ${p.fullName}")
          case PackageMember.Module(m) =>
            ctx.findModuleMember(m, n).map(ValueLike.Value(_, None))
        }
        case ValueLike.Value(ref, _) =>
          error(n.pos, s"Property of value is not supported")
      }
    case expr =>
      throw new AssertionError(expr.toString)
  }
}

object Namer {
  sealed abstract class ValueLike {
    def toValue(pos: Pos, msg: String): Result[VarRef]
  }
  object ValueLike {
    case class TopLevel(ref: PackageMember) extends ValueLike {
      override def toValue(pos: Pos, msg: String) = error(pos, msg)
    }
    case class Value(ref: VarRef, ctor: Option[(ModuleRef, String, Seq[Type])]) extends ValueLike {
      override def toValue(pos: Pos, msg: String) = ok(ref)
    }
  }

  case class Ctx(
    penv: NameEnv,
    currentModule: ModuleRef,
    venv: Map[String, ValueLike] = Map(),
    stack: List[Ctx] = Nil,
    localTypes: Map[String, Type] = Map()) {
    val depth: Int = stack.size

    def tlet(name: Name): Result[Ctx] = {
      require(stack.isEmpty)
      val varRef = VarRef.ModuleMember(currentModule, name.value)
      if (venv.get(name.value).exists(_ == varRef))
        error(name.pos, s"""Name "${name.value}" is already defined in ${currentModule.name}""")
      else
        ok(copy(venv = venv + (varRef.name -> ValueLike.Value(varRef, None))))
    }

    def findValue(name: String): Option[ValueLike] =
      venv.get(name) orElse penv.findModuleMember(currentModule, name).map {
        case NameEnv.ModuleMember(name, ctorInfo) =>
          val info = ctorInfo.map { args => (currentModule, name, args) }
          ValueLike.Value(VarRef.ModuleMember(currentModule, name), info)
      } orElse {
        penv.findMember(currentModule.pkg, name)
          .orElse(penv.findMember(PackageRef.Root, name))
          .map(ValueLike.TopLevel.apply)
      }

    def findPackageMember(pkg: PackageRef, name: String): Option[PackageMember] =
      penv.findMember(pkg, name)

    def findType(tname: TypeName): Result[Type] = tname match {
      case TypeName.Atom(pos, n) => n match {
        case n if localTypes.contains(n) => // TODO: Support qualified type name
          ok(localTypes(n))
        case "int" => ok(Type.Int)
        case "bool" => ok(Type.Bool)
        case unk => error(tname.pos, s"Type not found: $unk")
      }
      case TypeName.Fun(l, r) =>
        for {
          tl <- findType(l)
          tr <- findType(r)
        } yield Type.Fun(tl, tr)
      case TypeName.App(pos, n, ns) =>
        for {
          t <- findType(n)
          args <- ns.map(findType).validated
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

    def bindLocal(name: String): (VarRef.Local, Ctx) = {
      val ref = VarRef.Local(depth + 1, 0)
      (ref, copy(
        venv = venv + (name -> ValueLike.Value(ref, None)),
        stack = this :: stack))
    }

    def bindLocals(names: Seq[Name]): Result[(Seq[VarRef.Local], Ctx)] = {
      names.mapWithContextE(Set.empty[String]) { (a, name) =>
        if (a.contains(name.value))
          error(name.pos, s"Name conflict: ${name.value}")
        else
          ok((a + name.value, name.value))
      }.map { ns =>
        val refs = ns.zipWithIndex.map { case (_, i) => VarRef.Local(depth + 1, i + 1) }
        val c = copy(
          venv = venv ++ ns.zip(refs.map(ValueLike.Value(_, None))),
          stack = this :: stack)
        (refs, c)
      }
    }

    def addModuleMember(name: String): Ctx = copy(penv = penv.addModuleMember(currentModule, name, None))
    def findModuleMember(m: ModuleRef, name: Name): Result[VarRef.ModuleMember] =
      if (penv.moduleMemberExists(m, name.value))
        ok(VarRef.ModuleMember(m, name.value))
      else
        error(name.pos, s"Member ${name.value} is not found in module ${m.fullName}")

    def addImport(i: Import): Result[Ctx] = i.flatten.foldLeftE(this) {
      case (ctx, Import.Single(qname, alias)) =>
        val nameParts = qname.parts
        val aliasName = alias.map(_.value) getOrElse nameParts.last.value
        ctx.findValue(nameParts.head.value)
          .toResult(nameParts.head.pos, s"Member not found: ${nameParts.head.value}")
          .flatMap { head =>
            nameParts.tail.foldLeft[Result[ValueLike]](ok(head)) { (v, n) =>
              v.flatMap {
                case ValueLike.TopLevel(pm) => pm match {
                  case PackageMember.Package(ref) =>
                    ctx.findPackageMember(ref, n.value)
                      .toResult(n.pos, s"Package member not found: ${n.value}")
                      .map(ValueLike.TopLevel)
                  case PackageMember.Module(m) =>
                    penv.findModuleMember(m, n.value).map {
                      case NameEnv.ModuleMember(name, ctorInfo) =>
                        val info = ctorInfo.map { args => (m, name, args) }
                        ValueLike.Value(VarRef.ModuleMember(m, name), info)
                    }.toResult(n.pos, s"Member ${n.value} is not found in module $m")
                  case PackageMember.Class(ref) =>
                    error(n.pos, s"${ref.fullName} is class and field reference not supported")
                }
                case ValueLike.Value(ref, _) =>
                  error(n.pos, "Property of value is not importable")
              }
            }.map { v =>
              ctx.copy(venv = ctx.venv + (aliasName -> v))
            }
          }
    }

    def addDataType(name: Name, params: Seq[Name]): Result[Ctx] = {
      if (localTypes.contains(name.value)) {
        error(name.pos, s"Type ${name.value} is already defined")
      } else {
        val tpe =
          if (params.isEmpty) Type.Data(currentModule, name.value, Seq())
          else {
            val tvars = params.zipWithIndex.map(_._2).map(Type.Var(_))
            Type.Abs(tvars, Type.Data(currentModule, name.value, tvars))
          }
        val c = copy(
          localTypes = localTypes + (name.value -> tpe),
          penv = penv.addModuleTypeMember(currentModule, name.value))
        ok(c)
      }
    }
    def bindType(name: String, tpe: Type): Ctx =
      copy(localTypes = localTypes + (name -> tpe))

    def patCheckerName(name: String) = s"${name}$$check"
    def patExtractorName(name: String, i: Int) = s"$name$$get$i"
    def addCtor(dataName: String, tvars: Seq[Type.Var], ctorName: String, ts: Seq[Type]): Result[Ctx] = {
      // TODO: check duplicate
      val ctorRef = ValueLike.Value(VarRef.ModuleMember(currentModule, ctorName), Some((currentModule, ctorName, ts)))
      val newPenv = penv.addModuleMember(currentModule, ctorName, Some(ts))
      ok(copy(venv = venv + (ctorName -> ctorRef), penv = newPenv))
    }
    def locateCtor(name: Name): Result[(ModuleRef, String)] = findValue(name.value)
      .toResult(name.pos, s"Data constructor not found: ${name.value}")
      .flatMap {
        case ValueLike.TopLevel(_) =>
          error(name.pos, s"${name.value} is toplevel value, not a data constructor")
        case ValueLike.Value(ref, None) =>
          println(venv)
          error(name.pos, s"$ref is not a data constructor")
        case ValueLike.Value(ref, Some((module, ctorName, args))) =>
          ok((module, ctorName))
      }
  }
}
