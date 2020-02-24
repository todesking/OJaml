package com.todesking.ojaml.ml0.compiler.scala

import Result.ok
import Result.error
import Result.validate
import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => RT, NamedAST => NT }
import Namer.Ctx
import Namer.ValueLike

import util.Syntax._

class Namer(packageEnv: PackageEnv) {

  def appProgram(p: RT.Program): Result[(PackageEnv, Seq[NT.Module])] =
    p.items.mapWithContextEC(packageEnv) { (penv, x) =>
      appModule(p.pkg, p.imports, penv, x).map {
        case (pe, named) =>
          (pe, named)
      }
    }

  def appModule(pkg: QName, imports: Seq[Import], penv: PackageEnv, s: RT.Module): Result[(PackageEnv, NT.Module)] = {
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
          (c.penv, Pos.fill(NT.Module(pkg, s.name, ts), s.pos))
      }
    }
  }

  def appTerm(ctx: Ctx, t: RT.Term): Result[(Ctx, NT.Term)] = {
    appTerm0(ctx, t).map { case (ctx, tt) => (ctx, Pos.fill(tt, t.pos)) }
  }
  private[this] def appTerm0(ctx: Ctx, t: RT.Term): Result[(Ctx, NT.Term)] = t match {
    case RT.TLet(name, expr) =>
      for {
        e <- appExpr(ctx, expr)
        c <- ctx.tlet(name)
      } yield {
        (c.addModuleMember(name.value), NT.TLet(name, e))
      }
    case RT.Data(name, ctors) =>
      for {
        x1 <- ctx.addDataType(name)
        (data, ctx) = x1
        resolved <- ctors.map {
          case (name, params) =>
            params.map { tname => ctx.findType(tname) }.validated.map { ts => (name, ts) }
        }.validated
        ctx2 <- resolved.foldLeftE(ctx) { case (c, (n, ts)) => c.addCtor(data, n, ts) }
      } yield (ctx2, NT.Data(name, data, resolved))
  }

  def appExpr(ctx: Ctx, expr: RT.Expr): Result[NT.Expr] = {
    if (expr.pos == null) println(s"[WARN] NOPOS: $expr")
    appExpr0(ctx, expr).map(Pos.fill(_, expr.pos))
  }

  private[this] def appExpr0(ctx: Ctx, expr: RT.Expr): Result[NT.Expr] = expr match {
    case RT.Ref(name) =>
      valueLike(ctx, expr)
        .flatMap(_.toValue(name.pos, s"${name.value} is not a value"))
        .map(NT.Ref)
    case RT.Prop(e, n) =>
      valueLike(ctx, expr)
        .flatMap(_.toValue(n.pos, s"${n.value} is not a value"))
        .map(NT.Ref)
    case RT.LitInt(v) => ok(NT.LitInt(v))
    case RT.LitBool(v) => ok(NT.LitBool(v))
    case RT.LitString(v) => ok(NT.LitString(v))
    case RT.If(cond, th, el) =>
      for {
        e0 <- appExpr(ctx, cond)
        e1 <- appExpr(ctx, th)
        e2 <- appExpr(ctx, el)
      } yield NT.If(e0, e1, e2)
    case RT.Fun(name, tpeName, body) =>
      for {
        tpe <- tpeName.mapResult(ctx.findType)
        (ref, c) = ctx.bindLocal(name.value)
        tbody <- appExpr(c, body)
      } yield {
        NT.Fun(ref, tpe, tbody)
      }
    case RT.ELetRec(bs, body) =>
      for {
        x <- ctx.bindLocals(bs.map(_._1))
        (refs, c) = x
        ts <- validate(bs.map(_._2).map(_.mapResult(c.findType)))
        vs <- validate(bs.map(_._3).map(appExpr(c, _))).map(_.map(_.asInstanceOf[NT.Fun]))
        b <- appExpr(c, body)
      } yield NT.ELetRec(refs.zip(ts).zip(vs).map { case ((r, t), v) => (r, t, v) }, b)
    case RT.ELet(name, value, body) =>
      for {
        v <- appExpr(ctx, value)
        (ref, c) = ctx.bindLocal(name.value)
        b <- appExpr(c, body)
      } yield {
        NT.ELet(ref, v, b)
      }
    case RT.App(f, x) =>
      for {
        e0 <- appExpr(ctx, f)
        e1 <- appExpr(ctx, x)
      } yield NT.App(e0, e1)
    case RT.JCall(expr, name, args, isStatic) =>
      validate(args.map(appExpr(ctx, _))).flatMap { targs =>
        if (isStatic) {
          valueLike(ctx, expr).flatMap {
            case ValueLike.TopLevel(ref) => ref match {
              case PackageMember.Class(ref) =>
                ok(NT.JCallStatic(ref, name, targs))
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
            NT.JCallInstance(receiver, name, targs)
          }
        }
      }
    case RT.Match(expr, clauses) =>
      for {
        e <- appExpr(ctx, expr)
        (eVar, c) = ctx.bindLocal(s"$$it")
        body <- appClauses(c, eVar, clauses)
      } yield NT.ELet(eVar, e, body)
  }

  private[this] def appClauses(ctx: Ctx, eVar: VarRef, clauses: Seq[RT.Clause]): Result[NT.Expr] = {
    clauses.map {
      case RT.Clause(pat, body) =>
        for {
          x <- appPat(ctx, pat, NT.Ref(eVar))
          (checker, extractors) = x
          body <- appClauseBody(ctx, extractors, body)
        } yield (checker, body)
    }.validated.map {
      _.foldRight(NT.MatchError(): NT.Expr) {
        case ((checker, body), next) =>
          NT.If(checker, body, next)
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
      val fun = refs.foldRight(b) { case (ref, e) => NT.Fun(ref, None, b) }
      val app = extractors.foldLeft(fun) { case (f, (_, e)) => NT.App(f, e) }
      app
    }
  }

  private[this] def appPat(ctx: Ctx, pat: RT.Pat, target: NT.Expr): Result[(NT.Expr, Seq[(String, NT.Expr)])] = pat match {
    case RT.Pat.PAny() =>
      ok((NT.LitBool(true), Nil))
    case RT.Pat.Capture(name) =>
      ok((NT.LitBool(true), Seq(name.value -> target)))
    case RT.Pat.Ctor(name, args) =>
      def findFunction(name: String): Result[VarRef] = ctx
        .findValue(name)
        .toResult(pat.pos, s"function not found: $name")
        .flatMap {
          case ValueLike.TopLevel(_) =>
            error(pat.pos, s"$name is not a data constructor")
          case ValueLike.Value(ref, ctor) =>
            ok(ref)
        }
      findFunction(ctx.patCheckerName(name.value))
        .flatMap { checkerRef =>
          args.zipWithIndex.map {
            case (_, i) =>
              findFunction(ctx.patExtractorName(name.value, i))
          }.validated.flatMap { extractors =>
            val rootChecker = NT.App(NT.Ref(checkerRef), target)
            extractors.zip(args).map {
              case (extractor, arg) =>
                appPat(ctx, arg, NT.App(NT.Ref(extractor), target))
            }.validated.map { subPatterns =>
              val checker = subPatterns.map(_._1).foldLeft(rootChecker: NT.Expr) {
                case (l, r) =>
                  NT.If(l, r, NT.LitBool(false))
              }
              val extractor = subPatterns.flatMap(_._2)
              (checker, extractor)
            }
          }
        }
  }

  private[this] def valueLike(ctx: Ctx, expr: RT.Expr): Result[ValueLike] = expr match {
    case RT.Ref(name) =>
      ctx.findValue(name.value)
        .toResult(name.pos, s"Value ${name.value} is not found")
    case RT.Prop(e, n) =>
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
    case class Value(ref: VarRef, ctor: Option[(Type.Data, String, Seq[Type])]) extends ValueLike {
      override def toValue(pos: Pos, msg: String) = ok(ref)
    }
  }

  case class Ctx(
    penv: PackageEnv,
    currentModule: ModuleRef,
    venv: Map[String, ValueLike] = Map(),
    stack: List[Ctx] = Nil,
    localTypes: Map[String, Type] = Map()) {
    val depth: Int = stack.size

    def tlet(name: Name): Result[Ctx] = {
      require(stack.isEmpty)
      val varRef = VarRef.ModuleMember(currentModule, name.value)
      if (venv.contains(name.value))
        error(name.pos, s"""Name "${name.value}" is already defined in ${currentModule.name}""")
      else
        ok(copy(venv = venv + (varRef.name -> ValueLike.Value(varRef, None))))
    }

    def findValue(name: String): Option[ValueLike] =
      venv.get(name) orElse {
        if (penv.moduleMemberExists(currentModule, name)) {
          Some(ValueLike.Value(VarRef.ModuleMember(currentModule, name), None))
        } else {
          penv.findMember(currentModule.pkg, name)
            .orElse(penv.findMember(PackageRef.Root, name))
            .map(ValueLike.TopLevel.apply)
        }
      }

    def findPackageMember(pkg: PackageRef, name: String): Option[PackageMember] =
      penv.findMember(pkg, name)

    def findType(tname: TypeName): Result[Type] = tname match {
      case TypeName.Atom(n) => n match {
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

    def addModuleMember(name: String): Ctx = copy(penv = penv.addModuleMember(currentModule, name))
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
                    ctx.findModuleMember(m, n).map(ValueLike.Value(_, None))
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

    def addDataType(name: Name): Result[(Type.Data, Ctx)] = {
      if (localTypes.contains(name.value)) {
        error(name.pos, s"Type ${name.value} is already defined")
      } else {
        val tpe = Type.Data(currentModule, name.value)
        val c = copy(
          localTypes = localTypes + (name.value -> tpe),
          penv = penv.addModuleTypeMember(currentModule, name.value))
        ok((tpe, c))
      }
    }
    def patCheckerName(name: String) = s"${name}$$check"
    def patExtractorName(name: String, i: Int) = s"$name$$get$i"
    def addCtor(data: Type.Data, name: Name, ts: Seq[Type]): Result[Ctx] = {
      // TODO: check duplicate
      val ctor = name.value -> ValueLike.Value(VarRef.ModuleMember(currentModule, name.value), Some((data, name.value, ts)))
      val extractors = ts.zipWithIndex.map {
        case (t, i) =>
          val n = patExtractorName(name.value, i)
          n -> ValueLike.Value(VarRef.ModuleMember(currentModule, n), None)
      }
      val checkerName = patCheckerName(name.value)
      val checker = checkerName -> ValueLike.Value(VarRef.ModuleMember(currentModule, checkerName), None)
      ok(copy(venv = venv + ctor ++ extractors + checker))
    }
  }
}
