package com.todesking.ojaml.ml0.compiler.scala

import Compiler.Error
import util.Syntax._

class Namer(packageEnv: PackageEnv) {
  import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => RT, NamedAST => NT }
  import Namer.Result
  import Namer.Ctx
  import Namer.error
  import Namer.errorMessage
  import Namer.ValueLike

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
    imports.foldLeft[Result[Ctx]](Right(init)) { (c, i) =>
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
    case e: RT.Expr =>
      appExpr(ctx, e).map { te => (ctx, te) }
  }

  private[this] def validate[A](xs: Seq[Result[A]]): Result[Seq[A]] = {
    val rights = xs.collect { case Right(x) => x }
    if (rights.size == xs.size) Right(rights)
    else Left(xs.collect { case Left(x) => x }.flatten)
  }

  def appExpr(ctx: Ctx, expr: RT.Expr): Result[NT.Expr] = {
    if (expr.pos == null) println(s"[WARN] NOPOS: $expr")
    appExpr0(ctx, expr).map(Pos.fill(_, expr.pos))
  }

  private[this] def appExpr0(ctx: Ctx, expr: RT.Expr): Result[NT.Expr] = expr match {
    case RT.Ref(name) =>
      valueLike(ctx, expr).flatMap {
        case ValueLike.Value(ref) => Right(ref)
        case ValueLike.TopLevel(_) => error(name.pos, s"${name.value} is not a value")
      }
        .map(NT.Ref)
    case RT.Prop(e, n) =>
      valueLike(ctx, expr).flatMap {
        case ValueLike.TopLevel(ref) =>
          error(n.pos, s"${n.value} is not a value")
        case ValueLike.Value(ref) =>
          Right(NT.Ref(ref))
      }
    case RT.LitInt(v) => Right(NT.LitInt(v))
    case RT.LitBool(v) => Right(NT.LitBool(v))
    case RT.LitString(v) => Right(NT.LitString(v))
    case RT.If(cond, th, el) =>
      for {
        e0 <- appExpr(ctx, cond)
        e1 <- appExpr(ctx, th)
        e2 <- appExpr(ctx, el)
      } yield NT.If(e0, e1, e2)
    case RT.Fun(name, tpeName, body) =>
      for {
        tpe <- ctx.findType(tpeName)
        (ref, c) = ctx.bindLocal(name.value)
        tbody <- appExpr(c, body)
      } yield {
        NT.Fun(ref, tpe, tbody)
      }
    case RT.ELetRec(bs, body) =>
      for {
        x <- ctx.bindLocals(bs.map(_._1))
        (refs, c) = x
        ts <- validate(bs.map(_._2).map(c.findType))
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
                Right(NT.JCallStatic(ref, name, targs))
              case PackageMember.Module(ref) =>
                error(name.pos, "Module is not a class")
              case PackageMember.Package(ref) =>
                error(name.pos, "Package is not a class")
            }
            case ValueLike.Value(ref) =>
              error(name.pos, "Value is not a class")
          }
        } else {
          appExpr(ctx, expr).map { receiver =>
            NT.JCallInstance(receiver, name, targs)
          }
        }
      }
  }

  private[this] def valueLike(ctx: Ctx, expr: RT.Expr): Result[ValueLike] = expr match {
    case RT.Ref(name) =>
      ctx.findValue(name.value)
        .toRight(errorMessage(name.pos, s"Value ${name.value} is not found"))
    case RT.Prop(e, n) =>
      valueLike(ctx, e).flatMap {
        case ValueLike.TopLevel(ref) => ref match {
          case PackageMember.Class(c) =>
            error(n.pos, s"Property not supported for class")
          case PackageMember.Package(p) =>
            ctx.findPackageMember(p, n.value)
              .map(ValueLike.TopLevel)
              .toRight(errorMessage(n.pos, s"Member ${n.value} is not found in package ${p.fullName}"))
          case PackageMember.Module(m) =>
            ctx.findModuleMember(m, n).map(ValueLike.Value)
        }
        case ValueLike.Value(ref) =>
          error(n.pos, s"Property of value is not supported")
      }
    case expr =>
      throw new AssertionError(expr.toString)
  }
}

object Namer {
  type Result[A] = Either[Seq[Compiler.Error], A]
  def error[A](pos: Pos, msg: String): Result[A] = Left(errorMessage(pos, msg))
  def errorMessage[A](pos: Pos, msg: String): Seq[Error] = Seq(Error(pos, msg))

  sealed abstract class ValueLike
  object ValueLike {
    case class TopLevel(ref: PackageMember) extends ValueLike
    case class Value(ref: VarRef) extends ValueLike
  }

  case class Ctx(
    penv: PackageEnv,
    currentModule: ModuleRef,
    venv: Map[String, ValueLike] = Map(),
    stack: List[Ctx] = Nil) {
    val depth: Int = stack.size

    def tlet(name: Name): Result[Ctx] = {
      require(stack.isEmpty)
      val varRef = VarRef.ModuleMember(currentModule, name.value)
      if (venv.contains(name.value))
        Left(Seq(Error(name.pos, s"""Name "${name.value}" is already defined in ${currentModule.name}""")))
      else
        Right(copy(venv = venv + (varRef.name -> ValueLike.Value(varRef))))
    }

    def findValue(name: String): Option[ValueLike] =
      venv.get(name) orElse {
        penv.findMember(currentModule.pkg, name)
          .orElse(penv.findMember(PackageRef.Root, name))
          .map(ValueLike.TopLevel.apply)
      }

    def findPackageMember(pkg: PackageRef, name: String): Option[PackageMember] =
      penv.findMember(pkg, name)

    def findType(tname: TypeName): Result[Type] = tname match {
      case TypeName.Atom(n) => n match {
        case "int" => Right(Type.Int)
        case "bool" => Right(Type.Bool)
        case unk => Left(Seq(Error(tname.pos, s"Type not found: $unk")))
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
        venv = venv + (name -> ValueLike.Value(ref)),
        stack = this :: stack))
    }

    def bindLocals(names: Seq[Name]): Result[(Seq[VarRef.Local], Ctx)] = {
      names.mapWithContextE(Set.empty[String]) { (a, name) =>
        if (a.contains(name.value))
          error(name.pos, s"Name conflict: ${name.value}")
        else
          Right((a + name.value, name.value))
      }.map { ns =>
        val refs = ns.zipWithIndex.map { case (_, i) => VarRef.Local(depth + 1, i + 1) }
        val c = copy(
          venv = venv ++ ns.zip(refs.map(ValueLike.Value)),
          stack = this :: stack)
        (refs, c)
      }
    }

    def addModuleMember(name: String): Ctx = copy(penv = penv.addModuleMember(currentModule, name))
    def findModuleMember(m: ModuleRef, name: Name): Result[VarRef.ModuleMember] =
      if (penv.moduleMemberExists(m, name.value))
        Right(VarRef.ModuleMember(m, name.value))
      else
        error(name.pos, s"Member ${name.value} is not found in module ${m.fullName}")

    // TODO: Support type names
    // TODO: Support relative
    def addImport(i: Import): Result[Ctx] = {
      val nameParts = i.qname.parts
      val aliasName = i.qname.parts.last.value
      findPackageMember(PackageRef.Root, nameParts.head.value)
        .toRight(errorMessage(i.qname.pos, s"Member not found: ${nameParts.head.value}"))
        .flatMap { head =>
          nameParts.tail.foldLeft[Result[ValueLike]](Right(ValueLike.TopLevel(head))) { (v, n) =>
            v.flatMap {
              case ValueLike.TopLevel(pm) => pm match {
                case PackageMember.Package(ref) =>
                  findPackageMember(ref, n.value)
                    .toRight(errorMessage(n.pos, s"Package member not found: ${n.value}"))
                    .map(ValueLike.TopLevel)
                case PackageMember.Module(m) =>
                  findModuleMember(m, n).map(ValueLike.Value)
                case PackageMember.Class(ref) =>
                  error(n.pos, s"${ref.fullName} is class and field reference not supported")
              }
              case ValueLike.Value(ref) =>
                error(n.pos, "Property of value is not importable")
            }
          }.map { v =>
            copy(venv = venv + (aliasName -> v))
          }
        }
    }
  }
}
