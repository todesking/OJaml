package com.todesking.ojaml.ml0.compiler.scala

import Compiler.Error

class Namer(packageEnv: PackageEnv) {
  import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => RT, NamedAST => NT }
  import Namer.Result
  import Namer.Ctx
  import Namer.error
  import Util.MapWithContext

  def memberNames(s: NT.Struct): Set[String] = s.body.collect {
    case NT.TLet(name, _) => name.value
  }.toSet

  def appProgram(p: RT.Program): Result[Seq[NT.Struct]] =
    p.items.mapWithContextE(packageEnv) { (penv, x) =>
      appStruct(p.pkg, p.imports, penv, x).map { named =>
        (penv.addModule(named.moduleRef), named)
      }
    }

  def appStruct(pkg: QName, imports: Seq[Import], penv: PackageEnv, s: RT.Struct): Result[NT.Struct] = {
    val currentModule = ModuleRef(PackageRef.fromInternalName(pkg.internalName), s.name.value)
    val init = Ctx(penv, currentModule)
    imports.foldLeft[Result[Ctx]](Right(init)) { (c, i) =>
      c.flatMap(_.addImport(i))
    }.flatMap { ctx =>
      s.body.mapWithContextE(ctx) {
        case (c, t) =>
          appTerm(c, t)
      }.map(NT.Struct(pkg, s.name, _)).map(Pos.fill(_, s.pos))
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
        (c, NT.TLet(name, e))
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
      ctx.findValue(name.value).toRight(
        Seq(Error(name.pos, s"Value ${name.value} is not found"))).map(NT.Ref(_))
    case RT.Prop(e, n) =>
      appExpr(ctx, e).flatMap {
        case NT.Ref(r) =>
          r match {
            case VarRef.TopLevel(pm) => pm match {
              case PackageMember.Class(c) =>
                error(n.pos, s"Property not supported for class")
              case PackageMember.Package(p) =>
                ctx.findPackageMember(p, n.value).map(NT.Ref(_)).toRight(
                  Seq(Error(n.pos, s"Value ${n.value} is not found in ${p.fullName}")))
              case PackageMember.Module(m) =>
                Right(NT.Ref(VarRef.ModuleMember(m, n.value)))
            }
            case VarRef.ModuleMember(m, name) =>
              error(n.pos, s"Property not supported")
            case VarRef.Local(name) =>
              error(n.pos, s"Property not supported")
          }
        case e =>
          error(n.pos, s"Property not supported")
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
      (for {
        tpe <- ctx.findType(tpeName)
        (ref, c) = ctx.bindLocal(name.value)
        tbody <- appExpr(c, body)
      } yield {
        NT.Fun(ref, tpe, tbody)
      })
    case RT.App(f, x) =>
      for {
        e0 <- appExpr(ctx, f)
        e1 <- appExpr(ctx, x)
      } yield NT.App(e0, e1)
    case RT.JCall(expr, name, args, isStatic) =>
      validate(args.map(appExpr(ctx, _))).flatMap { targs =>
        appExpr(ctx, expr).map { receiver =>
          NT.JCall(receiver, name, targs, isStatic)
        }
      }
  }
}

sealed abstract class PackageMember
object PackageMember {
  case class Package(ref: PackageRef) extends PackageMember
  case class Class(ref: ClassRef) extends PackageMember
  case class Module(ref: ModuleRef) extends PackageMember
}

case class PackageEnv(cr: ClassRepo, modules: Map[PackageRef, Set[String]] = Map()) {
  def findMember(pkg: PackageRef, name: String): Option[PackageMember] =
    if(modules.get(pkg).exists(_.contains(name)))
      Some(PackageMember.Module(ModuleRef(pkg, name)))
    else if(cr.classExists(pkg, name))
      Some(PackageMember.Class(ClassRef(pkg, name)))
    else Some(PackageMember.Package(pkg.packageRef(name))) // TODO: Check package existence
  def addModule(m: ModuleRef) = modules.get(m.pkg).fold {
    copy(modules = Map(m.pkg -> Set(m.name)))
  } { names =>
    copy(modules = Map(m.pkg -> (names + m.name)))
  }
}

object Namer {
  type Result[A] = Either[Seq[Compiler.Error], A]
  def error[A](pos: Pos, msg: String): Result[A] = Left(Seq(Error(pos, msg)))

  case class Ctx(
    penv: PackageEnv,
    currentModule: ModuleRef,
    venv: Map[String, VarRef] = Map(),
    locals: Map[VarRef.Local, Int] = Map(),
    moduleMembers: Map[ModuleRef, Set[String]] = Map(),
    stack: List[Ctx] = Nil) {
    val depth = stack.size

    def localIndex(l: VarRef.Local) = locals(l)

    def tlet(name: Name): Result[Ctx] = {
      require(stack.isEmpty)
      val varRef = VarRef.ModuleMember(currentModule, name.value)
      if (venv.contains(name.value))
        Left(Seq(Error(name.pos, s"""Name "${name.value}" is already defined in ${currentModule.name}""")))
      else
        Right(copy(venv = venv + (varRef.name -> varRef)))
    }

    // TODO: check name conflict
    def bindModules(ms: Map[ModuleRef, Set[String]]): Ctx =
      copy(
        penv = ms.keys.foldLeft(penv) { (e, m) => e.addModule(m) },
        moduleMembers = moduleMembers ++ ms
      )
    def findValue(name: String): Option[VarRef] =
      venv.get(name) orElse penv.findMember(PackageRef.Root, name).map(VarRef.TopLevel.apply)

    def findPackageMember(pkg: PackageRef, name: String): Option[VarRef.TopLevel] =
      penv.findMember(pkg, name).map(VarRef.TopLevel.apply)

    def findType(name: Name): Result[Type] = name.value match {
      case "int" => Right(Type.Int)
      case "bool" => Right(Type.Bool)
      case unk =>
        Left(Seq(Error(name.pos, s"Type not found: ${name.value}")))
    }

    def bindLocal(name: String): (VarRef.Local, Ctx) = {
      val ref = VarRef.Local(depth + 1)
      (ref, copy(
        venv = venv + (name -> ref),
        locals = locals + (ref -> depth),
        stack = this :: stack))
    }

    def dropFrame(): Ctx =
      stack.head

    def addImport(i: Import): Result[Ctx] = {
      val name = i.qname.internalName
      val aliasName = i.qname.parts.last.value
      findValue(name).toRight(
        Seq(Error(i.qname.pos, s"Name not found: ${i.qname.value}"))).map { v =>
          copy(venv = venv + (aliasName -> v))
        }
    }
  }
}
