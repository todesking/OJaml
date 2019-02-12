package com.todesking.ojaml.ml0.compiler.scala

import Compiler.Error
import Util.SeqSyntax

class Namer(packageEnv: PackageEnv) {
  import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => RT, NamedAST => NT }
  import Namer.Result
  import Namer.Ctx
  import Namer.error
  import Util.SeqSyntax

  def appProgram(p: RT.Program): Result[(PackageEnv, Seq[NT.Struct])] =
    p.items.foldLeftE(packageEnv) { (penv, x) =>
      appStruct(p.pkg, p.imports, penv, x).map {
        case (pe, named) =>
          (pe, named)
      }
    }

  def appStruct(pkg: QName, imports: Seq[Import], penv: PackageEnv, s: RT.Struct): Result[(PackageEnv, NT.Struct)] = {
    val currentModule = ModuleRef(PackageRef.fromInternalName(pkg.internalName), s.name.value)
    if (penv.memberExists(currentModule.pkg, currentModule.name))
      return Left(Seq(Error(s.name.pos, s"${currentModule.fullName} already defined")))
    val init = Ctx(penv.addModule(currentModule), currentModule)
    imports.foldLeft[Result[Ctx]](Right(init)) { (c, i) =>
      c.flatMap(_.addImport(i))
    }.flatMap { ctx =>
      s.body.foldLeftE(ctx) {
        case (c, t) =>
          appTerm(c, t)
      }.map {
        case (c, ts) =>
          (c.penv, Pos.fill(NT.Struct(pkg, s.name, ts), s.pos))
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
                  Seq(Error(n.pos, s"Value ${n.value} is not found in package ${p.fullName}")))
              case PackageMember.Module(m) =>
                // TODO: check member existence
                ctx.findModuleMember(m, n).map(NT.Ref.apply)
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

case class PackageEnv(cr: ClassRepo, moduleMembers: Map[ModuleRef, Set[String]] = Map()) {
  def findMember(pkg: PackageRef, name: String): Option[PackageMember] = {
    if (modules.get(pkg).exists(_.contains(name)))
      Some(PackageMember.Module(ModuleRef(pkg, name)))
    else if (cr.classExists(pkg, name))
      Some(PackageMember.Class(ClassRef(pkg, name)))
    else if (packageExists(pkg, name))
      Some(PackageMember.Package(pkg.packageRef(name)))
    else
      None
  }

  lazy val modulePackages = moduleMembers.keys.flatMap { m =>
    m.pkg.parts.inits.map(PackageRef.fromParts)
  }.toSet

  lazy val modules = moduleMembers.keys.map { m =>
    m.pkg -> m.name
  }.groupBy(_._1).map { case (p, ns) => p -> ns.map(_._2).toSet }

  def packageExists(pkg: PackageRef, name: String) =
    cr.packageExists(pkg, name) || modulePackages.contains(pkg.packageRef(name))

  def memberExists(pkg: PackageRef, name: String) =
    findMember(pkg, name).nonEmpty

  def addModule(m: ModuleRef) =
    if (moduleMembers.contains(m)) this
    else copy(moduleMembers = moduleMembers + (m -> Set.empty[String]))

  def addModuleMember(m: ModuleRef, name: String) = {
    require(modules.get(m.pkg).exists(_.contains(m.name)), s"Module ${m.fullName} not found")
    moduleMembers.get(m).fold {
      copy(moduleMembers = moduleMembers + (m -> Set(name)))
    } { ms =>
      copy(moduleMembers = moduleMembers + (m -> (ms + name)))
    }
  }

  def moduleMemberExists(m: ModuleRef, name: String) =
    moduleMembers.get(m).exists(_.contains(name))

  def pretty = "PackageEnv\n" + modules.toSeq.sortBy(_._1.fullName).map {
    case (k, vs) =>
      s"  package ${k.fullName}\n" + vs.toSeq.sorted.map { v => s"  - module $v" }.mkString("\n")
  }.mkString("\n")
}

object Namer {
  type Result[A] = Either[Seq[Compiler.Error], A]
  def error[A](pos: Pos, msg: String): Result[A] = Left(Seq(Error(pos, msg)))

  case class Ctx(
    penv: PackageEnv,
    currentModule: ModuleRef,
    venv: Map[String, VarRef] = Map(),
    locals: Map[VarRef.Local, Int] = Map(),
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

    def findValue(name: String): Option[VarRef] =
      venv.get(name) orElse {
        penv.findMember(currentModule.pkg, name)
          .orElse(penv.findMember(PackageRef.Root, name))
          .map(VarRef.TopLevel.apply)
      }

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

    def addModuleMember(name: String) = copy(penv = penv.addModuleMember(currentModule, name))
    def findModuleMember(m: ModuleRef, name: Name): Result[VarRef.ModuleMember] =
      if (penv.moduleMemberExists(m, name.value))
        Right(VarRef.ModuleMember(m, name.value))
      else
        Left(Seq(Error(name.pos, s"Member ${name.value} is not found in module ${m.fullName}")))

    def addImport(i: Import): Result[Ctx] = {
      val nameParts = i.qname.parts
      val aliasName = i.qname.parts.last.value
      findPackageMember(PackageRef.Root, nameParts.head.value).toRight(Seq(Error(i.qname.pos, s"Value not found: ${nameParts.head.value}"))).flatMap { head =>
        nameParts.tail.foldLeft[Result[VarRef]](Right(head)) { (v, n) =>
          v.flatMap {
            case VarRef.TopLevel(pm) => pm match {
              case PackageMember.Package(ref) =>
                findPackageMember(ref, n.value).toRight(
                  Seq(Error(n.pos, s"Package member not found: ${n.value}")))
              case PackageMember.Module(m) =>
                findModuleMember(m, n)
              case PackageMember.Class(ref) =>
                Left(Seq(Error(n.pos, s"${ref.fullName} is class and field reference not supported")))
            }
            case VarRef.ModuleMember(ref, name) =>
              Left(Seq(Error(n.pos, s"$name is value")))
            case VarRef.Local(_) =>
              throw new AssertionError(s"WTF: $v, $n")
          }
        }.map { v =>
          copy(venv = venv + (aliasName -> v))
        }
      }
    }
  }
}
