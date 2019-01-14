package com.todesking.ojaml.ml0.compiler.scala

import Compiler.Error

class Namer(repo: ClassRepo) {
  import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => RT, NamedAST => NT }
  import Namer.Result
  import Namer.Ctx
  import Namer.error

  def appProgram(p: RT.Program): Result[NT.Struct] =
    appStruct(p.pkg, p.imports, p.item)

  def appStruct(pkg: QName, imports: Seq[Import], s: RT.Struct): Result[NT.Struct] = {
    val currentModule = ModuleRef(pkg.value, s.name.value)
    val init = Ctx(repo, currentModule)
    imports.foldLeft[Result[Ctx]](Right(init)) { (c, i) =>
      c.flatMap(_.addImport(i))
    }.flatMap { ctx =>
      val (_, named) =
        s.body.foldLeft[(Ctx, Result[Seq[NT.Term]])]((ctx, Right(Seq.empty[NT.Term]))) {
          case ((c, Right(a)), t) =>
            appTerm(c, t).fold({ l =>
              (c, Left(l))
            }, {
              case (cc, tt) =>
                (cc, Right(a :+ tt))
            })
          case ((c, Left(e)), t) =>
            (c, Left(e))
        }
      named.map(NT.Struct(pkg, s.name, _)).map(Pos.fill(_, s.pos))
    }
  }

  def appTerm(ctx: Ctx, t: RT.Term): Result[(Ctx, NT.Term)] = {
    appTerm0(ctx, t).map { case (ctx, tt) => tt.fillPos(t.pos); (ctx, tt) }
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

  def appExpr(ctx: Ctx, expr: RT.Expr): Result[NT.Expr] =
    appExpr0(ctx, expr).map(Pos.fill(_, expr.pos))

  private[this] def appExpr0(ctx: Ctx, expr: RT.Expr): Result[NT.Expr] = expr match {
    case RT.Ref(name) =>
      ctx.findValue(name.value).toRight(
        Seq(Error(name.pos, s"Value ${name.value} is not found"))).map(NT.Ref(_))
    case RT.Prop(e, n) =>
      appExpr(ctx, e).flatMap {
        case NT.Ref(r) =>
          r match {
            case VarRef.Class(name) =>
              error(n.pos, s"Property not supported for class")
            case VarRef.Package(name) =>
              ctx.findPackageMember(name, n.value).map(NT.Ref(_)).toRight(
                Seq(Error(n.pos, s"Value ${n.value} is not found in $name")))
            case VarRef.Module(m, name) =>
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

object Namer {
  type Result[A] = Either[Seq[Compiler.Error], A]
  def error[A](pos: Pos, msg: String): Result[A] = Left(Seq(Error(pos, msg)))

  object Ctx {
    def apply(repo: ClassRepo, currentModule: ModuleRef): Ctx =
      apply(
        repo = repo,
        currentModule = currentModule,
        venv = Map(),
        locals = Map(),
        stack = Nil)
  }
  case class Ctx(
    repo: ClassRepo,
    currentModule: ModuleRef,
    venv: Map[String, VarRef],
    locals: Map[VarRef.Local, Int], // todo: add depth and type to VarRef.Local
    stack: List[Ctx]) {
    val depth = stack.size

    def localIndex(l: VarRef.Local) = locals(l)

    def tlet(name: Name): Result[Ctx] = {
      require(stack.isEmpty)
      val varRef = VarRef.Module(currentModule, name.value)
      if (venv.contains(name.value))
        Left(Seq(Error(name.pos, s"""Name "${name.value}" is already defined in ${currentModule.name}""")))
      else
        Right(copy(venv = venv + (varRef.name -> varRef)))
    }

    def findValue(name: String): Option[VarRef] =
      venv.get(name) orElse {
        repo.find(name).map { klass => VarRef.Class(klass.name) }
      } orElse {
        Some(VarRef.Package(name))
      }
    def findPackageMember(pkg: String, name: String): Option[VarRef.Special] = {
      repo.find(s"$pkg/$name").fold[Option[VarRef.Special]] {
        Some(VarRef.Package(s"$pkg/$name"))
      } { klass =>
        Some(VarRef.Class(klass.name))
      }
    }

    def findType(name: Name): Result[Type] = name.value match {
      case "int" => Right(Type.Int)
      case "bool" => Right(Type.Bool)
      case unk =>
        Left(Seq(Error(name.pos, s"Type not found: ${name.value}")))
    }

    def findClass(name: String): Option[ClassSig] = repo.find(name)

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
