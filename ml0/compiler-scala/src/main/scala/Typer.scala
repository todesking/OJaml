package com.todesking.ojaml.ml0.compiler.scala

class Typer(classRepo: ClassRepo) {
  import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => RT, TAST => TT }
  import Typer.Result
  import Compiler.Error
  import Typer.Ctx
  import Typer.QuasiValue
  import Typer.error

  def typeProgram(p: RT.Program): Result[TT.Program] =
    typeStruct(p.pkg, p.imports, p.item).right.map(TT.Program(p.pkg, _))

  def typeStruct(pkg: QName, imports: Seq[Import], s: RT.Struct): Result[TT.Struct] = {
    val currentModule = ModuleRef(pkg.value, s.name.value)
    val init = Typer.Ctx(classRepo, currentModule)
    imports.foldLeft[Result[Ctx]](Right(init)) { (c, i) =>
      c.flatMap(_.addImport(i))
    }.flatMap { ctx =>
      val (_, typed) =
        s.body.foldLeft((ctx, Seq.empty[Result[TT.Term]])) {
          case ((c, a), t) =>
            typeTerm(c, t).fold({ l =>
              (c, a :+ Left(l))
            }, {
              case (cc, tt) =>
                (cc, a :+ Right(tt))
            })
        }
      validate(typed).flatMap(mkStruct(s.name, _))
    }
  }

  def mkStruct(name: Name, terms: Seq[TT.Term]): Result[TT.Struct] = {
    val letNames = terms.collect { case TT.TLet(n, _, _) => n }
    val (errors, _) =
      letNames.foldLeft((Seq.empty[Error], Set.empty[String])) {
        case ((es, ns), n) =>
          if (ns(n.value)) (es :+ Error(n.pos, s"""Name "${n.value}" is already defined."""), ns)
          else (es, ns + n.value)
      }
    if (errors.isEmpty) Right(TT.Struct(name, terms))
    else Left(errors)
  }

  private[this] def validate[A](xs: Seq[Result[A]]): Result[Seq[A]] = {
    val rights = xs.collect { case Right(x) => x }
    if (rights.size == xs.size) Right(rights)
    else Left(xs.collect { case Left(x) => x }.flatten)
  }

  def typeTerm(ctx: Ctx, t: RT.Term): Result[(Ctx, TT.Term)] = t match {
    case RT.TLet(name, expr) =>
      for {
        e <- typeExpr(ctx, expr)
        c <- ctx.tlet(name, e.tpe)
      } yield {
        (c, TT.TLet(name, e.tpe, e))
      }
    case e: RT.Expr =>
      typeExpr(ctx, e).map { te => (ctx, te) }
  }

  def typeExpr(ctx: Ctx, expr: RT.Expr): Result[TT.Expr] = typeExprQ(ctx, expr).flatMap {
    case QuasiValue.ClassValue(sig) =>
      error(expr.pos, s"Class ${sig.name} is not a value")
    case QuasiValue.PackageValue(name) =>
      error(expr.pos, s"Package ${name} is not a value")
    case QuasiValue.Value(value) =>
      Right(value)
  }

  def okQ(expr: TT.Expr) = Right(QuasiValue.Value(expr))

  def varRefToQ(ctx: Ctx, v: VarRef) = v match {
    case VarRef.Class(sig) => Right(QuasiValue.ClassValue(sig))
    case VarRef.Package(name) => Right(QuasiValue.PackageValue(name))
    case ref @ VarRef.Module(m, name) => okQ(TT.ModuleVarRef(m, name, ctx.varTable(ref)))
    case ref @ VarRef.Local(name) => okQ(TT.LocalRef(ctx.localIndex(ref), ctx.varTable(ref)))
  }

  def typeExprQ(ctx: Ctx, expr: RT.Expr): Result[QuasiValue] = expr match {
    case RT.Ref(name) =>
      ctx.findValue(name.value).fold[Result[QuasiValue]] {
        error(name.pos, s"Value ${name.value} is not found")
      }(varRefToQ(ctx, _))
    case RT.Prop(e, n) =>
      typeExprQ(ctx, e).flatMap {
        case QuasiValue.PackageValue(name) =>
          ctx.findPackageMember(name, n.value).toRight(
            Seq(Error(n.pos, s"Value ${n.value} is not found in $name")))
        case QuasiValue.ClassValue(name) =>
          error(n.pos, s"Property not supported for class")
        case QuasiValue.Value(e) =>
          error(n.pos, s"Property not supported for value")
      }
    case RT.LitInt(v) => okQ(TT.LitInt(v))
    case RT.LitBool(v) => okQ(TT.LitBool(v))
    case RT.LitString(v) => okQ(TT.LitString(v))
    case RT.If(cond, th, el) =>
      for {
        e0 <- typeExpr(ctx, cond)
        e1 <- typeExpr(ctx, th)
        e2 <- typeExpr(ctx, el)
        ret <- if (e0.tpe != Type.Bool) {
          error(cond.pos, s"Condition must be bool")
        } else if (e1.tpe != e2.tpe) {
          error(expr.pos, s"Then clause has thpe ${e1.tpe} but else clause has type ${e2.tpe}")
        } else {
          okQ(TT.If(e0, e1, e2, e1.tpe))
        }
      } yield ret
    case RT.Fun(name, tpeName, body) =>
      (for {
        tpe <- ctx.findType(tpeName)
        tbody <- typeExpr(ctx.newFrame(name.value, tpe), body)
      } yield {
        TT.Fun(tpe, tbody)
      }).map(QuasiValue.Value.apply)
    case RT.App(f, x) =>
      for {
        tf <- typeExpr(ctx, f)
        tx <- typeExpr(ctx, x)
        ret <- tf.tpe match {
          case Type.Fun(l, r) =>
            if (l == tx.tpe)
              okQ(TT.App(tf, tx, r))
            else
              error(x.pos, s"Argument type mismatch: $l required but ${tx.tpe} found")
          case t =>
            error(f.pos, s"Applying non-function type ${t}")
        }
      } yield ret
    case RT.JCall(expr, name, args, isStatic) =>
      validate(args.map(typeExpr(ctx, _))).flatMap { targs =>
        if (isStatic) {
          typeExprQ(ctx, expr).flatMap {
            case QuasiValue.PackageValue(name) =>
              error(expr.pos, s"Class required but $name is package")
            case QuasiValue.Value(e) =>
              error(expr.pos, s"Class required")
            case QuasiValue.ClassValue(klass) =>
              klass.findStaticMethod(name.value, targs.map(_.tpe)).fold[Result[QuasiValue]] {
                error(name.pos, s"Static method ${Type.prettyMethod(name.value, targs.map(_.tpe))} is not found in class ${klass.name}")
              } { method =>
                okQ(TT.JCallStatic(method, targs))
              }
          }
        } else {
          typeExpr(ctx, expr).flatMap { receiver =>
            ctx.findClass(receiver.tpe.boxed.className).fold[Result[QuasiValue]] {
              error(expr.pos, s"Class ${receiver.tpe.boxed.className} not found. Check classpath.")
            } { klass =>
              klass.findInstanceMethod(name.value, targs.map(_.tpe)).fold[Result[QuasiValue]] {
                error(name.pos, s"Instance method ${Type.prettyMethod(name.value, targs.map(_.tpe))} is not found in class ${receiver.tpe.boxed.className}")
              } { method =>
                okQ(TT.JCallInstance(method, receiver, targs))
              }
            }
          }
        }
      }
  }
}
object Typer {
  import Compiler.Error

  def error(pos: Pos, msg: String) = Left(Seq(Error(pos, msg)))

  sealed abstract class QuasiValue
  object QuasiValue {
    case class ClassValue(sig: ClassSig) extends QuasiValue
    case class PackageValue(name: String) extends QuasiValue
    case class Value(expr: TAST.Expr) extends QuasiValue
  }

  type Result[A] = Either[Seq[Compiler.Error], A]

  object Ctx {
    def apply(repo: ClassRepo, currentModule: ModuleRef): Ctx =
      apply(
        repo = repo,
        currentModule = currentModule,
        venv = Map(),
        locals = Map(),
        varTable = Map(),
        stack = Nil)
  }
  case class Ctx(
    repo: ClassRepo,
    currentModule: ModuleRef,
    venv: Map[String, VarRef],
    locals: Map[VarRef.Local, Int], // todo: add depth and type to VarRef.Local
    varTable: Map[VarRef.Typable, Type],
    stack: List[Ctx]) {
    val depth = stack.size

    def localIndex(l: VarRef.Local) = locals(l)

    def tlet(name: Name, tpe: Type): Result[Ctx] = {
      val varRef = VarRef.Module(currentModule, name.value)
      if (varTable.contains(varRef))
        Left(Seq(Error(name.pos, s"""Name "${name.value}" is already defined in ${currentModule.name}""")))
      else
        Right(copy(venv = venv + (varRef.name -> varRef), varTable = varTable + (varRef -> tpe)))
    }

    def findValue(name: String): Option[VarRef] =
      venv.get(name) orElse {
        repo.find(name).map(VarRef.Class.apply)
      } orElse {
        Some(VarRef.Package(name))
      }
    def findPackageMember(pkg: String, name: String): Option[QuasiValue] = {
      repo.find(s"$pkg/$name").fold[Option[QuasiValue]] {
        Some(QuasiValue.PackageValue(s"$pkg/$name"))
      } { klass =>
        Some(QuasiValue.ClassValue(klass))
      }
    }

    def findType(name: Name): Result[Type] = name.value match {
      case "int" => Right(Type.Int)
      case "bool" => Right(Type.Bool)
      case unk =>
        Left(Seq(Error(name.pos, s"Type not found: ${name.value}")))
    }

    def findClass(name: String): Option[ClassSig] = repo.find(name)

    def newFrame(name: String, tpe: Type): Ctx = {
      val ref = VarRef.Local(name)
      copy(
        venv = venv + (name -> ref),
        varTable = varTable + (ref -> tpe),
        locals = locals + (ref -> depth),
        stack = this :: stack)
    }

    def dropFrame(): Ctx =
      stack.head

    def addImport(i: Import): Result[Ctx] = {
      val name = i.qname.asClass
      val aliasName = i.qname.parts.last.value
      findValue(name).toRight(
        Seq(Error(i.qname.pos, s"Name not found: ${i.qname.value}"))).map { v =>
          copy(venv = venv + (aliasName -> v))
        }
    }
  }

}
