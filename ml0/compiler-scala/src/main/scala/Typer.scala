package com.todesking.ojaml.ml0.compiler.scala

class Typer(classRepo: ClassRepo) {
  import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => RT, TAST => TT }
  import Typer.Result
  import Compiler.Error
  import Typer.Ctx

  def typeProgram(p: RT.Program): Result[TT.Program] =
    typeStruct(p.pkg, p.item).right.map(TT.Program(p.pkg, _))

  def typeStruct(pkg: QName, s: RT.Struct): Result[TT.Struct] = {
    val currentModule = ModuleRef(pkg.value, s.name.value)
    val init = Typer.Ctx(classRepo, currentModule, Map(), Map(), Map(), Nil)

    val (ctx, typed) =
      s.body.foldLeft((init, Seq.empty[Result[TT.Term]])) {
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

  private[this] def error(pos: Pos, msg: String) = Left(Seq(Error(pos, msg)))

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

  def typeExpr(ctx: Ctx, expr: RT.Expr): Result[TT.Expr] = expr match {
    case RT.LitInt(v) => Right(TT.LitInt(v))
    case RT.LitBool(v) => Right(TT.LitBool(v))
    case RT.LitString(v) => Right(TT.LitString(v))
    case RT.Ref(name) =>
      ctx.lookupVar(name.value).fold[Result[TT.Expr]] {
        error(name.pos, s"Name not found: ${name.value}")
      } {
        case ref @ VarRef.Module(m, n) =>
          Right(TT.ModuleVarRef(m, n, ctx.varTable(ref)))
        case ref @ VarRef.Local(_) =>
          Right(TT.LocalRef(ctx.localIndex(ref), ctx.varTable(ref)))
      }
    case RT.Prop(e, n) =>
      error(expr.pos, "Not a value")
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
          Right(TT.If(e0, e1, e2, e1.tpe))
        }
      } yield ret
    case RT.Fun(name, tpeName, body) =>
      for {
        tpe <- ctx.findType(tpeName)
        tbody <- typeExpr(ctx.newFrame(name.value, tpe), body)
      } yield {
        TT.Fun(tpe, tbody)
      }
    case RT.App(f, x) =>
      for {
        tf <- typeExpr(ctx, f)
        tx <- typeExpr(ctx, x)
        ret <- tf.tpe match {
          case Type.Fun(l, r) =>
            if (l == tx.tpe)
              Right(TT.App(tf, tx, r))
            else
              error(x.pos, s"Argument type mismatch: $l required but ${tx.tpe} found")
          case t =>
            error(f.pos, s"Applying non-function type ${t}")
        }
      } yield ret
    case RT.JCall(expr, name, args, isStatic) =>
      validate(args.map(typeExpr(ctx, _))).flatMap { targs =>
        if (isStatic) {
          extractQname(expr).flatMap { qn =>
            ctx.findClass(qn.asClass).fold[Result[TT.Expr]] {
              error(qn.pos, s"Class not found: ${qn.value}")
            } { klass =>
              klass.findStaticMethod(name.value, targs.map(_.tpe)).fold[Result[TT.Expr]] {
                error(name.pos, s"Static method ${Type.prettyMethod(name.value, targs.map(_.tpe))} is not found in class ${qn.value}")
              } { method =>
                Right(TT.JCallStatic(method, targs))
              }
            }
          }
        } else {
          typeExpr(ctx, expr).flatMap { receiver =>
            ctx.findClass(receiver.tpe.boxed.className).fold[Result[TT.Expr]] {
              error(expr.pos, s"Class ${receiver.tpe.boxed.className} not found. Check classpath.")
            } { klass =>
              klass.findInstanceMethod(name.value, targs.map(_.tpe)).fold[Result[TT.Expr]] {
                error(name.pos, s"Instance method ${Type.prettyMethod(name.value, targs.map(_.tpe))} is not found in class ${receiver.tpe.boxed.className}")
              } { method =>
                Right(TT.JCallInstance(method, receiver, targs))
              }
            }
          }
        }
      }
  }

  def extractQname(expr: RT.Expr): Result[QName] = {
    def gather(e: RT.Expr): Result[Seq[Name]] = e match {
      case RT.Prop(e, name) => gather(e).map { ns => ns :+ name }
      case RT.Ref(name) => Right(Seq(name))
      case other => error(expr.pos, "Not a java class name")
    }
    gather(expr).map { ns =>
      val p = ns.head.pos
      val qn = QName(ns)
      qn.fillPos(p.location, p.line, p.col)
      qn
    }
  }

}
object Typer {
  import Compiler.Error

  type Result[A] = Either[Seq[Compiler.Error], A]

  case class Ctx(
    repo: ClassRepo,
    currentModule: ModuleRef,
    env: Map[String, VarRef],
    locals: Map[VarRef.Local, Int],
    varTable: Map[VarRef, Type],
    stack: List[Ctx]) {
    val depth = stack.size

    def lookupVar(name: String): Option[VarRef] =
      env.get(name)

    def localIndex(l: VarRef.Local) = locals(l)

    def tlet(name: Name, tpe: Type): Result[Ctx] = {
      val varRef = VarRef.Module(currentModule, name.value)
      if (varTable.contains(varRef))
        Left(Seq(Error(name.pos, s"""Name "${name.value}" is already defined in ${currentModule.name}""")))
      else
        Right(copy(env = env + (varRef.name -> varRef), varTable = varTable + (varRef -> tpe)))
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
        env = env + (name -> ref),
        varTable = varTable + (ref -> tpe),
        locals = locals + (ref -> depth),
        stack = this :: stack)
    }

    def dropFrame(): Ctx =
      stack.head
  }

}
