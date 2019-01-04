package com.todesking.ojaml.ml0.compiler.scala

class Typer {
  import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => RT, TAST => TT }
  import Typer.Result
  import Compiler.Error
  import Typer.Ctx

  def typeProgram(p: RT.Program): Result[TT.Program] =
    typeStruct(p.pkg, p.item).right.map(TT.Program(p.pkg, _))

  def typeStruct(pkg: QName, s: RT.Struct): Result[TT.Struct] = {
    val currentModule = ModuleRef(pkg.value, s.name.value)
    val init = Typer.Ctx(currentModule, Map(), Map(), Map(), Nil)

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

  def mkStruct(name: Name, terms: Seq[TT.Term]): Result[TT.Struct] = {
    val letNames = terms.collect { case TT.TLet(n, _, _) => n }
    val (errors, _) =
      letNames.foldLeft((Seq.empty[Error], Set.empty[String])) {
        case ((es, ns), n) =>
          if (ns(n.value)) (es :+ Error(n.pos.location, n.pos.line, n.pos.col, s"""Name "${n.value}" is already defined."""), ns)
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
      typeExpr(ctx, expr).flatMap {
        case (c, e) =>
          val tpe = e.tpe
          c.tlet(name, tpe).map { c => (c, TT.TLet(name, tpe, e)) }
      }
    case e: RT.Expr => typeExpr(ctx, e)
  }

  def typeExpr(ctx: Ctx, expr: RT.Expr): Result[(Ctx, TT.Expr)] = expr match {
    case RT.LitInt(v) => Right((ctx, TT.LitInt(v)))
    case RT.LitBool(v) => Right((ctx, TT.LitBool(v)))
    case RT.LitString(v) => Right((ctx, TT.LitString(v)))
    case RT.Ref(name) =>
      ctx.lookupVar(name.value).fold[Result[(Ctx, TT.Expr)]] {
        Left(Seq(Error(name.pos.location, name.pos.line, name.pos.col, s"Name not found: ${name.value}")))
      } {
        case ref @ VarRef.Module(m, n) =>
          Right((ctx, TT.ModuleVarRef(m, n, ctx.varTable(ref))))
        case ref @ VarRef.Local(_) =>
          Right((ctx, TT.LocalRef(ctx.localIndex(ref), ctx.varTable(ref))))
      }
    case RT.If(cond, th, el) =>
      typeExpr(ctx, cond).flatMap {
        case (c, e0) =>
          typeExpr(c, th).flatMap {
            case (c, e1) =>
              typeExpr(c, el).flatMap {
                case (c, e2) =>
                  if (e0.tpe != Type.Bool) {
                    Left(Seq(Error(cond.pos.location, cond.pos.line, cond.pos.col, s"Condition must be bool")))
                  } else if (e1.tpe != e2.tpe) {
                    Left(Seq(Error(expr.pos.location, expr.pos.line, expr.pos.col, s"Then clause has thpe ${e1.tpe} but else clause has type ${e2.tpe}")))
                  } else {
                    Right((c, TT.If(e0, e1, e2, e1.tpe)))
                  }
              }
          }
      }
    case RT.Fun(name, tpeName, body) =>
      ctx.findType(tpeName).flatMap { tpe =>
        typeExpr(ctx.newFrame(name.value, tpe), body).map {
          case (ctx, tbody) =>
            (ctx.dropFrame(), TT.Fun(tpe, tbody))
        }
      }
    case RT.App(f, x) =>
      typeExpr(ctx, f).flatMap {
        case (c, tf) =>
          typeExpr(c, x).flatMap {
            case (c, tx) =>
              tf.tpe match {
                case Type.Fun(l, r) =>
                  if (l == tx.tpe)
                    Right((c, TT.App(tf, tx, r)))
                  else
                    Left(Seq(Error(x.pos.location, x.pos.line, x.pos.col, s"Argument type mismatch: $l required but ${tx.tpe} found")))
                case t =>
                  Left(Seq(Error(f.pos.location, f.pos.line, f.pos.col, s"Applying non-function type ${t}")))
              }
          }
      }
  }

}
object Typer {
  import Compiler.Error

  type Result[A] = Either[Seq[Compiler.Error], A]

  case class Ctx(
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
        Left(Seq(Error(name.pos.location, name.pos.line, name.pos.col, s"""Name "${name.value}" is already defined in ${currentModule.name}""")))
      else
        Right(copy(env = env + (varRef.name -> varRef), varTable = varTable + (varRef -> tpe)))
    }

    def findType(name: Name): Result[Type] = name.value match {
      case "int" => Right(Type.Int)
      case "bool" => Right(Type.Bool)
      case unk =>
        Left(Seq(Error(name.pos.location, name.pos.line, name.pos.col, s"Type not found: ${name.value}")))
    }

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
