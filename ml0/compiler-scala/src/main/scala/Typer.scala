package com.todesking.ojaml.ml0.compiler.scala

class Typer {
  import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => RT, TAST => TT }
  import Typer.Result
  import Compiler.Error
  import Typer.Ctx

  implicit class SeqOps[A](self: Seq[A]) {
    def mapWith[B](init: B)(f: (B, A) => (B, A)): (B, Seq[A]) = ???
  }

  def typeProgram(p: RT.Program): Result[TT.Program] =
    typeStruct(p.pkg, p.item).right.map(TT.Program(p.pkg, _))

  def typeStruct(pkg: QName, s: RT.Struct): Result[TT.Struct] = {
    val currentModule = ModuleRef(pkg.value, s.name.value)
    val init = Typer.Ctx(currentModule, Map(), Map())

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

  def typeExpr(ctx: Ctx, e: RT.Expr): Result[(Ctx, TT.Expr)] = e match {
    case RT.LitInt(v) => Right((ctx, TT.LitInt(v)))
    case RT.Ref(name) =>
      ctx.env.get(name.value).fold[Result[(Ctx, TT.Expr)]] {
        Left(Seq(Error(name.pos.location, name.pos.line, name.pos.col, s"Name not found: ${name.value}")))
      } { ref =>
        Right((ctx, TT.Ref(ref, ctx.varTable(ref))))
      }
  }

}
object Typer {
  import Compiler.Error

  type Result[A] = Either[Seq[Compiler.Error], A]

  case class Ctx(currentModule: ModuleRef, env: Map[String, VarRef], varTable: Map[VarRef, Type]) {
    def tlet(name: Name, tpe: Type): Result[Ctx] = {
      val varRef = VarRef.ModuleVar(currentModule, name.value)
      if (varTable.contains(varRef))
        Left(Seq(Error(name.pos.location, name.pos.line, name.pos.col, s"""Name "${name.value}" is already defined in ${currentModule.name}""")))
      else
        Right(copy(env = env + (varRef.name -> varRef), varTable = varTable + (varRef -> tpe)))
    }
  }

}
