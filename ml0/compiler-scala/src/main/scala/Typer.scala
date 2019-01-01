package com.todesking.ojaml.ml0.compiler.scala

class Typer {
  import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => RT, TAST => TT }
  import Typer.Result
  import Compiler.Error

  def typeProgram(p: RT.Program): Result[TT.Program] =
    typeStruct(p.item).right.map(TT.Program(p.pkg, _))

  def typeStruct(s: RT.Struct): Result[TT.Struct] =
    validate(s.body.map(typeTerm)).flatMap(mkStruct(s.name, _))

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

  def typeTerm(t: RT.Term): Result[TT.Term] = t match {
    case RT.TLet(name, expr) =>
      for {
        e <- typeExpr(expr)
      } yield TT.TLet(name, e.tpe, e)
    case e: RT.Expr => typeExpr(e)
  }

  def typeExpr(e: RT.Expr): Result[TT.Expr] = e match {
    case RT.LitInt(v) => Right(TT.LitInt(v))
  }

}
object Typer {
  type Result[A] = Either[Seq[Compiler.Error], A]
}
