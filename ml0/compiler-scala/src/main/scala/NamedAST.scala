package com.todesking.ojaml.ml0.compiler.scala

import com.todesking.ojaml.ml0.compiler.scala.util.pretty
import pretty.PrettyPrinter
import pretty.Doc
import pretty.PrettySyntax._

sealed abstract class NamedAST {
  val pos: Pos
}
object NamedAST {
  def pretty(ast: NamedAST): String =
    PrettyPrinter.pretty(80, prettyDoc(ast, false))
  def prettyDoc(ast: NamedAST, paren: Boolean): Doc = ast match {
    case Module(pos, pkg, name, body) =>
      P.module(s"$pkg.$name", body.map(prettyDoc(_, false)))
    case TLet(pos, name, tpe, expr) =>
      P.tlet(name, tpe.map(_.toString), prettyDoc(expr, false))
    case Data(pos, name, tpe, ctors) =>
      P.data(name, Seq(), ctors.map { case (n, ts) => (n.value, ts.map(_.toString)) })
    case TExpr(pos, e) =>
      P.group(prettyDoc(e, false), ";;")
    case LitInt(pos, value) =>
      Doc.Text(value.toString)
    case LitBool(pos, value) =>
      Doc.Text(value.toString)
    case LitString(pos, value) =>
      s""""$value"""".doc
    case Ref(pos, ref) =>
      ref.toString.doc
    case JCallStatic(pos, target, name, args) =>
      P.jcall(
        target.fullName.doc,
        name.value,
        args.map(prettyDoc(_, false)),
        true)
    case JCallInstance(pos, expr, name, args) =>
      P.jcall(
        prettyDoc(expr, true),
        name.value,
        args.map(prettyDoc(_, false)),
        false)
    case If(pos, cond, th, el) =>
      P.paren(
        paren,
        P.eif(
          prettyDoc(cond, false),
          prettyDoc(th, false),
          prettyDoc(el, false)))
    case Fun(pos, param, tpe, body) =>
      P.fun(param.toString, tpe.map(_.toString), prettyDoc(body, false), paren)
    case App(pos, fun, arg) =>
      P.app(paren, prettyDoc(fun, true), prettyDoc(arg, true))
    case ELet(pos, ref, value, body) =>
      P.elet(ref.toString, prettyDoc(value, false), prettyDoc(body, false))
    case ELetRec(pos, bindings, body) =>
      P.eletrec(
        bindings.map {
          case (n, tn, f) =>
            (n.toString, tn.map(_.toString), prettyDoc(f, false))
        },
        prettyDoc(body, false))
    case MatchError(pos) =>
      "<matcherror>".doc
  }

  case class Module(pos: Pos, pkg: QName, name: Name, body: Seq[Term]) extends NamedAST {
    def moduleRef = ModuleRef(pkg.asPackage, name.value)
  }

  sealed abstract class Term extends NamedAST
  case class TLet(pos: Pos, name: Name, tpe: Option[Type], expr: Expr) extends Term
  case class Data(pos: Pos, name: Name, tvars: Seq[Type.Var], ctors: Seq[(Name, Seq[Type])]) extends Term
  case class TExpr(pos: Pos, expr: Expr) extends Term

  sealed abstract class Expr extends NamedAST

  sealed abstract class Lit extends Expr
  case class LitInt(pos: Pos, value: Int) extends Lit
  case class LitBool(pos: Pos, value: Boolean) extends Lit
  case class LitString(pos: Pos, value: String) extends Lit

  case class Ref(pos: Pos, ref: VarRef) extends Expr
  case class If(pos: Pos, cond: Expr, th: Expr, el: Expr) extends Expr
  case class App(pos: Pos, fun: Expr, arg: Expr) extends Expr
  case class Fun(pos: Pos, param: VarRef.Local, tpe: Option[Type], body: Expr) extends Expr
  case class ELet(pos: Pos, ref: VarRef.Local, value: Expr, body: Expr) extends Expr
  case class ELetRec(pos: Pos, bindings: Seq[(VarRef.Local, Option[Type], Fun)], body: Expr) extends Expr
  case class JCallInstance(pos: Pos, receiver: Expr, methodName: Name, args: Seq[Expr]) extends Expr
  case class JCallStatic(pos: Pos, target: ClassRef, methodName: Name, args: Seq[Expr]) extends Expr

  case class MatchError(pos: Pos) extends Expr
}
