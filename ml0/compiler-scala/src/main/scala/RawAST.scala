package com.todesking.ojaml.ml0.compiler.scala

import com.todesking.ojaml.ml0.compiler.scala.util.pretty
import pretty.PrettyPrinter
import pretty.Doc
import pretty.PrettySyntax._

sealed abstract class RawAST extends HasPos
object RawAST {
  def pretty(ast: RawAST): String =
    PrettyPrinter.pretty(80, prettyDoc(ast, false))
  def prettyDoc(ast: RawAST, paren: Boolean): Doc = ast match {
    case Program(pkg, imports, items) =>
      P.bgroup(
        P.pkg(pkg),
        P.imports(imports),
        P.bgroup(
          items.map(prettyDoc(_, false))))
    case Module(name, body) =>
      P.module(name.value, body.map(prettyDoc(_, false)))
    case TLet(name, expr) =>
      P.tlet(name, None, prettyDoc(expr, false))
    case Data(name, ctors) =>
      P.data(name, ctors.map {
        case (n, ts) => (n.value, ts.map(_.toString))
      })
    case LitInt(value) =>
      Doc.Text(value.toString)
    case LitBool(value) =>
      Doc.Text(value.toString)
    case LitString(value) =>
      s""""$value"""".doc
    case Ref(name) =>
      name.value.doc
    case JCall(expr, name, args, isStatic) =>
      P.jcall(
        prettyDoc(expr, true),
        name.value,
        args.map(prettyDoc(_, false)),
        isStatic)
    case If(cond, th, el) =>
      P.eif(
        prettyDoc(cond, false),
        prettyDoc(th, false),
        prettyDoc(el, false))
    case Fun(name, tpeName, body) =>
      P.fun(name.value, tpeName.map(_.toString), prettyDoc(body, false))
    case App(fun, arg) =>
      P.app(paren, prettyDoc(fun, false), prettyDoc(arg, true))
    case ELet(name, value, body) =>
      P.elet(name.value, prettyDoc(value, false), prettyDoc(body, false))
    case ELetRec(bindings, body) =>
      P.eletrec(
        bindings.map {
          case (n, tn, f) =>
            (n.value, tn.map(_.toString), prettyDoc(f, false))
        },
        prettyDoc(body, false))
    case Prop(expr, name) =>
      P.prop(prettyDoc(expr, true), name)
  }

  case class Program(pkg: QName, imports: Seq[Import], items: Seq[Module]) extends RawAST

  case class Module(name: Name, body: Seq[Term]) extends RawAST

  sealed abstract class Term extends RawAST
  case class TLet(name: Name, expr: Expr) extends Term
  case class Data(name: Name, ctors: Seq[(Name, Seq[TypeName])]) extends Term

  sealed abstract class Expr extends Term

  sealed abstract class Lit extends Expr

  case class LitInt(value: Int) extends Lit
  case class LitBool(value: Boolean) extends Lit
  case class LitString(value: String) extends Lit

  case class Ref(name: Name) extends Expr
  case class JCall(expr: Expr, name: Name, args: Seq[Expr], isStatic: Boolean) extends Expr
  case class If(cond: Expr, th: Expr, el: Expr) extends Expr
  case class Fun(name: Name, tpeName: Option[TypeName], body: Expr) extends Expr
  case class App(fun: Expr, arg: Expr) extends Expr
  case class ELet(name: Name, value: Expr, body: Expr) extends Expr
  case class ELetRec(bindings: Seq[(Name, Option[TypeName], Fun)], body: Expr) extends Expr
  case class Prop(expr: Expr, name: Name) extends Expr
}
