package com.todesking.ojaml.ml0.compiler.scala

import com.todesking.ojaml.ml0.compiler.scala.util.pretty
import pretty.PrettyPrinter
import pretty.Doc
import pretty.PrettySyntax._

sealed abstract class RawAST {
  val pos: Pos
}
object RawAST {
  def pretty(ast: RawAST): String =
    PrettyPrinter.pretty(80, prettyDoc(ast, false))
  def prettyDoc(ast: RawAST, paren: Boolean): Doc = ast match {
    case Program(_, pkg, items) =>
      P.bgroup(
        P.pkg(pkg),
        P.bgroup(
          items.map(prettyDoc(_, false))))
    case TImport(pos, spec) =>
      P.imports(Seq(spec))
    case Module(_, name, body) =>
      P.module(name.value, body.map(prettyDoc(_, false)))
    case TLet(_, name, tname, expr) =>
      P.tlet(name.value, tname.map(_.toString), prettyDoc(expr, false))
    case TLetRec(pos, bindings) =>
      P.bgroup(
        bindings.map {
          case (name, tname, expr) =>
            P.tlet(s"(rec)$name", tname.map(_.toString), prettyDoc(expr, false))
        })
    case Data(_, name, tvars, ctors) =>
      P.data(name, tvars, ctors.map {
        case (n, ts) => (n.value, ts.map(_.toString))
      })
    case TExpr(_, expr) =>
      P.group(prettyDoc(expr, false), ";;")
    case Lit(_, value) =>
      Doc.Text(value.toString)
    case Ref(_, qname) =>
      qname.fullName.doc
    case JCall(_, expr, name, args, isStatic) =>
      P.jcall(
        prettyDoc(expr, true),
        name.value,
        args.map(prettyDoc(_, false)),
        isStatic)
    case If(_, cond, th, el) =>
      P.eif(
        prettyDoc(cond, false),
        prettyDoc(th, false),
        prettyDoc(el, false))
    case Fun(_, name, tpeName, body) =>
      P.fun(name.value, tpeName.map(_.toString), prettyDoc(body, false), paren)
    case App(_, fun, arg) =>
      P.app(paren, prettyDoc(fun, false), prettyDoc(arg, true))
    case ELet(_, name, value, body) =>
      P.elet(name.value, prettyDoc(value, false), prettyDoc(body, false))
    case ELetRec(_, bindings, body) =>
      P.eletrec(
        bindings.map {
          case (n, tn, f) =>
            (n.value, tn.map(_.toString), prettyDoc(f, false))
        },
        prettyDoc(body, false))
    case Match(_, expr, cs) =>
      P.paren(paren, P.pmatch(
        prettyDoc(expr, false),
        cs.map(prettyDoc(_, false))))
    case Clause(_, p, b) =>
      P.group(
        P.group("|", prettyDoc(p, false), "=>"),
        prettyDoc(b, false))
    case Pat.PAny(_) =>
      "_".doc
    case Pat.Ctor(_, name, args) =>
      P.paren(paren, P.group(
        name.doc,
        args.map(prettyDoc(_, true))))
    case Pat.Capture(_, name) =>
      name.doc
    case Pat.Lit(_, v) =>
      v.toString.doc
  }

  case class Program(pos: Pos, pkg: QName, items: Seq[TopLevel]) extends RawAST

  sealed abstract class TopLevel extends RawAST
  case class TImport(pos: Pos, spec: Import) extends TopLevel
  case class Module(pos: Pos, name: Name, body: Seq[Term]) extends TopLevel

  sealed abstract class Term extends RawAST
  case class TLet(pos: Pos, name: Name, typeName: Option[TypeName], expr: Expr) extends Term
  case class TLetRec(pos: Pos, bindings: Seq[(Name, Option[TypeName], Fun)]) extends Term

  case class Data(pos: Pos, name: Name, tparams: Seq[Name], ctors: Seq[(Name, Seq[TypeName])]) extends Term
  case class TExpr(pos: Pos, expr: Expr) extends Term

  sealed abstract class Expr extends RawAST

  case class Lit(pos: Pos, value: LitValue) extends Expr

  case class Ref(pos: Pos, qname: QName) extends Expr
  case class JCall(pos: Pos, expr: Expr, name: Name, args: Seq[Expr], isStatic: Boolean) extends Expr
  case class If(pos: Pos, cond: Expr, th: Expr, el: Expr) extends Expr
  case class Fun(pos: Pos, name: Name, tpeName: Option[TypeName], body: Expr) extends Expr
  case class App(pos: Pos, fun: Expr, arg: Expr) extends Expr
  case class ELet(pos: Pos, name: Name, value: Expr, body: Expr) extends Expr
  case class ELetRec(pos: Pos, bindings: Seq[(Name, Option[TypeName], Fun)], body: Expr) extends Expr

  case class Match(pos: Pos, expr: Expr, clauses: Seq[Clause]) extends Expr
  case class Clause(pos: Pos, pat: Pat, body: Expr) extends RawAST
  sealed abstract class Pat extends RawAST
  object Pat {
    case class Ctor(pos: Pos, name: String, args: Seq[Pat]) extends Pat
    case class PAny(pos: Pos) extends Pat
    case class Capture(pos: Pos, name: String) extends Pat
    case class Lit(pos: Pos, value: LitValue) extends Pat
  }
}
