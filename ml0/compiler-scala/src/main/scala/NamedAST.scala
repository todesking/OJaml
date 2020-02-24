package com.todesking.ojaml.ml0.compiler.scala

import com.todesking.ojaml.ml0.compiler.scala.util.pretty
import pretty.PrettyPrinter
import pretty.Doc
import pretty.PrettySyntax._

sealed abstract class NamedAST extends HasPos
object NamedAST {
  def pretty(ast: NamedAST): String =
    PrettyPrinter.pretty(80, prettyDoc(ast, false))
  def prettyDoc(ast: NamedAST, paren: Boolean): Doc = ast match {
    case Module(pkg, name, body) =>
      P.module(s"${pkg.value}.${name.value}", body.map(prettyDoc(_, false)))
    case TLet(name, expr) =>
      P.tlet(name, None, prettyDoc(expr, false))
    case Data(name, tpe, ctors) =>
      P.data(name, ctors.map { case (n, ts) => (n.value, ts.map(_.toString)) })
    case LitInt(value) =>
      Doc.Text(value.toString)
    case LitBool(value) =>
      Doc.Text(value.toString)
    case LitString(value) =>
      s""""$value"""".doc
    case Ref(ref) =>
      ref.toString.doc
    case JCallStatic(target, name, args) =>
      P.jcall(
        target.fullName.doc,
        name.value,
        args.map(prettyDoc(_, false)),
        true)
    case JCallInstance(expr, name, args) =>
      P.jcall(
        prettyDoc(expr, true),
        name.value,
        args.map(prettyDoc(_, false)),
        false)
    case If(cond, th, el) =>
      P.paren(
        paren,
        P.eif(
          prettyDoc(cond, false),
          prettyDoc(th, false),
          prettyDoc(el, false)))
    case Fun(param, tpe, body) =>
      P.fun(param.toString, tpe.map(_.toString), prettyDoc(body, false))
    case App(fun, arg) =>
      P.app(paren, prettyDoc(fun, true), prettyDoc(arg, true))
    case ELet(ref, value, body) =>
      P.elet(ref.toString, prettyDoc(value, false), prettyDoc(body, false))
    case ELetRec(bindings, body) =>
      P.eletrec(
        bindings.map {
          case (n, tn, f) =>
            (n.toString, tn.map(_.toString), prettyDoc(f, false))
        },
        prettyDoc(body, false))
    case Match(expr, cs) =>
      P.paren(paren, P.pmatch(
        prettyDoc(expr, false),
        cs.map(prettyDoc(_, false))))
    case MatchError() =>
      "<matcherror>".doc
    case Clause(p, b) =>
      P.group(
        P.group("|", prettyDoc(p, false), "=>"),
        prettyDoc(b, false))
    case Pat.PAny() =>
      "_".doc
    case Pat.Ctor(data, name, args) =>
      P.paren(paren, P.group(
        s"${name.value}[${args.map(_._1).mkString(", ")}]",
        args.map(_._2).map(prettyDoc(_, true))))
    case Pat.Capture(name) =>
      name.value.doc
  }

  case class Module(pkg: QName, name: Name, body: Seq[Term]) extends NamedAST {
    def moduleRef = ModuleRef(pkg.asPackage, name.value)
  }

  sealed abstract class Term extends NamedAST
  case class TLet(name: Name, expr: Expr) extends Term

  case class Data(name: Name, tpe: Type.Data, ctors: Seq[(Name, Seq[Type])]) extends Term

  sealed abstract class Expr extends NamedAST

  sealed abstract class Lit extends Expr
  case class LitInt(value: Int) extends Lit
  case class LitBool(value: Boolean) extends Lit
  case class LitString(value: String) extends Lit

  case class Ref(ref: VarRef) extends Expr
  case class If(cond: Expr, th: Expr, el: Expr) extends Expr
  case class App(fun: Expr, arg: Expr) extends Expr
  case class Fun(param: VarRef.Local, tpe: Option[Type], body: Expr) extends Expr
  case class ELet(ref: VarRef.Local, value: Expr, body: Expr) extends Expr
  case class ELetRec(bindings: Seq[(VarRef.Local, Option[Type], Fun)], body: Expr) extends Expr
  case class JCallInstance(receiver: Expr, methodName: Name, args: Seq[Expr]) extends Expr
  case class JCallStatic(target: ClassRef, methodName: Name, args: Seq[Expr]) extends Expr

  case class MatchError() extends Expr

  case class Match(expr: Expr, clauses: Seq[Clause]) extends Expr
  case class Clause(pat: Pat, body: Expr) extends NamedAST with HasPos
  sealed abstract class Pat extends NamedAST with HasPos
  object Pat {
    case class Ctor(dataType: Type.Data, name: Name, args: Seq[(Type, Pat)]) extends Pat
    case class PAny() extends Pat
    case class Capture(name: Name) extends Pat
  }
}
