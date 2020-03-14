package com.todesking.ojaml.ml0.compiler.scala

import com.todesking.ojaml.ml0.compiler.scala.util.pretty
import pretty.PrettyPrinter
import pretty.Doc
import pretty.PrettySyntax._

sealed abstract class TypedAST {
  val pos: Pos
}
object TypedAST {
  def pretty(ast: TypedAST): String =
    PrettyPrinter.pretty(80, prettyDoc(ast, false))
  def prettyDoc(ast: TypedAST, paren: Boolean): Doc = ast match {
    case Module(pos, pkg, name, body) =>
      P.module(s"${pkg.value}.${name.value}", body.map(prettyDoc(_, false)))
    case TLet(pos, name, tpe, expr) =>
      P.tlet(name, Some(tpe.toString), prettyDoc(expr, false))
    case Data(pos, name, tpe, ctors) =>
      P.data(name, ctors.map { case (n, ts) => (n.value, ts.map(_.toString)) })
    case LitInt(pos, value) =>
      Doc.Text(value.toString)
    case LitBool(pos, value) =>
      Doc.Text(value.toString)
    case LitString(pos, value) =>
      s""""$value"""".doc
    case LocalRef(pos, depth, index, tpe) =>
      s"(local:$depth,$index: $tpe)".doc
    case ModuleVarRef(pos, module, name, tpe) =>
      s"(${module.fullName}.$name: $tpe)".doc
    case JCallStatic(pos, method, args) =>
      P.jcall(
        method.klass.fullName.doc,
        s"(${method.name}/${method.descriptor})",
        args.map(prettyDoc(_, false)),
        true)
    case JCallInstance(pos, method, receiver, args) =>
      P.jcall(
        prettyDoc(receiver, true),
        s"($method)",
        args.map(prettyDoc(_, false)),
        false)
    case If(pos, cond, th, el, tpe) =>
      P.eif(
        prettyDoc(cond, false),
        prettyDoc(th, false),
        prettyDoc(el, false))
    case Fun(pos, body, tpe) =>
      P.funT(tpe.toString, prettyDoc(body, false))
    case App(pos, fun, arg, tpe) =>
      P.app(paren, prettyDoc(fun, false), prettyDoc(arg, true))
    case LetRec(pos, values, body) =>
      P.eletrec(
        values.map { f =>
          ("?", None, prettyDoc(f, false))
        },
        prettyDoc(body, false))
    case JNew(pos, ref, args) =>
      P.group(
        s"new ${ref.fullName}(",
        P.mks(", ".doc)(args.map(prettyDoc(_, false))),
        ")")
    case Upcast(pos, body, tpe) =>
      prettyDoc(body, true) ^^ ": ".doc ^^ tpe.toString().doc
    case TAbs(pos, params, body, tpe) =>
      s"[${params.map(_.toString()).mkString(", ")}]".doc ^^ prettyDoc(body, true)
    case MatchError(pos, tpe) =>
      s"<matcherror>: $tpe".doc
    case unk =>
      unk.toString.doc
  }
  case class Module(pos: Pos, pkg: QName, name: Name, body: Seq[Term]) extends TypedAST {
    def moduleRef = ModuleRef(pkg.asPackage, name.value)
  }

  sealed abstract class Term extends TypedAST
  case class TLet(pos: Pos, name: Name, tpe: Type, expr: Expr) extends Term
  case class Data(pos: Pos, name: Name, tpe: Type.Data, ctors: Seq[(Name, Seq[Type])]) extends Term
  case class TExpr(pos: Pos, expr: Expr) extends Term

  sealed abstract class Expr extends TypedAST {
    def tpe: Type
  }
  sealed abstract class Lit(override val tpe: Type) extends Expr {
    def value: Any
  }
  case class LitInt(pos: Pos, value: Int) extends Lit(Type.Int)
  case class LitBool(pos: Pos, value: Boolean) extends Lit(Type.Bool)
  case class LitString(pos: Pos, value: String) extends Lit(Type.String)

  case class ModuleVarRef(pos: Pos, module: ModuleRef, name: String, tpe: Type) extends Expr
  case class LocalRef(pos: Pos, depth: Int, index: Int, tpe: Type) extends Expr
  case class LetRec(pos: Pos, values: Seq[Fun], body: Expr) extends Expr {
    override def tpe: Type = body.tpe
  }
  case class If(pos: Pos, cond: Expr, th: Expr, el: Expr, tpe: Type) extends Expr
  case class App(pos: Pos, fun: Expr, arg: Expr, tpe: Type) extends Expr
  case class Fun(pos: Pos, body: Expr, tpe: Type) extends Expr
  case class TAbs(pos: Pos, params: Seq[Type.Var], body: Expr, tpe: Type.Abs) extends Expr

  case class JCallStatic(pos: Pos, method: MethodSig, args: Seq[Expr]) extends Expr {
    require(method.isStatic)
    require(method.args.size == args.size)
    override def tpe: Type = method.ret.map(_.tpe).getOrElse(Type.Unit)
  }
  case class JCallInstance(pos: Pos, method: MethodSig, receiver: Expr, args: Seq[Expr]) extends Expr {
    require(!method.isStatic)
    require(method.args.size == args.size)
    override def tpe: Type = method.ret.map(_.tpe).getOrElse(Type.Unit)
  }
  case class JNew(pos: Pos, ref: ClassRef, args: Seq[Expr]) extends Expr {
    override def tpe = Type.Klass(ref)
  }
  case class Upcast(pos: Pos, body: Expr, tpe: Type.Reference) extends Expr

  case class MatchError(pos: Pos, tpe: Type) extends Expr

  case class Match(pos: Pos, expr: Expr, clauses: Seq[Clause], tpe: Type) extends Expr
  case class Clause(pos: Pos, pat: Pat, body: Expr) extends TypedAST
  sealed abstract class Pat extends TypedAST
  object Pat {
    case class Ctor(pos: Pos, dataType: Type.Data, name: Name, args: Seq[(Type, Pat)]) extends Pat
    case class PAny(pos: Pos) extends Pat
  }
}
