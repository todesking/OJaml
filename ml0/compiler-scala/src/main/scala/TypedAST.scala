package com.todesking.ojaml.ml0.compiler.scala

import com.todesking.ojaml.ml0.compiler.scala.util.pretty
import pretty.PrettyPrinter
import pretty.Doc
import pretty.PrettySyntax._

sealed abstract class TypedAST
object TypedAST {
  def pretty(ast: TypedAST): String =
    PrettyPrinter.pretty(80, prettyDoc(ast, false))
  def prettyDoc(ast: TypedAST, paren: Boolean): Doc = ast match {
    case Module(pkg, name, body) =>
      P.module(s"${pkg.value}.${name.value}", body.map(prettyDoc(_, false)))
    case TLet(name, tpe, expr) =>
      P.tlet(name, Some(tpe.toString), prettyDoc(expr, false))
    case Data(name, tpe, ctors) =>
      P.data(name, ctors.map { case (n, ts) => (n.value, ts.map(_.toString)) })
    case LitInt(value) =>
      Doc.Text(value.toString)
    case LitBool(value) =>
      Doc.Text(value.toString)
    case LitString(value) =>
      s""""$value"""".doc
    case LocalRef(depth, index, tpe) =>
      s"(local:$depth,$index: $tpe)".doc
    case ModuleVarRef(module, name, tpe) =>
      s"(${module.fullName}.$name: $tpe)".doc
    case JCallStatic(method, args) =>
      P.jcall(
        method.klass.fullName.doc,
        s"(${method.name}/${method.descriptor})",
        args.map(prettyDoc(_, false)),
        true)
    case JCallInstance(method, receiver, args) =>
      P.jcall(
        prettyDoc(receiver, true),
        s"($method)",
        args.map(prettyDoc(_, false)),
        false)
    case If(cond, th, el, tpe) =>
      P.eif(
        prettyDoc(cond, false),
        prettyDoc(th, false),
        prettyDoc(el, false))
    case Fun(body, tpe) =>
      P.funT(tpe.toString, prettyDoc(body, false))
    case App(fun, arg, tpe) =>
      P.app(paren, prettyDoc(fun, false), prettyDoc(arg, true))
    case LetRec(values, body) =>
      P.eletrec(
        values.map { f =>
          ("?", None, prettyDoc(f, false))
        },
        prettyDoc(body, false))
    case JNew(ref, args) =>
      P.group(
        s"new ${ref.fullName}(",
        P.mks(", ".doc)(args.map(prettyDoc(_, false))),
        ")")
    case Upcast(body, tpe) =>
      prettyDoc(body, true) ^^ ": ".doc ^^ tpe.toString().doc
    case TAbs(params, body, tpe) =>
      s"[${params.map(_.toString()).mkString(", ")}]".doc ^^ prettyDoc(body, true)
  }
  case class Module(pkg: QName, name: Name, body: Seq[Term]) extends TypedAST {
    def moduleRef = ModuleRef(pkg.asPackage, name.value)
  }

  sealed abstract class Term extends TypedAST
  case class TLet(name: Name, tpe: Type, expr: Expr) extends Term
  case class Data(name: Name, tpe: Type.Data, ctors: Seq[(Name, Seq[Type])]) extends Term

  sealed abstract class Expr extends Term {
    def tpe: Type
  }
  sealed abstract class Lit(override val tpe: Type) extends Expr {
    def value: Any
  }
  case class LitInt(value: Int) extends Lit(Type.Int)
  case class LitBool(value: Boolean) extends Lit(Type.Bool)
  case class LitString(value: String) extends Lit(Type.String)

  case class ModuleVarRef(module: ModuleRef, name: String, tpe: Type) extends Expr
  case class LocalRef(depth: Int, index: Int, tpe: Type) extends Expr
  case class LetRec(values: Seq[Fun], body: Expr) extends Expr {
    override def tpe: Type = body.tpe
  }
  case class If(cond: Expr, th: Expr, el: Expr, tpe: Type) extends Expr
  case class App(fun: Expr, arg: Expr, tpe: Type) extends Expr
  case class Fun(body: Expr, tpe: Type) extends Expr
  case class TAbs(params: Seq[Type.Var], body: Expr, tpe: Type.Abs) extends Expr

  case class JCallStatic(method: MethodSig, args: Seq[Expr]) extends Expr {
    require(method.isStatic)
    require(method.args.size == args.size)
    override def tpe: Type = method.ret.getOrElse(Type.Unit)
  }
  case class JCallInstance(method: MethodSig, receiver: Expr, args: Seq[Expr]) extends Expr {
    require(!method.isStatic)
    require(method.args.size == args.size)
    override def tpe: Type = method.ret.getOrElse(Type.Unit)
  }
  case class JNew(ref: ClassRef, args: Seq[Expr]) extends Expr {
    override def tpe = Type.Klass(ref)
  }
  case class Upcast(body: Expr, tpe: Type.Reference) extends Expr
}
