package com.todesking.ojaml.ml0.compiler.scala
import com.todesking.ojaml.ml0.compiler.scala.util.pretty.PrettyPrinter
import com.todesking.ojaml.ml0.compiler.scala.util.pretty.Doc
import com.todesking.ojaml.ml0.compiler.scala.util.pretty.PrettySyntax._

case class FieldRef(klass: ClassRef, name: String, tpe: Type)

sealed abstract class JAST
object JAST {
  case class MethodDef(name: String, isStatic: Boolean, params: Seq[Type], ret: Option[Type], body: Seq[Term]) extends JAST
  case class FieldDef(name: String, tpe: Type) extends JAST
  case class ClassDef(ref: ClassRef, fields: Seq[FieldDef], methods: Seq[MethodDef], datas: Seq[Data]) extends JAST {
    def methodSig(md: MethodDef) = MethodSig(ref, md.isStatic, false, md.name, md.params, md.ret)
  }

  def pretty(ast: JAST): String =
    PrettyPrinter.pretty(80, prettyDoc(ast, false))
  def prettyDoc(ast: JAST, paren: Boolean): Doc = ast match {
    case ClassDef(ref, fields, methods, datas) =>
      P.bgroup(
        s"class $ref {",
        P.bgroupi(fields.map(prettyDoc(_, false))),
        P.bgroupi(methods.map(prettyDoc(_, false))),
        P.bgroupi(datas.map(prettyDoc(_, false))),
        "}")
    case FieldDef(name, tpe) =>
      s"field $name: $tpe".doc
    case MethodDef(name, isStatic, params, ret, body) =>
      P.group(
        s"def ${if (isStatic) "static " else ""}$name(${params.mkString(", ")}): ${ret.map(_.toString) getOrElse "void"} {",
        P.groupi(body.map(prettyDoc(_, false))),
        "}")
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
    case If(cond, th, el, tpe) =>
      P.eif(
        prettyDoc(cond, false),
        prettyDoc(th, false),
        prettyDoc(el, false))
    case Fun(body, tpe) =>
      P.funT(tpe.toString, prettyDoc(body, false))
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
    case PutStatic(f, b) =>
      P.group(
        s"$f = ",
        prettyDoc(b, false))
    case Downcast(e, t) =>
      P.group(
        s"(${t.ref.fullName})",
        prettyDoc(e, true))
    case b @ Box(e) =>
      P.group(
        s"(${b.tpe.ref.fullName})",
        prettyDoc(e, true))
    case b @ Unbox(e) =>
      P.group(
        s"(${b.tpe.javaName})",
        prettyDoc(e, true))
    case Invoke(sig, r, a) =>
      val adoc = P.mks(",".doc)(a.map(prettyDoc(_, false)))
      if (sig.isStatic) {
        P.group(
          s"[${sig.klass.fullName}].${sig.name}(",
          adoc,
          ")")
      } else {
        P.group(
          prettyDoc(r.get, true),
          s".[${sig.klass.fullName}].${sig.name}(",
          adoc,
          ")")
      }
  }

  sealed abstract class Term extends JAST
  case class TLet(name: Name, tpe: Type, expr: Expr) extends Term
  case class Data(name: Name, tpe: Type.Data, ctors: Seq[(Name, Seq[Type])]) extends Term

  case class PutStatic(ref: FieldRef, expr: Expr) extends Term

  sealed abstract class Expr extends JAST {
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
  case class Fun(body: Expr, tpe: Type) extends Expr

  // TODO: test void-method invocation
  case class Invoke(method: MethodSig, receiver: Option[Expr], args: Seq[Expr]) extends Expr {
    require(method.args.size == args.size)
    require(method.isStatic ^ receiver.nonEmpty)
    override val tpe: Type = method.ret getOrElse Type.Unit
  }
  // TODO: term InvokeVoid

  case class JNew(ref: ClassRef, args: Seq[Expr]) extends Expr {
    override def tpe = Type.Klass(ref)
  }
  case class Upcast(body: Expr, tpe: Type.Reference) extends Expr
  case class Box(expr: Expr) extends Expr {
    require(expr.tpe.boxed != expr.tpe)
    override val tpe = expr.tpe.boxed
  }
  case class Unbox(expr: Expr) extends Expr {
    require(expr.tpe.unboxed.nonEmpty)
    override def tpe: Type = expr.tpe.unboxed.get
  }
  case class Downcast(body: Expr, tpe: Type.Reference) extends Expr
}
