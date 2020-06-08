package com.todesking.ojaml.ml0.compiler.scala
import com.todesking.ojaml.ml0.compiler.scala.util.pretty.PrettyPrinter
import com.todesking.ojaml.ml0.compiler.scala.util.pretty.Doc
import com.todesking.ojaml.ml0.compiler.scala.util.pretty.PrettySyntax._

sealed abstract class JAST
object JAST {
  case class MethodDef(name: String, isStatic: Boolean, params: Seq[JType], ret: Option[JType], body: Seq[Term]) extends JAST
  case class FieldDef(name: String, isStatic: Boolean, tpe: JType) extends JAST
  case class ClassDef(filePath: String, ref: ClassRef, superRef: ClassRef, fields: Seq[FieldDef], methods: Seq[MethodDef]) extends JAST {
    def methodSig(md: MethodDef) = MethodSig(ref, md.isStatic, false, md.name, md.params, md.ret)
  }

  def pretty(ast: JAST): String =
    PrettyPrinter.pretty(80, prettyDoc(ast, false))
  def prettyDoc(ast: JAST, paren: Boolean): Doc = ast match {
    case ClassDef(fileName, ref, superRef, fields, methods) =>
      P.bgroup(
        s"class ${ref.fullName} extends ${superRef.fullName} {",
        P.bgroupi(fields.map(prettyDoc(_, false))),
        P.bgroupi(methods.map(prettyDoc(_, false))),
        "}")
    case FieldDef(name, isStatic, tpe) =>
      s"${if (isStatic) "static " else ""}field $name: $tpe".doc
    case MethodDef(name, isStatic, params, ret, body) =>
      P.group(
        s"def ${if (isStatic) "static " else ""}$name(${params.map(_.hname).mkString(", ")}): ${ret.map(_.hname) getOrElse "void"} {",
        P.groupi(body.map(prettyDoc(_, false))),
        "}")
    case TExpr(pos, expr) =>
      prettyDoc(expr, false)
    case TReturn(pos, expr) =>
      P.group(
        "return",
        prettyDoc(expr, false))
    case EPos(pos, expr) =>
      prettyDoc(expr, paren)
    case Lit(value) =>
      Doc.Text(value.toString)
    case If(cond, th, el, tpe) =>
      P.eif(
        prettyDoc(cond, false),
        prettyDoc(th, false),
        prettyDoc(el, false))
    case JNew(ref, args) =>
      P.group(
        s"new ${ref.fullName}(",
        P.mks(", ".doc)(args.map(prettyDoc(_, false))),
        ")")
    case PutStatic(pos, f, b) =>
      P.group(
        s"$f = ",
        prettyDoc(b, false))
    case GetField(ref, target) =>
      P.group(
        P.group(prettyDoc(target, true), "."),
        ref.toString.doc)
    case GetStatic(ref) =>
      P.group(
        ref.toString.doc)
    case PutField(pos, ref, target, expr) =>
      P.group(
        P.group(prettyDoc(target, true), "."),
        s"$ref =",
        prettyDoc(expr, false))
    case Cast(e, t) =>
      P.group(
        s"(${t})",
        prettyDoc(e, true))
    case b @ Box(e) =>
      P.group(
        s"(${b.tpe})",
        prettyDoc(e, true))
    case b @ Unbox(e) =>
      P.group(
        s"(${b.tpe})",
        prettyDoc(e, true))
    case Invoke(sig, special, r, a) =>
      val adoc = P.groupi(P.mks(",".doc)(a.map(prettyDoc(_, false))))
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
    case GetLocal(index, t) =>
      s"(local $index: $t)".doc
    case GetObjectFromUncheckedArray(arr, index) =>
      P.group(
        prettyDoc(arr, true),
        s"[$index]")
    case Null(tpe) =>
      "null".doc
    case NewObjectArray(size) =>
      s"new Object[$size]".doc
    case PutValuesToArray(arr, vs) =>
      P.group(
        prettyDoc(arr, false),
        "<-",
        P.group("[", P.mks(", ".doc)(vs.map(prettyDoc(_, false))), "]"))
    case Throw(e, t) =>
      P.group(
        "throw",
        prettyDoc(e, true))
    case InstanceOf(e, ref) =>
      P.group(
        s"<instanceof>[${ref.fullName}](",
        P.groupi(prettyDoc(e, false)),
        ")")
  }

  sealed abstract class Term extends JAST
  case class TExpr(pos: Pos, expr: Expr) extends Term
  case class TReturn(pos: Pos, expr: Expr) extends Term

  case class PutStatic(pos: Pos, ref: FieldRef, expr: Expr) extends Term
  case class PutField(pos: Pos, ref: FieldRef, target: Expr, expr: Expr) extends Term

  sealed abstract class Expr extends JAST {
    def tpe: JType
  }

  case class EPos(pos: Pos, expr: Expr) extends Expr {
    override def tpe = expr.tpe
  }

  case class Lit(value: LitValue) extends Expr {
    override def tpe: JType = value.tpe.jtype
  }

  case class If(cond: Expr, th: Expr, el: Expr, tpe: JType) extends Expr

  // TODO: test void-method invocation
  case class Invoke(method: MethodSig, special: Boolean, receiver: Option[Expr], args: Seq[Expr]) extends Expr {
    require(method.args.size == args.size)
    require(method.isStatic ^ receiver.nonEmpty)
    override val tpe: JType = method.ret getOrElse JType.TUnit
  }
  // TODO: term InvokeVoid

  case class JNew(ref: ClassRef, args: Seq[Expr]) extends Expr {
    override def tpe = JType.TKlass(ref)
  }
  case class Box(expr: Expr) extends Expr {
    require(expr.tpe.isPrimitive)
    override val tpe = expr.tpe.boxed
  }
  case class Unbox(expr: Expr) extends Expr {
    require(expr.tpe.isBoxed)
    override def tpe: JType = expr.tpe.unboxed.get
  }
  case class Cast(body: Expr, tpe: JType.TReference) extends Expr
  case class GetLocal(index: Int, tpe: JType) extends Expr
  case class GetObjectFromUncheckedArray(expr: Expr, index: Int) extends Expr {
    override def tpe: JType = JType.TObject
  }
  case class Null(tpe: JType) extends Expr
  case class NewObjectArray(size: Int) extends Expr {
    override def tpe: JType = JType.ObjectArray
  }
  case class GetField(ref: FieldRef, target: Expr) extends Expr {
    def tpe: JType = ref.tpe
  }
  case class GetStatic(ref: FieldRef) extends Expr {
    def tpe: JType = ref.tpe
  }
  case class PutValuesToArray(arr: Expr, values: Seq[Expr]) extends Expr {
    override def tpe: JType = arr.tpe
  }
  case class Throw(expr: Expr, tpe: JType) extends Expr
  case class InstanceOf(expr: Expr, ref: ClassRef) extends Expr {
    def tpe: JType = JType.TBool
  }
}
