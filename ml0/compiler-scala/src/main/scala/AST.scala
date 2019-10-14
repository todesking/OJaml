package com.todesking.ojaml.ml0.compiler.scala

import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position

import com.todesking.ojaml.ml0.compiler.scala.util.pretty
import pretty.PrettyPrinter
import pretty.Doc
import pretty.PrettySyntax._

case class Pos(location: String, line: Int, col: Int) {
  override def toString = s"$location:$line:$col"
}
object Pos {
  def fill[A <: HasPos](v: A, p: Pos): A = { v.fillPos(p); v }
}
trait HasPos {
  private[this] var _pos: Pos = null
  def fillPos(location: String, line: Int, col: Int): Unit =
    fillPos(Pos(location, line, col))
  def fillPos(pos: Pos): Unit =
    if (_pos == null) _pos = pos
  def pos: Pos = _pos
}

case class Name(value: String) extends HasPos {
  override def toString = s"Name($value)"
}
case class QName(parts: Seq[Name]) extends HasPos {
  require(parts.nonEmpty)
  def value: String = parts.map(_.value).mkString(".")
  def internalName: String = parts.map(_.value).mkString("/")
  def asPackage: PackageRef = PackageRef.fromParts(parts.map(_.value))
  override def toString = s"QName($value)"
}
sealed abstract class TypeName extends HasPos
object TypeName {
  case class Atom(name: String) extends TypeName {
    override def toString = name
  }
  case class Fun(l: TypeName, r: TypeName) extends TypeName {
    override def toString = s"$l -> $r"
  }
}

case class Import(qname: QName)

object P {
  trait DocLike {
    def toDocs: Seq[Doc]
  }
  object DocLike {
    implicit def doc2Like(doc: Doc): DocLike = new DocLike {
      override def toDocs = Seq(doc)
    }
    implicit def docs2Like[A](docs: Seq[A])(implicit f: A => DocLike): DocLike = new DocLike {
      override def toDocs = docs.map(f).flatMap(_.toDocs)
    }
    implicit def str2Like(str: String) = doc2Like(Doc.Text(str))
  }
  private[this] def join(docs: Seq[Doc])(f: (Doc, Doc) => Doc) = docs.foldLeft[Doc](Doc.Nil)(f)
  def bgroup(items: DocLike*) = Doc.BGroup(join(items.flatMap(_.toDocs))(_ ^| _))
  def bgroupi(items: DocLike*) = Doc.Nest(2, bgroup(items: _*))
  def group(items: DocLike*) = Doc.Group(join(items.flatMap(_.toDocs))(_ ^| _))
  def groupi(items: DocLike*) = Doc.Nest(2, group(items: _*))
  def mks(sep: Doc)(items: Seq[Doc]) =
    if (items.isEmpty) Doc.Nil
    else items.tail.foldLeft[Doc](items.head) { (a, x) => a ^^ sep ^| x }
  def paren(enable: Boolean, doc: Doc) = if (enable) P.group("(", doc, ")") else doc

  private[this] def withTpe(doc: Doc, tpe: Option[String]) =
    tpe.fold(doc) { t =>
      doc ^^ s": $t".doc
    }

  def pkg(name: QName): Doc =
    Doc.Text(s"package ${name.value}")
  def imports(items: Seq[Import]): Doc =
    bgroup(items.map { x =>
      Doc.Text(s"import ${x.qname.value}")
    })
  def module(name: String, body: Seq[Doc]) =
    bgroup(
      s"module $name {",
      bgroupi(body),
      "}")
  def tlet(name: Name, tpe: Option[String], body: Doc) =
    group(
      s"let ${name.value} =",
      groupi(body))
  def data(name: Name, ctors: Seq[(String, Seq[String])]) =
    group(
      s"data ${name.value} =",
      groupi(
        mks(Doc.Text(","))(ctors.map { case (n, ns) => Doc.Text((n +: ns).mkString(" ")) })))
  def jcall(receiver: Doc, name: String, args: Seq[Doc], isStatic: Boolean) =
    group(
      receiver,
      (if (isStatic) "##" else "#").doc ^^ name.doc ^^ "(".doc,
      groupi(mks(",".doc)(args)),
      ")")
  def eif(cond: Doc, th: Doc, el: Doc) =
    group(
      "if", groupi(cond),
      "then", groupi(th),
      "else", groupi(el))
  def fun(name: String, tpeName: Option[String], body: Doc) =
    group(
      ("fun".doc ^^ name.doc
        ^^ tpeName.fold[Doc](Doc.Nil) { t => ": ".doc ^| t.toString.doc } ^^ " =>".doc),
      groupi(body))
  def funT(tpe: String, body: Doc) =
    group(
      s"(fun: $tpe) ? =>",
      groupi(body))
  def app(par: Boolean, fun: Doc, arg: Doc) =
    paren(par, fun ^^ " ".doc ^^ arg)
  def elet(name: String, value: Doc, body: Doc) =
    group(
      s"let $name =",
      groupi(value),
      "in",
      groupi(body))
  def eletrec(bindings: Seq[(String, Option[String], Doc)], body: Doc) =
    group(
      "let rec",
      mks(";".doc)(
        bindings.map {
          case (n, t, f) =>
            groupi(
              n.doc ^^ t.fold[Doc](Doc.Nil) { t =>
                s": $t".doc
              } ^^ " =".doc,
              groupi(f))
        }),
      "in",
      groupi(body))
  def prop(expr: Doc, name: Name) =
    group(expr ^^ ".".doc, name.value)

}

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
      P.eif(
        prettyDoc(cond, false),
        prettyDoc(th, false),
        prettyDoc(el, false))
    case Fun(param, tpe, body) =>
      P.fun(param.toString, tpe.map(_.toString), prettyDoc(body, false))
    case App(fun, arg) =>
      P.app(paren, prettyDoc(fun, false), prettyDoc(arg, true))
    case ELet(ref, value, body) =>
      P.elet(ref.toString, prettyDoc(value, false), prettyDoc(body, false))
    case ELetRec(bindings, body) =>
      P.eletrec(
        bindings.map {
          case (n, tn, f) =>
            (n.toString, tn.map(_.toString), prettyDoc(f, false))
        },
        prettyDoc(body, false))
  }

  case class Module(pkg: QName, name: Name, body: Seq[Term]) extends NamedAST {
    def moduleRef = ModuleRef(pkg.asPackage, name.value)
  }

  sealed abstract class Term extends NamedAST
  case class TLet(name: Name, expr: Expr) extends Term

  case class Data(name: Name, tpe: Type.Data, ctors: Seq[(Name, Seq[Type])]) extends Term

  sealed abstract class Expr extends Term

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
}

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
