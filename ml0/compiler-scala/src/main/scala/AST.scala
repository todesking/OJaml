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

object AST {
  def pretty(ast: AnyAST): String =
    PrettyPrinter.pretty(80, ast.pretty(false))
}

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
  def mks(sep: Doc)(items: Seq[Doc]) = {
    items.tail.foldLeft[Doc](items.head) { (a, x) => a ^^ sep ^| x }
  }
  def paren(enable: Boolean, doc: Doc) = if (enable) P.group("(", doc, ")") else doc
}
import P._
trait AnyAST {
  def pretty(group: Boolean): Doc
}

sealed abstract class RawAST extends HasPos with AnyAST
object RawAST {
  case class Program(pkg: QName, imports: Seq[Import], items: Seq[Module]) extends RawAST {
    override def pretty(group: Boolean) = P.bgroup(
      s"package ${pkg.value}".doc,
      imports.map(_.qname.value).map { s => s"import $s".doc },
      items.map(_.pretty(false)))
  }

  case class Module(name: Name, body: Seq[Term]) extends RawAST {
    override def pretty(group: Boolean) = bgroup(
      s"module ${name.value} {",
      bgroupi(body.map(_.pretty(false))),
      "}")
  }

  sealed abstract class Term extends RawAST
  case class TLet(name: Name, expr: Expr) extends Term {
    override def pretty(group: Boolean) = P.group(s"let ${name.value} =", P.groupi(expr.pretty(false)))
  }

  case class Data(name: Name, defs: Seq[(Name, Seq[TypeName])]) extends Term {
    override def pretty(group: Boolean) = P.group(
      s"data ${name.value} =",
      P.group(defs.map { case (n, ns) => (n +: ns).mkString(" ") }))
  }

  sealed abstract class Expr extends Term

  sealed abstract class Lit extends Expr {
    def value: Any
    override def pretty(group: Boolean) = value.toString.doc
  }

  case class LitInt(value: Int) extends Lit
  case class LitBool(value: Boolean) extends Lit
  case class LitString(value: String) extends Lit {
    override def pretty(group: Boolean) = s""""$value"""".doc
  }

  case class Ref(name: Name) extends Expr {
    override def pretty(group: Boolean) = name.value.doc
  }
  case class JCall(expr: Expr, name: Name, args: Seq[Expr], isStatic: Boolean) extends Expr {
    override def pretty(group: Boolean) = P.group(
      expr.pretty(true),
      (if (isStatic) "##" else "#").doc ^^ name.value.doc ^^ "(".doc,
      P.groupi(P.mks(",".doc)(args.map(_.pretty(false)))),
      ")")
  }
  case class If(cond: Expr, th: Expr, el: Expr) extends Expr {
    override def pretty(group: Boolean) = P.group(
      "if".doc,
      P.groupi(cond.pretty(false)),
      "then",
      P.groupi(th.pretty(false)),
      "else",
      P.groupi(el.pretty(false)))
  }
  case class Fun(name: Name, tpeName: Option[TypeName], body: Expr) extends Expr {
    override def pretty(group: Boolean) = P.group(
      "fun".doc,
      name.value.doc ^^ tpeName.map { t => ":".doc ^| t.toString.doc }.getOrElse(Doc.Nil) ^^ " =>".doc,
      P.groupi(body.pretty(false)))
  }
  case class App(fun: Expr, arg: Expr) extends Expr {
    override def pretty(group: Boolean) = P.paren(group, P.group(fun.pretty(false) ^| P.groupi(arg.pretty(true))))
  }
  case class ELet(name: Name, value: Expr, body: Expr) extends Expr {
    override def pretty(group: Boolean) = P.group(
      s"let ${name.value} =",
      P.groupi(value.pretty(false)),
      "in",
      P.groupi(body.pretty(false)))
  }
  case class ELetRec(bindings: Seq[(Name, Option[TypeName], Fun)], body: Expr) extends Expr {
    override def pretty(group: Boolean) = P.group(
      "let rec",
      P.mks(";".doc)(
        bindings.map {
          case (n, t, f) =>
            P.groupi(
              s"${n.value}".doc ^^ t.map { t => s": $t".doc }.getOrElse(Doc.Nil) ^^ " =".doc,
              P.groupi(f.pretty(false)))
        }),
      "in",
      P.groupi(body.pretty(false)))
  }
  case class Prop(expr: Expr, name: Name) extends Expr {
    override def pretty(group: Boolean) = P.group(
      expr.pretty(true) ^^ ".".doc,
      name.value.doc)
  }
}

sealed abstract class NamedAST extends HasPos with AnyAST
object NamedAST {
  case class Module(pkg: QName, name: Name, body: Seq[Term]) extends NamedAST {
    def moduleRef = ModuleRef(pkg.asPackage, name.value)
    override def pretty(group: Boolean) = P.bgroup(
      s"module ${pkg.value}.${name.value} {",
      bgroupi(body.map(_.pretty(false))),
      "}")
  }

  sealed abstract class Term extends NamedAST
  case class TLet(name: Name, expr: Expr) extends Term {
    override def pretty(group: Boolean) = P.group(s"let ${name.value} =", P.groupi(expr.pretty(false)))
  }

  sealed abstract class Expr extends Term

  sealed abstract class Lit extends Expr {
    def value: Any
    override def pretty(group: Boolean) = value.toString.doc
  }
  case class LitInt(value: Int) extends Lit
  case class LitBool(value: Boolean) extends Lit
  case class LitString(value: String) extends Lit {
    override def pretty(group: Boolean) = s""""$value"""".doc
  }

  private[this] def prettyVarRef(ref: VarRef) = ref match {
    case VarRef.ModuleMember(m, name) => s"${m.fullName}.$name".doc
    case VarRef.Local(d, i) => s"local($d, $i)".doc
  }

  case class Ref(ref: VarRef) extends Expr {
    override def pretty(group: Boolean) = prettyVarRef(ref)
  }
  case class If(cond: Expr, th: Expr, el: Expr) extends Expr {
    override def pretty(group: Boolean) = P.group(
      "if".doc,
      P.groupi(cond.pretty(false)),
      "then".doc,
      P.groupi(th.pretty(false)),
      "else".doc,
      P.groupi(el.pretty(false)))
  }
  case class App(fun: Expr, arg: Expr) extends Expr {
    override def pretty(group: Boolean) = P.paren(group, P.group(fun.pretty(false) ^| P.groupi(arg.pretty(true))))
  }
  case class Fun(param: VarRef.Local, tpe: Option[Type], body: Expr) extends Expr {
    override def pretty(group: Boolean) = P.group(
      "fun".doc,
      prettyVarRef(param) ^^ tpe.map { t => ":".doc ^| t.toString.doc }.getOrElse(Doc.Nil) ^^ " =>".doc,
      P.groupi(body.pretty(false)))
  }
  case class ELet(ref: VarRef.Local, value: Expr, body: Expr) extends Expr {
    override def pretty(group: Boolean) = P.group(
      s"let ".doc ^^ prettyVarRef(ref) ^^ " =".doc,
      P.groupi(value.pretty(false)),
      "in",
      P.groupi(body.pretty(false)))
  }
  case class ELetRec(bindings: Seq[(VarRef.Local, Option[Type], Fun)], body: Expr) extends Expr {
    override def pretty(group: Boolean) = P.group(
      "let rec",
      P.mks(";".doc)(
        bindings.map {
          case (n, t, f) =>
            P.groupi(
              prettyVarRef(n) ^^ t.map { t => s": $t".doc }.getOrElse(Doc.Nil) ^^ " =".doc,
              P.groupi(f.pretty(false)))
        }),
      "in",
      P.groupi(body.pretty(false)))
  }
  case class JCallInstance(receiver: Expr, methodName: Name, args: Seq[Expr]) extends Expr {
    override def pretty(group: Boolean) = P.group(
      receiver.pretty(false) ^^ ".".doc,
      methodName.value.doc ^^ "#(".doc,
      P.groupi(P.mks(",".doc)(args.map(_.pretty(false)))),
      ")")
  }
  case class JCallStatic(target: ClassRef, methodName: Name, args: Seq[Expr]) extends Expr {
    override def pretty(group: Boolean) = P.group(
      target.fullName.doc ^^ ".".doc,
      methodName.value.doc ^^ "##(".doc,
      P.groupi(P.mks(",".doc)(args.map(_.pretty(false)))),
      ")")
  }
}

sealed abstract class TypedAST extends AnyAST
object TypedAST {
  case class Module(pkg: QName, name: Name, body: Seq[Term]) extends TypedAST {
    def moduleRef = ModuleRef(pkg.asPackage, name.value)
    override def pretty(group: Boolean) = bgroup(
      s"module ${pkg.value}.${name.value} {",
      bgroupi(body.map(_.pretty(false))),
      "}")
  }

  sealed abstract class Term extends TypedAST
  case class TLet(name: Name, tpe: Type, expr: Expr) extends Term {
    override def pretty(group: Boolean) = P.group(s"let ${name.value}: $tpe =", P.groupi(expr.pretty(false)))
  }

  sealed abstract class Expr extends Term {
    def tpe: Type
  }
  sealed abstract class Lit(override val tpe: Type) extends Expr {
    def value: Any
    override def pretty(group: Boolean) = s"($value: $tpe)".doc
  }
  case class LitInt(value: Int) extends Lit(Type.Int)
  case class LitBool(value: Boolean) extends Lit(Type.Bool)
  case class LitString(value: String) extends Lit(Type.String)

  case class ModuleVarRef(module: ModuleRef, name: String, tpe: Type) extends Expr {
    override def pretty(group: Boolean) =
      s"(${module.fullName}.$name: $tpe)".doc
  }
  case class LocalRef(depth: Int, index: Int, tpe: Type) extends Expr {
    override def pretty(group: Boolean) = s"local(($depth, $index): $tpe)".doc
  }
  case class LetRec(values: Seq[Fun], body: Expr) extends Expr {
    override def tpe: Type = body.tpe
    override def pretty(group: Boolean) = P.group(
      "let rec",
      P.mks(";".doc)(
        values.map { f =>
          P.groupi(
            s"_: ${f.tpe} =",
            P.groupi(f.pretty(false)))
        }),
      "in",
      P.groupi(body.pretty(false)))
  }
  case class If(cond: Expr, th: Expr, el: Expr, tpe: Type) extends Expr {
    override def pretty(group: Boolean) = P.group(
      "(if",
      P.groupi(cond.pretty(false)),
      "then",
      P.groupi(th.pretty(false)),
      "else",
      P.groupi(el.pretty(false)),
      s"): $tpe")
  }
  case class App(fun: Expr, arg: Expr, tpe: Type) extends Expr {
    override def pretty(group: Boolean) = P.paren(group, P.group(
      fun.pretty(false),
      P.groupi(arg.pretty(true)),
      P.groupi(s": $tpe")))
  }
  case class Fun(body: Expr, tpe: Type) extends Expr {
    override def pretty(group: Boolean) = P.group(
      "(fun ? =>",
      P.groupi(body.pretty(false)),
      s"): $tpe")
  }
  case class TAbs(params: Seq[Type.Var], body: Expr, tpe: Type.Abs) extends Expr {
    override def pretty(group: Boolean) = P.group(
      "(",
      P.group(
        "[",
        P.mks(", ".doc)(params.map { case Type.Var(i) => s"?$i".doc }),
        "]"),
      "(",
      body.pretty(false),
      s"): $tpe")
  }

  case class JCallStatic(method: MethodSig, args: Seq[Expr]) extends Expr {
    require(method.isStatic)
    require(method.args.size == args.size)
    override def tpe: Type = method.ret.getOrElse(Type.Unit)
    override def pretty(group: Boolean) = P.group(
      s"(${method.klass.fullName}.${method.name}(",
      P.groupi(P.mks(",".doc)(args.map(_.pretty(false)))),
      s"): ${method.ret}")
  }
  case class JCallInstance(method: MethodSig, receiver: Expr, args: Seq[Expr]) extends Expr {
    require(!method.isStatic)
    require(method.args.size == args.size)
    override def tpe: Type = method.ret.getOrElse(Type.Unit)
    override def pretty(group: Boolean) = P.group(
      "(",
      receiver.pretty(false),
      s"##[$method](",
      P.groupi(P.mks(", ".doc)(args.map(_.pretty(false)))),
      s"): ${method.ret}")
  }
}
