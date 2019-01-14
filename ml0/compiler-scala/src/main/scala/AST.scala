package com.todesking.ojaml.ml0.compiler.scala

import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position

case class Pos(location: String, line: Int, col: Int) {
  override def toString = s"$location:$line:$col"
}
object Pos {
  def fill[A <: HasPos](v: A, p: Pos) = { v.fillPos(p); v }
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
  def value = parts.map(_.value).mkString(".")
  def internalName = parts.map(_.value).mkString("/")
  override def toString = s"QName($value)"
}

case class Import(qname: QName)

object AST {
  def pretty(x: Any, indent: Int = 0): String = x match {
    case p: AnyAST =>
      val base = ("  " * indent) + p.productPrefix
      if (p.productArity == 0) base
      else if (p.productArity == 1) base + pretty(p.productElement(0), indent + 1).replaceAll("^\\s+", " ")
      else base + "\n" + p.productIterator.map(pretty(_, indent + 1)).mkString("\n")
    case s: Seq[_] =>
      s.map(pretty(_, indent + 1)).mkString("\n")
    case x =>
      ("  " * indent) + s"$x"
  }
}

trait AnyAST extends Product

sealed abstract class RawAST extends HasPos with AnyAST
object RawAST {
  case class Program(pkg: QName, imports: Seq[Import], items: Seq[Struct]) extends RawAST
  case class Struct(name: Name, body: Seq[Term]) extends RawAST

  sealed abstract class Term extends RawAST
  case class TLet(name: Name, expr: Expr) extends Term

  sealed abstract class Expr extends Term
  case class LitInt(value: Int) extends Expr
  case class LitBool(value: Boolean) extends Expr
  case class LitString(value: String) extends Expr
  case class Ref(name: Name) extends Expr
  case class JCall(expr: Expr, name: Name, args: Seq[Expr], isStatic: Boolean) extends Expr
  case class If(cond: Expr, th: Expr, el: Expr) extends Expr
  case class Fun(name: Name, tpeName: Name, body: Expr) extends Expr
  case class App(fun: Expr, arg: Expr) extends Expr
  case class Prop(expr: Expr, name: Name) extends Expr
}

sealed abstract class NamedAST extends HasPos with AnyAST
object NamedAST {
  case class Struct(pkg: QName, name: Name, body: Seq[Term]) extends NamedAST {
    def moduleRef = ModuleRef(pkg.value, name.value)
  }

  sealed abstract class Term extends NamedAST
  case class TLet(name: Name, expr: Expr) extends Term

  sealed abstract class Expr extends Term

  sealed abstract class Lit extends Expr
  case class LitInt(value: Int) extends Lit
  case class LitBool(value: Boolean) extends Lit
  case class LitString(value: String) extends Lit

  case class Ref(ref: VarRef) extends Expr
  case class If(cond: Expr, th: Expr, el: Expr) extends Expr
  case class App(fun: Expr, arg: Expr) extends Expr
  case class Fun(param: VarRef.Local, tpe: Type, body: Expr) extends Expr

  case class JCall(receiver: Expr, methodName: Name, args: Seq[Expr], isStatic: Boolean) extends Expr
}

sealed abstract class TypedAST extends AnyAST
object TypedAST {
  case class Struct(pkg: QName, name: Name, body: Seq[Term]) extends TypedAST {
    def moduleRef = ModuleRef(pkg.value, name.value)
  }

  sealed abstract class Term extends TypedAST
  case class TLet(name: Name, tpe: Type, expr: Expr) extends Term

  sealed abstract class Expr extends Term {
    def tpe: Type
  }
  sealed abstract class Lit(override val tpe: Type) extends Expr
  case class LitInt(value: Int) extends Lit(Type.Int)
  case class LitBool(value: Boolean) extends Lit(Type.Bool)
  case class LitString(value: String) extends Lit(Type.String)

  case class ModuleVarRef(module: ModuleRef, name: String, tpe: Type) extends Expr
  // index: local var index, outmost = 0
  case class LocalRef(index: Int, tpe: Type) extends Expr
  case class If(cond: Expr, th: Expr, el: Expr, tpe: Type) extends Expr
  case class App(fun: Expr, arg: Expr, tpe: Type) extends Expr
  case class Fun(argType: Type, body: Expr) extends Expr {
    override val tpe = Type.Fun(argType, body.tpe)
  }

  case class JCallStatic(method: MethodSig, args: Seq[Expr]) extends Expr {
    require(method.isStatic)
    require(method.args.size == args.size)
    override def tpe = method.ret.getOrElse(Type.Unit)
  }
  case class JCallInstance(method: MethodSig, receiver: Expr, args: Seq[Expr]) extends Expr {
    require(!method.isStatic)
    require(method.args.size == args.size)
    override def tpe = method.ret.getOrElse(Type.Unit)
  }
}
