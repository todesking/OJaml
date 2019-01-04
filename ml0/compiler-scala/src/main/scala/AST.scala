package com.todesking.ojaml.ml0.compiler.scala

import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position

case class Pos(location: String, line: Int, col: Int)
trait HasPos {
  private[this] var _pos: Pos = null
  def fillPos(location: String, line: Int, col: Int) = if (_pos == null) _pos =
    Pos(location, line, col)
  def pos: Pos = _pos
}

case class Name(value: String) extends HasPos
case class QName(parts: Seq[Name]) extends HasPos {
  require(parts.nonEmpty)
  def value = parts.map(_.value).mkString(".")
}

sealed abstract class RawAST extends HasPos
object RawAST {
  case class Program(pkg: QName, item: Struct) extends RawAST
  case class Struct(name: Name, body: Seq[Term]) extends RawAST

  sealed abstract class Term extends RawAST
  case class TLet(name: Name, expr: Expr) extends Term

  sealed abstract class Expr extends Term
  case class LitInt(value: Int) extends Expr
  case class LitBool(value: Boolean) extends Expr
  case class Ref(name: Name) extends Expr
  case class JClass(name: QName) extends Expr
  case class JCall(receiver: Expr, name: Name, args: Seq[Expr]) extends Expr
  case class If(cond: Expr, th: Expr, el: Expr) extends Expr
  case class Fun(name: Name, tpeName: Name, body: Expr) extends Expr
  case class App(fun: Expr, arg: Expr) extends Expr
}

case class ModuleRef(pkg: String, name: String)

sealed abstract class VarRef
object VarRef {
  case class Module(module: ModuleRef, name: String) extends VarRef
  case class Local(name: String) extends VarRef
}

sealed abstract class TAST
object TAST {
  case class Program(pkg: QName, item: Struct) extends TAST
  case class Struct(name: Name, body: Seq[Term]) extends TAST

  sealed abstract class Term extends TAST
  case class TLet(name: Name, tpe: Type, expr: Expr) extends Term

  sealed abstract class Expr extends Term {
    def tpe: Type
  }
  sealed abstract class Lit(override val tpe: Type) extends Expr
  case class LitInt(value: Int) extends Lit(Type.Int)
  case class LitBool(value: Boolean) extends Lit(Type.Bool)

  case class ModuleVarRef(module: ModuleRef, name: String, tpe: Type) extends Expr
  // index: local var index, outmost = 0
  case class LocalRef(index: Int, tpe: Type) extends Expr
  case class If(cond: Expr, th: Expr, el: Expr, tpe: Type) extends Expr
  case class App(fun: Expr, arg: Expr, tpe: Type) extends Expr
  case class Fun(argType: Type, body: Expr) extends Expr {
    override val tpe = Type.Fun(argType, body.tpe)
  }
}
