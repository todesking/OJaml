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
  case class LitInt(value: Int) extends Expr {
    override def tpe = Type.Int
  }
}
