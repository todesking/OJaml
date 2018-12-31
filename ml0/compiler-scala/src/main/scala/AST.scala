package com.todesking.ojaml.ml0.compiler.scala

sealed abstract class AST
object AST {
  case class Program(pkg: String, item: Struct) extends AST
  case class Struct(name: String, body: Seq[Term]) extends AST

  sealed abstract class Term extends AST
  case class TLet(name: String, expr: Expr) extends Term

  sealed abstract class Expr extends Term
  case class LitInt(value: Int) extends Expr
}

sealed abstract class TAST
object TAST {
  case class Program(pkg: String, item: Struct) extends TAST
  case class Struct(name: String, body: Seq[Term]) extends TAST

  sealed abstract class Term extends TAST
  case class TLet(name: String, tpe: Type, expr: Expr) extends Term

  sealed abstract class Expr extends Term {
    def tpe: Type
  }
  case class LitInt(value: Int) extends Expr {
    override def tpe = Type.Int
  }
}
