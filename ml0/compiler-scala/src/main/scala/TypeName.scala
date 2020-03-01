package com.todesking.ojaml.ml0.compiler.scala

sealed abstract class TypeName {
  val pos: Pos
}
object TypeName {
  case class Atom(pos: Pos, name: String) extends TypeName {
    override def toString = name
  }
  case class Fun(l: TypeName, r: TypeName) extends TypeName {
    override val pos = l.pos
    override def toString = s"$l -> $r"
  }
}
