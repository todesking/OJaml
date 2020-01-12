package com.todesking.ojaml.ml0.compiler.scala

sealed abstract class TypeName extends HasPos
object TypeName {
  case class Atom(name: String) extends TypeName {
    override def toString = name
  }
  case class Fun(l: TypeName, r: TypeName) extends TypeName {
    override def toString = s"$l -> $r"
  }
}
