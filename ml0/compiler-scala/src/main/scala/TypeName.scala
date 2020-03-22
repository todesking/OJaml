package com.todesking.ojaml.ml0.compiler.scala

sealed abstract class TypeName {
  val pos: Pos
  override final def toString = toString(false)
  def toString(group: Boolean): String
}
object TypeName {
  private[this] def wrap(group: Boolean, naked: String) =
    if (group) s"($naked)" else naked

  case class Atom(pos: Pos, name: String) extends TypeName {
    override def toString(group: Boolean) = name
  }
  // TOTO: Make pos to point "=>"
  case class Fun(l: TypeName, r: TypeName) extends TypeName {
    override val pos = l.pos
    override def toString(group: Boolean) = wrap(group, s"$l -> $r")
  }
  case class App(pos: Pos, name: TypeName, args: Seq[TypeName]) extends TypeName {
    override def toString(group: Boolean) = wrap(group, s"$name ${args.mkString(" ")}")
  }
}
