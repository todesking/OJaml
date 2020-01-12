package com.todesking.ojaml.ml0.compiler.scala

case class Pos(location: String, line: Int, col: Int) {
  override def toString = s"$location:$line:$col"
}
object Pos {
  def fill[A <: HasPos](v: A, p: Pos): A = { v.fillPos(p); v }
}
