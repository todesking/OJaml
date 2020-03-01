package com.todesking.ojaml.ml0.compiler.scala

case class Name(pos: Pos, value: String) {
  override def toString = s"Name($value)"
}
