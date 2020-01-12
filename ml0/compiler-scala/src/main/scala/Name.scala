package com.todesking.ojaml.ml0.compiler.scala

case class Name(value: String) extends HasPos {
  override def toString = s"Name($value)"
}
