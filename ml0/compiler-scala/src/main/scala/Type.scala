package com.todesking.ojaml.ml0.compiler.scala

trait Type

object Type {
  case object Int extends Type
  case object Bool extends Type
  case class Fun(l: Type, r: Type) extends Type
}
