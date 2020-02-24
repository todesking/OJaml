package com.todesking.ojaml.ml0.compiler.scala

case class FieldRef(klass: ClassRef, name: String, tpe: JType) {
  override def toString = s"FieldRef(${klass.fullName}.$name: $tpe)"
}

