package com.todesking.ojaml.ml0.compiler.scala

case class MemberRef(module: ModuleRef, name: String) {
  override def toString = s"$module.$name"
}
