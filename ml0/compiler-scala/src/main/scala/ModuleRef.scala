package com.todesking.ojaml.ml0.compiler.scala

case class ModuleRef(pkg: String, name: String) {
  override def toString = s"ModuleRef($pkg.$name)"
}
