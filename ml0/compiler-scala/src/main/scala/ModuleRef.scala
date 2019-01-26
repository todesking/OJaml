package com.todesking.ojaml.ml0.compiler.scala

case class ModuleRef(pkg: PackageRef, name: String) {
  override def toString = s"ModuleRef($fullName)"
  def fullName = s"${pkg.fullName}.$name"
  def internalName = s"${pkg.internalName}/$name"
}
