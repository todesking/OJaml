package com.todesking.ojaml.ml0.compiler.scala

case class ModuleRef(pkg: PackageRef, name: String) {
  override def toString = s"ModuleRef(${pkg.fullName}.$name)"
  def internalName = s"${pkg.internalName}/$name"
}
