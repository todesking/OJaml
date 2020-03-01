package com.todesking.ojaml.ml0.compiler.scala

case class QName(parts: Seq[Name]) {
  require(parts.nonEmpty)
  def value: String = parts.map(_.value).mkString(".")
  def pos = parts.head.pos
  def internalName: String = parts.map(_.value).mkString("/")
  def asPackage: PackageRef = PackageRef.fromParts(parts.map(_.value))
  override def toString = s"QName($value)"
}
