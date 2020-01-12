package com.todesking.ojaml.ml0.compiler.scala

case class QName(parts: Seq[Name]) extends HasPos {
  require(parts.nonEmpty)
  def value: String = parts.map(_.value).mkString(".")
  def internalName: String = parts.map(_.value).mkString("/")
  def asPackage: PackageRef = PackageRef.fromParts(parts.map(_.value))
  override def toString = s"QName($value)"
}
