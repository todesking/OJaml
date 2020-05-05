package com.todesking.ojaml.ml0.compiler.scala

case class QName(parts: Seq[Name]) {
  require(parts.nonEmpty)
  def fullName: String = parts.map(_.value).mkString(".")
  def pos = parts.head.pos
  def lastPartPos = parts.last.pos
  val size = parts.size
  val head = parts.head
  def last = parts.last
  def internalName: String = parts.map(_.value).mkString("/")
  def asPackage: PackageRef = PackageRef.fromParts(parts.map(_.value))
  override def toString = fullName
}
