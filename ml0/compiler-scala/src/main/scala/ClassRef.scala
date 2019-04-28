package com.todesking.ojaml.ml0.compiler.scala

case class ClassRef(pkg: PackageRef, name: String) {
  require(!name.contains(".") && !name.contains("/"), name)
  def parts: Seq[String] = pkg.parts :+ name
  def fullName: String = parts.mkString(".")
  def internalName: String = parts.mkString("/")
  def resourceName = s"$internalName.class"
  override def toString = s"ClassRef($fullName)"
}
object ClassRef {
  def fromInternalName(n: String): ClassRef =
    fromParts(n.split("/"))
  def fromParts(parts: Seq[String]) =
    ClassRef(PackageRef.fromParts(parts.init), parts.last)
}
