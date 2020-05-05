package com.todesking.ojaml.ml0.compiler.scala

sealed abstract class PackageRef {
  def parts: Seq[String]
  def fullName: String = parts.mkString(".")
  def internalName: String = parts.mkString("/")
  def classRef(name: String) = ClassRef(this, name)
  def moduleRef(name: String) = ModuleRef(this, name)
  def packageRef(name: String) = PackageRef.Child(this, name)
  def parentOption: Option[PackageRef]
  override def toString = fullName
}
object PackageRef {
  def root(name: String) = Root.packageRef(name)

  case object Root extends PackageRef {
    override def parts: Seq[Nothing] = Seq()
    override def parentOption = None
  }
  case class Child(parent: PackageRef, name: String) extends PackageRef {
    require(!name.contains(".") && !name.contains("/"), name)
    override def parts: Seq[String] = parent.parts :+ name
    override def parentOption = Some(parent)
  }

  // TODO: Handle ""
  def fromInternalName(n: String): PackageRef = { require(n.nonEmpty); fromParts(n.split("/")) }
  def fromParts(parts: Seq[String]): PackageRef =
    parts.foldLeft[PackageRef](Root) { (p, n) => Child(p, n) }
}

