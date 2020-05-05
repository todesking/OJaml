package com.todesking.ojaml.ml0.compiler.scala

import util.Syntax._

import NameEnv._
import NameEnv.Ref

case class NameEnv(
  cr: Classpath,
  hierarchies: Map[Ref, Map[String, Ref]] = Map(),
  types: Map[Ref, Type] = Map(),
  values: Set[Ref] = Set(),
  ctors: Map[Ref, (ModuleRef, String, Int)] = Map()) {
  def findRef(name: String): Option[Ref] =
    findRef(Ref.rootPackage, name)

  def findRef(parent: Ref, name: String): Option[Ref] = (
    hierarchies.get(parent).flatMap(_.get(name))
    orElse (parent match {
      case Ref.Package(pkg) =>
        if (cr.packageExists(pkg, name)) Some(Ref.Package(pkg.packageRef(name)))
        else if (cr.classExists(pkg, name)) Some(Ref.Klass(pkg.classRef(name)))
        else None
      case Ref.Klass(klass) => None
      case Ref.Module(module) => None
      case Ref.Member(module, name) => None
    }))

  def exists(pkg: PackageRef, name: String): Boolean =
    findRef(Ref.Package(pkg), name).nonEmpty

  def findType(ref: Ref): Option[Type] = ref match {
    case Ref.Klass(klass) => Some(Type.Klass(klass))
    case _ => types.get(ref)
  }

  def valueExists(ref: Ref): Boolean =
    values.contains(ref)

  def valueExists(module: ModuleRef, name: String): Boolean =
    valueExists(Ref.Member(module, name))

  def typeExists(module: ModuleRef, name: String): Boolean =
    findType(Ref.Member(module, name)).nonEmpty

  def findCtor(ref: Ref): Option[(ModuleRef, String, Int)] =
    ctors.get(ref)

  def addModule(module: ModuleRef): NameEnv =
    addRef(Ref.Module(module))

  def addTypeMember(module: ModuleRef, name: String, tpe: Type): NameEnv =
    addMember(module, name)
      .copy(types = types + (Ref.Member(module, name) -> tpe))

  def addValueMember(module: ModuleRef, name: String): NameEnv =
    addMember(module, name)
      .copy(values = values + Ref.Member(module, name))

  def addCtorMember(module: ModuleRef, name: String, arity: Int): NameEnv =
    addValueMember(module, name)
      .copy(ctors = ctors + (Ref.Member(module, name) -> (module, name, arity)))

  private[this] def addMember(module: ModuleRef, name: String): NameEnv =
    addRef(Ref.Member(module, name))

  private def addRef(ref: Ref): NameEnv = {
    parentOf(ref).fold {
      // root package
      this
    } { parent =>
      addRef(parent).addHierarchy(parent, ref)
    }
  }

  private def addHierarchy(parent: Ref, child: Ref): NameEnv = {
    val items = hierarchies.get(parent) getOrElse Map()
    copy(hierarchies = hierarchies + (parent -> (items + (nameOf(child) -> child))))
  }

  def parentOf(ref: Ref): Option[Ref] = ref match {
    case Ref.Package(pkg) =>
      pkg.parentOption.map(Ref.Package(_))
    case Ref.Klass(klass) =>
      Some(Ref.Package(klass.pkg))
    case Ref.Module(module) =>
      Some(Ref.Package(module.pkg))
    case Ref.Member(module, name) =>
      Some(Ref.Module(module))
  }

  def nameOf(ref: Ref): String = ref match {
    case Ref.Package(pkg) =>
      pkg.parts.lastOption getOrElse ""
    case Ref.Klass(klass) =>
      klass.name
    case Ref.Module(module) =>
      module.name
    case Ref.Member(module, name) =>
      name
  }

  def children(ref: Ref): Seq[Ref] =
    hierarchies.get(ref).map(_.values.toSeq) getOrElse Seq()

  lazy val modules: Seq[ModuleRef] =
    hierarchies.values.flatMap(_.values.collect { case Ref.Module(m) => m }).toSeq

  def pretty: String = "NameEnv\n" + modules.sortBy(_.fullName).map { module =>
    s"  module $module\n" + children(Ref.Module(module)).map { member =>
      val name = nameOf(member)
      (
        findType(member).fold("") { tpe => s"    t: $name = $tpe\n" }
        + (if (valueExists(member)) s"    v: $name\n" else "")
        + findCtor(member).fold("") { case (module, name, arity) => s"    c: $name arity=$arity" })
    }.mkString("\n")
  }.mkString("\n")
}

object NameEnv {
  case class ModuleMember(name: String, ctorInfo: Option[Seq[Type]])

  sealed abstract class Ref
  object Ref {
    val rootPackage = Package(PackageRef.Root)

    case class Package(pkg: PackageRef) extends Ref {
      override def toString = s"package $pkg"
    }
    case class Klass(klass: ClassRef) extends Ref {
      override def toString = s"class $klass"
    }
    case class Module(module: ModuleRef) extends Ref {
      override def toString = s"module $module"
    }
    case class Member(module: ModuleRef, name: String) extends Ref {
      override def toString = s"member $module.$name"
    }
  }
}
