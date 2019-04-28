package com.todesking.ojaml.ml0.compiler.scala

import Compiler.Error
import Util.SeqSyntax

case class PackageEnv(cr: ClassRepo, moduleMembers: Map[ModuleRef, Set[String]] = Map()) {
  def findMember(pkg: PackageRef, name: String): Option[PackageMember] = {
    if (modules.get(pkg).exists(_.contains(name)))
      Some(PackageMember.Module(ModuleRef(pkg, name)))
    else if (cr.classExists(pkg, name))
      Some(PackageMember.Class(ClassRef(pkg, name)))
    else if (packageExists(pkg, name))
      Some(PackageMember.Package(pkg.packageRef(name)))
    else
      None
  }

  lazy val modulePackages: Set[PackageRef] = moduleMembers.keys.flatMap { m =>
    m.pkg.parts.inits.map(PackageRef.fromParts)
  }.toSet

  lazy val modules: Map[PackageRef, Set[String]] = moduleMembers.keys.map { m =>
    m.pkg -> m.name
  }.groupBy(_._1).map { case (p, ns) => p -> ns.map(_._2).toSet }

  def packageExists(pkg: PackageRef, name: String): Boolean =
    cr.packageExists(pkg, name) || modulePackages.contains(pkg.packageRef(name))

  def memberExists(pkg: PackageRef, name: String): Boolean =
    findMember(pkg, name).nonEmpty

  def addModule(m: ModuleRef): PackageEnv =
    if (moduleMembers.contains(m)) this
    else copy(moduleMembers = moduleMembers + (m -> Set.empty[String]))

  def addModuleMember(m: ModuleRef, name: String): PackageEnv = {
    require(modules.get(m.pkg).exists(_.contains(m.name)), s"Module ${m.fullName} not found")
    moduleMembers.get(m).fold {
      copy(moduleMembers = moduleMembers + (m -> Set(name)))
    } { ms =>
      copy(moduleMembers = moduleMembers + (m -> (ms + name)))
    }
  }

  def moduleMemberExists(m: ModuleRef, name: String): Boolean =
    moduleMembers.get(m).exists(_.contains(name))

  def pretty: String = "PackageEnv\n" + modules.toSeq.sortBy(_._1.fullName).map {
    case (k, vs) =>
      s"  package ${k.fullName}\n" + vs.toSeq.sorted.map { v => s"  - module $v" }.mkString("\n")
  }.mkString("\n")
}
