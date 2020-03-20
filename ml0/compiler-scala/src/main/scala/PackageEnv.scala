package com.todesking.ojaml.ml0.compiler.scala

import util.Syntax._

import PackageEnv._
case class PackageEnv(
  cr: ClassRepo,
  moduleMembers: Map[ModuleRef, Map[String, ModuleMember]] = Map(),
  moduleTypeMembers: Map[ModuleRef, Set[String]] = Map()) {
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
  def findModuleMember(module: ModuleRef, name: String): Option[ModuleMember] =
    for {
      m <- moduleMembers.get(module)
      mm <- m.get(name)
    } yield mm

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
    else copy(moduleMembers = moduleMembers + (m -> Map()))

  def addModuleMember(m: ModuleRef, name: String, ctorInfo: Option[Seq[Type]]): PackageEnv = {
    require(modules.get(m.pkg).exists(_.contains(m.name)), s"Module ${m.fullName} not found")
    val mm = ModuleMember(name, ctorInfo)
    moduleMembers.get(m).fold {
      copy(moduleMembers = moduleMembers + (m -> Map(mm.name -> mm)))
    } { ms =>
      copy(moduleMembers = moduleMembers + (m -> (ms + (mm.name -> mm))))
    }
  }
  def addModuleTypeMember(m: ModuleRef, name: String): PackageEnv = {
    require(modules.get(m.pkg).exists(_.contains(m.name)), s"Module ${m.fullName} not found")
    moduleTypeMembers.get(m).fold {
      copy(moduleTypeMembers = moduleTypeMembers + (m -> Set(name)))
    } { ms =>
      copy(moduleTypeMembers = moduleTypeMembers + (m -> (ms + name)))
    }
  }

  def moduleMemberExists(m: ModuleRef, name: String): Boolean =
    moduleMembers.get(m).exists(_.contains(name))

  def pretty: String = "PackageEnv\n" + modules.toSeq.sortBy(_._1.fullName).map {
    case (k, vs) =>
      s"  package ${k.fullName}\n" + vs.toSeq.sorted.flatMap { v =>
        val m = k.moduleRef(v)
        (
          Seq(s"  - module $v") ++
          moduleTypeMembers.getOrElse(m, Set()).toSeq.sorted.map { name => s"    t: $name" } ++
          moduleMembers.getOrElse(m, Map()).values.toSeq.sortBy(_.name).map {
            case PackageEnv.ModuleMember(name, None) =>
              s"    v: ${name}"
            case PackageEnv.ModuleMember(name, Some(ts)) =>
              s"    v: ${name} ctor: ${ts.mkString(", ")}"
          })
      }.mkString("\n")
  }.mkString("\n")
}

object PackageEnv {
  case class ModuleMember(name: String, ctorInfo: Option[Seq[Type]])
}
