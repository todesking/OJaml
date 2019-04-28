package com.todesking.ojaml.ml0.compiler.scala

import Compiler.Error
import Util.SeqSyntax

sealed abstract class PackageMember
object PackageMember {
  case class Package(ref: PackageRef) extends PackageMember
  case class Class(ref: ClassRef) extends PackageMember
  case class Module(ref: ModuleRef) extends PackageMember
}

