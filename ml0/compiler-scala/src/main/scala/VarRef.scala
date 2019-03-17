package com.todesking.ojaml.ml0.compiler.scala

sealed abstract class VarRef
object VarRef {
  sealed abstract class Special extends VarRef
  case class TopLevel(ref: PackageMember) extends VarRef

  sealed abstract class Typable extends VarRef
  case class ModuleMember(module: ModuleRef, name: String) extends Typable
  case class Local(depth: Int, index: Int) extends Typable
}
