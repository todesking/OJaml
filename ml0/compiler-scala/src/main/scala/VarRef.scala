package com.todesking.ojaml.ml0.compiler.scala

sealed abstract class VarRef
object VarRef {
  case class ModuleMember(module: ModuleRef, name: String) extends VarRef
  case class Local(depth: Int, index: Int) extends VarRef
}
