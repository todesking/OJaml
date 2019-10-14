package com.todesking.ojaml.ml0.compiler.scala

sealed abstract class VarRef
object VarRef {
  case class ModuleMember(module: ModuleRef, name: String) extends VarRef {
    override def toString = s"${module.fullName}.$name"
  }
  case class Local(depth: Int, index: Int) extends VarRef {
    override def toString = s"local($depth, $index)"
  }
}
