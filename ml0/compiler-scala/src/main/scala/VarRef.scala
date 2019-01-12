package com.todesking.ojaml.ml0.compiler.scala

sealed abstract class VarRef
object VarRef {
  sealed abstract class Typable extends VarRef
  case class Class(sig: ClassSig) extends VarRef
  case class Package(name: String) extends VarRef
  case class Module(module: ModuleRef, name: String) extends Typable
  case class Local(name: String) extends Typable
}
