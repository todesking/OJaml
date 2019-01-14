package com.todesking.ojaml.ml0.compiler.scala

sealed abstract class VarRef
object VarRef {
  sealed abstract class Special extends VarRef
  case class Class(name: String) extends Special
  case class Package(name: String) extends Special

  sealed abstract class Typable extends VarRef
  case class Module(module: ModuleRef, name: String) extends Typable
  case class Local(depth: Int) extends Typable
}
