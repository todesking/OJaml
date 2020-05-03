package com.todesking.ojaml.ml0.compiler.scala

case class TypeEnv(classpath: Classpath, types: Map[VarRef.ModuleMember, Type]) {
  def addTypes(xs: Map[VarRef.ModuleMember, Type]): TypeEnv =
    copy(types = types ++ xs)
}

