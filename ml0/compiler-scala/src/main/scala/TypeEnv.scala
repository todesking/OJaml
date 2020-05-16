package com.todesking.ojaml.ml0.compiler.scala

case class TypeEnv(classpath: Classpath, types: Map[MemberRef, Type]) {
  def addTypes(xs: Map[MemberRef, Type]): TypeEnv =
    copy(types = types ++ xs)
}

