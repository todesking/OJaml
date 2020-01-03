package com.todesking.ojaml.ml0.compiler.scala

import org.objectweb.asm

case class MethodSig(klass: ClassRef, isStatic: Boolean, isInterface: Boolean, name: String, args: Seq[JType], ret: Option[JType]) {
  require(!(isStatic && isInterface))

  def isInstance: Boolean = !isStatic

  lazy val descriptor: String =
    asm.Type.getMethodType(JType.toAsm(ret), args.map(JType.toAsm).toArray: _*).getDescriptor

  override def toString = s"${klass.fullName}.$name: $descriptor"
}
