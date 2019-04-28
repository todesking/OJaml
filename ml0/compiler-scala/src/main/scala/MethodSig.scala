package com.todesking.ojaml.ml0.compiler.scala

import org.objectweb.asm

case class MethodSig(klass: ClassRef, isStatic: Boolean, isInterface: Boolean, name: String, args: Seq[Type], ret: Option[Type]) {
  require(!(isStatic && isInterface))

  def isInstance = !isStatic

  lazy val descriptor =
    asm.Type.getMethodType(Type.toAsm(ret), args.map(Type.toAsm).toArray: _*).getDescriptor
}
