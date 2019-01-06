package com.todesking.ojaml.ml0.compiler.scala

import org.objectweb.asm

sealed abstract class Type {
  def boxed: Type.Reference
}
object Type {
  sealed abstract class Primitive extends Type
  case object PInt extends Primitive {
    override def boxed = Int
  }
  case object PBool extends Primitive {
    override def boxed = Bool
  }

  sealed abstract class Reference extends Type {
    def className: String
    def externalName = className.replaceAll("/", ".")
    override def boxed = this
  }
  object Reference {
    def unapply(r: Reference): Option[String] = Some(r.className)
  }

  case class Klass(className: String) extends Reference {
    require(!className.contains("."), s"Need internal name: $className")
  }
  case class Fun(l: Type, r: Type) extends Reference {
    override def className = Fun.className
  }
  object Fun {
    val className = "com/todesking/ojaml/ml0/runtime/Fun"
  }

  val Int = Klass("java/lang/Integer")
  val Bool = Klass("java/lang/Boolean")
  val String = Klass("java/lang/String")
  val Unit = Klass("com/todesking/ml0/runtime/Unit")

  def from(t: asm.Type): Option[Type] = t.getSort match {
    case asm.Type.VOID => None
    case asm.Type.INT => Some(Type.PInt)
    case asm.Type.BOOLEAN => Some(Type.PBool)
    case asm.Type.OBJECT => Some(Type.Klass(t.getInternalName))
    case _ => Some(Type.Unit) // :(
  }

  def toAsm(t: Option[Type]): asm.Type = t match {
    case None => asm.Type.VOID_TYPE
    case Some(t) => toAsm(t)
  }
  def toAsm(t: Type): asm.Type = t match {
    case PInt => asm.Type.INT_TYPE
    case PBool => asm.Type.BOOLEAN_TYPE
    case Reference(name) => asm.Type.getObjectType(name)
  }

  def prettyMethod(name: String, args: Seq[Type], ret: Option[Type]): String = {
    s"$name(${args.mkString(", ")}): $ret"
  }
  def prettyMethod(name: String, args: Seq[Type]): String = {
    s"$name(${args.mkString(", ")}): ???"
  }
}
