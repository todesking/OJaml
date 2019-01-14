package com.todesking.ojaml.ml0.compiler.scala

import org.objectweb.asm

sealed abstract class Type {
  def boxed: Type.Reference
  def unboxed: Option[Type.Primitive]
  def toString(group: Boolean): String
}
object Type {
  sealed abstract class Primitive extends Type {
    override def unboxed = None
    override def toString(group: Boolean) = toString
  }
  case object Int extends Primitive {
    override def boxed = BoxedInt
  }
  case object Bool extends Primitive {
    override def boxed = BoxedBool
  }

  sealed abstract class Reference extends Type {
    def className: String
    def externalName = className.replaceAll("/", ".")
    override def boxed = this
    override def unboxed = boxMap.get(className)
    override def toString(group: Boolean) = toString
  }
  object Reference {
    def unapply(r: Reference): Option[String] = Some(r.className)
  }

  case class Klass(className: String) extends Reference {
    require(!className.contains("."), s"Need internal name: $className")
  }
  case class Fun(l: Type, r: Type) extends Reference {
    override def className = Fun.className
    override def toString = toString(false)
    override def toString(group: Boolean) = {
      val naked = s"${l.toString(true)} -> $r"
      if (group) s"($naked)" else naked
    }
  }
  object Fun {
    val className = "com/todesking/ojaml/ml0/runtime/Fun"
  }

  val BoxedInt = Klass("java/lang/Integer")
  val BoxedBool = Klass("java/lang/Boolean")
  val String = Klass("java/lang/String")
  val Unit = Klass("com/todesking/ml0/runtime/Unit")

  val boxMap: Map[String, Primitive] = Map(
    BoxedInt.className -> Int,
    BoxedBool.className -> Bool)

  def from(t: asm.Type): Option[Type] = t.getSort match {
    case asm.Type.VOID => None
    case asm.Type.INT => Some(Type.Int)
    case asm.Type.BOOLEAN => Some(Type.Bool)
    case asm.Type.OBJECT => Some(Type.Klass(t.getInternalName))
    case _ => Some(Type.Unit) // :(
  }

  def toAsm(t: Option[Type]): asm.Type = t match {
    case None => asm.Type.VOID_TYPE
    case Some(t) => toAsm(t)
  }
  def toAsm(t: Type): asm.Type = t match {
    case Int => asm.Type.INT_TYPE
    case Bool => asm.Type.BOOLEAN_TYPE
    case Reference(name) => asm.Type.getObjectType(name)
  }

  def prettyMethod(name: String, args: Seq[Type], ret: Option[Type]): String = {
    s"$name(${args.mkString(", ")}): $ret"
  }
  def prettyMethod(name: String, args: Seq[Type]): String = {
    s"$name(${args.mkString(", ")}): ???"
  }
}
