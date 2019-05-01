package com.todesking.ojaml.ml0.compiler.scala

import org.objectweb.asm

sealed abstract class Type {
  def boxed: Type.Reference
  def unboxed: Option[Type.Primitive]
  def toString(group: Boolean): String
  def freeTypeVariables: Set[Type]
  def substitute(a: Type.Var, t: Type): Type
}
object Type {
  sealed abstract class Primitive extends Type {
    override def unboxed: None.type = None
    override def toString(group: Boolean): String = toString
    override def substitute(a: Type.Var, t: Type) = this
    override def freeTypeVariables = Set()
  }
  case object Int extends Primitive {
    override def boxed: Klass = BoxedInt
  }
  case object Bool extends Primitive {
    override def boxed: Klass = BoxedBool
  }

  sealed abstract class Reference extends Type {
    def ref: ClassRef
    override def boxed: Reference = this
    override def unboxed: Option[Primitive] = boxMap.get(ref.internalName)
    override def toString(group: Boolean): String = toString
  }
  object Reference {
    def unapply(r: Reference): Option[ClassRef] = Some(r.ref)
  }

  case class Var(id: Int) extends Reference {
    override def ref = Object.ref
    override def toString(group: Boolean) = s"?$id"
    override def substitute(a: Type.Var, t: Type) = if (a == this) t else this
    override def freeTypeVariables = Set(this)
  }

  case class Klass(ref: ClassRef) extends Reference {
    override def substitute(a: Type.Var, t: Type) = this
    override def freeTypeVariables = Set()
  }
  object Klass {
    def apply(internalName: String): Klass =
      Klass(ClassRef.fromInternalName(internalName))
  }
  val Object = Klass("java/lang/Object")
  case class Fun(l: Type, r: Type) extends Reference {
    override def ref: ClassRef = Fun.ref
    override def toString: String = toString(false)
    override def toString(group: Boolean): String = {
      val naked = s"${l.toString(true)} -> ${r.toString(false)}"
      if (group) s"($naked)" else naked
    }
    override def substitute(a: Type.Var, t: Type) =
      Fun(l.substitute(a, t), r.substitute(a, t))
    override def freeTypeVariables = l.freeTypeVariables ++ r.freeTypeVariables
  }
  object Fun {
    val ref: ClassRef = ClassRef.fromInternalName("com/todesking/ojaml/ml0/runtime/Fun")
  }

  val BoxedInt = Klass("java/lang/Integer")
  val BoxedBool = Klass("java/lang/Boolean")
  val String = Klass("java/lang/String")
  val Unit = Klass("com/todesking/ml0/runtime/Unit")

  val boxMap: Map[String, Primitive] = Map(
    BoxedInt.ref.internalName -> Int,
    BoxedBool.ref.internalName -> Bool)

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
    case Reference(ref) => asm.Type.getObjectType(ref.internalName)
  }

  def prettyMethod(name: String, args: Seq[Type], ret: Option[Type]): String = {
    s"$name(${args.mkString(", ")}): $ret"
  }
  def prettyMethod(name: String, args: Seq[Type]): String = {
    s"$name(${args.mkString(", ")}): ???"
  }
}
