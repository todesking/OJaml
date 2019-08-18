package com.todesking.ojaml.ml0.compiler.scala

import org.objectweb.asm

sealed abstract class Type {
  def boxed: Type.Reference
  def unboxed: Option[Type.Primitive]
  def toString(group: Boolean): String
  override def toString = toString(false)
  def freeTypeVariables: Set[Type.Var]
  def substitute(a: Type.Var, t: Type): Type
  def javaName: String
}
object Type {
  sealed abstract class Primitive extends Type {
    override def unboxed: None.type = None
    override def substitute(a: Type.Var, t: Type) = this
    override def freeTypeVariables = Set()
  }
  case object Int extends Primitive {
    override def boxed: Klass = BoxedInt
    override def toString(group: Boolean) = "int"
    override def javaName = "int"
  }
  case object Bool extends Primitive {
    override def boxed: Klass = BoxedBool
    override def toString(group: Boolean) = "bool"
    override def javaName = "boolean"
  }

  sealed abstract class Reference extends Type {
    def ref: ClassRef
    override def boxed: Reference = this
    override def unboxed: Option[Primitive] = boxMap.get(ref.internalName)
    override def toString(group: Boolean): String = ref.fullName
    override def javaName = ref.fullName
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

  case class Data(module: ModuleRef, name: String) extends Reference {
    override def ref = ClassRef(module.pkg, s"${module.name}$$data_$name")
    override def substitute(a: Type.Var, t: Type) = this
    override def freeTypeVariables = Set()
    override def toString = name
  }

  case class Abs(params: Seq[Var], body: Type) extends Reference {
    override def ref = body.boxed.ref
    override def toString(group: Boolean) = {
      val naked = s"[${params.map(_.toString(false)).mkString(", ")}] ${body.toString(false)}"
      if (group) s"($naked)" else naked
    }
    override def substitute(a: Type.Var, t: Type) =
      if (params.contains(a)) this
      else Abs(params, body.substitute(a, t))
    override def freeTypeVariables = body.freeTypeVariables -- params
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
