package com.todesking.ojaml.ml0.compiler.scala

import org.objectweb.asm

sealed abstract class JType {
  def isPrimitive: Boolean
  def isBoxed: Boolean
  def boxed: JType.TReference
  def unboxed: Option[JType.TPrimitive]
  def jname: String
  def jsig: String
  def hname: String // for human
  def tpe: Type
}
object JType {
  sealed abstract class TReference extends JType {
    override def isPrimitive: Boolean = false
    override def isBoxed = unboxMap.contains(this)
    override def boxed = this
    override def unboxed = unboxMap.get(this)
  }

  case class TKlass(ref: ClassRef) extends TReference {
    override def jname = ref.internalName
    override def jsig = s"L$jname;"
    override def hname = ref.fullName
    override def tpe = Type.Klass(ref)
  }
  object TKlass {
    def fromInternalName(s: String) =
      TKlass(ClassRef.fromInternalName(s))
  }

  case class TArray(element: JType) extends TReference {
    override def jname = s"[${element.jsig}"
    override def jsig = jname
    override def hname = s"${element.hname}[]"
    override def tpe = ???
  }

  sealed abstract class TPrimitive extends JType {
    override def isBoxed = false
    override def isPrimitive: Boolean = true
    override def unboxed = None
    override def boxed = boxMap(this)
    override def jsig = jname
    override def tpe = this match {
      case TInt => Type.Int
      case TBool => Type.Bool
    }
  }
  case object TInt extends TPrimitive {
    override def jname = "I"
    override def hname = "int"
  }
  case object TBool extends TPrimitive {
    override def jname = "Z"
    override def hname = "boolean"
  }

  private[this] def klass(name: String) = TKlass.fromInternalName(name)

  val BoxedInt = klass("java/lang/Integer")
  val BoxedBool = klass("java/lang/Boolean")

  val TUnit = klass("com/todesking/ojaml/ml0/runtime/Unit")
  val TString = klass("java/lang/String")
  val TObject = klass("java/lang/Object")

  val ObjectArray = TArray(TObject)
  val Fun = klass("com/todesking/ojaml/ml0/runtime/Fun")

  val boxMap: Map[TPrimitive, TKlass] = Map(
    TInt -> BoxedInt,
    TBool -> BoxedBool)
  val unboxMap: Map[TReference, TPrimitive] = boxMap.map { case (k, v) => v -> k }

  def from(t: asm.Type): Option[JType] = t.getSort match {
    case asm.Type.VOID => None
    case asm.Type.INT => Some(TInt)
    case asm.Type.BOOLEAN => Some(TBool)
    case asm.Type.OBJECT => Some(TKlass.fromInternalName(t.getInternalName))
    case _ => Some(TUnit) // :(
  }

  def toAsm(t: Option[JType]): asm.Type = t match {
    case None => asm.Type.VOID_TYPE
    case Some(t) => toAsm(t)
  }
  def toAsm(t: JType): asm.Type = t match {
    case TInt => asm.Type.INT_TYPE
    case TBool => asm.Type.BOOLEAN_TYPE
    case TKlass(ref) => asm.Type.getObjectType(ref.internalName)
    case TArray(elm) => asm.Type.getType(t.jsig)
  }
}
