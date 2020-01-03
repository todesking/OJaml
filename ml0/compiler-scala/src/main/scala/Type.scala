package com.todesking.ojaml.ml0.compiler.scala

sealed abstract class Type {
  def boxed: Type.Reference
  def unboxed: Option[Type.Primitive]
  def toString(group: Boolean): String
  override def toString = toString(false)
  def freeTypeVariables: Set[Type.Var]
  def substitute(a: Type.Var, t: Type): Type
  def javaName: String
  def jtype: JType
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
    override def jtype = JType.TInt
  }
  case object Bool extends Primitive {
    override def boxed: Klass = BoxedBool
    override def toString(group: Boolean) = "bool"
    override def javaName = "boolean"
    override def jtype = JType.TBool
  }

  sealed abstract class Reference extends Type {
    def ref: ClassRef
    override def boxed: Reference = this
    override def unboxed: Option[Primitive] = boxMap.get(ref.internalName)
    override def toString(group: Boolean): String = ref.fullName
    override def javaName = ref.fullName
    override def jtype = JType.TKlass(ref)
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
  val Unit = Klass("com/todesking/ojaml/ml0/runtime/Unit")

  val boxMap: Map[String, Primitive] = Map(
    BoxedInt.ref.internalName -> Int,
    BoxedBool.ref.internalName -> Bool)

  def prettyMethod(name: String, args: Seq[Type], ret: Option[Type]): String = {
    s"$name(${args.mkString(", ")}): $ret"
  }
  def prettyMethod(name: String, args: Seq[Type]): String = {
    s"$name(${args.mkString(", ")}): ???"
  }
}
