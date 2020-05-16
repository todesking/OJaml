package com.todesking.ojaml.ml0.compiler.scala

sealed abstract class Type {
  def toString(group: Boolean): String
  override def toString = toString(false)
  def freeTypeVariables: Set[Type.Var]
  def substitute(a: Type.Var, t: Type): Type
  def jtype: JType
}
object Type {
  sealed abstract class Primitive extends Type {
    override def substitute(a: Type.Var, t: Type) = this
    override def freeTypeVariables = Set()
  }
  case object Int extends Primitive {
    override def toString(group: Boolean) = "Int"
    override def jtype = JType.TInt
  }
  case object Bool extends Primitive {
    override def toString(group: Boolean) = "Bool"
    override def jtype = JType.TBool
  }

  sealed abstract class Reference extends Type {
    override def jtype: JType.TReference
  }

  case class Var(id: Int) extends Reference {
    override def toString(group: Boolean) = s"?$id"
    override def substitute(a: Type.Var, t: Type) = if (a == this) t else this
    override def freeTypeVariables = Set(this)
    override def jtype = JType.TObject
  }

  case class Data(module: ModuleRef, name: String, args: Seq[Type]) extends Reference {
    override def substitute(a: Type.Var, t: Type) = copy(args = args.map(_.substitute(a, t)))
    override def freeTypeVariables = args.flatMap(_.freeTypeVariables).toSet
    override def jtype = JType.dataClass(module, name)
    override def toString(group: Boolean): String =
      if (args.isEmpty) name else {
        val naked = s"$name ${args.map(_.toString(true)).mkString(" ")}"
        if (group) s"($naked)" else naked
      }
  }

  case class Abs(params: Seq[Var], body: Type) extends Type {
    override def jtype = body.jtype
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
    override def toString(group: Boolean): String = ref.fullName
    override def jtype = JType.TKlass(ref)
  }
  object Klass {
    def apply(internalName: String): Klass =
      Klass(ClassRef.fromInternalName(internalName))
  }
  case class Fun(l: Type, r: Type) extends Reference {
    override def toString(group: Boolean): String = {
      val naked = s"${l.toString(true)} -> ${r.toString(false)}"
      if (group) s"($naked)" else naked
    }
    override def substitute(a: Type.Var, t: Type) =
      Fun(l.substitute(a, t), r.substitute(a, t))
    override def freeTypeVariables = l.freeTypeVariables ++ r.freeTypeVariables
    override def jtype = JType.TKlass(Fun.ref)
  }
  object Fun {
    val ref: ClassRef = ClassRef.fromInternalName("com/todesking/ojaml/ml0/runtime/Fun")
  }

  val String = Klass("java/lang/String")
  val Unit = Klass("com/todesking/ojaml/ml0/runtime/Unit")

  def prettyMethod(name: String, args: Seq[Type], ret: Option[Type]): String = {
    s"$name(${args.mkString(", ")}): $ret"
  }
  def prettyMethod(name: String, args: Seq[Type]): String = {
    s"$name(${args.mkString(", ")}): ???"
  }
}
