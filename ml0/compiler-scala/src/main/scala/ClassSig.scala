package com.todesking.ojaml.ml0.compiler.scala

case class ClassSig(ref: ClassRef, parent: Option[ClassSig], interfaces: Seq[ClassSig], methods: Seq[MethodSig]) {
  def findAnyMethod(name: String, args: Seq[Type]): Option[MethodSig] =
    methods.find(m => m.name == name && assignableFrom(m.args, args))
  def findStaticMethod(name: String, args: Seq[Type]): Option[MethodSig] =
    findAnyMethod(name, args).find(_.isStatic)
  def findInstanceMethod(name: String, args: Seq[Type]): Option[MethodSig] =
    findAnyMethod(name, args).find(_.isInstance)

  private[this] def assignableFrom(xs: Seq[Type], ys: Seq[Type]): Boolean =
    xs.size == ys.size && xs.zip(ys).forall { case (x, y) => assignableFrom(x, y) }
  private[this] def assignableFrom(x: Type, y: Type): Boolean =
    x.boxed == y.boxed
}
