package com.todesking.ojaml.ml0.compiler.scala

sealed abstract class LitValue {
  val tpe: Type
  val value: Any
}
object LitValue {
  def of(v: String) = StringValue(v)
  def of(v: Int) = IntValue(v)
  def of(v: Boolean) = BoolValue(v)

  case class StringValue(value: String) extends LitValue {
    override val tpe = Type.String
  }
  case class IntValue(value: Int) extends LitValue {
    override val tpe = Type.Int
  }
  case class BoolValue(value: Boolean) extends LitValue {
    override val tpe = Type.Bool
  }
}
