package com.todesking.ojaml.ml0.compiler.scala

// Note: Result is just a type alias
object Result {
  case class Error(pos: Pos, message: String)
  def error[A](pos: Pos, msg: String): Result[A] = Left(errorValue(pos, msg))
  def errorValue(pos: Pos, msg: String): Seq[Error] = Seq(Error(pos, msg))
}
