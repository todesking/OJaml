package com.todesking.ojaml.ml0.compiler.scala

sealed abstract class Result[+A] {
  def map[B](f: A => B): Result[B] =
    flatMap { x => Result.ok(f(x)) }
  def flatMap[B](f: A => Result[B]): Result[B]
  def fold[B](left: Seq[Result.Message] => B)(right: A => B): B
  def foreach(f: A => Unit): Unit
  def toEither: Either[Seq[Result.Message], A]
  def tapFail(f: Seq[Result.Message] => Unit): Result[A]
}
object Result {
  case class Message(pos: Pos, message: String)
  case class Success[A](value: A) extends Result[A] {
    override def flatMap[B](f: A => Result[B]): Result[B] =
      f(value)
    override def fold[B](left: Seq[Message] => B)(right: A => B): B =
      right(value)
    override def foreach(f: A => Unit): Unit = f(value)
    override def toEither: Either[Seq[Result.Message], A] = Right(value)
    override def tapFail(f: Seq[Message] => Unit): Result[A] = this
  }
  case class Failure(messages: Seq[Message]) extends Result[Nothing] {
    override def flatMap[B](f: Nothing => Result[B]): Result[B] =
      this
    override def fold[B](left: Seq[Message] => B)(right: Nothing => B): B =
      left(messages)
    override def foreach(f: Nothing => Unit): Unit = {}
    override def toEither: Either[Seq[Result.Message], Nothing] = Left(messages)
    def tapFail(f: Seq[Message] => Unit): Result[Nothing] = {
      f(messages)
      this
    }
  }

  implicit class MergeOps(val self: Result[Seq[Message]]) extends AnyVal {
    def merge = self match {
      case Success(xs) => xs
      case Failure(xs) => xs
    }
  }

  def ok[A](value: A): Result[A] =
    Success(value)

  def error(pos: Pos, msg: String): Result[Nothing] =
    Failure(Seq(Message(pos, msg)))

  def errors[A](errors: Seq[Message]): Result[Nothing] =
    Failure(errors)

  def validate[A](xs: Seq[Result[A]]): Result[Seq[A]] = {
    val rights = xs.collect { case Success(x) => x }
    if (rights.size == xs.size) ok(rights)
    else errors(xs.collect { case Failure(x) => x }.flatten)
  }

}
