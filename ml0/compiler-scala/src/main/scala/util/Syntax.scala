package com.todesking.ojaml.ml0.compiler.scala.util

import com.todesking.ojaml.ml0.compiler.scala.Pos
import com.todesking.ojaml.ml0.compiler.scala.Result

object Syntax {
  implicit class SeqSyntax[A](val self: Seq[A]) extends AnyVal {
    def mapWithContextEC[B, C, E](init: B)(f: (B, A) => Either[E, (B, C)]): Either[E, (B, Seq[C])] =
      self.foldLeft[Either[E, (B, Seq[C])]](Right((init, Seq.empty[C]))) {
        case (Right((c, a)), x) => f(c, x).map { case (cc, y) => (cc, a :+ y) }
        case (Left(e), x) => Left(e)
      }

    def mapWithContextE[B, C, E](init: B)(f: (B, A) => Either[E, (B, C)]): Either[E, Seq[C]] =
      self.mapWithContextEC(init)(f).map(_._2)

    def mapWithContext[B, C](init: B)(f: (B, A) => (B, C)): Seq[C] =
      self.foldLeft((init, Seq.empty[C])) {
        case ((c, a), x) =>
          val (cc, y) = f(c, x)
          (cc, a :+ y)
      }._2

    def mapE[B, E](f: A => Either[E, B]): Either[E, Seq[B]] =
      self.mapWithContextE(()) { (_, x) => f(x).map { y => ((), y) } }
  }

  implicit class SeqResultSyntax[A](val self: Seq[Result[A]]) extends AnyVal {
    def validated: Result[Seq[A]] = {
      val rights = self.collect { case Right(x) => x }
      if (rights.size == self.size) Right(rights)
      else Left(self.collect { case Left(x) => x }.flatten)
    }
  }

  implicit class OptionSyntax[A](val self: Option[A]) extends AnyVal {
    def toResult(pos: Pos, message: String): Result[A] =
      self.toRight(Result.errorValue(pos, message))
    def mapResult[B](f: A => Result[B]): Result[Option[B]] =
      self.fold[Result[Option[B]]](Right(None))(f(_).map(Some.apply))
  }
}
