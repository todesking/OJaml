package com.todesking.ojaml.ml0.compiler.scala.util

import com.todesking.ojaml.ml0.compiler.scala.Pos
import com.todesking.ojaml.ml0.compiler.scala.Result

object Syntax {
  // E means "with error"
  // C means "with context"
  implicit class SeqSyntax[A](val self: Seq[A]) extends AnyVal {
    def foldLeftE[B](init: B)(f: (B, A) => Result[B]): Result[B] =
      self.foldLeft(Result.ok(init)) {
        case (a, x) =>
          a.flatMap(f(_, x))
      }
    def mapWithContextEC[B, C, E](init: B)(f: (B, A) => Result[(B, C)]): Result[(B, Seq[C])] =
      self.foldLeftE((init, Seq.empty[C])) {
        case ((c, a), x) => f(c, x).map { case (cc, y) => (cc, a :+ y) }
      }

    def mapWithContextE[B, C, E](init: B)(f: (B, A) => Result[(B, C)]): Result[Seq[C]] =
      self.mapWithContextEC(init)(f).map(_._2)

    def mapWithContextC[B, C](init: B)(f: (B, A) => (B, C)): (B, Seq[C]) =
      self.foldLeft((init, Seq.empty[C])) {
        case ((c, a), x) =>
          val (cc, y) = f(c, x)
          (cc, a :+ y)
      }

    def mapWithContext[B, C](init: B)(f: (B, A) => (B, C)): Seq[C] =
      mapWithContextC(init)(f)._2

    def mapE[B, E](f: A => Result[B]): Result[Seq[B]] =
      self.mapWithContextE(()) { (_, x) => f(x).map { y => ((), y) } }
  }

  implicit class SeqResultSyntax[A](val self: Seq[Result[A]]) extends AnyVal {
    def validated: Result[Seq[A]] = {
      val rights = self.collect { case Result.Success(x) => x }
      if (rights.size == self.size) Result.ok(rights)
      else Result.errors(self.collect { case Result.Failure(x) => x }.flatten)
    }
  }

  implicit class OptionSyntax[A](val self: Option[A]) extends AnyVal {
    def toResult(pos: Pos, message: String): Result[A] =
      self.map(Result.ok).getOrElse(Result.error(pos, message))
    def mapResult[B](f: A => Result[B]): Result[Option[B]] =
      self.fold[Result[Option[B]]](Result.ok(None))(f(_).map(Some.apply))
  }

  implicit class DebugSyntax[A](val self: A) extends AnyVal {
    def debug(f: PartialFunction[A, String]): A = {
      if (f.isDefinedAt(self)) println(f(self))
      self
    }
  }
}
