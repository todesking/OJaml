package com.todesking.ojaml.ml0.compiler.scala.util

object Syntax {
  implicit class SeqSyntax[A](self: Seq[A]) {
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
}
