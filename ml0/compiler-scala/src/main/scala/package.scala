package com.todesking.ojaml.ml0.compiler

package object scala {
  type Result[A] = Either[Seq[Result.Error], A]
}
