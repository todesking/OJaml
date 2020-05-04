package com.todesking.ojaml.ml0.test

import com.todesking.ojaml.ml0.Repl

import java.nio.file.Path
import java.nio.file.Files
import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfterEach

import Repl.{ Result => RR }

class ReplTest extends FunSuite with BeforeAndAfterEach {
  private[this] var repl: Repl = _
  override def beforeEach(): Unit = {
    this.repl = new Repl()
    repl.evalPredef()
  }
  override def afterEach() = {
    repl.close()
  }

  val exprExamples = Seq[(String, PartialFunction[RR, Unit])](
    "1" -> { case RR.Value(_, 1, "int") => },
    "1 + 1" -> { case RR.Value(_, 2, "int") => },
    "aaa" -> { case RR.CompileError(_) => })
  exprExamples.foreach {
    case (src, assertFun) =>
      test(s"expr: $src") {
        assertFun(repl.eval(src))
      }
  }

  val longExamples = Seq[(Seq[String], PartialFunction[RR, Unit])](
    Seq(
      "let x = 1 ;",
      "x") -> { case RR.Value(_, 1, "int") => },
    Seq(
      "data E = L int | R int ;",
      "let m ee = match ee | L x => 1 | _ => 2 ;",
      "m (L 1)") -> { case RR.Value(_, 1, "int") => })
  longExamples.foreach {
    case (srcs, assertFun) =>
      test(s"inputs: ${srcs.mkString(" ")}") {
        srcs.init.foreach { src =>
          repl.eval(src) match {
            case RR.Empty | RR.Value(_, _, _) =>
            case err => fail(err.toString)
          }
        }
        assertFun(repl.eval(srcs.last))
      }
  }

  test("name override") {
    val res0 = repl.eval("let x = 1 ;")
    assert(res0 == Repl.Result.Value("x", 1, "int"))

    val res1 = repl.eval("let x = 2 ;")
    assert(res1 == Repl.Result.Value("x", 2, "int"))
  }
}
