package com.todesking.ojaml.ml0.compiler.scala

object Parser extends scala.util.parsing.combinator.RegexParsers {
  def parse(s: String): ParseResult[AST.Program] =
    parseAll(program, s)

  private[this] var _skipWS = true
  override def skipWhitespace = _skipWS

  private[this] def kwd(a: String): Parser[Unit] =
    regex(s"${java.util.regex.Pattern.quote(a)}\\s+".r) ^^ { _ => () }

  private[this] val discard = { _: Any => () }

  private[this] def commentWS: Parser[Unit] = """\s*""".r ~ comment.? ^^ discard
  private[this] def comment: Parser[Unit] = "(*" ~ (comment | (not("*)") ~ ".".r)).* ~ "*)" ~ """\s*""".r ^^ discard
  override protected def handleWhiteSpace(source: CharSequence, offset: Int): Int = {
    if (!_skipWS) offset
    else {
      val oSource = new scala.util.parsing.input.CharSequenceReader(source, offset)
      try {
        _skipWS = false
        val newOffset = parse(commentWS, oSource).next.offset
        newOffset
      } finally {
        _skipWS = true
      }
    }
  }

  val name: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r
  val qname: Parser[Seq[String]] = rep1sep(name, ".")

  lazy val program = pkg ~ struct ^^ { case p ~ s => AST.Program(p, s) }
  lazy val pkg = kwd("package") ~> qname ^^ { _.mkString(".") }
  lazy val struct = kwd("struct") ~> (name <~ "{") ~ rep(term) <~ "}" ^^ {
    case n ~ ts => AST.Struct(n, ts)
  }

  def term: Parser[AST.Term] = tlet
  def tlet = kwd("let") ~> (name <~ "=") ~ expr <~ ";;" ^^ { case n ~ e => AST.TLet(n, e) }
  def expr: Parser[AST.Expr] = lit_int
  val lit_int = """[0-9]+""".r ^^ { i => AST.LitInt(i.toInt) }
}
