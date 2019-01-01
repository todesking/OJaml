package com.todesking.ojaml.ml0.compiler.scala

object Parser extends scala.util.parsing.combinator.RegexParsers {
  import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => T }

  def parse(s: String): ParseResult[T.Program] =
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

  val name: Parser[Name] = positioned("""[a-zA-Z][a-zA-Z0-9_]*""".r ^^ { s => Name(s) })
  val qname: Parser[QName] = positioned(rep1sep(name, ".") ^^ { xs => QName(xs) })

  lazy val program = positioned(pkg ~ struct ^^ { case p ~ s => T.Program(p, s) })
  lazy val pkg = kwd("package") ~> qname
  lazy val struct = positioned(kwd("struct") ~> (name <~ "{") ~ rep(term) <~ "}" ^^ {
    case n ~ ts => T.Struct(n, ts)
  })

  def term: Parser[T.Term] = tlet
  def tlet = positioned(kwd("let") ~> (name <~ "=") ~ expr <~ ";;" ^^ { case n ~ e => T.TLet(n, e) })
  def expr: Parser[T.Expr] = lit_int
  val lit_int = positioned("""[0-9]+""".r ^^ { i => T.LitInt(i.toInt) })
}
