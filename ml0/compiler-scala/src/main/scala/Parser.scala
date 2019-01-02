package com.todesking.ojaml.ml0.compiler.scala

class Parser(sourceLocation: String) extends scala.util.parsing.combinator.RegexParsers {
  import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => T }

  def parse(s: String): ParseResult[T.Program] =
    parseAll(program, s)

  private[this] var _skipWS = true
  override def skipWhitespace = _skipWS

  private[this] def kwd(a: String): Parser[Unit] =
    regex(s"${java.util.regex.Pattern.quote(a)}\\s+".r) ^^ { _ => () }

  private[this] val discard = { _: Any => () }

  private[this] def commentWS: Parser[Unit] = """\s*""".r ~ comment.* ^^ discard
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

  private[this] def withpos[T <: HasPos](p: => Parser[T]): Parser[T] = Parser { in =>
    p(in) match {
      case Success(t, in1) => Success({ t.fillPos(sourceLocation, in.pos.line, in.pos.column); t }, in1)
      case ns: NoSuccess => ns
    }
  }

  val name: Parser[Name] = withpos("""[a-zA-Z][a-zA-Z0-9_]*""".r ^^ { s => Name(s) })
  val qname: Parser[QName] = withpos(rep1sep(name, ".") ^^ { xs => QName(xs) })

  lazy val program = withpos(pkg ~ struct ^^ { case p ~ s => T.Program(p, s) })
  lazy val pkg = kwd("package") ~> qname
  lazy val struct = withpos(kwd("struct") ~> (name <~ "{") ~ rep(term) <~ "}" ^^ {
    case n ~ ts => T.Struct(n, ts)
  })

  def term: Parser[T.Term] = tlet
  def tlet = withpos(kwd("let") ~> (name <~ "=") ~ expr <~ ";;" ^^ { case n ~ e => T.TLet(n, e) })
  def expr: Parser[T.Expr] = lit_int | var_ref
  val lit_int = withpos("""[0-9]+""".r ^^ { i => T.LitInt(i.toInt) })
  val var_ref = withpos(name ^^ { n => T.Ref(n) })
}

