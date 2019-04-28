package com.todesking.ojaml.ml0.compiler.scala

class Parser(sourceLocation: String) extends scala.util.parsing.combinator.RegexParsers {
  import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => T }

  def parse(s: String): ParseResult[T.Program] =
    parseAll(program, s)

  private[this] val discard = { _: Any => () }

  val keywords: Set[String] = Set(
    "package",
    "import",
    "module",
    "let",
    "rec",
    "in",
    "fun",
    "if", "then", "else",
    "=>")
  private[this] def kwd(a: String): Parser[Unit] = {
    require(keywords contains a)
    regex(s"${java.util.regex.Pattern.quote(a)}\\s+".r) ^^ { _ => () }
  }

  private[this] var _skipWS = true
  override def skipWhitespace: Boolean = _skipWS

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

  val normalName: Parser[Name] = withpos("""[a-zA-Z][a-zA-Z0-9_]*""".r ^? { case s if !keywords(s) => Name(s) })
  val opName: Parser[Name] = withpos("""==|<=|>=|[-+*/%<>&|]""".r ^? { case s if !keywords(s) => Name(s) })
  val name: Parser[Name] = normalName | opName
  val qname: Parser[QName] = withpos(rep1sep(name, ".") ^^ { xs => QName(xs) })

  def typename: Parser[TypeName] = withpos(rep1sep(typename1, kwd("=>")) ^^ { ts =>
    ts.tail.foldLeft(ts.head) { (l, r) => TypeName.Fun(l, r) }
  })
  def typename1: Parser[TypeName] = withpos(name ^^ { n => TypeName.Atom(n.value) }) | ("(" ~> typename) <~ ")"

  lazy val program: Parser[RawAST.Program] = withpos(pkg ~ rep(`import`) ~ rep1(module) ^^ { case p ~ is ~ ss => T.Program(p, is, ss) })
  lazy val pkg: Parser[QName] = kwd("package") ~> qname
  lazy val `import`: Parser[Import] = kwd("import") ~> qname ^^ { qn => Import(qn) }
  lazy val module: Parser[RawAST.Module] = withpos(kwd("module") ~> (normalName <~ "{") ~ rep(term) <~ "}" ^^ {
    case n ~ ts => T.Module(n, ts)
  })

  def term: Parser[T.Term] = tlet
  def tlet: Parser[RawAST.TLet] = withpos(kwd("let") ~> (name <~ "=") ~ expr <~ ";;" ^^ { case n ~ e => T.TLet(n, e) })

  def expr: Parser[T.Expr] = expr1

  def binop(pat: Parser[String]): Parser[Name] =
    withpos(pat ^^ { p => Name(p) })

  def bin_expr(expr: Parser[T.Expr], pat: Parser[Name]): Parser[T.Expr] = withpos(
    expr ~ rep(pat ~ expr) ^^ {
      case e ~ es =>
        es.foldLeft(e) {
          case (l, op ~ r) =>
            T.App(Pos.fill(T.App(Pos.fill(T.Ref(op), op.pos), l), op.pos), r)
        }
    })

  def expr1: Parser[T.Expr] =
    Seq(
      binop("&" | "|"),
      binop("==" | "<" | "<=" | ">=" | ">"),
      binop("+" | "-"),
      binop("*" | "/" | "%")).foldRight(expr3) { (op, e) =>
        bin_expr(e, op)
      }

  def expr3: Parser[RawAST.Expr] = eif | fun | eletr | elet | app

  lazy val expr4: Parser[RawAST.Expr] = withpos(expr5 ~ jcall.? ^^ {
    case e ~ None => e
    case e ~ Some("#" ~ n ~ args) => T.JCall(e, n, args, false)
    case e ~ Some("##" ~ n ~ args) => T.JCall(e, n, args, true)
    case _ => throw new AssertionError()
  })

  lazy val jcall: Parser[String ~ Name ~ List[RawAST.Expr]] = ("##" | "#") ~ name ~ ("(" ~> repsep(expr, ",") <~ ")")

  def expr5: Parser[RawAST.Expr] = withpos(expr6 ~ prop.? ^^ {
    case e ~ None => e
    case e ~ Some(ns) => ns.foldLeft(e) { (e, n) => Pos.fill(T.Prop(e, n), n.pos) }
  })
  val prop: Parser[List[Name]] = "." ~> rep1sep(name, ".")

  def expr6: Parser[RawAST.Expr] = paren | lit_bool | lit_int | lit_string | var_ref

  def paren: Parser[RawAST.Expr] = ("(" ~> expr) <~ ")"

  def app: Parser[RawAST.Expr] = withpos(rep1(expr4) ^^ {
    case e :: es =>
      es.foldLeft(e) { (f, e) =>
        val app = T.App(f, e)
        app.fillPos(f.pos.location, f.pos.line, f.pos.col)
        app
      }
    case _ => throw new AssertionError()
  })

  val lit_int: Parser[RawAST.LitInt] = withpos("""[0-9]+""".r ^^ { i => T.LitInt(i.toInt) })
  val lit_bool: Parser[RawAST.LitBool] = withpos(("true" | "false") ^^ { case "true" => T.LitBool(true) case "false" => T.LitBool(false) })
  val lit_string: Parser[RawAST.LitString] = withpos(("\"" ~> """[^"]+""".r) <~ "\"" ^^ { s => T.LitString(s) })

  val eif: Parser[RawAST.If] = withpos((kwd("if") ~> expr) ~ (kwd("then") ~> expr) ~ (kwd("else") ~> expr) ^^ { case cond ~ th ~ el => T.If(cond, th, el) })
  def fun: Parser[RawAST.Fun] = withpos((kwd("fun") ~> normalName) ~ (":" ~> typename1) ~ ("=>" ~> expr) ^^ { case name ~ tpe ~ expr => T.Fun(name, tpe, expr) })
  val var_ref: Parser[RawAST.Ref] = withpos(normalName ^^ { n => T.Ref(n) })
  val eletr: Parser[RawAST.ELetRec] = withpos((kwd("let") ~> kwd("rec")) ~> rep1sep(normalName ~ (":" ~> typename) ~ ("=" ~> fun), ";") ~ (kwd("in") ~> expr) ^^ {
    case bs ~ body =>
      val bindings = bs.map { case n ~ t ~ e => (n, t, e) }
      T.ELetRec(bindings, body)
  })
  val elet: Parser[RawAST.ELet] = withpos((kwd("let") ~> name) ~ ("=" ~> expr) ~ (kwd("in") ~> expr) ^^ {
    case name ~ e1 ~ e2 =>
      T.ELet(name, e1, e2)
  })
}

