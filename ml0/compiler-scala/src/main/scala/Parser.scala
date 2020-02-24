package com.todesking.ojaml.ml0.compiler.scala

class Parser(sourceLocation: String) extends scala.util.parsing.combinator.RegexParsers {
  import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => T }

  def parse(s: String): Result[T.Program] =
    translate(parseAll(program, s))

  def parseTerm(s: String): Result[T.Term] =
    translate(parseAll(term, s))

  private[this] def translate[A](a: ParseResult[A]): Result[A] = a match {
    case Success(value, _) => Result.ok(value)
    case NoSuccess(msg, next) =>
      Result.error(Pos(sourceLocation, next.pos.line, next.pos.column), s"Parse error: $msg\n${next.pos.longString}")
  }

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
    "=>",
    "data",
    "match",
    "|",
    "_")
  private[this] def kwd(a: String): Parser[Unit] = {
    require(keywords contains a)
    (regex(s"${java.util.regex.Pattern.quote(a)}\\s+".r) ^^ { _ => () }).named(s"keyword($a)")
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

  private[this] def asName(p: Parser[String]) =
    withpos(p ^? ({ case s if !keywords(s) => Name(s) }, { s => s"Invalid name: $s" }))

  val small_name: Parser[Name] = asName("""[a-z][a-zA-Z0-9_]*""".r)
  val capital_name: Parser[Name] = asName("""[A-Z][a-zA-Z0-9_]*""".r)
  val normal_name = small_name | capital_name
  val opName: Parser[Name] = asName("""==|<=|>=|[-+*/%<>]|&&|\|\|""".r)
  val name: Parser[Name] = normal_name | opName
  val qname: Parser[QName] = withpos(rep1sep(name, ".") ^^ { xs => QName(xs) })

  val ctor_name = capital_name
  val var_name = small_name

  // TODO: Support parenthesis
  def typename: Parser[TypeName] = withpos(rep1sep(typename1, kwd("=>")) ^^ { ts =>
    ts.tail.foldLeft(ts.head) { (l, r) => TypeName.Fun(l, r) }
  })
  def typename1: Parser[TypeName] = withpos(name ^^ { n => TypeName.Atom(n.value) }) | ("(" ~> typename) <~ ")"

  lazy val program: Parser[RawAST.Program] = withpos(pkg ~ rep(`import`) ~ rep1(module) ^^ { case p ~ is ~ ss => T.Program(p, is, ss) })
  lazy val pkg: Parser[QName] = kwd("package") ~> qname
  lazy val `import`: Parser[Import] = kwd("import") ~> import_body
  lazy val import_body: Parser[Import] = import_group | import_single
  lazy val import_single: Parser[Import] = qname ~ (kwd("=>") ~> name).? ^^ { case qn ~ n => Import.Single(qn, n) }
  lazy val import_group: Parser[Import] = (qname <~ ("." ~ "{")) ~ (rep1sep(import_body, ",") <~ "}") ^^ { case qn ~ xs => Import.Group(qn, xs) }
  lazy val module: Parser[RawAST.Module] = withpos(kwd("module") ~> (normal_name <~ "{") ~ rep(term) <~ "}" ^^ {
    case n ~ ts => T.Module(n, ts)
  })

  def term: Parser[T.Term] = tlet | data
  def tlet: Parser[RawAST.TLet] = withpos((kwd("let") ~> name) ~ (fun_params.? <~ "=") ~ expr <~ ";;" ^^ {
    case n ~ None ~ e =>
      T.TLet(n, e)
    case name ~ Some(params) ~ expr =>
      T.TLet(name, mkLetBody(params, expr))
  })

  private[this] def mkLetBody(params: Seq[Name ~ Option[TypeName]], body: T.Expr): T.Expr = params match {
    case (name ~ tpe) :: xs =>
      Pos.fill(T.Fun(name, tpe, mkLetBody(xs, body)), name.pos)
    case Nil =>
      body
  }

  def data = withpos((kwd("data") ~> name) ~ ("=" ~> rep1sep(datadef, ",")) <~ ";;" ^^ {
    case name ~ ddefs =>
      T.Data(name, ddefs.map { case n ~ ns => (n, ns) })
  })

  def datadef = name ~ rep(typename)

  def expr: Parser[T.Expr] = withpos(expr1)

  def binop(pat: Parser[String]): Parser[Name] =
    withpos(pat ^^ { p => Name(p) })

  def bin_expr(expr: Parser[T.Expr], pat: Parser[Name]): Parser[T.Expr] = withpos(
    expr ~ rep(pat ~ expr) ^^ {
      case e ~ es =>
        es.foldLeft(e) {
          case (l, op ~ r) =>
            val refOp = Pos.fill(T.Ref(op), op.pos)
            val app1 = Pos.fill(T.App(refOp, l), op.pos)
            val app2 = Pos.fill(T.App(app1, r), op.pos)
            app2
        }
    })

  def expr1: Parser[T.Expr] =
    Seq(
      binop("&&" | "||"),
      binop("==" | "<" | "<=" | ">=" | ">"),
      binop("+" | "-"),
      binop("*" | "/" | "%")).foldRight(expr3) { (op, e) =>
        bin_expr(e, op)
      }

  def expr3: Parser[RawAST.Expr] = ematch | eif | fun | eletr | elet | app

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
  def fun: Parser[RawAST.Fun] = withpos((kwd("fun") ~> fun_params1) ~ ("=>" ~> expr) ^^ {
    case params ~ expr =>
      mkLetBody(params, expr).asInstanceOf[RawAST.Fun]
  })
  def fun_params1 = rep1(normal_name ~ (":" ~> typename1).?)
  def fun_params = rep(normal_name ~ (":" ~> typename1).?)
  val var_ref: Parser[RawAST.Ref] = withpos(normal_name ^^ { n => T.Ref(n) })
  val eletr: Parser[RawAST.ELetRec] = withpos((kwd("let") ~> kwd("rec")) ~> rep1sep(letrec_binding, ";") ~ (kwd("in") ~> expr) ^^ {
    case bs ~ body =>
      T.ELetRec(bs, body)
  })
  def letrec_binding = (normal_name ~ (":" ~> typename).? ~ fun_params ~ ("=" ~> expr)).flatMap {
    case n ~ t ~ ps ~ e =>
      val expr = mkLetBody(ps, e)
      expr match {
        case f @ T.Fun(_, _, _) => success((n, t, f))
        case _ => err("bound values of let rec must be fun") // TODO: This error don't stop backtracking. why?
      }
  }
  val elet: Parser[RawAST.ELet] = withpos((kwd("let") ~> name) ~ fun_params ~ ("=" ~> expr) ~ (kwd("in") ~> expr) ^^ {
    case name ~ params ~ e1 ~ e2 =>
      T.ELet(name, mkLetBody(params, e1), e2)
  })
  lazy val ematch = withpos((kwd("match") ~> expr) ~ rep(ematch_clause) ^^ {
    case e ~ cs =>
      T.Match(e, cs)
  })
  lazy val ematch_clause = withpos((kwd("|") ~> ematch_pat) ~ (kwd("=>") ~> expr) ^^ {
    case p ~ e =>
      T.Clause(p, e)
  })
  lazy val ematch_pat: Parser[T.Pat] = (ematch_ctor | ematch_any | ematch_capture | (("(" ~> ematch_pat) <~ ")")).named("ematch_pat")
  lazy val ematch_ctor: Parser[T.Pat] = withpos(
    ctor_name ~ rep(ematch_pat) ^^ {
      case n ~ ps =>
        T.Pat.Ctor(n, ps)
    }).named("ematch_ctor")
  lazy val ematch_any = withpos(
    kwd("_") ^^ { case _ => T.Pat.PAny() }).named("ematch_any")
  lazy val ematch_capture = withpos(
    var_name ^^ { case n => T.Pat.Capture(n) }).named("ematch_capture")
}

