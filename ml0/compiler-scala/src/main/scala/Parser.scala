package com.todesking.ojaml.ml0.compiler.scala

class Parser(sourceLocation: String) extends scala.util.parsing.combinator.RegexParsers {
  import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => T }

  def parse(s: String): Result[T.Program] =
    translate(parseAll(program, s))

  def parseTerm(s: String): Result[T.Term] =
    translate(parseAll(term, s))

  def parseExpr(s: String): Result[T.Expr] =
    translate(parseAll(expr, s))

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

  private[this] def withpos[A, B](p: => Parser[A])(f: (Pos, A) => B): Parser[B] = Parser { in =>
    p(in) match {
      case Success(x, in1) => Success(f(Pos(sourceLocation, in.pos.line, in.pos.column), x), in1)
      case ns: NoSuccess => ns
    }
  }

  private[this] def asName(p: Parser[String]) =
    withpos(p ^? ({ case s if !keywords(s) => s }, { s => s"Invalid name: $s" })) { case (pos, s) => Name(pos, s) }

  val small_name: Parser[Name] = asName("""[a-z][a-zA-Z0-9_]*""".r)
  val capital_name: Parser[Name] = asName("""[A-Z][a-zA-Z0-9_]*""".r)
  val normal_name = small_name | capital_name
  val opName: Parser[Name] = asName("""==|<=|>=|[-+*/%<>]|&&|\|\|""".r)
  val name: Parser[Name] = normal_name | opName
  val qname: Parser[QName] = rep1sep(name, ".") ^^ { case xs => QName(xs) }

  val ctor_name = capital_name
  val var_name = normal_name
  val var_qname = rep1sep(normal_name, ".") ^^ { case xs => QName(xs) }
  val tvar_name = small_name

  val eot = ";"

  // TODO: Support parenthesis
  def typename: Parser[TypeName] = rep1sep(typename_app | typename1, kwd("=>")) ^^ { ts =>
    ts.tail.foldLeft(ts.head) { (l, r) => TypeName.Fun(l, r) }
  }
  def typename1: Parser[TypeName] = typename_atom | typename_group
  def typename_app = typename_atom ~ rep1(typename1) ^^ { case n ~ args => TypeName.App(n.pos, n, args) }
  def typename_atom = qname ^^ { n => TypeName.Atom(n.pos, n) }
  def typename_group = ("(" ~> typename) <~ ")"

  lazy val program: Parser[RawAST.Program] = withpos(pkg ~ rep(`import`) ~ rep1(module)) { case (pos, p ~ is ~ ss) => T.Program(pos, p, is, ss) }
  lazy val pkg: Parser[QName] = kwd("package") ~> qname
  lazy val `import`: Parser[Import] = kwd("import") ~> import_body
  lazy val import_body: Parser[Import] = import_group | import_single
  lazy val import_single: Parser[Import] = qname ~ (kwd("=>") ~> name).? ^^ { case qn ~ n => Import.Single(qn, n) }
  lazy val import_group: Parser[Import] = (qname <~ ("." ~ "{")) ~ (rep1sep(import_body, ",") <~ "}") ^^ { case qn ~ xs => Import.Group(qn, xs) }
  lazy val module: Parser[RawAST.Module] = withpos(kwd("module") ~> (normal_name <~ "{") ~ rep(term) <~ "}") {
    case (pos, n ~ ts) => T.Module(pos, n, ts)
  }

  def term: Parser[T.Term] = tlet | data | texpr
  def tlet: Parser[RawAST.TLet] = tlet_fun | tlet_generic
  val tlet_generic: Parser[RawAST.TLet] = withpos((kwd("let") ~> name) ~ (":" ~> typename).? ~ ("=" ~> expr) <~ eot) {
    case (pos, n ~ tn ~ e) =>
      T.TLet(pos, n, tn, e)
  }
  val tlet_fun: Parser[RawAST.TLet] = withpos((kwd("let") ~> name) ~ (":" ~> typename1).? ~ (fun_params1 <~ "=") ~ expr <~ eot) {
    case (pos, name ~ tn ~ params ~ expr) =>
      T.TLet(pos, name, tn, mkLetBody(params, expr))
  }

  private[this] def mkLetBody(params: Seq[Name ~ Option[TypeName]], body: T.Expr): T.Expr = params match {
    case (name ~ tpe) :: xs =>
      T.Fun(body.pos, name, tpe, mkLetBody(xs, body))
    case Nil =>
      body
  }

  def data = withpos((kwd("data") ~> name) ~ rep(tvar_name) ~ ("=" ~> rep1sep(datadef, "|")) <~ eot) {
    case (pos, name ~ vars ~ ddefs) =>
      T.Data(pos, name, vars, ddefs.map { case n ~ ns => (n, ns) })
  }

  def texpr = withpos(expr <~ eot) { case (pos, e) => T.TExpr(pos, e) }

  def datadef = name ~ rep(typename1)

  def expr: Parser[T.Expr] = expr1

  def binop(pat: Parser[String]): Parser[Name] =
    withpos(pat) { case (pos, p) => Name(pos, p) }

  def bin_expr(expr: Parser[T.Expr], pat: Parser[Name]): Parser[T.Expr] =
    expr ~ rep(pat ~ expr) ^^ {
      case e ~ es =>
        es.foldLeft(e) {
          case (l, Name(pos, "&&") ~ r) =>
            T.If(pos, l, r, T.LitBool(pos, false))
          case (l, Name(pos, "||") ~ r) =>
            T.If(pos, l, T.LitBool(pos, true), r)
          case (l, op ~ r) =>
            T.App(
              op.pos,
              T.App(
                op.pos,
                T.Ref(op.pos, QName(Seq(op))),
                l),
              r)
        }
    }

  def expr1: Parser[T.Expr] =
    Seq(
      binop("&&" | "||"),
      binop("==" | "<" | "<=" | ">=" | ">"),
      binop("+" | "-"),
      binop("*" | "/" | "%")).foldRight(expr3) { (op, e) =>
        bin_expr(e, op)
      }

  def expr3: Parser[RawAST.Expr] = ematch | eif | fun | eletr | elet | app

  lazy val expr4: Parser[RawAST.Expr] = expr6 ~ jcall.? ^^ {
    case e ~ None => e
    case e ~ Some("#" ~ n ~ args) => T.JCall(n.pos, e, n, args, false)
    case e ~ Some("##" ~ n ~ args) => T.JCall(n.pos, e, n, args, true)
    case _ => throw new AssertionError()
  }

  lazy val jcall: Parser[String ~ Name ~ List[RawAST.Expr]] = ("##" | "#") ~ name ~ ("(" ~> repsep(expr, ",") <~ ")")

  def expr6: Parser[RawAST.Expr] = paren | lit_bool | lit_int | lit_string | var_ref

  def paren: Parser[RawAST.Expr] = ("(" ~> expr) <~ ")"

  def app: Parser[RawAST.Expr] = rep1(expr4) ^^ {
    case e :: es =>
      es.foldLeft(e) { (f, e) => T.App(e.pos, f, e) }
    case _ => throw new AssertionError()
  }

  val lit_int: Parser[RawAST.LitInt] = withpos("""[0-9]+""".r) { case (pos, i) => T.LitInt(pos, i.toInt) }
  val lit_bool: Parser[RawAST.LitBool] = withpos(("true" | "false")) {
    case (pos, "true") => T.LitBool(pos, true)
    case (pos, "false") => T.LitBool(pos, false)
    case _ => throw new AssertionError()
  }
  val lit_string: Parser[RawAST.LitString] =
    withpos(("\"" ~> """[^"]+""".r) <~ "\"") { (pos, s) => T.LitString(pos, s) }

  val eif: Parser[RawAST.If] =
    withpos((kwd("if") ~> expr) ~ (kwd("then") ~> expr) ~ (kwd("else") ~> expr)) { case (pos, cond ~ th ~ el) => T.If(pos, cond, th, el) }
  def fun: Parser[RawAST.Fun] = withpos((kwd("fun") ~> fun_params1) ~ ("=>" ~> expr)) {
    case (pos, params ~ expr) =>
      mkLetBody(params, expr).asInstanceOf[RawAST.Fun]
  }
  def fun_params1 = rep1(normal_name ~ (":" ~> typename1).?)
  def fun_params = rep(normal_name ~ (":" ~> typename1).?)
  val var_ref: Parser[RawAST.Ref] = var_qname ^^ { n => T.Ref(n.pos, n) }
  val eletr: Parser[RawAST.ELetRec] = withpos((kwd("let") ~ kwd("rec") ~> rep1sep(letrec_binding, ";")) ~ (kwd("in") ~> expr)) {
    case (pos, bs ~ body) =>
      T.ELetRec(pos, bs, body)
  }
  def letrec_binding = (normal_name ~ (":" ~> typename).? ~ fun_params ~ ("=" ~> expr)).flatMap {
    case n ~ t ~ ps ~ e =>
      val expr = mkLetBody(ps, e)
      expr match {
        case f @ T.Fun(_, _, _, _) => success((n, t, f))
        case _ => err("bound values of let rec must be fun") // TODO: This error don't stop backtracking. why?
      }
  }
  val elet: Parser[RawAST.ELet] = withpos((kwd("let") ~> name) ~ fun_params ~ ("=" ~> expr) ~ (kwd("in") ~> expr)) {
    case (pos, name ~ params ~ e1 ~ e2) =>
      T.ELet(pos, name, mkLetBody(params, e1), e2)
  }
  lazy val ematch = withpos((kwd("match") ~> expr) ~ rep(ematch_clause)) {
    case (pos, e ~ cs) =>
      T.Match(pos, e, cs)
  }
  lazy val ematch_clause = withpos((kwd("|") ~> ematch_pat) ~ (kwd("=>") ~> expr)) {
    case (pos, p ~ e) =>
      T.Clause(pos, p, e)
  }
  lazy val ematch_pat: Parser[T.Pat] = (ematch_ctor | ematch_any | ematch_capture | (("(" ~> ematch_pat) <~ ")")).named("ematch_pat")
  lazy val ematch_ctor: Parser[T.Pat] =
    ctor_name ~ rep(ematch_pat) ^^ {
      case n ~ ps =>
        T.Pat.Ctor(n.pos, n.value, ps)
    }
  lazy val ematch_any = withpos(kwd("_")) { case (pos, _) => T.Pat.PAny(pos) }
  lazy val ematch_capture = var_name ^^ { case n => T.Pat.Capture(n.pos, n.value) }
}

