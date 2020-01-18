package com.todesking.ojaml.ml0.compiler.scala

import com.todesking.ojaml.ml0.compiler.scala.util.pretty
import pretty.PrettyPrinter
import pretty.Doc
import pretty.PrettySyntax._

object P {
  trait DocLike {
    def toDocs: Seq[Doc]
  }
  object DocLike {
    implicit def doc2Like(doc: Doc): DocLike = new DocLike {
      override def toDocs = Seq(doc)
    }
    implicit def docs2Like[A](docs: Seq[A])(implicit f: A => DocLike): DocLike = new DocLike {
      override def toDocs = docs.map(f).flatMap(_.toDocs)
    }
    implicit def str2Like(str: String) = doc2Like(Doc.Text(str))
  }
  private[this] def join(docs: Seq[Doc])(f: (Doc, Doc) => Doc) = docs.foldLeft[Doc](Doc.Nil)(f)
  def bgroup(items: DocLike*) = Doc.BGroup(join(items.flatMap(_.toDocs))(_ ^| _))
  def bgroupi(items: DocLike*) = Doc.Nest(2, bgroup(items: _*))
  def group(items: DocLike*) = Doc.Group(join(items.flatMap(_.toDocs))(_ ^| _))
  def groupi(items: DocLike*) = Doc.Nest(2, group(items: _*))
  def mks(sep: Doc)(items: Seq[Doc]) =
    if (items.isEmpty) Doc.Nil
    else items.tail.foldLeft[Doc](items.head) { (a, x) => a ^^ sep ^| x }
  def paren(enable: Boolean, doc: Doc) = if (enable) P.group("(", doc, ")") else doc

  private[this] def withTpe(doc: Doc, tpe: Option[String]) =
    tpe.fold(doc) { t =>
      doc ^^ s": $t".doc
    }

  def pkg(name: QName): Doc =
    Doc.Text(s"package ${name.value}")
  def imports(items: Seq[Import]): Doc =
    bgroup(items.flatMap(_.flatten).map { x =>
      Doc.Text(s"import ${x.qname.value}")
    })
  def module(name: String, body: Seq[Doc]) =
    bgroup(
      s"module $name {",
      bgroupi(body),
      "}")
  def tlet(name: Name, tpe: Option[String], body: Doc) =
    group(
      s"let ${name.value} =",
      groupi(body))
  def data(name: Name, ctors: Seq[(String, Seq[String])]) =
    group(
      s"data ${name.value} =",
      groupi(
        mks(Doc.Text(","))(ctors.map { case (n, ns) => Doc.Text((n +: ns).mkString(" ")) })))
  def jcall(receiver: Doc, name: String, args: Seq[Doc], isStatic: Boolean) =
    group(
      receiver,
      (if (isStatic) "##" else "#").doc ^^ name.doc ^^ "(".doc,
      groupi(mks(",".doc)(args)),
      ")")
  def eif(cond: Doc, th: Doc, el: Doc) =
    group(
      "if", groupi(cond),
      "then", groupi(th),
      "else", groupi(el))
  def fun(name: String, tpeName: Option[String], body: Doc) =
    group(
      ("fun".doc ^^ name.doc
        ^^ tpeName.fold[Doc](Doc.Nil) { t => ": ".doc ^| t.toString.doc } ^^ " =>".doc),
      groupi(body))
  def funT(tpe: String, body: Doc) =
    group(
      s"(fun: $tpe) ? =>",
      groupi(body))
  def app(par: Boolean, fun: Doc, arg: Doc) =
    paren(par, fun ^^ " ".doc ^^ arg)
  def elet(name: String, value: Doc, body: Doc) =
    group(
      s"let $name =",
      groupi(value),
      "in",
      groupi(body))
  def eletrec(bindings: Seq[(String, Option[String], Doc)], body: Doc) =
    group(
      "let rec",
      mks(";".doc)(
        bindings.map {
          case (n, t, f) =>
            groupi(
              n.doc ^^ t.fold[Doc](Doc.Nil) { t =>
                s": $t".doc
              } ^^ " =".doc,
              groupi(f))
        }),
      "in",
      groupi(body))
  def prop(expr: Doc, name: Name) =
    group(expr ^^ ".".doc, name.value)
  def pmatch(expr: Doc, cs: Seq[Doc]) =
    group(
      "match",
      expr,
      bgroup(cs))
}
