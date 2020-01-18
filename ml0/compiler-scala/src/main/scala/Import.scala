package com.todesking.ojaml.ml0.compiler.scala

sealed abstract class Import {
  def flatten: Seq[Import.Single]
}
object Import {
  case class Single(qname: QName, alias: Option[Name]) extends Import {
    override def flatten = Seq(this)
  }
  case class Group(qname: QName, items: Seq[Import]) extends Import {
    override def flatten: Seq[Single] = items.flatMap(_.flatten).map {
      case Single(qn, a) =>
        Single(QName(qname.parts ++ qn.parts), a)
    }
  }
}
