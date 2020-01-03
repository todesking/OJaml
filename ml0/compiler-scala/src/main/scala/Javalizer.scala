package com.todesking.ojaml.ml0.compiler.scala

import com.todesking.ojaml.ml0.compiler.scala.{ JAST => J }
import com.todesking.ojaml.ml0.compiler.scala.{ TypedAST => T }
import com.todesking.ojaml.ml0.compiler.scala.TypedAST.ModuleVarRef

object Javalizer {
  class ClassBuilder(moduleClass: ClassRef) {
    private[this] var nextFunClassID = 0
    private[this] var methodDefs = Seq.empty[J.MethodDef]
    private[this] var fieldDefs = Seq.empty[J.FieldDef]
    private[this] var initializers = Seq.empty[J.Term]
    private[this] var funClassDefs = Seq.empty[J.ClassDef]
    private[this] var datas = Seq.empty[J.Data]

    def build() = J.ClassDef(
      moduleClass,
      fieldDefs,
      methodDefs :+ buildInitializer(),
      datas)

    private[this] def buildInitializer() = {
      J.MethodDef(
        "<clinit>",
        true,
        Seq(),
        None,
        initializers)
    }

    def addStaticField(name: String, tpe: Type): FieldRef = {
      val fd = J.FieldDef(name, tpe)
      fieldDefs :+= fd
      FieldRef(moduleClass, name, tpe)
    }
    def addInitializer(insn: J.Term): Unit = {
      initializers :+= insn
    }
    def addData(data: J.Data) = {
      datas :+= data
    }
  }
  class Transformer(moduleClass: ClassRef) {
    private[this] var nextFunClassID = 0
    private[this] val builder = new ClassBuilder(moduleClass)
    private[this] var klasses = Vector.empty[J.ClassDef]
    def build() = builder.build() +: klasses

    def appTerm(t: T.Term): Unit = t match {
      case T.TLet(name, tpe, expr) =>
        val f = builder.addStaticField(name.value, tpe)
        val c = appExpr(expr, 0)
        builder.addInitializer(
          J.PutStatic(f, appExpr(expr, 0)))
      case T.Data(name, tpe, ctors) =>
        builder.addData(J.Data(name, tpe, ctors))
    }
    def appExpr(expr: T.Expr, depth: Int): J.Expr = expr match {
      case T.LitInt(v) => J.LitInt(v)
      case T.LitBool(v) => J.LitBool(v)
      case T.LitString(v) => J.LitString(v)
      case T.ModuleVarRef(m, n, t) => J.ModuleVarRef(m, n, t)
      case T.LocalRef(d, index, t) =>
        if (depth == d) {
          if (index == 0) {
            unbox(J.Downcast(J.GetLocal(1, t.boxed), t.boxed), t)
          } else {
            unbox(
              J.Downcast(
                J.GetObjectFromUncheckedArray(J.GetLocal(1, Type.Object), index - 1),
                t.boxed),
              t)
          }
        } else {
          val sig = MethodSig(
            Type.Fun.ref,
            false,
            false,
            "getLocal",
            Seq(Type.Int, Type.Int),
            Some(Type.Object))
          unbox(
            J.Downcast(
              J.Invoke(
                sig,
                Some(J.GetLocal(0, Type.Klass(Type.Fun.ref))),
                Seq(J.LitInt(d), J.LitInt(index))),
              t.boxed),
            t)
        }
      case T.LetRec(vs, b) =>
        val funs = vs.map(appExpr(_, depth + 1)).map(_.asInstanceOf[J.Fun])
        J.LetRec(funs, appExpr(b, depth + 1))
      case T.If(c, th, el, t) =>
        J.If(appExpr(c, depth), appExpr(th, depth), appExpr(el, depth), t)
      case T.App(f, a, t) =>
        val appSig = MethodSig(Type.Fun.ref, false, false, "app", Seq(Type.Object), Some(Type.Object))
        val invoke = J.Invoke(
          appSig,
          Some(box(appExpr(f, depth))),
          Seq(box(appExpr(a, depth))))
        unbox(J.Downcast(invoke, t.boxed), t)
      case T.Fun(b, t) => J.Fun(appExpr(b, depth + 1), t)
      case T.TAbs(ps, b, t) => appExpr(b, depth)
      case T.JCallStatic(m, a) =>
        val args = m.args.zip(a).map {
          case (t, a) =>
            autobox(appExpr(a, depth), t)
        }
        J.Invoke(m, None, args)
      case T.JCallInstance(m, r, a) =>
        val args = m.args.zip(a).map {
          case (t, a) =>
            autobox(appExpr(a, depth), t)
        }
        J.Invoke(m, Some(box(appExpr(r, depth))), args)
      case T.JNew(r, a) => J.JNew(r, a.map(appExpr(_, depth)))
      case T.Upcast(b, t) => J.Upcast(appExpr(b, depth), t)
    }

    def autobox(e: J.Expr, to: Type) =
      if (e.tpe == to) e
      else if (e.tpe.boxed == to) box(e)
      else if (e.tpe == to.boxed) unbox(e, to)
      else throw new RuntimeException(s"Can't autobox: ${e.tpe} -> $to")

    def box(e: J.Expr): J.Expr =
      if (e.tpe == e.tpe.boxed) e
      else J.Box(e)

    def unbox(e: J.Expr, to: Type): J.Expr =
      if (e.tpe == to) e
      else J.Unbox(e)

  }
}
class Javalizer {
  val appMethod = MethodSig(
    Type.Fun.ref,
    false,
    false,
    "app",
    Seq(Type.Object),
    Some(Type.Object))

  def apply(m: T.Module): Seq[J.ClassDef] = {
    val moduleClass = ClassRef(m.pkg.asPackage, m.name.value)
    val transformer = new Javalizer.Transformer(moduleClass)
    m.body.foreach { t => transformer.appTerm(t) }
    transformer.build()
  }
}
