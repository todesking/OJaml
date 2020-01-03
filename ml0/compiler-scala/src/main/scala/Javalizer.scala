package com.todesking.ojaml.ml0.compiler.scala

import com.todesking.ojaml.ml0.compiler.scala.{ JAST => J }
import com.todesking.ojaml.ml0.compiler.scala.{ TypedAST => T }
import com.todesking.ojaml.ml0.compiler.scala.TypedAST.ModuleVarRef

object Javalizer {
  class ClassBuilder(ref: ClassRef, superRef: ClassRef) {
    private[this] var nextFunClassID = 0
    private[this] var methodDefs = Vector.empty[J.MethodDef]
    private[this] var fieldDefs = Vector.empty[J.FieldDef]
    private[this] var clinits = Vector.empty[J.Term]
    private[this] var funClassDefs = Vector.empty[J.ClassDef]
    private[this] var datas = Vector.empty[J.Data]

    def build() = J.ClassDef(
      ref,
      superRef,
      fieldDefs,
      methodDefs :+ buildClinit(),
      datas)

    private[this] def buildClinit() =
      J.MethodDef(
        "<clinit>",
        true,
        Seq(),
        None,
        clinits)

    def addStaticField(name: String, tpe: Type): FieldRef = {
      val fd = J.FieldDef(name, tpe)
      fieldDefs :+= fd
      FieldRef(ref, name, tpe)
    }
    def addClinit(insn: J.Term): Unit = {
      clinits :+= insn
    }
    def addData(data: J.Data) = {
      datas :+= data
    }
    def addMethod(m: J.MethodDef) = {
      methodDefs :+= m
    }
  }
  class Transformer(moduleClass: ClassRef) {
    private[this] var nextFunClassID = 0
    private[this] val builder = new ClassBuilder(moduleClass, Type.Object.ref)
    private[this] var klasses = Vector.empty[J.ClassDef]
    def build() = builder.build() +: klasses

    def appTerm(t: T.Term): Unit = t match {
      case T.TLet(name, tpe, expr) =>
        val f = builder.addStaticField(name.value, tpe)
        val c = appExpr(expr, 0)
        builder.addClinit(
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
                false,
                Some(J.GetLocal(0, Type.Klass(Type.Fun.ref))),
                Seq(J.LitInt(d), J.LitInt(index))),
              t.boxed),
            t)
        }
      case T.LetRec(vs, b) =>
        val funs = vs.map(appExpr(_, depth + 1))
        val body = appExpr(b, depth + 1)
        val funKlass = createFun(body, Some(funs))
        val funNewArgs =
          if (depth == 0) Seq(J.Null(Type.Object), J.Null(Type.Klass(Type.Fun.ref)))
          else Seq(J.GetLocal(1, Type.Object), J.GetLocal(0, Type.Klass(Type.Fun.ref)))
        unbox(
          J.Downcast(
            J.Invoke(
              MethodSig(Type.Fun.ref, false, false, "app", Seq(Type.Object), Some(Type.Object)),
              false,
              Some(J.JNew(funKlass, funNewArgs)),
              Seq(J.NewObjectArray(vs.size))),
            b.tpe.boxed), b.tpe)
      case T.If(c, th, el, t) =>
        J.If(appExpr(c, depth), appExpr(th, depth), appExpr(el, depth), t)
      case T.App(f, a, t) =>
        val appSig = MethodSig(Type.Fun.ref, false, false, "app", Seq(Type.Object), Some(Type.Object))
        val invoke = J.Invoke(
          appSig,
          false,
          Some(box(appExpr(f, depth))),
          Seq(box(appExpr(a, depth))))
        unbox(J.Downcast(invoke, t.boxed), t)
      case T.Fun(b, t) =>
        val funKlass = createFun(appExpr(b, depth + 1), None)
        val args =
          if (depth == 0) Seq(J.Null(Type.Object), J.Null(Type.Klass(Type.Fun.ref)))
          else Seq(J.GetLocal(1, Type.Object), J.GetLocal(0, Type.Klass(Type.Fun.ref)))
        J.Upcast(J.JNew(funKlass, args), Type.Klass(Type.Fun.ref))
      case T.TAbs(ps, b, t) => appExpr(b, depth)
      case T.JCallStatic(m, a) =>
        val args = m.args.zip(a).map {
          case (t, a) =>
            autobox(appExpr(a, depth), t)
        }
        J.Invoke(m, false, None, args)
      case T.JCallInstance(m, r, a) =>
        val args = m.args.zip(a).map {
          case (t, a) =>
            autobox(appExpr(a, depth), t)
        }
        J.Invoke(m, false, Some(box(appExpr(r, depth))), args)
      case T.JNew(r, a) => J.JNew(r, a.map(appExpr(_, depth)))
      case T.Upcast(b, t) => J.Upcast(appExpr(b, depth), t)
    }

    def createFun(body: J.Expr, recValues: Option[Seq[J.Expr]]) = {
      val funKlass = ClassRef(moduleClass.pkg, s"${moduleClass.name}$$${nextFunClassID}")
      nextFunClassID += 1
      val builder = new ClassBuilder(funKlass, Type.Fun.ref)
      val initBody = Seq(
        J.TExpr(
          J.Invoke(
            MethodSig(Type.Fun.ref, false, false, "<init>", Seq(Type.Object, Type.Klass(Type.Fun.ref)), None),
            true,
            Some(J.GetLocal(0, Type.Klass(Type.Fun.ref))),
            Seq(J.GetLocal(1, Type.Klass(Type.Fun.ref)), J.GetLocal(2, Type.Object)))))
      builder.addMethod(J.MethodDef("<init>", false, Seq(Type.Object, Type.Klass(Type.Fun.ref)), None, initBody))
      val prepareRec = recValues.fold(Seq.empty[J.Term]) { rvs =>
        Seq(J.PutValuesToUncheckedObjectArray(J.GetLocal(1, Type.Object), rvs))
      }
      builder.addMethod(J.MethodDef(
        "app",
        false,
        Seq(Type.Object),
        Some(Type.Object),
        prepareRec ++ Seq(J.TReturn(box(body)))))
      klasses :+= builder.build()
      funKlass
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