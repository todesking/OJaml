package com.todesking.ojaml.ml0.compiler.scala

import com.todesking.ojaml.ml0.compiler.scala.{ JAST => J }
import com.todesking.ojaml.ml0.compiler.scala.{ TypedAST => T }
import com.todesking.ojaml.ml0.compiler.scala.TypedAST.ModuleVarRef

import JType.TObject

object Javalizer {
  class ClassBuilder(ref: ClassRef, superRef: ClassRef) {
    private[this] var nextFunClassID = 0
    private[this] var methodDefs = Vector.empty[J.MethodDef]
    private[this] var fieldDefs = Vector.empty[J.FieldDef]
    private[this] var clinits = Vector.empty[J.Term]
    private[this] var funClassDefs = Vector.empty[J.ClassDef]

    def build() = J.ClassDef(
      ref,
      superRef,
      fieldDefs,
      methodDefs :+ buildClinit())

    private[this] def buildClinit() =
      J.MethodDef(
        "<clinit>",
        true,
        Seq(),
        None,
        clinits)

    def addStaticField(name: String, tpe: JType): FieldRef = {
      fieldDefs :+= J.FieldDef(name, true, tpe)
      FieldRef(ref, name, tpe)
    }
    def addField(name: String, tpe: JType): FieldRef = {
      fieldDefs :+= J.FieldDef(name, false, tpe)
      FieldRef(ref, name, tpe)
    }
    def addClinit(insn: J.Term): Unit = {
      clinits :+= insn
    }
    def addMethod(m: J.MethodDef) = {
      methodDefs :+= m
    }
  }
  class Transformer(moduleClass: ClassRef) {
    private[this] var nextFunClassID = 0
    private[this] val builder = new ClassBuilder(moduleClass, TObject.ref)
    private[this] var klasses = Vector.empty[J.ClassDef]
    def build() = builder.build() +: klasses

    def appTerm(t: T.Term): Unit = t match {
      case T.TLet(name, tpe, expr) =>
        val f = builder.addStaticField(name.value, tpe.jtype)
        val c = appExpr(expr, 0)
        builder.addClinit(
          J.PutStatic(f, appExpr(expr, 0)))
      case T.Data(name, tpe, ctors) =>
        appData(name.value, tpe, ctors.map { case (k, v) => k.value -> v.map(_.jtype) })
    }
    def appExpr(expr: T.Expr, depth: Int): J.Expr = expr match {
      case T.LitInt(v) => J.LitInt(v)
      case T.LitBool(v) => J.LitBool(v)
      case T.LitString(v) => J.LitString(v)
      case T.ModuleVarRef(m, n, t) => J.ModuleVarRef(m, n, t.jtype)
      case T.LocalRef(d, index, tt) =>
        val t = tt.jtype
        if (depth == d) {
          if (index == 0) {
            unbox(J.Cast(J.GetLocal(1, t.boxed), t.boxed), t)
          } else {
            unbox(
              J.Cast(
                J.GetObjectFromUncheckedArray(J.GetLocal(1, TObject), index - 1),
                t.boxed),
              t)
          }
        } else {
          val sig = MethodSig(
            Type.Fun.ref,
            false,
            false,
            "getLocal",
            Seq(JType.TInt, JType.TInt),
            Some(TObject))
          unbox(
            J.Cast(
              J.Invoke(
                sig,
                false,
                Some(J.GetLocal(0, JType.Fun)),
                Seq(J.LitInt(d), J.LitInt(index))),
              t.boxed),
            t)
        }
      case T.LetRec(vs, b) =>
        val funs = vs.map(appExpr(_, depth + 1))
        val body = appExpr(b, depth + 1)
        val funKlass = createFun(body, Some(funs))
        val funNewArgs =
          if (depth == 0) Seq(J.Null(TObject), J.Null(JType.Fun))
          else Seq(J.GetLocal(1, TObject), J.GetLocal(0, JType.Fun))
        unbox(
          J.Cast(
            J.Invoke(
              MethodSig(Type.Fun.ref, false, false, "app", Seq(TObject), Some(TObject)),
              false,
              Some(J.JNew(funKlass, funNewArgs)),
              Seq(J.NewObjectArray(vs.size))),
            body.tpe.boxed), body.tpe)
      case T.If(c, th, el, t) =>
        J.If(appExpr(c, depth), appExpr(th, depth), appExpr(el, depth), t.jtype)
      case T.App(f, a, t) =>
        val appSig = MethodSig(Type.Fun.ref, false, false, "app", Seq(TObject), Some(TObject))
        val invoke = J.Invoke(
          appSig,
          false,
          Some(box(appExpr(f, depth))),
          Seq(box(appExpr(a, depth))))
        unbox(J.Cast(invoke, t.jtype.boxed), t.jtype)
      case T.Fun(b, t) =>
        val funKlass = createFun(appExpr(b, depth + 1), None)
        val args =
          if (depth == 0) Seq(J.Null(TObject), J.Null(JType.Fun))
          else Seq(J.GetLocal(1, TObject), J.GetLocal(0, JType.Fun))
        J.Cast(J.JNew(funKlass, args), JType.Fun)
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
      case T.Upcast(b, t) => J.Cast(appExpr(b, depth), t.jtype)
    }

    def appData(name: String, dataType: Type, ctors: Seq[(String, Seq[JType])]): Unit = {
      val tpe = dataType.jtype
      val dataBaseClass = ClassRef.fromInternalName("com/todesking/ojaml/ml0/runtime/Data")

      val dataKlass = ClassRef.fromInternalName(tpe.jname)
      val dataBuilder = new ClassBuilder(dataKlass, dataBaseClass)

      val initBody = Seq(
        J.TExpr(
          J.Invoke(
            MethodSig(dataBaseClass, false, false, "<init>", Seq(JType.TInt), None),
            true,
            Some(J.GetLocal(0, JType.TKlass(dataBaseClass))),
            Seq(J.GetLocal(1, JType.TInt)))))
      dataBuilder.addMethod(J.MethodDef(
        "<init>",
        false,
        Seq(JType.TInt),
        None,
        initBody))
      klasses :+= dataBuilder.build()

      ctors.foreach {
        case (name, params) =>
          val ctorKlass = ClassRef(dataKlass.pkg, s"${dataKlass.name}$$${name}")
          val ctorBuilder = new ClassBuilder(ctorKlass, dataKlass)
          val paramFields = params.zipWithIndex.map { case (t, i) => FieldRef(ctorKlass, s"v$i", t) }
          paramFields.foreach { field =>
            ctorBuilder.addField(field.name, field.tpe)
          }

          val initBody = Seq(
            J.TExpr(
              J.Invoke(
                MethodSig(dataKlass, false, false, "<init>", Seq(JType.TInt), None),
                true,
                Some(J.GetLocal(0, JType.TKlass(ctorKlass))),
                Seq(J.LitInt(params.size))))) ++ paramFields.zipWithIndex.map {
              case (field, i) =>
                J.PutField(field, J.GetLocal(0, JType.TKlass(ctorKlass)), J.GetLocal(i + 1, field.tpe))
            }
          ctorBuilder.addMethod(J.MethodDef("<init>", false, params, None, initBody))

          val valuesBody = Seq(
            J.TReturn(
              J.PutValuesToUncheckedObjectArray(
                J.NewObjectArray(params.size),
                paramFields.map { field =>
                  box(J.GetField(field, J.GetLocal(0, JType.TKlass(ctorKlass))))
                })))
          ctorBuilder.addMethod(J.MethodDef("values", false, Seq(), Some(JType.ObjectArray), valuesBody))

          val nameBody = Seq(
            J.TReturn(J.LitString(name)))
          ctorBuilder.addMethod(J.MethodDef("name", false, Seq(), Some(JType.TString), nameBody))

          klasses :+= ctorBuilder.build()
      }
    }

    def createFun(body: J.Expr, recValues: Option[Seq[J.Expr]]) = {
      val funKlass = ClassRef(moduleClass.pkg, s"${moduleClass.name}$$${nextFunClassID}")
      nextFunClassID += 1
      val builder = new ClassBuilder(funKlass, Type.Fun.ref)
      val initBody = Seq(
        J.TExpr(
          J.Invoke(
            MethodSig(Type.Fun.ref, false, false, "<init>", Seq(TObject, JType.Fun), None),
            true,
            Some(J.GetLocal(0, JType.Fun)),
            Seq(J.GetLocal(1, JType.Fun), J.GetLocal(2, TObject)))))
      builder.addMethod(J.MethodDef("<init>", false, Seq(TObject, JType.Fun), None, initBody))
      val prepareRec = recValues.fold(Seq.empty[J.Term]) { rvs =>
        Seq(J.TExpr(J.PutValuesToUncheckedObjectArray(J.GetLocal(1, TObject), rvs)))
      }
      builder.addMethod(J.MethodDef(
        "app",
        false,
        Seq(TObject),
        Some(TObject),
        prepareRec ++ Seq(J.TReturn(box(body)))))
      klasses :+= builder.build()
      funKlass
    }

    def autobox(e: J.Expr, to: JType) =
      if (e.tpe == to) e
      else if (e.tpe.boxed == to) box(e)
      else if (e.tpe == to.boxed) unbox(e, to)
      else throw new RuntimeException(s"Can't autobox: ${e.tpe} -> $to")

    def box(e: J.Expr): J.Expr =
      if (e.tpe == e.tpe.boxed) e
      else J.Box(e)

    def unbox(e: J.Expr, to: JType): J.Expr =
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
    Seq(TObject),
    Some(TObject))

  def apply(m: T.Module): Seq[J.ClassDef] = {
    val moduleClass = ClassRef(m.pkg.asPackage, m.name.value)
    val transformer = new Javalizer.Transformer(moduleClass)
    m.body.foreach { t => transformer.appTerm(t) }
    transformer.build()
  }
}
