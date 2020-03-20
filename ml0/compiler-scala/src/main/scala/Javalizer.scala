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
  class Transformer(moduleRef: ModuleRef, moduleClass: ClassRef) {
    private[this] var nextFunClassID = 0
    private[this] val builder = new ClassBuilder(moduleClass, TObject.ref)
    private[this] var klasses = Vector.empty[J.ClassDef]
    def build() = builder.build() +: klasses

    def appTerm(t: T.Term): Unit = t match {
      case T.TLet(pos, name, tpe, expr) =>
        val f = builder.addStaticField(name.value, tpe.jtype)
        expr.foreach { expr =>
          val c = appExpr(expr, 0)
          builder.addClinit(
            J.PutStatic(f, appExpr(expr, 0)))
        }
      case T.Data(pos, name, params, ctors) =>
        appData(name.value, ctors)
      case T.TExpr(pos, expr) =>
        builder.addClinit(J.TExpr(appExpr(expr, 0)))
    }
    def appExpr(expr: T.Expr, depth: Int): J.Expr = expr match {
      case T.LitInt(pos, v) => J.LitInt(v)
      case T.LitBool(pos, v) => J.LitBool(v)
      case T.LitString(pos, v) => J.LitString(v)
      case T.ModuleVarRef(pos, m, n, t) =>
        J.GetStatic(FieldRef(m.classRef, n, t.jtype))
      case T.LocalRef(pos, d, index, tt) =>
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
      case T.LetRec(pos, vs, b) =>
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
      case T.If(pos, c, th, el, t) =>
        J.If(appExpr(c, depth), appExpr(th, depth), appExpr(el, depth), t.jtype)
      case T.App(pos, f, a, t) =>
        val appSig = MethodSig(Type.Fun.ref, false, false, "app", Seq(TObject), Some(TObject))
        val invoke = J.Invoke(
          appSig,
          false,
          Some(box(appExpr(f, depth))),
          Seq(box(appExpr(a, depth))))
        unbox(J.Cast(invoke, t.jtype.boxed), t.jtype)
      case T.Fun(pos, b, t) =>
        val funKlass = createFun(appExpr(b, depth + 1), None)
        val args =
          if (depth == 0) Seq(J.Null(TObject), J.Null(JType.Fun))
          else Seq(J.GetLocal(1, TObject), J.GetLocal(0, JType.Fun))
        J.Cast(J.JNew(funKlass, args), JType.Fun)
      case T.TAbs(pos, ps, b, t) => appExpr(b, depth)
      case T.JCallStatic(pos, m, a) =>
        val args = m.args.zip(a).map {
          case (t, a) =>
            autobox(appExpr(a, depth), t)
        }
        J.Invoke(m, false, None, args)
      case T.JCallInstance(pos, m, r, a) =>
        val args = m.args.zip(a).map {
          case (t, a) =>
            autobox(appExpr(a, depth), t)
        }
        J.Invoke(m, false, Some(box(appExpr(r, depth))), args)
      case T.JNew(pos, r, a) => J.JNew(r, a.map(appExpr(_, depth)))
      case T.Upcast(pos, b, t) => J.Cast(appExpr(b, depth), t.jtype)
      case T.MatchError(pos, t) =>
        J.Throw(
          J.JNew(ClassRef.fromInternalName("java/lang/RuntimeException"), Seq(J.LitString("match error"))),
          t.jtype)
    }

    def appData(name: String, ctors: Seq[T.DataCtor]): Unit = {
      val dataBaseClass = ClassRef.fromInternalName("com/todesking/ojaml/ml0/runtime/Data")

      val dataKlass = JType.dataClass(moduleRef, name).ref
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
        case T.DataCtor(pos, ctorName, oparams, checkerName, extractorNames) =>
          val params = oparams.map(_.jtype)
          val ctorKlass = ClassRef(dataKlass.pkg, s"${dataKlass.name}$$${ctorName}")
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
              J.PutValuesToArray(
                J.NewObjectArray(params.size),
                paramFields.map { field =>
                  box(J.GetField(field, J.GetLocal(0, JType.TKlass(ctorKlass))))
                })))
          ctorBuilder.addMethod(J.MethodDef("values", false, Seq(), Some(JType.ObjectArray), valuesBody))

          val nameBody = Seq(
            J.TReturn(J.LitString(ctorName)))
          ctorBuilder.addMethod(J.MethodDef("name", false, Seq(), Some(JType.TString), nameBody))

          klasses :+= ctorBuilder.build()

          val fieldCtor = FieldRef(moduleClass, ctorName, if (params.isEmpty) JType.TKlass(dataKlass) else JType.Fun)
          val ctorTree = params.foldRight(
            T.Upcast(
              pos,
              T.JNew(
                pos,
                ctorKlass,
                oparams.zipWithIndex.map {
                  case (t, i) =>
                    T.LocalRef(pos, i + 1, 0, t)
                }),
              Type.Klass(dataKlass)): T.Expr) { (t, e) => T.Fun(pos, e, e.tpe) }
          builder.addClinit(J.PutStatic(fieldCtor, appExpr(ctorTree, 0)))

          val fieldCheck = FieldRef(moduleClass, checkerName, JType.Fun)
          val funCheck = createFun(J.InstanceOf(J.GetLocal(1, JType.TObject), ctorKlass), None)
          builder.addClinit(
            J.PutStatic(fieldCheck, J.JNew(funCheck, Seq(J.Null(JType.TObject), J.Null(JType.Fun)))))
          paramFields.zip(extractorNames).foreach {
            case (field, checkerName) =>
              val fieldGet = FieldRef(moduleClass, checkerName, JType.Fun)
              val funGet = createFun(J.GetField(field, J.Cast(J.GetLocal(1, JType.TObject), JType.TKlass(ctorKlass))), None)
              builder.addClinit(
                J.PutStatic(fieldGet, J.JNew(funGet, Seq(J.Null(JType.TObject), J.Null(JType.Fun)))))
          }
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
        Seq(J.TExpr(J.PutValuesToArray(J.Cast(J.GetLocal(1, JType.TObject), JType.ObjectArray), rvs)))
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
    val transformer = new Javalizer.Transformer(m.moduleRef, moduleClass)
    m.body.foreach { t => transformer.appTerm(t) }
    transformer.build()
  }
}
