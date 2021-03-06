package com.todesking.ojaml.ml0.compiler.scala

import com.todesking.ojaml.ml0.compiler.scala.{ JAST => J }
import com.todesking.ojaml.ml0.compiler.scala.{ TypedAST => T }
import com.todesking.ojaml.ml0.compiler.scala.TypedAST.RefMember

import JType.TObject

object Javalizer {
  class ClassBuilder(filePath: String, ref: ClassRef, superRef: ClassRef) {
    private[this] var nextFunClassID = 0
    private[this] var methodDefs = Vector.empty[J.MethodDef]
    private[this] var fieldDefs = Vector.empty[J.FieldDef]
    private[this] var clinits = Vector.empty[J.Term]
    private[this] var funClassDefs = Vector.empty[J.ClassDef]

    def build() = J.ClassDef(
      filePath,
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
  class Transformer(filePath: String, moduleRef: ModuleRef, moduleClass: ClassRef) {
    private[this] var nextFunClassID = 0
    private[this] val builder = new ClassBuilder(filePath, moduleClass, TObject.ref)
    private[this] var klasses = Vector.empty[J.ClassDef]
    def build() = builder.build() +: klasses

    def appTerm(t: T.Term): Unit = t match {
      case T.TLet(pos, name, tpe, expr) =>
        val f = builder.addStaticField(name.value, tpe.jtype)
        expr.foreach { expr =>
          builder.addClinit(
            J.PutStatic(pos, f, appExpr(expr, 0, Map())))
        }
      case T.TLetRec(pos, bindings) =>
        bindings.foreach {
          case (name, tpe, fun) =>
            val f = builder.addStaticField(name.value, tpe.jtype)
            builder.addClinit(
              J.PutStatic(pos, f, appExpr(fun, 0, Map())))
        }
      case T.Data(pos, name, params, ctors) =>
        appData(pos, name.value, ctors)
      case T.TExpr(pos, expr) =>
        builder.addClinit(J.TExpr(pos, appExpr(expr, 0, Map())))
    }

    def appExpr(expr: T.Expr, depth: Int, locals: Map[String, (Int, Int)]): J.Expr = expr match {
      case T.Lit(pos, v) => J.EPos(pos, J.Lit(v))
      case T.RefMember(pos, m, t) =>
        J.EPos(
          pos,
          J.GetStatic(FieldRef(m.module.classRef, m.name, t.jtype)))
      case T.RefLocal(pos, name, tt) =>
        val (d, index) = locals(name)
        J.EPos(pos, appRefLocal(depth, d, index, tt.jtype))
      case T.ELetRec(pos, vs, b) =>
        val newLocals = locals ++ vs.map(_._1).zipWithIndex.map { case (name, i) => name.value -> (depth + 1, i + 1) }
        val funs = vs.map { case (name, tpe, fun) => appExpr(fun, depth + 1, newLocals) }
        val body = appExpr(b, depth + 1, newLocals)
        val funKlass = createFun(pos, body, Some(funs))
        val funNewArgs =
          if (depth == 0) Seq(J.Null(TObject), J.Null(JType.Fun))
          else Seq(J.GetLocal(1, TObject), J.GetLocal(0, JType.Fun))
        J.EPos(
          pos,
          unbox(
            J.Cast(
              J.Invoke(
                MethodSig(Type.Fun.ref, false, false, "app", Seq(TObject), Some(TObject)),
                false,
                Some(J.JNew(funKlass, funNewArgs)),
                Seq(J.NewObjectArray(vs.size))),
              body.tpe.boxed), body.tpe))
      case T.If(pos, c, th, el, t) =>
        J.EPos(
          pos,
          J.If(
            appExpr(c, depth, locals),
            appExpr(th, depth, locals),
            appExpr(el, depth, locals),
            t.jtype))
      case T.App(pos, f, a, t) =>
        val appSig = MethodSig(Type.Fun.ref, false, false, "app", Seq(TObject), Some(TObject))
        val invoke = J.Invoke(
          appSig,
          false,
          Some(box(appExpr(f, depth, locals))),
          Seq(box(appExpr(a, depth, locals))))
        J.EPos(pos, unbox(J.Cast(invoke, t.jtype.boxed), t.jtype))
      case T.Fun(pos, param, b, t) =>
        val newLocals = locals + (param -> (depth + 1, 0))
        val funKlass = createFun(pos, appExpr(b, depth + 1, newLocals), None)
        val args =
          if (depth == 0) Seq(J.Null(TObject), J.Null(JType.Fun))
          else Seq(J.GetLocal(1, TObject), J.GetLocal(0, JType.Fun))
        J.EPos(
          pos,
          J.Cast(J.JNew(funKlass, args), JType.Fun))
      case T.TAbs(pos, ps, b, t) =>
        appExpr(b, depth, locals)
      case T.JCallStatic(pos, m, a) =>
        val args = m.args.zip(a).map {
          case (t, a) =>
            autobox(appExpr(a, depth, locals), t)
        }
        J.EPos(
          pos,
          J.Invoke(m, false, None, args))
      case T.JCallInstance(pos, m, r, a) =>
        val args = m.args.zip(a).map {
          case (t, a) =>
            autobox(appExpr(a, depth, locals), t)
        }
        J.EPos(
          pos,
          J.Invoke(m, false, Some(box(appExpr(r, depth, locals))), args))
      case T.MatchError(pos, t) =>
        J.EPos(
          pos,
          J.Throw(
            J.JNew(
              ClassRef.fromInternalName("java/lang/RuntimeException"),
              Seq(J.Lit(LitValue.of("match error")))),
            t.jtype))
    }

    def appRefLocal(currentDepth: Int, depth: Int, index: Int, tpe: JType) = {
      if (depth == currentDepth) {
        if (index == 0) {
          unbox(J.Cast(J.GetLocal(1, tpe.boxed), tpe.boxed), tpe)
        } else {
          unbox(
            J.Cast(
              J.GetObjectFromUncheckedArray(J.GetLocal(1, TObject), index - 1),
              tpe.boxed),
            tpe)
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
              Seq(J.Lit(LitValue.of(depth)), J.Lit(LitValue.of(index)))),
            tpe.boxed),
          tpe)
      }
    }

    def appData(pos: Pos, name: String, ctors: Seq[T.DataCtor]): Unit = {
      val dataBaseClass = ClassRef.fromInternalName("com/todesking/ojaml/ml0/runtime/Data")

      val dataKlass = JType.dataClass(moduleRef, name).ref
      val dataBuilder = new ClassBuilder(filePath, dataKlass, dataBaseClass)

      val initBody = Seq(
        J.TExpr(
          pos,
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
          val ctorBuilder = new ClassBuilder(filePath, ctorKlass, dataKlass)
          val paramFields = params.zipWithIndex.map { case (t, i) => FieldRef(ctorKlass, s"v$i", t) }
          paramFields.foreach { field =>
            ctorBuilder.addField(field.name, field.tpe)
          }

          val initBody = Seq(
            J.TExpr(
              pos,
              J.Invoke(
                MethodSig(dataKlass, false, false, "<init>", Seq(JType.TInt), None),
                true,
                Some(J.GetLocal(0, JType.TKlass(ctorKlass))),
                Seq(J.Lit(LitValue.of(params.size)))))) ++ paramFields.zipWithIndex.map {
              case (field, i) =>
                J.PutField(pos, field, J.GetLocal(0, JType.TKlass(ctorKlass)), J.GetLocal(i + 1, field.tpe))
            }
          ctorBuilder.addMethod(J.MethodDef("<init>", false, params, None, initBody))

          val valuesBody = Seq(
            J.TReturn(
              pos,
              J.PutValuesToArray(
                J.NewObjectArray(params.size),
                paramFields.map { field =>
                  box(J.GetField(field, J.GetLocal(0, JType.TKlass(ctorKlass))))
                })))
          ctorBuilder.addMethod(J.MethodDef("values", false, Seq(), Some(JType.ObjectArray), valuesBody))

          val nameBody = Seq(
            J.TReturn(pos, J.Lit(LitValue.of(ctorName))))
          ctorBuilder.addMethod(J.MethodDef("name", false, Seq(), Some(JType.TString), nameBody))

          klasses :+= ctorBuilder.build()

          val fieldCtor = FieldRef(moduleClass, ctorName, if (params.isEmpty) JType.TKlass(dataKlass) else JType.Fun)

          val innerDepth = params.size
          val ctorTree =
            params.zipWithIndex.foldRight(
              J.JNew(ctorKlass, oparams.zipWithIndex.map {
                case (tpe, depth) =>
                  appRefLocal(innerDepth, depth + 1, 0, tpe.jtype)
              }): J.Expr) {
                case ((tpe, depth), body) =>
                  val funKlass = createFun(pos, body, None)
                  val args =
                    if (depth == 0) Seq(J.Null(TObject), J.Null(JType.Fun))
                    else Seq(J.GetLocal(1, TObject), J.GetLocal(0, JType.Fun))
                  J.JNew(funKlass, args)
              }
          builder.addClinit(J.PutStatic(pos, fieldCtor, ctorTree))

          val fieldCheck = FieldRef(moduleClass, checkerName, JType.Fun)
          val funCheck = createFun(pos, J.InstanceOf(J.GetLocal(1, JType.TObject), ctorKlass), None)
          builder.addClinit(
            J.PutStatic(pos, fieldCheck, J.JNew(funCheck, Seq(J.Null(JType.TObject), J.Null(JType.Fun)))))
          paramFields.zip(extractorNames).foreach {
            case (field, checkerName) =>
              val fieldGet = FieldRef(moduleClass, checkerName, JType.Fun)
              val funGet = createFun(pos, J.GetField(field, J.Cast(J.GetLocal(1, JType.TObject), JType.TKlass(ctorKlass))), None)
              builder.addClinit(
                J.PutStatic(pos, fieldGet, J.JNew(funGet, Seq(J.Null(JType.TObject), J.Null(JType.Fun)))))
          }
      }
    }

    def createFun(pos: Pos, body: J.Expr, recValues: Option[Seq[J.Expr]]) = {
      val funKlass = ClassRef(moduleClass.pkg, s"${moduleClass.name}$$${nextFunClassID}")
      nextFunClassID += 1
      val builder = new ClassBuilder(filePath, funKlass, Type.Fun.ref)
      val initBody = Seq(
        J.TExpr(
          pos,
          J.Invoke(
            MethodSig(Type.Fun.ref, false, false, "<init>", Seq(TObject, JType.Fun), None),
            true,
            Some(J.GetLocal(0, JType.Fun)),
            Seq(J.GetLocal(1, JType.TObject), J.GetLocal(2, JType.Fun)))))
      builder.addMethod(J.MethodDef("<init>", false, Seq(TObject, JType.Fun), None, initBody))
      val prepareRec = recValues.fold(Seq.empty[J.Term]) { rvs =>
        Seq(J.TExpr(pos, J.PutValuesToArray(J.Cast(J.GetLocal(1, JType.TObject), JType.ObjectArray), rvs)))
      }
      builder.addMethod(J.MethodDef(
        "app",
        false,
        Seq(TObject),
        Some(TObject),
        prepareRec ++ Seq(J.TReturn(pos, box(body)))))
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
    val transformer = new Javalizer.Transformer(m.pos.location, m.moduleRef, moduleClass)
    m.body.foreach { t => transformer.appTerm(t) }
    transformer.build()
  }
}
