package com.todesking.ojaml.ml0.compiler.scala

import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files

import org.objectweb.asm

import asm.{ Opcodes => op }

class Assembler(baseDir: Path) {
  import com.todesking.ojaml.ml0.compiler.scala.{ TypedAST => TT }

  val sym2name: Map[Char, String] = """
  |+ plus
  |- minus
  |/ slash
  |% percent
  |* asterisk""".stripMargin
    .drop(1)
    .split("\n")
    .map(_.split(" "))
    .map { case Array(a, b) => a(0) -> b }
    .toMap

  def escape(name: String): String = name.map { c =>
    sym2name.get(c).fold {
      c.toString
    } { n =>
      s"$$${n}_"
    }
  }.mkString("")

  val funClass: String = Type.Fun.ref.internalName
  val funSig = s"L$funClass;"

  val objectClass = "java/lang/Object"
  val objectSig = s"L$objectClass;"

  def descriptor(tpe: Type): String = Type.toAsm(tpe).getDescriptor
  def tname(tpe: Type): ClassRef = tpe match {
    case Type.Reference(name) => name
  }

  def msig(m: ModuleRef) =
    s"${m.pkg.internalName}/${m.name}"

  def write(pkg: String, name: String, data: Array[Byte]): Path = {
    val packageDir = baseDir.resolve(pkg.replaceAll("\\.", "/"))
    val out = packageDir.resolve(s"$name.class")
    Files.createDirectories(packageDir)
    Files.write(out, data)
  }

  def methodEnd(mw: asm.MethodVisitor): Unit = {
    mw.visitMaxs(0, 0)
    mw.visitEnd()
  }

  def emit(struct: TT.Module): Unit = {
    val pkg = struct.pkg.value
    val className = s"$pkg.${struct.name.value}".replaceAll("\\.", "/")
    val cw = new asm.ClassWriter(asm.ClassWriter.COMPUTE_FRAMES)
    cw.visit(
      op.V1_8,
      op.ACC_PUBLIC,
      className,
      null,
      objectClass,
      Array())

    struct.body.foreach {
      case TT.TLet(name, tpe, expr) =>
        cw.visitField(
          op.ACC_PUBLIC | op.ACC_STATIC,
          escape(name.value),
          descriptor(tpe),
          null,
          null)
      case _: TT.Expr =>
      // ignore
    }

    var funID = 0
    def emitFun(body: TT.Expr, depth: Int, recValues: Option[Seq[TT.Expr]]): String = {
      val cname = s"${struct.name.value}$$$funID"
      val qcname = s"$pkg.$cname".replaceAll("\\.", "/")
      funID += 1

      val cw = new asm.ClassWriter(asm.ClassWriter.COMPUTE_FRAMES)
      cw.visit(
        op.V1_8,
        op.ACC_PUBLIC,
        qcname,
        null,
        funClass,
        Array())
      val init = cw.visitMethod(
        op.ACC_PUBLIC,
        "<init>",
        s"($objectSig$funSig)V",
        null,
        Array())
      init.visitVarInsn(op.ALOAD, 0)
      init.visitVarInsn(op.ALOAD, 1)
      init.visitVarInsn(op.ALOAD, 2)
      init.visitMethodInsn(
        op.INVOKESPECIAL,
        funClass,
        "<init>",
        s"($objectSig$funSig)V", false)
      init.visitInsn(op.RETURN)
      methodEnd(init)
      val app = cw.visitMethod(
        op.ACC_PUBLIC,
        "app",
        s"($objectSig)$objectSig",
        null,
        Array())
      recValues.foreach { rvs =>
        app.visitVarInsn(op.ALOAD, 1)
        app.visitTypeInsn(op.CHECKCAST, s"[$objectSig")
        rvs.zipWithIndex.foreach {
          case (rv, i) =>
            app.visitInsn(op.DUP)
            app.visitLdcInsn(i)
            eval(app, rv, depth + 1)
            autobox(app, rv.tpe, rv.tpe.boxed)
            app.visitInsn(op.AASTORE)
        }
        app.visitLdcInsn(op.POP)
      }
      eval(app, body, depth + 1)
      box(app, body.tpe)
      app.visitInsn(op.ARETURN)
      methodEnd(app)
      cw.visitEnd()
      write(pkg, cname, cw.toByteArray)
      qcname
    }

    def eval(method: asm.MethodVisitor, expr: TT.Expr, depth: Int): Unit = expr match {
      case TT.LitInt(v) =>
        method.visitLdcInsn(v)
      case TT.LitBool(v) =>
        method.visitLdcInsn(v)
      case TT.LitString(v) =>
        method.visitLdcInsn(v)
      case TT.ModuleVarRef(module, name, tpe) =>
        method.visitFieldInsn(op.GETSTATIC, msig(module), escape(name), descriptor(tpe))
      case TT.LocalRef(d, index, tpe) =>
        if (depth == d) {
          method.visitVarInsn(op.ALOAD, 1)
          if (index == 0) {
            // stack top is the target value
          } else {
            method.visitTypeInsn(op.CHECKCAST, s"[$objectSig")
            method.visitLdcInsn(index - 1)
            method.visitInsn(op.AALOAD)
          }
        } else {
          method.visitVarInsn(op.ALOAD, 0)
          method.visitLdcInsn(d)
          method.visitLdcInsn(index)
          method.visitMethodInsn(
            op.INVOKEVIRTUAL,
            funClass,
            "getLocal",
            s"(II)$objectSig", false)
        }
        method.visitTypeInsn(op.CHECKCAST, tpe.boxed.ref.internalName)
        unbox(method, tpe.boxed)
      case TT.LetRec(values, body) =>
        val klass = emitFun(body, depth, Some(values))
        method.visitTypeInsn(op.NEW, klass)
        method.visitInsn(op.DUP)
        if (depth == 0) {
          method.visitInsn(op.ACONST_NULL)
          method.visitInsn(op.ACONST_NULL)
        } else {
          method.visitVarInsn(op.ALOAD, 1)
          method.visitVarInsn(op.ALOAD, 0)
        }
        method.visitMethodInsn(
          op.INVOKESPECIAL,
          klass,
          "<init>",
          s"($objectSig$funSig)V", false)
        method.visitLdcInsn(values.size)
        method.visitTypeInsn(op.ANEWARRAY, Type.Object.ref.internalName) // letrec environement
        method.visitMethodInsn(op.INVOKEVIRTUAL, funClass, "app", s"($objectSig)$objectSig", false)
        method.visitTypeInsn(op.CHECKCAST, body.tpe.boxed.ref.internalName)
        autobox(method, body.tpe.boxed, body.tpe)
      case TT.If(cond, th, el, tpe) =>
        val lElse = new asm.Label()
        val lEnd = new asm.Label()
        eval(method, cond, depth)
        method.visitJumpInsn(op.IFEQ, lElse)
        eval(method, th, depth)
        method.visitJumpInsn(op.GOTO, lEnd)
        method.visitLabel(lElse)
        eval(method, el, depth)
        method.visitLabel(lEnd)
      case TT.Fun(body, tpe) =>
        val klass = emitFun(body, depth, None)
        method.visitTypeInsn(op.NEW, klass)
        method.visitInsn(op.DUP)
        if (depth == 0) {
          method.visitInsn(op.ACONST_NULL)
          method.visitInsn(op.ACONST_NULL)
        } else {
          method.visitVarInsn(op.ALOAD, 1)
          method.visitVarInsn(op.ALOAD, 0)
        }
        method.visitMethodInsn(
          op.INVOKESPECIAL,
          klass,
          "<init>",
          s"($objectSig$funSig)V", false)
      case TT.App(f, x, tpe) =>
        eval(method, f, depth)
        eval(method, x, depth)
        box(method, x.tpe)
        method.visitMethodInsn(
          op.INVOKEVIRTUAL,
          funClass,
          "app",
          s"($objectSig)$objectSig", false)
        method.visitTypeInsn(op.CHECKCAST, tpe.boxed.ref.internalName)
        autobox(method, tpe.boxed, tpe)
      case TT.TAbs(ps, e, t) =>
        eval(method, e, depth)
      case e @ TT.JCallStatic(target, args) =>
        args.zip(target.args).foreach {
          case (x, t) =>
            eval(method, x, depth)
            autobox(method, x.tpe, t)
        }
        method.visitMethodInsn(
          op.INVOKESTATIC,
          target.klass.internalName,
          target.name,
          target.descriptor, false)
        autobox(method, target.ret, e.tpe)
      case e @ TT.JCallInstance(target, receiver, args) =>
        eval(method, receiver, depth)
        autobox(method, receiver.tpe, receiver.tpe.boxed)
        args.zip(target.args).foreach {
          case (x, t) =>
            eval(method, x, depth)
            autobox(method, x.tpe, t)
        }
        method.visitMethodInsn(
          if (target.isInterface) op.INVOKEINTERFACE else op.INVOKEVIRTUAL,
          target.klass.internalName,
          target.name,
          target.descriptor, target.isInterface)
        autobox(method, target.ret, e.tpe)
    }

    val clinit = cw.visitMethod(
      op.ACC_PUBLIC | op.ACC_STATIC,
      "<clinit>",
      "()V",
      null,
      Array())
    struct.body.foreach {
      case TT.TLet(name, tpe, expr) =>
        eval(clinit, expr, 0)
        autobox(clinit, expr.tpe, tpe)
        clinit.visitFieldInsn(op.PUTSTATIC, className, escape(name.value), descriptor(tpe))
      case e: TT.Expr =>
      // ignore for now
    }

    clinit.visitInsn(op.RETURN)
    methodEnd(clinit)
    cw.visitEnd()

    write(pkg, struct.name.value, cw.toByteArray)
  }

  private[this] def autobox(method: asm.MethodVisitor, from: Option[Type], to: Type): Unit = from match {
    case Some(f) => autobox(method, f, to)
    case None =>
      if (to != Type.Unit) throw new AssertionError(s"Unit type expected but actual is $to")
      method.visitInsn(op.ACONST_NULL)
      method.visitTypeInsn(op.CHECKCAST, Type.Unit.ref.internalName)
  }
  private[this] def autobox(method: asm.MethodVisitor, from: Type, to: Type): Unit = {
    if (from == to) {
      // do nothing
    } else if (from.boxed == to) {
      box(method, from)
    } else if (from == to.boxed) {
      unbox(method, from)
    } else {
      throw new RuntimeException(s"$from and $to is not compatible")
    }
  }
  private[this] def box(method: asm.MethodVisitor, tpe: Type): Unit = {
    if (tpe == tpe.boxed) {
      // do nothing
    } else {
      val desc = s"(${descriptor(tpe)})${descriptor(tpe.boxed)}"
      method.visitMethodInsn(op.INVOKESTATIC, tpe.boxed.ref.internalName, "valueOf", desc, false)
    }
  }
  private[this] def unbox(method: asm.MethodVisitor, tpe: Type): Unit = {
    tpe.unboxed.foreach { prim =>
      val name =
        prim match {
          case Type.Int => "intValue"
          case Type.Bool => "booleanValue"
        }
      val desc = s"()${descriptor(prim)}"
      method.visitMethodInsn(op.INVOKEVIRTUAL, tpe.boxed.ref.internalName, name, desc, false)
    }
  }
}
