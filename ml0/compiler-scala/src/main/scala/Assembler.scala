package com.todesking.ojaml.ml0.compiler.scala

import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files

import org.objectweb.asm

import asm.{ Opcodes => op }

class Assembler(baseDir: Path) {
  import com.todesking.ojaml.ml0.compiler.scala.{ TAST => TT }
  def emit(p: TT.Program): Unit =
    emitStruct(p.pkg.value, p.item)

  val funClass = "com/todesking/ojaml/ml0/runtime/Fun"
  val funSig = s"L$funClass;"

  val objectClass = "java/lang/Object"
  val objectSig = s"L$objectClass;"

  def sig(tpe: Type) = s"L${tname(tpe)};"
  def tname(tpe: Type) = tpe match {
    case Type.Int => "java/lang/Integer"
    case Type.Bool => "java/lang/Boolean"
    case Type.Fun(l, r) => funClass
  }

  def msig(m: ModuleRef) =
    s"${m.pkg}.${m.name}".replaceAll("\\.", "/")

  def write(pkg: String, name: String, data: Array[Byte]) = {
    val packageDir = baseDir.resolve(pkg.replaceAll("\\.", "/"))
    val out = packageDir.resolve(s"$name.class")
    Files.createDirectories(packageDir)
    println(s"Emit: $out")
    Files.write(out, data)
  }

  def methodEnd(mw: asm.MethodVisitor): Unit = {
    mw.visitMaxs(0, 0)
    mw.visitEnd()
  }

  def emitStruct(pkg: String, struct: TT.Struct): Unit = {
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
          name.value,
          sig(tpe),
          null,
          null)
      case _: TT.Expr =>
      // ignore
    }

    var funID = 0
    def emitFun(argType: Type, body: TT.Expr, depth: Int): String = {
      val cname = s"${struct.name.value}$$$funID"
      val qcname = s"${pkg}.$cname".replaceAll("\\.", "/")
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
      init.visitLdcInsn(depth)
      init.visitMethodInsn(
        op.INVOKESPECIAL,
        funClass,
        "<init>",
        s"(${objectSig}${funSig}I)V")
      init.visitInsn(op.RETURN)
      methodEnd(init)
      val app = cw.visitMethod(
        op.ACC_PUBLIC,
        "app",
        s"($objectSig)$objectSig",
        null,
        Array())
      eval(app, body, depth + 1)
      app.visitInsn(op.ARETURN)
      methodEnd(app)
      cw.visitEnd()
      write(pkg, cname, cw.toByteArray)
      qcname
    }

    def eval(method: asm.MethodVisitor, expr: TT.Expr, depth: Int): Unit = expr match {
      case TT.LitInt(v) =>
        method.visitLdcInsn(v)
        method.visitMethodInsn(
          op.INVOKESTATIC,
          "java/lang/Integer",
          "valueOf",
          "(I)Ljava/lang/Integer;")
      case TT.LitBool(v) =>
        method.visitLdcInsn(v)
        method.visitMethodInsn(
          op.INVOKESTATIC,
          "java/lang/Boolean",
          "valueOf",
          "(Z)Ljava/lang/Boolean;")
      case TT.ModuleVarRef(module, name, tpe) =>
        method.visitFieldInsn(op.GETSTATIC, msig(module), name, sig(tpe))
      case TT.LocalRef(index, tpe) =>
        if (depth - 1 == index) {
          method.visitVarInsn(op.ALOAD, 1)
        } else {
          method.visitVarInsn(op.ALOAD, 0) // push this
          method.visitLdcInsn(index)
          method.visitMethodInsn(
            op.INVOKEVIRTUAL,
            funClass,
            "getLocal",
            s"(I)$objectSig")
        }
        method.visitTypeInsn(op.CHECKCAST, tname(tpe))
      case TT.If(cond, th, el, tpe) =>
        val lElse = new asm.Label()
        val lEnd = new asm.Label()
        eval(method, cond, depth)
        method.visitMethodInsn(
          op.INVOKEVIRTUAL,
          "java/lang/Boolean",
          "booleanValue",
          "()Z")
        method.visitJumpInsn(op.IFEQ, lElse)
        eval(method, th, depth)
        method.visitJumpInsn(op.GOTO, lEnd)
        method.visitLabel(lElse)
        eval(method, el, depth)
        method.visitLabel(lEnd)
      case TT.Fun(tpe, body) =>
        val klass = emitFun(tpe, body, depth)
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
          s"($objectSig$funSig)V")
      case TT.App(f, x, tpe) =>
        eval(method, f, depth)
        eval(method, x, depth)
        method.visitMethodInsn(
          op.INVOKEVIRTUAL,
          funClass,
          "app",
          s"($objectSig)$objectSig")
        method.visitTypeInsn(
          op.CHECKCAST,
          tname(tpe))
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
        clinit.visitFieldInsn(op.PUTSTATIC, className, name.value, sig(tpe))
      case e: TT.Expr =>
      // ignore for now
    }

    clinit.visitInsn(op.RETURN)
    methodEnd(clinit)
    cw.visitEnd()

    write(pkg, struct.name.value, cw.toByteArray)
  }
}
