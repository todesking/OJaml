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

  def sig(tpe: Type) = tpe match {
    case Type.Int => "Ljava/lang/Integer;"
  }

  def msig(m: ModuleRef) =
    s"${m.pkg}.${m.name}".replaceAll("\\.", "/")

  def emitStruct(pkg: String, struct: TT.Struct): Unit = {
    val className = s"$pkg.${struct.name.value}".replaceAll("\\.", "/")
    val cw = new asm.ClassWriter(asm.ClassWriter.COMPUTE_FRAMES)
    cw.visit(
      op.V1_8,
      op.ACC_PUBLIC,
      className,
      null,
      "java/lang/Object",
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

    val clinit = cw.visitMethod(
      op.ACC_PUBLIC | op.ACC_STATIC,
      "<clinit>",
      "()V",
      null,
      Array())
    def eval(expr: TT.Expr) = expr match {
      case TT.LitInt(v) =>
        clinit.visitLdcInsn(v)
        clinit.visitMethodInsn(
          op.INVOKESTATIC,
          "java/lang/Integer",
          "valueOf",
          "(I)Ljava/lang/Integer;")
      case TT.Ref(ref, tpe) =>
        ref match {
          case VarRef.ModuleVar(m, name) =>
            clinit.visitFieldInsn(op.GETSTATIC, msig(m), name, sig(tpe))
        }
    }
    struct.body.foreach {
      case TT.TLet(name, tpe, expr) =>
        eval(expr)
        clinit.visitFieldInsn(op.PUTSTATIC, className, name.value, sig(tpe))
      case e: TT.Expr =>
      // ignore for now
    }

    clinit.visitInsn(op.RETURN)
    clinit.visitMaxs(0, 0)
    clinit.visitEnd()
    cw.visitEnd()
    val data = cw.toByteArray()
    val packageDir = baseDir.resolve(pkg.replaceAll("\\.", "/"))
    val out = packageDir.resolve(s"${struct.name.value}.class")
    Files.createDirectories(packageDir)
    println(s"Emit: $out")
    Files.write(out, data)
  }
}
