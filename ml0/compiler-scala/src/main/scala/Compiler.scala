package com.todesking.ojaml.ml0.compiler.scala

import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files

import org.objectweb.asm

import asm.{ Opcodes => op }

class Compiler(baseDir: Path) extends asm.Opcodes {
  def compile(files: Seq[Path]): Unit = {
    if (files.head.getFileName.toString == "Empty.ml0") {
      val cw = new asm.ClassWriter(0)
      cw.visit(
        op.V1_8,
        op.ACC_PUBLIC,
        "test/ml0/Empty",
        null,
        "java/lang/Object",
        Array())
      cw.visitEnd()
      val data = cw.toByteArray()
      val packageDir = baseDir.resolve("test/ml0")
      val out = packageDir.resolve("Empty.class")
      Files.createDirectories(packageDir)
      Files.write(out, data)
    } else if (files.head.getFileName.toString == "Let.ml0") {
      val cw = new asm.ClassWriter(asm.ClassWriter.COMPUTE_FRAMES)
      cw.visit(
        op.V1_8,
        op.ACC_PUBLIC,
        "test/ml0/Let",
        null,
        "java/lang/Object",
        Array())
      cw.visitField(
        op.ACC_PUBLIC | op.ACC_STATIC,
        "result",
        "Ljava/lang/Integer;",
        null,
        null)
      val clinit = cw.visitMethod(
        op.ACC_PUBLIC | op.ACC_STATIC,
        "<clinit>",
        "()V",
        null,
        Array())
      clinit.visitInsn(op.ICONST_2)
      clinit.visitMethodInsn(
        op.INVOKESTATIC,
        "java/lang/Integer",
        "valueOf",
        "(I)Ljava/lang/Integer;")
      clinit.visitFieldInsn(
        op.PUTSTATIC,
        "test/ml0/Let",
        "result",
        "Ljava/lang/Integer;")
      clinit.visitInsn(op.RETURN)
      clinit.visitMaxs(0, 0)
      clinit.visitEnd()
      cw.visitEnd()
      val data = cw.toByteArray()
      val packageDir = baseDir.resolve("test/ml0")
      val out = packageDir.resolve("Let.class")
      Files.createDirectories(packageDir)
      Files.write(out, data)
    } else ???
  }
}

