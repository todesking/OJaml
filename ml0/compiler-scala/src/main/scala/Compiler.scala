package com.todesking.ojaml.ml0.compiler.scala

import java.nio.file.Path
import java.nio.file.Files

import org.objectweb.asm

import asm.{ Opcodes => op }

class Compiler(baseDir: Path) extends asm.Opcodes {
  def compile(files: Seq[Path]): Unit = {
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
  }
}

