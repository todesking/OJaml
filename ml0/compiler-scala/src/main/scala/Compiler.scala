package com.todesking.ojaml.ml0.compiler.scala

import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files

import org.objectweb.asm

import asm.{ Opcodes => op }

class Compiler(baseDir: Path) extends asm.Opcodes {
  def compile(files: Seq[Path]): Unit = {
    files.foreach(compile1)
  }
  def compile1(file: Path): Unit = {
    val content = new String(Files.readAllBytes(file))
    Parser.parse(content) match {
      case Parser.NoSuccess(msg, next) =>
        val line = next.pos.line
        val col = next.pos.column
        throw new RuntimeException(s"$file:$line:$col Parse error: $msg\n${next.pos.longString}")
      case Parser.Success(ast, _) =>
        emit(typing(ast))
    }

    /*
    if (file.getFileName.toString == "Empty.ml0") {
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
    } else if (file.getFileName.toString == "Let.ml0") {
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
    */
  }

  def typing(p: AST.Program): TAST.Program =
    TAST.Program(p.pkg, typing(p.item))

  def typing(s: AST.Struct): TAST.Struct =
    TAST.Struct(s.name, s.body.map(typing))

  def typing(t: AST.Term): TAST.Term = t match {
    case AST.TLet(name, expr) =>
      val e = typing(expr)
      TAST.TLet(name, e.tpe, e)
    case e: AST.Expr => typing(e)
  }

  def typing(e: AST.Expr): TAST.Expr = e match {
    case AST.LitInt(v) => TAST.LitInt(v)
  }

  def emit(p: TAST.Program): Unit = {
    emitStruct(p.pkg, p.item)
  }

  def sig(tpe: Type) = tpe match {
    case Type.Int => "Ljava/lang/Integer;"
  }

  def emitStruct(pkg: String, struct: TAST.Struct): Unit = {
    val className = s"$pkg.${struct.name}".replaceAll("\\.", "/")
    val cw = new asm.ClassWriter(asm.ClassWriter.COMPUTE_FRAMES)
    cw.visit(
      op.V1_8,
      op.ACC_PUBLIC,
      className,
      null,
      "java/lang/Object",
      Array())

    struct.body.foreach {
      case TAST.TLet(name, tpe, expr) =>
        cw.visitField(
          op.ACC_PUBLIC | op.ACC_STATIC,
          name,
          sig(tpe),
          null,
          null)
      case _: TAST.Expr =>
      // ignore
    }

    val clinit = cw.visitMethod(
      op.ACC_PUBLIC | op.ACC_STATIC,
      "<clinit>",
      "()V",
      null,
      Array())
    def eval(expr: TAST.Expr) = expr match {
      case TAST.LitInt(v) =>
        clinit.visitLdcInsn(v)
        clinit.visitMethodInsn(
          op.INVOKESTATIC,
          "java/lang/Integer",
          "valueOf",
          "(I)Ljava/lang/Integer;")
    }
    struct.body.foreach {
      case TAST.TLet(name, tpe, expr) =>
        eval(expr)
        clinit.visitFieldInsn(op.PUTSTATIC, className, name, sig(tpe))
      case e: TAST.Expr =>
      // ignore for now
    }

    clinit.visitInsn(op.RETURN)
    clinit.visitMaxs(0, 0)
    clinit.visitEnd()
    cw.visitEnd()
    val data = cw.toByteArray()
    val packageDir = baseDir.resolve(pkg.replaceAll("\\.", "/"))
    val out = packageDir.resolve(s"${struct.name}.class")
    Files.createDirectories(packageDir)
    println(s"Emit: $out")
    Files.write(out, data)
  }
}

