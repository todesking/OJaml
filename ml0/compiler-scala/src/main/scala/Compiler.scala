package com.todesking.ojaml.ml0.compiler.scala

import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files

import org.objectweb.asm

import asm.{ Opcodes => op }

import Compiler.{ Result, Error }

class Compiler(baseDir: Path) extends asm.Opcodes {
  import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => RT, TAST => TT }

  def compile(files: Seq[Path]): Result =
    compileContents(files.map(FileContent.read))

  def compileContents(files: Seq[FileContent]): Result = {
    files.foreach { f =>
      val es = compile1(f)
      if (es.nonEmpty) return Result(es)
    }
    Result(Seq())
  }

  def compile1(file: FileContent): Seq[Error] = {
    Parser.parse(file.content) match {
      case Parser.NoSuccess(msg, next) =>
        val line = next.pos.line
        val col = next.pos.column
        Seq(Error(file.path, line, col, s"Parse error: $msg\n${next.pos.longString}"))
      case Parser.Success(ast, _) =>
        typing(ast).flatMap(emit).fold(l => l, r => Seq())
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

  type TResult[A] = Either[Seq[Error], A]

  def typing(p: RT.Program): TResult[TT.Program] =
    typing(p.item).right.map(TT.Program(p.pkg, _))

  def typing(s: RT.Struct): TResult[TT.Struct] =
    validate(s.body.map(typing)).right.map(TT.Struct(s.name, _))

  private[this] def validate[A](xs: Seq[TResult[A]]): TResult[Seq[A]] = {
    val rights = xs.collect { case Right(x) => x }
    if (rights.size == xs.size) Right(rights)
    else Left(xs.collect { case Left(x) => x }.flatten)
  }

  def typing(t: RT.Term): TResult[TT.Term] = t match {
    case RT.TLet(name, expr) =>
      for {
        e <- typing(expr)
      } yield TT.TLet(name, e.tpe, e)
    case e: RT.Expr => typing(e)
  }

  def typing(e: RT.Expr): TResult[TT.Expr] = e match {
    case RT.LitInt(v) => Right(TT.LitInt(v))
  }

  def emit(p: TT.Program): TResult[Unit] = {
    emitStruct(p.pkg.value, p.item)
  }

  def sig(tpe: Type) = tpe match {
    case Type.Int => "Ljava/lang/Integer;"
  }

  def emitStruct(pkg: String, struct: TT.Struct): TResult[Unit] = {
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
    Right(())
  }
}

object Compiler {
  case class Error(path: Path, line: Int, col: Int, message: String)
  case class Result(errors: Seq[Error]) {
    def isSuccess = errors.isEmpty
  }
}

