package com.todesking.ojaml.ml0.compiler.scala

import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files

import Compiler.{ Result, Error }

class Compiler(baseDir: Path) {
  import com.todesking.ojaml.ml0.compiler.scala.{ RawAST => RT, TAST => TT }

  lazy val typer = new Typer()
  lazy val assembler = new Assembler(baseDir)

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
    val parser = new Parser(file.path.toString)
    parser.parse(file.content) match {
      case parser.NoSuccess(msg, next) =>
        val line = next.pos.line
        val col = next.pos.column
        Seq(Error(file.path.toString, line, col, s"Parse error: $msg\n${next.pos.longString}"))
      case parser.Success(ast, _) =>
        typer.typeProgram(ast).fold(l => l, { r => assembler.emit(r); Seq() })
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
}

object Compiler {
  case class Error(location: String, line: Int, col: Int, message: String)
  case class Result(errors: Seq[Error]) {
    def isSuccess = errors.isEmpty
  }
}

