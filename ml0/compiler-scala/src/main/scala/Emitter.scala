package com.todesking.ojaml.ml0.compiler.scala

import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files

import org.objectweb.asm

import asm.{ Opcodes => op }
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.ClassVisitor

object Asm {
  import asm.{ MethodVisitor => MV }

  def autoload(m: MV, i: Int, t: JType): Unit = t match {
    case JType.TInt =>
      m.visitVarInsn(op.ILOAD, i)
    case JType.TBool =>
      m.visitVarInsn(op.ILOAD, i)
    // TODO: Other primitives and array
    case _: JType.TReference =>
      m.visitVarInsn(op.ALOAD, i)
  }
  def autostore(m: MV, i: Int, t: JType): Unit = t match {
    case JType.TInt =>
      m.visitVarInsn(op.ISTORE, i)
    case JType.TBool =>
      m.visitVarInsn(op.ISTORE, i)
    // TODO: Other primitives and array
    case _: JType.TReference =>
      m.visitVarInsn(op.ASTORE, i)
  }
}

class Emitter(baseDir: Path) {
  import com.todesking.ojaml.ml0.compiler.scala.{ JAST => J }
  import com.todesking.ojaml.ml0.compiler.scala.{ Asm => A }

  private[this] val sym2name: Map[Char, String] = """
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

  private[this] def descriptor(tpe: JType): String = JType.toAsm(tpe).getDescriptor

  private[this] def msig(m: ModuleRef) =
    s"${m.pkg.internalName}/${m.name}"

  private[this] def write(pkg: String, name: String, data: Array[Byte]): Path = {
    val packageDir = baseDir.resolve(pkg.replaceAll("\\.", "/"))
    val out = packageDir.resolve(s"$name.class")
    Files.createDirectories(packageDir)
    Files.write(out, data)
  }

  private[this] def methodEnd(mw: asm.MethodVisitor): Unit = {
    mw.visitMaxs(0, 0)
    mw.visitEnd()
  }

  private[this] def defineField(cw: asm.ClassWriter, name: String, tpe: JType, isStatic: Boolean): Unit = {
    val flags = if (isStatic) op.ACC_PUBLIC | op.ACC_STATIC else op.ACC_PUBLIC
    cw.visitField(
      flags,
      escape(name),
      descriptor(tpe),
      null,
      null)
  }

  private[this] def methodDescriptor(ret: JType, params: Seq[JType]): String =
    asm.Type.getMethodType(JType.toAsm(ret), params.map(JType.toAsm).toArray: _*).getDescriptor

  def emit(klass: J.ClassDef): Unit = {
    val pkg = klass.ref.pkg
    val className = klass.ref.internalName
    val cw = new asm.ClassWriter(asm.ClassWriter.COMPUTE_FRAMES)
    cw.visit(
      op.V1_8,
      op.ACC_PUBLIC,
      className,
      null,
      klass.superRef.internalName,
      Array())
    val fileName = java.nio.file.Paths.get(klass.filePath).getFileName.toString
    cw.visitSource(fileName, null)

    klass.fields.foreach { f =>
      defineField(cw, f.name, f.tpe, f.isStatic)
    }

    klass.methods.foreach { m => defineMethod(cw, klass, m) }

    cw.visitEnd()

    write(pkg.fullName, klass.ref.name, cw.toByteArray)
  }

  class MethodContext(sourcePath: String, mw: asm.MethodVisitor) {
    // offset -> (labe, lineNum)
    private[this] var lineNumbers = Map.empty[Int, (asm.Label, Int)]
    private[this] var posStack = List.empty[Pos]

    private[this] def recordPos(pos: Pos): Unit = {
      require(pos.location == sourcePath)
      val label = new asm.Label()
      mw.visitLabel(label)
      lineNumbers += label.getOffset -> (label, pos.line)
    }
    private[this] def pushPos[A](pos: Pos)(f: => A): A = {
      recordPos(pos)
      posStack = pos :: posStack
      val value = f
      posStack = posStack.tail
      posStack.headOption.foreach { p => recordPos(p) }
      value
    }
    def finish(): Unit = {
      lineNumbers.foreach {
        case (off, (label, line)) =>
          mw.visitLineNumber(line, label)
      }
    }
    def appTerm(term: J.Term): Unit = term match {
      case J.PutStatic(pos, ref, expr) =>
        pushPos(pos) {
          eval(mw, expr)
          mw.visitFieldInsn(op.PUTSTATIC, ref.klass.internalName, escape(ref.name), descriptor(ref.tpe))
        }
      case J.PutField(pos, ref, target, expr) =>
        pushPos(pos) {
          eval(mw, target)
          eval(mw, expr)
          mw.visitFieldInsn(op.PUTFIELD, ref.klass.internalName, escape(ref.name), descriptor(ref.tpe))
        }
      case J.TExpr(pos, expr) =>
        pushPos(pos) {
          eval(mw, expr)
          mw.visitInsn(op.POP)
        }
      case J.TReturn(pos, expr) =>
        pushPos(pos) {
          eval(mw, expr)
          expr.tpe match {
            case _: JType.TReference =>
              mw.visitInsn(op.ARETURN)
            case tpe =>
              throw new NotImplementedError(s"Not supported yet: $tpe")
          }
        }
    }
    def eval(method: asm.MethodVisitor, expr: J.Expr): Unit = expr match {
      case J.EPos(pos, expr) =>
        pushPos(pos) {
          eval(method, expr)
        }
      case J.Lit(v) =>
        method.visitLdcInsn(v.value)
      case J.GetField(ref, target) =>
        eval(method, target)
        method.visitFieldInsn(op.GETFIELD, ref.klass.internalName, escape(ref.name), descriptor(ref.tpe))
      case J.GetStatic(ref) =>
        method.visitFieldInsn(op.GETSTATIC, ref.klass.internalName, escape(ref.name), descriptor(ref.tpe))
      case J.If(cond, th, el, tpe) =>
        val lElse = new asm.Label()
        val lEnd = new asm.Label()
        eval(method, cond)
        method.visitJumpInsn(op.IFEQ, lElse)
        eval(method, th)
        method.visitJumpInsn(op.GOTO, lEnd)
        method.visitLabel(lElse)
        eval(method, el)
        method.visitLabel(lEnd)
      case J.JNew(ref, args) =>
        method.visitTypeInsn(op.NEW, ref.internalName)
        method.visitInsn(op.DUP)
        args.foreach { a =>
          eval(method, a)
        }
        method.visitMethodInsn(
          op.INVOKESPECIAL,
          ref.internalName,
          "<init>",
          asm.Type.getMethodType(asm.Type.VOID_TYPE, args.map(_.tpe).map(JType.toAsm).toArray: _*).getDescriptor,
          false)
      case J.Cast(body, tpe) =>
        eval(method, body)
        method.visitTypeInsn(op.CHECKCAST, tpe.jname)
      case J.Box(body) =>
        eval(method, body)
        box(method, body.tpe)
      case J.Unbox(body) =>
        eval(method, body)
        unbox(method, body.tpe)
      case J.Invoke(sig, special, receiver, args) =>
        receiver.foreach(eval(method, _))
        args.foreach(eval(method, _))
        val insn =
          if (special) op.INVOKESPECIAL
          else if (sig.isInterface) op.INVOKEINTERFACE
          else if (sig.isStatic) op.INVOKESTATIC
          else op.INVOKEVIRTUAL
        method.visitMethodInsn(
          insn,
          sig.klass.internalName,
          sig.name,
          sig.descriptor,
          sig.isInterface)
        if (sig.ret.isEmpty) {
          method.visitInsn(op.ACONST_NULL)
        }
      case J.GetLocal(index, tpe) =>
        Asm.autoload(method, index, tpe)
      case J.GetObjectFromUncheckedArray(arr, index) =>
        eval(method, arr)
        method.visitTypeInsn(op.CHECKCAST, JType.ObjectArray.jname)
        method.visitLdcInsn(index)
        method.visitInsn(op.AALOAD)
      case J.Null(tpe) =>
        method.visitInsn(op.ACONST_NULL)
      case J.NewObjectArray(size) =>
        method.visitLdcInsn(size)
        method.visitTypeInsn(op.ANEWARRAY, JType.TObject.jname)
      case J.PutValuesToArray(arr, values) =>
        eval(method, arr)
        values.zipWithIndex.foreach {
          case (v, i) =>
            method.visitInsn(op.DUP)
            method.visitLdcInsn(i)
            eval(method, v)
            method.visitInsn(op.AASTORE)
        }
      case J.Throw(e, t) =>
        eval(method, e)
        method.visitInsn(op.ATHROW)
      case J.InstanceOf(e, ref) =>
        eval(method, e)
        method.visitTypeInsn(op.INSTANCEOF, ref.internalName)
    }
  }

  private[this] def defineMethod(cw: ClassVisitor, klass: J.ClassDef, m: J.MethodDef) = {
    var flags = op.ACC_PUBLIC
    if (m.isStatic) flags |= op.ACC_STATIC

    val mw = cw.visitMethod(
      flags,
      m.name,
      klass.methodSig(m).descriptor,
      null,
      Array())

    val ctx = new MethodContext(klass.filePath, mw)
    m.body.foreach { term =>
      ctx.appTerm(term)
    }
    ctx.finish()

    if (m.ret.isEmpty)
      mw.visitInsn(op.RETURN)
    methodEnd(mw)
  }

  private[this] def box(method: asm.MethodVisitor, tpe: JType): Unit = {
    if (tpe.isPrimitive) {
      val desc = s"(${descriptor(tpe)})${descriptor(tpe.boxed)}"
      method.visitMethodInsn(op.INVOKESTATIC, tpe.boxed.jname, "valueOf", desc, false)
    }
  }
  private[this] def unbox(method: asm.MethodVisitor, tpe: JType): Unit = {
    tpe.unboxed.foreach { prim =>
      val name =
        prim match {
          case JType.TInt => "intValue"
          case JType.TBool => "booleanValue"
        }
      val desc = s"()${descriptor(prim)}"
      method.visitMethodInsn(op.INVOKEVIRTUAL, tpe.boxed.jname, name, desc, false)
    }
  }
}
