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
    // TODO: Other primitives and array
    case _: JType.TReference =>
      m.visitVarInsn(op.ALOAD, i)
  }
  def autostore(m: MV, i: Int, t: JType): Unit = t match {
    case JType.TInt =>
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

  private[this] val objectClass = "java/lang/Object"
  private[this] val objectSig = s"L$objectClass;"

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

    def defineStaticField(cw: asm.ClassWriter, name: String, tpe: JType): Unit = {
      cw.visitField(
        op.ACC_PUBLIC | op.ACC_STATIC,
        escape(name),
        descriptor(tpe),
        null,
        null)
    }
    def defineField(cw: asm.ClassWriter, name: String, tpe: JType): Unit = {
      cw.visitField(
        op.ACC_PUBLIC,
        escape(name),
        descriptor(tpe),
        null,
        null)
    }

    def methodDescriptor(ret: JType, params: Seq[JType]): String =
      asm.Type.getMethodType(JType.toAsm(ret), params.map(JType.toAsm).toArray: _*).getDescriptor

    def emitDataType(cw: asm.ClassWriter, tpe: Type.Data, ctors: Seq[(Name, Seq[Type])]): Unit = {
      val dataBaseClass = ClassRef.fromInternalName("com/todesking/ojaml/ml0/runtime/Data")
      val cw = new asm.ClassWriter(asm.ClassWriter.COMPUTE_FRAMES)
      cw.visit(
        op.V1_8,
        op.ACC_PUBLIC | op.ACC_ABSTRACT,
        tpe.ref.internalName,
        null,
        dataBaseClass.internalName,
        Array())
      val init = cw.visitMethod(
        op.ACC_PUBLIC,
        "<init>",
        "(I)V",
        null,
        Array())
      init.visitVarInsn(op.ALOAD, 0)
      init.visitVarInsn(op.ILOAD, 1)
      init.visitMethodInsn(
        op.INVOKESPECIAL,
        dataBaseClass.internalName,
        "<init>",
        "(I)V",
        false)
      init.visitInsn(op.RETURN)
      methodEnd(init)
      cw.visitEnd()
      write(tpe.ref.pkg.internalName, tpe.ref.name, cw.toByteArray)

      ctors.foreach {
        case (name, params) =>
          val subClass = ClassRef(tpe.ref.pkg, s"${tpe.ref.name}$$${escape(name.value)}")
          val cw = new asm.ClassWriter(asm.ClassWriter.COMPUTE_FRAMES)
          cw.visit(
            op.V1_8,
            op.ACC_PUBLIC,
            subClass.internalName,
            null,
            tpe.ref.internalName,
            Array())
          params.zipWithIndex.foreach {
            case (t, i) =>
              defineField(cw, s"v$i", t.jtype)
          }

          val init = cw.visitMethod(
            op.ACC_PUBLIC,
            "<init>",
            asm.Type.getMethodType(asm.Type.VOID_TYPE, params.map(_.jtype).map(JType.toAsm): _*).getDescriptor,
            null,
            Array())
          init.visitVarInsn(op.ALOAD, 0)
          init.visitLdcInsn(params.size)
          init.visitMethodInsn(
            op.INVOKESPECIAL,
            tpe.ref.internalName,
            "<init>",
            "(I)V",
            false)
          params.zipWithIndex.foreach {
            case (t, i) =>
              init.visitVarInsn(op.ALOAD, 0)
              A.autoload(init, i + 1, t.jtype)
              init.visitFieldInsn(op.PUTFIELD, subClass.internalName, s"v$i", descriptor(t.jtype))
          }
          init.visitInsn(op.RETURN)
          methodEnd(init)

          val tosImpl = cw.visitMethod(
            op.ACC_PUBLIC,
            "toString",
            "()Ljava/lang/String;",
            null,
            Array())
          tosImpl.visitLdcInsn(name.value)
          params.zipWithIndex.foreach {
            case (t, i) =>
              tosImpl.visitLdcInsn(" ")
              tosImpl.visitMethodInsn(
                op.INVOKEVIRTUAL,
                "java/lang/String",
                "concat",
                "(Ljava/lang/String;)Ljava/lang/String;",
                false)
              tosImpl.visitVarInsn(op.ALOAD, 0)
              tosImpl.visitFieldInsn(op.GETFIELD, subClass.internalName, s"v$i", descriptor(t.jtype))
              autobox(tosImpl, t.jtype, t.jtype.boxed)
              tosImpl.visitLdcInsn(true)
              tosImpl.visitMethodInsn(
                op.INVOKESTATIC,
                dataBaseClass.internalName,
                "format",
                s"(L${objectClass};Z)Ljava/lang/String;",
                false)
              tosImpl.visitMethodInsn(
                op.INVOKEVIRTUAL,
                "java/lang/String",
                "concat",
                "(Ljava/lang/String;)Ljava/lang/String;",
                false)
          }
          tosImpl.visitInsn(op.ARETURN)
          methodEnd(tosImpl)

          cw.visitEnd()
          write(subClass.pkg.internalName, subClass.name, cw.toByteArray)
      }
    }

    klass.datas.foreach {
      case J.Data(name, tpe, ctors) =>
        emitDataType(cw, tpe, ctors)
    }
    klass.fields.foreach { f =>
      defineStaticField(cw, f.name, f.tpe)
    }

    klass.methods.foreach { m => defineMethod(cw, klass, m) }

    cw.visitEnd()

    write(pkg.fullName, klass.ref.name, cw.toByteArray)
  }

  private[this] def eval(method: asm.MethodVisitor, expr: J.Expr): Unit = expr match {
    case J.LitInt(v) =>
      method.visitLdcInsn(v)
    case J.LitBool(v) =>
      method.visitLdcInsn(v)
    case J.LitString(v) =>
      method.visitLdcInsn(v)
    case J.ModuleVarRef(module, name, tpe) =>
      method.visitFieldInsn(op.GETSTATIC, msig(module), escape(name), descriptor(tpe))
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
    case J.Upcast(body, tpe) =>
      eval(method, body)
      method.visitTypeInsn(op.CHECKCAST, tpe.jname)
    case J.Box(body) =>
      eval(method, body)
      box(method, body.tpe)
    case J.Unbox(body) =>
      eval(method, body)
      unbox(method, body.tpe)
    case J.Downcast(body, tpe) =>
      eval(method, body)
      method.visitTypeInsn(op.CHECKCAST, tpe.jname)
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
      method.visitVarInsn(op.ALOAD, index)
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
    m.body.foreach {
      case J.PutStatic(ref, expr) =>
        eval(mw, expr)
        mw.visitFieldInsn(op.PUTSTATIC, ref.klass.internalName, escape(ref.name), descriptor(ref.tpe))
      case J.TExpr(expr) =>
        eval(mw, expr)
        mw.visitInsn(op.POP)
      case J.TReturn(expr) =>
        eval(mw, expr)
        expr.tpe match {
          case _: JType.TReference =>
            mw.visitInsn(op.ARETURN)
          case tpe =>
            throw new NotImplementedError(s"Not supported yet: $tpe")
        }
      case J.PutValuesToUncheckedObjectArray(arr, values) =>
        eval(mw, arr)
        mw.visitTypeInsn(op.CHECKCAST, JType.ObjectArray.jname)
        values.zipWithIndex.foreach {
          case (v, i) =>
            mw.visitInsn(op.DUP)
            mw.visitLdcInsn(i)
            eval(mw, v)
            mw.visitInsn(op.AASTORE)
        }
    }
    if (m.ret.isEmpty)
      mw.visitInsn(op.RETURN)
    methodEnd(mw)
  }

  private[this] def autobox(method: asm.MethodVisitor, from: Option[JType], to: JType): Unit = from match {
    case Some(f) => autobox(method, f, to)
    case None =>
      if (to != JType.TUnit) throw new AssertionError(s"Unit type expected but actual is $to")
      method.visitInsn(op.ACONST_NULL)
      method.visitTypeInsn(op.CHECKCAST, JType.TUnit.jname)
  }
  private[this] def autobox(method: asm.MethodVisitor, from: JType, to: JType): Unit = {
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
