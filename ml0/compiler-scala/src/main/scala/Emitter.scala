package com.todesking.ojaml.ml0.compiler.scala

import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files

import org.objectweb.asm

import asm.{ Opcodes => op }

object Asm {
  import asm.{ MethodVisitor => MV }

  def autoload(m: MV, i: Int, t: Type): Unit = t match {
    case Type.Int =>
      m.visitVarInsn(op.ILOAD, i)
    // TODO: Other primitives and array
    case Type.Reference(_) =>
      m.visitVarInsn(op.ALOAD, i)
  }
  def autostore(m: MV, i: Int, t: Type): Unit = t match {
    case Type.Int =>
      m.visitVarInsn(op.ISTORE, i)
    // TODO: Other primitives and array
    case Type.Reference(_) =>
      m.visitVarInsn(op.ASTORE, i)
  }
}

class Emitter(baseDir: Path) {
  import com.todesking.ojaml.ml0.compiler.scala.{ JAST => J }
  import com.todesking.ojaml.ml0.compiler.scala.{ Asm => A }

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

  def emit(klass: J.ClassDef): Unit = {
    val pkg = klass.ref.pkg
    val className = klass.ref.internalName
    val cw = new asm.ClassWriter(asm.ClassWriter.COMPUTE_FRAMES)
    cw.visit(
      op.V1_8,
      op.ACC_PUBLIC,
      className,
      null,
      objectClass,
      Array())

    var funID = 0
    def emitFun(body: J.Expr, depth: Int, recValues: Option[Seq[J.Expr]]): String = {
      val cname = s"${klass.ref.name}$$$funID"
      val qcname = s"${pkg.internalName}/$cname"
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
      write(pkg.fullName, cname, cw.toByteArray)
      qcname
    }

    def eval(method: asm.MethodVisitor, expr: J.Expr, depth: Int): Unit = expr match {
      case J.LitInt(v) =>
        method.visitLdcInsn(v)
      case J.LitBool(v) =>
        method.visitLdcInsn(v)
      case J.LitString(v) =>
        method.visitLdcInsn(v)
      case J.ModuleVarRef(module, name, tpe) =>
        method.visitFieldInsn(op.GETSTATIC, msig(module), escape(name), descriptor(tpe))
      case J.LocalRef(d, index, tpe) =>
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
      case J.LetRec(values, body) =>
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
      case J.If(cond, th, el, tpe) =>
        val lElse = new asm.Label()
        val lEnd = new asm.Label()
        eval(method, cond, depth)
        method.visitJumpInsn(op.IFEQ, lElse)
        eval(method, th, depth)
        method.visitJumpInsn(op.GOTO, lEnd)
        method.visitLabel(lElse)
        eval(method, el, depth)
        method.visitLabel(lEnd)
      case J.Fun(body, tpe) =>
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
        method.visitTypeInsn(op.CHECKCAST, funClass)
      case J.TAbs(ps, e, t) =>
        eval(method, e, depth)
      case e @ J.JCallInstance(target, receiver, args) =>
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
      case J.JNew(ref, args) =>
        method.visitTypeInsn(op.NEW, ref.internalName)
        method.visitInsn(op.DUP)
        args.foreach { a =>
          eval(method, a, depth)
        }
        method.visitMethodInsn(
          op.INVOKESPECIAL,
          ref.internalName,
          "<init>",
          asm.Type.getMethodType(asm.Type.VOID_TYPE, args.map(_.tpe).map(Type.toAsm).toArray: _*).getDescriptor,
          false)
      case J.Upcast(body, tpe) =>
        eval(method, body, depth)
        method.visitTypeInsn(op.CHECKCAST, tpe.ref.internalName)
      case J.Box(body) =>
        eval(method, body, depth)
        box(method, body.tpe)
      case J.Unbox(body) =>
        eval(method, body, depth)
        unbox(method, body.tpe)
      case J.Downcast(body, tpe) =>
        eval(method, body, depth)
        method.visitTypeInsn(op.CHECKCAST, tpe.ref.internalName)
      case J.Invoke(sig, receiver, args) =>
        receiver.foreach(eval(method, _, depth))
        args.foreach(eval(method, _, depth))
        val insn =
          if (sig.isInterface) op.INVOKEINTERFACE
          else if (sig.isStatic) op.INVOKESTATIC
          else op.INVOKEVIRTUAL
        method.visitMethodInsn(
          insn,
          sig.klass.internalName,
          sig.name,
          sig.descriptor,
          sig.isInterface)
    }

    def defineStaticField(cw: asm.ClassWriter, name: String, tpe: Type): Unit = {
      cw.visitField(
        op.ACC_PUBLIC | op.ACC_STATIC,
        escape(name),
        descriptor(tpe),
        null,
        null)
    }
    def defineField(cw: asm.ClassWriter, name: String, tpe: Type): Unit = {
      cw.visitField(
        op.ACC_PUBLIC,
        escape(name),
        descriptor(tpe),
        null,
        null)
    }

    def methodDescriptor(ret: Type, params: Seq[Type]): String =
      asm.Type.getMethodType(Type.toAsm(ret), params.map(Type.toAsm).toArray: _*).getDescriptor

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
              defineField(cw, s"v$i", t)
          }

          val init = cw.visitMethod(
            op.ACC_PUBLIC,
            "<init>",
            asm.Type.getMethodType(asm.Type.VOID_TYPE, params.map(Type.toAsm): _*).getDescriptor,
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
              A.autoload(init, i + 1, t)
              init.visitFieldInsn(op.PUTFIELD, subClass.internalName, s"v$i", descriptor(t))
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
              tosImpl.visitFieldInsn(op.GETFIELD, subClass.internalName, s"v$i", descriptor(t))
              autobox(tosImpl, t, t.boxed)
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
        ctors.foreach {
          case (name, params) =>
          // defineStaticField(cw, name.value, params.foldRight(tpe: Type) { (from, to) => Type.Fun(from, to) })
        }
    }
    klass.fields.foreach { f =>
      defineStaticField(cw, f.name, f.tpe)
    }

    klass.methods.foreach { m =>
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
          eval(mw, expr, 0)
          mw.visitFieldInsn(op.PUTSTATIC, ref.klass.internalName, escape(ref.name), descriptor(ref.tpe))
      }
      m.ret match {
        case None =>
          mw.visitInsn(op.RETURN)
        case Some(t) =>
          ???
      }
      methodEnd(mw)
    }

    cw.visitEnd()

    write(pkg.fullName, klass.ref.name, cw.toByteArray)
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
