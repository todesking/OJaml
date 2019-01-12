package com.todesking.ojaml.ml0.compiler.scala

import java.nio.file.Path

import org.objectweb.asm

case class MethodSig(klass: String, isStatic: Boolean, isInterface: Boolean, name: String, args: Seq[Type], ret: Option[Type]) {
  require(!(isStatic && isInterface))

  def isInstance = !isStatic

  lazy val descriptor =
    asm.Type.getMethodType(Type.toAsm(ret), args.map(Type.toAsm).toArray: _*).getDescriptor
}
case class ClassSig(name: String, parent: Option[ClassSig], interfaces: Seq[ClassSig], methods: Seq[MethodSig]) {
  def findAnyMethod(name: String, args: Seq[Type]): Option[MethodSig] =
    methods.find(m => m.name == name && assignableFrom(m.args, args))
  def findStaticMethod(name: String, args: Seq[Type]): Option[MethodSig] =
    findAnyMethod(name, args).find(_.isStatic)
  def findInstanceMethod(name: String, args: Seq[Type]): Option[MethodSig] =
    findAnyMethod(name, args).find(_.isInstance)

  private[this] def assignableFrom(xs: Seq[Type], ys: Seq[Type]): Boolean =
    xs.size == ys.size && xs.zip(ys).forall { case (x, y) => assignableFrom(x, y) }
  private[this] def assignableFrom(x: Type, y: Type): Boolean =
    x.boxed == y.boxed
}

class ClassRepo(cl: ClassLoader) {
  private[this] var _cache = Map.empty[String, ClassSig]

  // I don't know why, but getResource("java") returns null even when getResource("java/lang/Integer.class") returns non-null.
  // So package existance is not knowable.
  // def packageExists(name: String): Boolean = cl.getResource(name) != null

  def classExists(name: String): Boolean = cl.getResource(s"$name.class") != null

  def find(name: String): Option[ClassSig] = {
    val r = cl.getResource(s"$name.class")
    if (r == null) None
    else Some(load(name))
  }

  private[this] def load(name: String): ClassSig = _cache.get(name).getOrElse {
    val c = load0(name)
    this._cache = _cache + (name -> c)
    c
  }

  private[this] def load0(name: String) = {
    val f = parse(name)
    ClassSig(f.name, f.superName.map(load), f.interfaces.map(load), f.methods)
  }

  private[this] def parse(name: String): ClassRepo.ClassFile = {
    val s = cl.getResourceAsStream(s"$name.class")
    val cr = new asm.ClassReader(s)
    val p = new ClassRepo.ClassParser
    cr.accept(p, asm.ClassReader.SKIP_CODE)
    p.result()
  }
}

object ClassRepo {
  case class ClassFile(name: String, superName: Option[String], interfaces: Seq[String], methods: Seq[MethodSig])
  class ClassParser extends asm.ClassVisitor(asm.Opcodes.ASM7) {
    private[this] var name: String = null
    private[this] var superName: String = null
    private[this] var interfaces: Seq[String] = null
    private[this] var methods = Seq.empty[MethodSig]
    private[this] var isInterface: Boolean = _

    override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]) = {
      this.name = name
      this.superName = superName
      this.interfaces = interfaces.toSeq
      this.isInterface = (access & asm.Opcodes.ACC_INTERFACE) != 0
      super.visit(version, access, name, signature, superName, interfaces)
    }

    override def visitMethod(access: Int, mname: String, descriptor: String, signature: String, exceptions: Array[String]) = {
      val isStatic = (access & asm.Opcodes.ACC_STATIC) != 0
      val t = asm.Type.getType(descriptor)
      val m = MethodSig(name, isStatic, isInterface, mname, t.getArgumentTypes.map(translate(_, mname, signature)), Type.from(t.getReturnType))
      this.methods = this.methods :+ m
      super.visitMethod(access, mname, descriptor, signature, exceptions)
    }

    private[this] def translate(t: asm.Type, mname: String, signature: String): Type = Type.from(t) getOrElse {
      throw new RuntimeException(s"Void argument type not allowed: $name$signature")
    }

    def result() = ClassFile(name, Option(superName), interfaces, methods)
  }
}
