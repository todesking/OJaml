package com.todesking.ojaml.ml0.compiler.scala

import java.nio.file.Path

import org.objectweb.asm
import org.objectweb.asm.MethodVisitor

class ClassRepo(cl: ClassLoader) {
  private[this] var _cache = Map.empty[ClassRef, ClassSig]

  // Note: Some jar contains only file, not directory.
  // i.e. contains "java/lang/Integer.class" but no "java/"
  // So this hack required.
  val knownPackages: Set[String] = Set(
    "java",
    "java/lang",
    "java/util")
  def packageExists(pkg: PackageRef, name: String): Boolean = {
    val path = pkg.packageRef(name).internalName
    cl.getResource(path) != null || knownPackages.contains(path)
  }

  def classExists(pkg: PackageRef, name: String): Boolean = classExists(pkg.classRef(name))
  def classExists(ref: ClassRef): Boolean = cl.getResource(ref.resourceName) != null

  def find(pkg: PackageRef, name: String): Option[ClassSig] =
    find(ClassRef(pkg, name))

  def find(ref: ClassRef): Option[ClassSig] = {
    val r = cl.getResource(ref.resourceName)
    if (r == null) None
    else Some(load(ref))
  }

  private[this] def load(ref: ClassRef): ClassSig = _cache.getOrElse(ref, {
    val c = load0(ref)
    this._cache = _cache + (ref -> c)
    c
  })

  private[this] def load0(ref: ClassRef) = {
    val f = parse(ref.resourceName)
    assert(f.ref == ref, s"${f.ref} == $ref")
    ClassSig(ref, f.superRef.map(load), f.interfaces.map(load), f.methods)
  }

  private[this] def parse(resourceName: String): ClassRepo.ClassFile = {
    val s = cl.getResourceAsStream(resourceName)
    val cr = new asm.ClassReader(s)
    val p = new ClassRepo.ClassParser
    cr.accept(p, asm.ClassReader.SKIP_CODE)
    p.result()
  }
}

object ClassRepo {
  case class ClassFile(ref: ClassRef, superRef: Option[ClassRef], interfaces: Seq[ClassRef], methods: Seq[MethodSig])
  class ClassParser extends asm.ClassVisitor(asm.Opcodes.ASM7) {
    private[this] var ref: ClassRef = null
    private[this] var superRef: ClassRef = null
    private[this] var interfaces: Seq[ClassRef] = null
    private[this] var methods = Seq.empty[MethodSig]
    private[this] var isInterface: Boolean = _

    override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit = {
      this.ref = ClassRef.fromInternalName(name)
      this.superRef = if (superName == null) null else ClassRef.fromInternalName(superName)
      this.interfaces = interfaces.toSeq.map(ClassRef.fromInternalName)
      this.isInterface = (access & asm.Opcodes.ACC_INTERFACE) != 0
      super.visit(version, access, name, signature, superName, interfaces)
    }

    override def visitMethod(access: Int, mname: String, descriptor: String, signature: String, exceptions: Array[String]): MethodVisitor = {
      val isStatic = (access & asm.Opcodes.ACC_STATIC) != 0
      val t = asm.Type.getType(descriptor)
      val m = MethodSig(ref, isStatic, isInterface, mname, t.getArgumentTypes.map(translate(_, mname, signature)), Type.from(t.getReturnType))
      this.methods = this.methods :+ m
      super.visitMethod(access, mname, descriptor, signature, exceptions)
    }

    private[this] def translate(t: asm.Type, mname: String, signature: String): Type = Type.from(t) getOrElse {
      throw new RuntimeException(s"Void argument type not allowed: ${ref.fullName}$signature")
    }

    def result() = ClassFile(ref, Option(superRef), interfaces, methods)
  }
}
