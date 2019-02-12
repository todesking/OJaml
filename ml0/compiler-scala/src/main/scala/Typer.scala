package com.todesking.ojaml.ml0.compiler.scala

class Typer(classRepo: ClassRepo, moduleVars: Map[VarRef.ModuleMember, Type]) {
  import com.todesking.ojaml.ml0.compiler.scala.{ NamedAST => NT, TypedAST => TT }
  import Typer.Result
  import Compiler.Error
  import Typer.Ctx
  import Typer.QuasiValue
  import Typer.{ QuasiValue => Q }
  import Typer.error

  def appModule(s: NT.Module): Result[TT.Module] = {
    val init = Typer.Ctx(s.moduleRef, classRepo).bindModuleValues(moduleVars)
    val (_, typed) =
      s.body.foldLeft((init, Seq.empty[Result[TT.Term]])) {
        case ((c, a), t) =>
          appTerm(c, t).fold({ l =>
            (c, a :+ Left(l))
          }, {
            case (cc, tt) =>
              (cc, a :+ Right(tt))
          })
      }
    validate(typed).map(TT.Module(s.pkg, s.name, _))
  }

  private[this] def validate[A](xs: Seq[Result[A]]): Result[Seq[A]] = {
    val rights = xs.collect { case Right(x) => x }
    if (rights.size == xs.size) Right(rights)
    else Left(xs.collect { case Left(x) => x }.flatten)
  }

  def appTerm(ctx: Ctx, t: NT.Term): Result[(Ctx, TT.Term)] = t match {
    case NT.TLet(name, expr) =>
      for {
        e <- appExpr(ctx, expr)
        c <- ctx.bindModuleValue(name, e.tpe)
      } yield {
        (c, TT.TLet(name, e.tpe, e))
      }
    case e: NT.Expr =>
      appExpr(ctx, e).map { te => (ctx, te) }
  }

  def appExpr(ctx: Ctx, expr: NT.Expr): Result[TT.Expr] = appExprQ(ctx, expr).flatMap {
    case Q.ClassValue(sig) =>
      error(expr.pos, s"Class ${sig.ref.fullName} is not a value")
    case Q.PackageValue(ref) =>
      error(expr.pos, s"Package ${ref.fullName} is not a value")
    case Q.Value(value) =>
      Right(value)
  }

  def okQ(expr: TT.Expr) = Right(Q.Value(expr))

  def varRefToQ(ctx: Ctx, v: VarRef, pos: Pos): Result[QuasiValue] = v match {
    case VarRef.TopLevel(pm) => pm match {
      case PackageMember.Class(ref) =>
        ctx.findClass(ref).map(Q.ClassValue(_)).toRight(
          Seq(Error(pos, s"Class not found: ${ref.fullName}")))
      case PackageMember.Module(ref) =>
        ???
      case PackageMember.Package(ref) =>
        Right(Q.PackageValue(ref))
    }
    case ref @ VarRef.ModuleMember(m, name) =>
      okQ(TT.ModuleVarRef(m, name, ctx.typeOf(ref)))
    case ref @ VarRef.Local(index) => okQ(TT.LocalRef(index, ctx.typeOf(ref)))
  }

  def appExprQ(ctx: Ctx, expr: NT.Expr): Result[QuasiValue] = expr match {
    case NT.Ref(ref) =>
      varRefToQ(ctx, ref, expr.pos)
    case NT.LitInt(v) => okQ(TT.LitInt(v))
    case NT.LitBool(v) => okQ(TT.LitBool(v))
    case NT.LitString(v) => okQ(TT.LitString(v))
    case NT.If(cond, th, el) =>
      for {
        e0 <- appExpr(ctx, cond)
        e1 <- appExpr(ctx, th)
        e2 <- appExpr(ctx, el)
        ret <- if (e0.tpe != Type.Bool) {
          error(cond.pos, s"Condition must be bool")
        } else if (e1.tpe != e2.tpe) {
          error(expr.pos, s"Then clause has thpe ${e1.tpe} but else clause has type ${e2.tpe}")
        } else {
          okQ(TT.If(e0, e1, e2, e1.tpe))
        }
      } yield ret
    case NT.Fun(param, tpe, body) =>
      appExpr(ctx.bindLocal(param, tpe), body)
        .map(TT.Fun(tpe, _))
        .map(Q.Value.apply)
    case NT.ELet(ref, value, body) =>
      appExpr(ctx, value).flatMap { v =>
        val fun = NT.Fun(ref, v.tpe, body)
        fun.fillPos(expr.pos)
        appExpr(ctx, fun).map {
          case f @ TT.Fun(argType, b) =>
            assert(argType == v.tpe)
            Q.Value(TT.App(f, v, f.tpe.r))
          case unk =>
            throw new AssertionError(s"$unk")
        }
      }
    case NT.App(f, x) =>
      for {
        tf <- appExpr(ctx, f)
        tx <- appExpr(ctx, x)
        ret <- tf.tpe match {
          case Type.Fun(l, r) =>
            if (l == tx.tpe)
              okQ(TT.App(tf, tx, r))
            else
              error(x.pos, s"Argument type mismatch: $l required but ${tx.tpe} found")
          case t =>
            error(f.pos, s"Applying non-function type ${t}")
        }
      } yield ret
    case NT.JCall(expr, name, args, isStatic) =>
      validate(args.map(appExpr(ctx, _))).flatMap { targs =>
        if (isStatic) {
          appExprQ(ctx, expr).flatMap {
            case QuasiValue.ClassValue(klass) =>
              klass.findStaticMethod(name.value, targs.map(_.tpe)).fold[Result[QuasiValue]] {
                error(name.pos, s"Static method ${Type.prettyMethod(name.value, targs.map(_.tpe))} is not found in class ${klass.ref.fullName}")
              } { method =>
                okQ(TT.JCallStatic(method, targs))
              }
            case _ =>
              error(expr.pos, s"Class required")
          }
        } else {
          appExpr(ctx, expr).flatMap { receiver =>
            ctx.findClass(receiver.tpe.boxed.ref).fold[Result[QuasiValue]] {
              error(expr.pos, s"Class ${receiver.tpe.boxed.ref.fullName} not found. Check classpath.")
            } { klass =>
              klass.findInstanceMethod(name.value, targs.map(_.tpe)).fold[Result[QuasiValue]] {
                error(name.pos, s"Instance method ${Type.prettyMethod(name.value, targs.map(_.tpe))} is not found in class ${receiver.tpe.boxed.ref.fullName}")
              } { method =>
                okQ(TT.JCallInstance(method, receiver, targs))
              }
            }
          }
        }
      }
  }
}
object Typer {
  import Compiler.Error

  def error(pos: Pos, msg: String) = Left(Seq(Error(pos, msg)))

  def moduleVarsOf(s: TypedAST.Module): Map[VarRef.ModuleMember, Type] = s.body.collect {
    case TypedAST.TLet(name, tpe, _) =>
      VarRef.ModuleMember(s.moduleRef, name.value) -> tpe
  }.toMap

  sealed abstract class QuasiValue
  object QuasiValue {
    case class ClassValue(sig: ClassSig) extends QuasiValue
    case class PackageValue(ref: PackageRef) extends QuasiValue
    case class Value(expr: TypedAST.Expr) extends QuasiValue
  }

  type Result[A] = Either[Seq[Compiler.Error], A]

  case class Ctx(
    currentModule: ModuleRef,
    repo: ClassRepo,
    typeTable: Map[VarRef.Typable, Type] = Map()) {

    def typeOf(ref: VarRef.Typable): Type =
      typeTable(ref) // If lookup failed, that means a bug.

    def findClass(ref: ClassRef): Option[ClassSig] = repo.find(ref)

    def bindModuleValue(name: Name, tpe: Type): Result[Ctx] = {
      val ref = VarRef.ModuleMember(currentModule, name.value)
      if (typeTable.contains(ref)) Left(Seq(Error(name.pos, s"Member ${name} is already defined")))
      else Right(copy(typeTable = typeTable + (ref -> tpe)))
    }

    def bindModuleValues(mvs: Map[VarRef.ModuleMember, Type]): Ctx =
      copy(typeTable = typeTable ++ mvs)

    def bindLocal(ref: VarRef.Local, tpe: Type): Ctx =
      copy(typeTable = typeTable + (ref -> tpe))
  }

}
