package c4.ir

import c4.util.{Located => L}
import cats._

import scala.language.higherKinds

//
// higher-kinds is used here for eliminating loc in later phases.
//
// C is short for "Container".
//
////////// about "Option[C[Unit]]" //////////
//
// Option[C[Unit]] is basically a loc-erasable version of Option[Loc];
// before loc-erasure, it is Option[Located[Unit]]:
//    case Some(L(loc, _)) => "true", and loc
//    case None => "false", no loc
// similarly, after loc-erasure, it becomes Option[Id[Unit]]:
//    case Some(_) => "true"
//    case None => "false"
//
////////// about "Type[Nothing]" //////////
//
// if:
//    FeatureRichLocated <: Located
// then it is natual to have:
//    Type[FeatureRichLocated] <: Type[Located]
// so Type must be variant, i.e. Type[+C[_]].
// given that, since we want:
//    T_void <: Type[Id]
//    T_void <: Type[Located]
// T_void must be defined as Type[Nothing], not Type[Any].
//

///// Type /////

sealed abstract class Type[+C[_]]
case object T_void extends Type[Nothing]
case object T_i8 extends Type[Nothing]
case object T_u8 extends Type[Nothing]
case object T_i16 extends Type[Nothing]
case object T_u16 extends Type[Nothing]
case object T_i32 extends Type[Nothing]
case object T_u32 extends Type[Nothing]
case object T_i64 extends Type[Nothing]
case object T_u64 extends Type[Nothing]
case object T_float extends Type[Nothing]
case object T_double extends Type[Nothing]
case object T_fp80 extends Type[Nothing] // x86's "long double"

final case class T_struct(id: Int) extends Type[Nothing]
final case class T_union(id: Int) extends Type[Nothing]
final case class T_enum(id: Int) extends Type[Nothing]

// the Option[C[Unit]] in argTps: isRegister?
//
// for "void f(void)", args=Seq()
// for "void f()", T_func_unspecified is used
final case class T_func[C[_]](
    retTp: C[QType[C]],
    args: Seq[(Option[C[Unit]], C[QType[C]], Option[C[String]])],
    varargs: Option[C[Unit]]) extends Type[C]

final case class T_func_unspecified[C[_]](
    retTp: C[QType[C]]) extends Type[C]

final case class T_ptr[C[_]](tp: C[QType[C]]) extends Type[C]

///// Qualified Type /////

final case class QType[C[_]](qs: TypeQualifiers[C], tp: C[Type[C]]) {
}

final case class TypeQualifiers[+C[_]](
  isConst: Option[C[Unit]],
  isVolatile: Option[C[Unit]]) {

  override def toString: String = {
    val x = Seq(
      isConst.map(_ => "const").getOrElse(""),
      isVolatile.map(_ => "volatile").getOrElse("")
    ).filter(_ != "").mkString(" ")
    s"<$x>"
  }
}

object TypeQualifiers {
  val None = TypeQualifiers(scala.None, scala.None)
}

///// Sue Defn /////

final case class SuField[C[_]](
  name: Option[C[String]],
  tp: C[QType[C]],
  bitFieldSize: Option[C[Int]])

final case class StructDefn[C[_]](
    name: Option[C[String]],
    body: Option[Seq[C[SuField[C]]]]) {
  // size: Int
  // offset: [Int]
  // alignment: Int
}

///// func defn /////

// entry maps to a basic block id.
object Linkage extends Enumeration {
  val Internal, External = Value
}

// for func, if 'static' exists => internal, otherwise (fallback) => external
final case class FuncDefn[C[_]](
  linkage: C[Linkage.Value],
  tp: C[T_func[C]],
  entry: Option[Int])

///// "IR" /////

object QType {
  def deLoc(qs: TypeQualifiers[L]): TypeQualifiers[Id] =
    TypeQualifiers[Id](
      qs.isConst.map(_.deLoc),
      qs.isVolatile.map(_.deLoc)
    )
  def deLoc(tp: L[Type[L]]): Type[Id] =
    tp.value match {
      case T_void => T_void
      case T_i8 => T_i8
      case T_u8 => T_u8
      case T_i16 => T_i16
      case T_u16 => T_u16
      case T_i32 => T_i32
      case T_u32 => T_u32
      case T_i64 => T_i64
      case T_u64 => T_u64
      case T_float => T_float
      case T_double => T_double
      case T_fp80 => T_fp80
      case T_struct(x) => T_struct(x)
      case T_union(x) => T_union(x)
      case T_enum(x) => T_enum(x)
      case T_func(retTp, args, varargs) =>
        val newArgs = args.map { p =>
          (p._1.map(_.deLoc), deLoc(p._2.value), p._3.map(_.deLoc))
        }
        T_func[Id](deLoc(retTp.value), newArgs, varargs.map(_.deLoc))
      case T_func_unspecified(retTp) =>
        T_func_unspecified[Id](deLoc(retTp.value))
      case T_ptr(tp) => T_ptr[Id](deLoc(tp.value))
    }
  def deLoc(qt: QType[L]): QType[Id] =
    QType[Id](deLoc(qt.qs), deLoc(qt.tp))
}

sealed abstract class Instruction(hasRet: Boolean)
sealed abstract class InstructionWithRet(tp: Type[Id]) extends Instruction(true)
sealed abstract class InstructionNoRet extends Instruction(false)

final case class InstAlloca(tp: QType[Id])
  extends InstructionWithRet(T_ptr[Id](tp))
final case class InstStoreArg(argName: String, argTp: QType[Id], dest: Int)
  extends InstructionNoRet

final class BasicBlock(val id: Int) {
  type Inst = Either[(InstructionWithRet, Int), InstructionNoRet]
  private var insts = Seq.empty[Inst]

  def addInst(inst: Inst): this.type = { insts = insts :+ inst; this }
  def getInsts = insts

  // TODO: br (jmp, if, switch)

  override def toString = s"BasicBlock($id)"
}
