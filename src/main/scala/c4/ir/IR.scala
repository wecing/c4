package c4.ir

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
// so Type must be variant, i.e. Type[+C[X]].
// given that, since we want:
//    T_void <: Type[Id]
//    T_void <: Type[Located]
// T_void must be defined as Type[Nothing], not Type[Any].
//

///// Type /////

sealed abstract class Type[+C[X]]
object T_void extends Type[Nothing]
object T_i8 extends Type[Nothing]
object T_u8 extends Type[Nothing]
object T_i16 extends Type[Nothing]
object T_u16 extends Type[Nothing]
object T_i32 extends Type[Nothing]
object T_u32 extends Type[Nothing]
object T_i64 extends Type[Nothing]
object T_u64 extends Type[Nothing]
object T_float extends Type[Nothing]
object T_double extends Type[Nothing]
object T_fp80 extends Type[Nothing] // x86's "long double"

final case class T_struct(id: Int) extends Type[Nothing]
final case class T_union(id: Int) extends Type[Nothing]
final case class T_enum(id: Int) extends Type[Nothing]

final case class T_func[C[X]](
    retTp: C[QType[C]],
    argTps: Seq[C[QType[C]]],
    varargs: Option[C[Unit]]) extends Type[C]

final case class T_ptr[C[X]](tp: C[QType[C]]) extends Type[C]

///// Qualified Type /////

final case class QType[C[X]](qs: TypeQualifiers[C], tp: C[Type[C]])

final case class TypeQualifiers[C[X]](
  isConst: Option[C[Unit]],
  isVolatile: Option[C[Unit]])

final case class SuField[C[X]](name: Option[C[String]], tp: C[QType[C]], bitFieldSize: C[Int])

final case class StructDefn[C[X]](
    name: Option[C[String]],
    body: Option[Seq[C[SuField[C]]]]) {
  // size: Int
  // offset: [Int]
  // alignment: Int
  // defnloc: C[Unit]
}
