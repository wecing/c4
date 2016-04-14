package c4.ast

import c4.io._
import c4.util.{Loc, LocRange, Located => L}

sealed abstract class ExprUnaryOp
object PrefixInc extends ExprUnaryOp // ++n
object PrefixDec extends ExprUnaryOp // --n
object Ref extends ExprUnaryOp // &n
object Deref extends ExprUnaryOp // *n
object Pos extends ExprUnaryOp // +n
object Neg extends ExprUnaryOp // -n
object BitNot extends ExprUnaryOp // ~n
object LogicNot extends ExprUnaryOp // !n
object SizeofVal extends ExprUnaryOp // sizeof(1)

sealed abstract class Expr
final case class Id(id: String) extends Expr
final case class IntegerLit(tok: TokInteger) extends Expr
final case class FloatLit(tok: TokFloat) extends Expr
final case class DoubleLit(tok: TokDouble) extends Expr
final case class LongDoubleLit(tok: TokLongDouble) extends Expr
final case class CharLit(tok: TokChar) extends Expr
final case class WideCharLit(tok: TokWideChar) extends Expr
final case class StrLit(tok: TokStr) extends Expr
final case class WideStrLit(tok: TokWideStr) extends Expr

// final case class SizeofType(...) extends Expr // TODO

sealed abstract class TypeSpecifier
object Void extends TypeSpecifier
object Char extends TypeSpecifier
object Short extends TypeSpecifier
object Int extends TypeSpecifier
object Long extends TypeSpecifier
object Float extends TypeSpecifier
object Double extends TypeSpecifier
object Signed extends TypeSpecifier
object Unsigned extends TypeSpecifier
final case class StructSpecifier(
  name: Option[L[TokId]],
  body: Option[Seq[L[StructDeclaration]]]) extends TypeSpecifier
final case class UnionSpecifier(
  name: Option[L[TokId]],
  body: Option[Seq[L[StructDeclaration]]]) extends TypeSpecifier
final case class EnumSpecifier(
  name: Option[L[TokId]],
  body: Option[Seq[(L[TokId], Option[L[Expr]])]]
) extends TypeSpecifier
final case class TypedefName(tok: TokId) extends TypeSpecifier

sealed abstract class TypeQualifier
object Const extends TypeQualifier
object Volatile extends TypeQualifier

final case class StructDeclaration(
  spQlList: Seq[L[Either[TypeSpecifier, TypeQualifier]]],
  declratorList: Seq[(Option[L[Declarator]], Option[L[Expr]])])

object StructDeclaration {
  def ofLoc(
    x: Seq[L[Either[TypeSpecifier, TypeQualifier]]],
    y: Seq[(Option[L[Declarator]], Option[L[Expr]])],
    semiLoc: Loc
  ): L[StructDeclaration] = {
    L(LocRange.of(x.head.loc, semiLoc), StructDeclaration(x, y))
  }
}

sealed abstract class Declarator // TODO

// type StructDeclarator = ([L[Declarator], Option[L[Expr]])