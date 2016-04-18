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

final case class Declarator(ptr: Option[L[Pointer]], dd: L[DirectDeclarator])

sealed abstract class DirectDeclarator
final case class DirectDeclaratorId(tok: L[TokId]) extends DirectDeclarator
final case class DirectDeclaratorDeclarator(d: L[Declarator])
  extends DirectDeclarator
final case class DirectDeclaratorArray(
  dd: L[DirectDeclarator],
  size: Option[L[Expr]]
) extends DirectDeclarator
final case class DirectDeclaratorFuncTypes(
  dd: L[DirectDeclarator],
  paramTypes: Seq[L[ParamDeclaration]],
  ellipsis: Option[Loc]
) extends DirectDeclarator
final case class DirectDeclaratorIdsList(
  dd: L[DirectDeclarator],
  ids: Seq[L[TokId]]
) extends DirectDeclarator

final case class AbstractDeclarator(
  ptr: Option[L[Pointer]],
  dad: Option[L[DirectAbstractDeclarator]] // huh?
)

sealed abstract class DirectAbstractDeclarator
final case class DirectAbstractDeclaratorSimple(
  ad: L[AbstractDeclarator]
) extends DirectAbstractDeclarator
final case class DirectAbstractDeclaratorArray(
  dad: Option[L[DirectAbstractDeclarator]],
  size: Option[L[Expr]]
) extends DirectAbstractDeclarator
final case class DirectAbstractDeclaratorFunc(
  dad: Option[L[DirectAbstractDeclarator]],
  params: Option[(Seq[L[ParamDeclaration]], Option[Loc])]
) extends DirectAbstractDeclarator

sealed abstract class ParamDeclaration
final case class ParamDeclarationNamed(
  dss: Seq[L[DeclarationSpecifier]],
  d: L[Declarator]
) extends ParamDeclaration
final case class ParamDeclarationTypeOnly(
  dss: Seq[L[DeclarationSpecifier]],
  ad: L[AbstractDeclarator]
) extends ParamDeclaration
final case class ParamDeclarationTypeOnlySimple(
  dss: Seq[L[DeclarationSpecifier]]
) extends ParamDeclaration

sealed abstract class DeclarationSpecifier
final case class DeclarationSpecifierStorageClass(
  scs: L[StorageClassSpecifier]
) extends DeclarationSpecifier
final case class DeclarationSpecifierTypeSpecifier(
  ts: L[TypeSpecifier]
) extends DeclarationSpecifier
final case class DeclarationSpecifierTypeQualifier(
  tq: L[TypeQualifier]
) extends DeclarationSpecifier

sealed abstract class StorageClassSpecifier
object Typedef extends StorageClassSpecifier
object Extern extends StorageClassSpecifier
object Static extends StorageClassSpecifier
object Auto extends StorageClassSpecifier
object Register extends StorageClassSpecifier

final case class Pointer(qs: Seq[L[TypeQualifier]], ptr: Option[L[Pointer]])

sealed abstract class Initializer
final case class InitializerExpr(expr: L[Expr]) extends Initializer
final case class InitializerStruct(il: Seq[L[Initializer]]) extends Initializer

final case class Declaration(
  dss: Seq[L[DeclarationSpecifier]],
  ids: Option[Seq[(L[Declarator], Option[L[Initializer]])]]
)

final case class FunctionDef(
  dss: Seq[L[DeclarationSpecifier]],
  d: L[Declarator],
  dl: Option[Seq[L[Declaration]]],
  body: L[CompoundStmt]
)

sealed abstract class Stmt
final case class CompoundStmt(
  dl: Seq[L[Declaration]],
  sl: Seq[L[Stmt]]
) extends Stmt
sealed abstract class LabeledStmt extends Stmt
final case class LabeledStmtId(id: L[TokId], stmt: L[Stmt]) extends LabeledStmt
final case class LabeledStmtCase(e: L[Expr], stmt: L[Stmt]) extends LabeledStmt
final case class LabeledStmtDefault(stmt: L[Stmt]) extends LabeledStmt
final case class ExprStmt(e: Option[L[Expr]]) extends Stmt
final case class IfStmt(
  cond: L[Expr],
  thenS: L[Stmt],
  elseS: Option[L[Stmt]]
) extends Stmt
final case class SwitchStmt(e: L[Expr], body: L[Stmt]) extends Stmt
final case class WhileStmt(e: L[Expr], body: L[Stmt]) extends Stmt
final case class DoWhileStmt(body: L[Stmt], e: L[Expr]) extends Stmt
final case class ForStmt(
  e1: Option[L[Expr]],
  e2: Option[L[Expr]],
  e3: Option[L[Expr]],
  body: L[Stmt]
) extends Stmt
final case class GotoStmt(id: L[TokId]) extends Stmt
object Continue extends Stmt
object Break extends Stmt
final case class Return(e: Option[L[Expr]]) extends Stmt

final case class TypeName(
  sqs: Seq[L[Either[TypeSpecifier, TypeQualifier]]],
  ad: Option[L[AbstractDeclarator]]
)
