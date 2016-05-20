package c4.ir

import c4.ast._
import c4.io.TokId
import c4.messaging.{IllegalSourceException, LocMessage, Message}
import c4.util.{Loc, LocRange, Located => L}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object IrReader {
  def fromAst(warnings: ArrayBuffer[Message],
              root: AST.TranslationUnit): Unit = {
    val reader = new IrReader(warnings, root)
    reader._read()
  }

  // with these we chould check if an id is redefined as another type
  // in the same category. (e.g. "struct T {}; union T {};")
  object SueType extends Enumeration {
    val Struct, Union, Enum = Value
  }
  object OrdinaryType extends Enumeration {
    val TypeName, Variable = Value
  }

  private class Scope(_outer: Option[Scope]) {
    def this(scope: Scope) = this(Some(scope))

    var nextId = 1

    // name spaces:
    // - label names
    // - name of structures, unions, and enumerations
    val nsSue: mutable.Map[String, SueType.Value] = mutable.Map()
    // - the members of structures and unions
    // - all other identifiers, called ordinary identifiers
    val nsOrdinary: mutable.Map[String, OrdinaryType.Value] = mutable.Map()
    val typedefs: mutable.Map[String, QType[L]] = mutable.Map()

    val outer: Option[Scope] = _outer
    val isFileScope: Boolean = _outer.isEmpty
  }

  private def mergeTypeQualifiers(qs: TypeQualifiers[L],
                                  lq: L[TypeQualifier]): TypeQualifiers[L] = {
    val loc = lq.loc
    lq.value match {
      case Const =>
        qs.isConst match {
          case Some(x) => throw IllegalSourceException(
            loc, "Duplicated type qualifier const")
          case None => qs.copy(isConst = Some(L.unit(loc)))
        }
      case Volatile =>
        qs.isVolatile match {
          case Some(x) => throw IllegalSourceException(
            loc, "Duplicated type qualifier volatile")
          case None => qs.copy(isVolatile = Some(L.unit(loc)))
        }
    }
  }
}

class IrReader private (val warnings: ArrayBuffer[Message],
                        val root: AST.TranslationUnit) {
  import IrReader._

  private def _read(): Unit = {
    val fileScope = new Scope(None)
    for (ed <- root) {
      _visitExternalDecl(fileScope, ed)
    }
  }

  private def _toIrType(scope: Scope,
                              ltss: Seq[L[TypeSpecifier]],
                              fallbackLoc: Loc,
                              qs: TypeQualifiers[L]): QType[L] = {
    val loc = ltss.map(_.loc) match {
      case Seq() =>
        warnings += LocMessage(
          fallbackLoc, "type specifier missing, defaults to int")
        fallbackLoc
      case Seq(x) => x
      case xs => LocRange.of(xs.head, xs.last)
    }

    def Q(tp: Type[L]) = QType(TypeQualifiers[L](None, None), L(loc, tp))

    ltss.map(_.value) match {
      case Seq(Void)                                          => Q(T_void)
      case Seq(Char) | Seq(Signed, Char)                      => Q(T_i8)
      case Seq(Unsigned, Char)                                => Q(T_u8)
      case Seq(Short) | Seq(Signed, Short) |
           Seq(Short, Int) | Seq(Signed, Short, Int)          => Q(T_i16)
      case Seq(Unsigned, Short) | Seq(Unsigned, Short, Int)   => Q(T_u16)
      case Seq(Int) | Seq(Signed) | Seq(Signed, Int) | Seq()  => Q(T_i32)
      case Seq(Unsigned) | Seq(Unsigned, Int)                 => Q(T_u32)
      case Seq(Long) | Seq(Signed, Long) |
           Seq(Long, Int) | Seq(Signed, Long, Int)            => Q(T_i64)
      case Seq(Unsigned, Long) | Seq(Unsigned, Long, Int)     => Q(T_u64)
      case Seq(Float)                                         => Q(T_float)
      case Seq(Double)                                        => Q(T_double)
      case Seq(Long, Double)                                  => Q(T_fp80)
      // TODO: struct, union, enum
      case Seq(TypedefName(TokId(name))) =>
        scope.nsOrdinary.get(name) match {
          case Some(OrdinaryType.TypeName) =>
            val qt = scope.typedefs(name)
            QType(qt.qs, L(loc, qt.tp.value))
          case Some(_) =>
            throw IllegalSourceException(loc, s"$name is not a typename")
          case None =>
            throw IllegalSourceException(loc, s"unknown type name $name")
        }
    }
  }

  private def _visitExternalDecl(scope: Scope,
                                       ed: L[AST.ExternalDecl]): Unit = {
    ed.value match {
      case Left(L(loc, fDef: FunctionDef)) =>
        // println(s"root func def: declarator = ${fDef.d.value}")
      case Right(L(declLoc, decl: Declaration)) =>
        // println(s"root decl: declarators = ${decl.ids.map(_.map(_._1))}")
        val storageClassAst: Seq[L[StorageClassSpecifier]] = decl.dss.collect {
          case L(_, x: DeclarationSpecifierStorageClass) => x.scs
        }
        val typeSpecifiersAst: Seq[L[TypeSpecifier]] = decl.dss.collect {
          case L(_, x: DeclarationSpecifierTypeSpecifier) => x.ts
        }
        val typeQualifiersAst: Seq[L[TypeQualifier]] = decl.dss.collect {
          case L(_, x: DeclarationSpecifierTypeQualifier) => x.tq
        }

        val storageClass: Option[L[StorageClassSpecifier]] =
          storageClassAst match {
            case x +: Seq() => Some(x)
            case Seq() => None
            case xs => throw IllegalSourceException(
              declLoc, s"unexpected storage classes: ${xs.map(_.value)}")
          }
        val typeQualifiers: TypeQualifiers[L] =
          typeQualifiersAst.foldLeft(TypeQualifiers[L](None, None)) {
            (qs, q) => mergeTypeQualifiers(qs, q)
          }
        // TODO: handle struct/union/"long long"
        // TODO: typedef
        // TODO: initexpr
    }
  }
}

