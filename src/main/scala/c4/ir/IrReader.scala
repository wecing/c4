package c4.ir

import java.util.concurrent.atomic.AtomicInteger

import c4.ast._
import c4.io.TokId
import c4.messaging.{IllegalSourceException, LocMessage, Message}
import c4.util.{Loc, LocRange, ProgrammingError, Located => L}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object IrReader {
  def fromAst(warnings: ArrayBuffer[Message],
              root: AST.TranslationUnit): Unit = {
    val reader = new IrReader(warnings, root)
    reader._read()
  }

  def collectWhile[T, P](opt: Option[T], f: T => (P, Option[T])): Seq[P] = {
    opt match {
      case None => Seq.empty
      case Some(t) =>
        val (p, nextOpt) = f(t)
        p +: collectWhile(nextOpt, f)
    }
  }

  // with these we chould check if an id is redefined as another type
  // in the same category. (e.g. "struct T {}; union T {};")
  object SueType extends Enumeration {
    val Struct, Union, Enum = Value

    def ir(sueType: SueType.Value, id: Int): Type[Nothing] = {
      sueType match {
        case Struct => T_struct(id)
        case Union => T_union(id)
        case Enum => T_enum(id)
      }
    }
  }
  object OrdinaryType extends Enumeration {
    val TypeName, Variable = Value
  }

  private class Scope(
      _outer: Option[Scope],
      _curBeingDefinedSueName: Option[String] = None
  ) {
    def this(scope: Scope) = this(Some(scope), None)
    def this(scope: Scope, x: Option[String]) = this(Some(scope), x)

    val nextId: AtomicInteger = {
      _outer.map(_.nextId).getOrElse(new AtomicInteger(1))
    }

    // name spaces:
    // - label names
    // - name of structures, unions, and enumerations
    val nsSue: mutable.Map[String, (SueType.Value, Int)] = mutable.Map()
    // lazy val nsSueOuterScopes: Map[String, (SueType.Value, Int)] = {
    //   val maps: Seq[mutable.Map[String, (SueType.Value, Int)]] =
    //     collectWhile(_outer, (s: Scope) => s.nsSueCurScope -> s._outer)
    //   val acc = Map.empty[String, (SueType.Value, Int)]
    //   maps.foldLeft(acc) { (x, y) => x ++ y }
    // }
    val structDefs: mutable.Map[Int, L[StructDefn[L]]] = {
      _outer.map(_.structDefs).getOrElse(mutable.Map())
    }
    // - the members of structures and unions
    // - all other identifiers, called ordinary identifiers
    val nsOrdinary: mutable.Map[String, OrdinaryType.Value] = mutable.Map()
    val typedefs: mutable.Map[String, QType[L]] = mutable.Map()

    val outer: Option[Scope] = _outer
    val isFileScope: Boolean = _outer.isEmpty

    // for detecting nested redefinitions, like:
    //    struct T { struct T {int x;} tt; };
    val beingDefinedSueNames: Set[String] = {
      val outer = _outer map { _.beingDefinedSueNames } getOrElse Set.empty
      outer ++ _curBeingDefinedSueName
    }
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

  private def mergeTypeQualifiers(
      x: TypeQualifiers[L],
      y: TypeQualifiers[L]
  ): TypeQualifiers[L] = {
    val sum = y.isConst match {
      case None => x
      case Some(L(loc, _)) => mergeTypeQualifiers(x, L(loc, Const))
    }
    y.isVolatile match {
      case None => sum
      case Some(L(loc, _)) => mergeTypeQualifiers(sum, L(loc, Volatile))
    }
  }

  def selectFst2[A,B,C](t: (A, B, C)): (A, B) = t._1 -> t._2
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

  private def _extractFuncArgs(
      newScope: Scope,
      paramTypes: Seq[L[ParamDeclaration]]
  ): Seq[(Option[L[Unit]], L[QType[L]], Option[L[String]])] = {
    // int main(void)
    paramTypes match {
      case Seq(L(_, ParamDeclarationTypeOnlySimple(Seq(L(_,
              DeclarationSpecifierTypeSpecifier(L(_, Void))))))) =>
        return Seq()
      case _ =>
    }

    paramTypes map { ld =>
      type DeclOpt = Option[Either[L[Declarator], L[AbstractDeclarator]]]
      val (dss, declOpt: DeclOpt) = ld.value match {
        case x: ParamDeclarationNamed => x.dss -> Some(Left(x.d))
        case x: ParamDeclarationTypeOnly => x.dss -> Some(Right(x.ad))
        case x: ParamDeclarationTypeOnlySimple => x.dss -> None
      }

      val (isRegister: Option[L[Unit]], baseTp: L[QType[L]]) = {
        // pretend there is a declarator, so 'struct T' always
        // refer to previously defined 'struct T'.
        _extractDSS(newScope, dss, ld.loc, true).value match {
          case (Some(L(loc, Register)), lqtp) =>
            Some(L.unit(loc)) -> lqtp
          case (Some(L(loc, q)), _) =>
            throw IllegalSourceException(loc,
              s"Type Qualifier $q cannot be applied to function parameters")
          case (None, lqtp) =>
            None -> lqtp
        }
      }

      val (tp: L[QType[L]], nameOpt: Option[L[String]]) = declOpt match {
        case None => baseTp -> None
        case Some(Left(_ld)) =>
          val p = _unpackDeclarator(newScope, baseTp, _ld)
          p._1 -> Some(p._2)
        case Some(Right(lad)) =>
          _unpackAbstractDeclarator(newScope, baseTp, lad) -> None
      }
      if (tp.value.tp.value == T_void) {
        throw IllegalSourceException(tp.loc,
          s"void is not a legal function parameter type")
      }

      (isRegister, tp, nameOpt)
    }
  }

  private def _unpackPtr(
      lqtp: L[QType[L]],
      lptrOpt: Option[L[Pointer]]
  ): L[QType[L]] = lptrOpt match {
    case None => lqtp
    case Some(L(loc, Pointer(qs, ptr))) =>
      val starLoc = loc.head
      val withPtr: L[Type[L]] = L.of(lqtp.loc, starLoc, T_ptr(lqtp))
      val withQs = QType(
        qs.foldLeft(TypeQualifiers[L](None, None)) { (_qs, lq) =>
          mergeTypeQualifiers(_qs, lq)
        },
        withPtr
      )
      val qsEndLoc = (starLoc +: qs.map(_.loc)).last
      _unpackPtr(L.of(lqtp.loc, qsEndLoc, withQs), ptr)
  }

  private def _unpackAbstractDeclarator(
      scope: Scope,
      lqtp: L[QType[L]],
      lad: L[AbstractDeclarator]
  ): L[QType[L]] = {
    def unpackDAD(
        scope: Scope,
        lqtp: L[QType[L]],
        ldad: L[DirectAbstractDeclarator]
    ): L[QType[L]] = ldad.value match {
      case x: DirectAbstractDeclaratorSimple =>
        _unpackAbstractDeclarator(scope, lqtp, x.ad)
      case x: DirectAbstractDeclaratorArray =>
        ??? // TODO: array
      case x: DirectAbstractDeclaratorFunc =>
        x.params match {
          case None => ??? // TODO: empty params list
          case Some((ps: Seq[L[ParamDeclaration]], ellipsis: Option[Loc])) =>
            val args = _extractFuncArgs(new Scope(scope), ps).map(selectFst2)
            val funcTp: Type[L] = T_func(lqtp, args, ellipsis.map(L.of(_, ())))
            val lqFuncTp: L[QType[L]] =
              L(ldad.loc, QType(TypeQualifiers.None, L(ldad.loc, funcTp)))
            x.dad match {
              case None => lqFuncTp
              case Some(_dad) => unpackDAD(scope, lqFuncTp, _dad)
            }
        }
    }

    val withPtr: L[QType[L]] = _unpackPtr(lqtp, lad.value.ptr)
    lad.value.dad match {
      case None => withPtr
      case Some(dad) => unpackDAD(scope, withPtr, dad)
    }
  }

  private def _unpackDeclarator(
      scope: Scope,
      lqtp: L[QType[L]],
      ld: L[Declarator]
  ): (L[QType[L]], L[String]) = {
    def unpackDirectDeclarator(
        scope: Scope,
        lqtp: L[QType[L]],
        ldd: L[DirectDeclarator]
    ): (L[QType[L]], L[String]) = ldd.value match {
      case DirectDeclaratorId(L(loc, TokId(id))) =>
        lqtp -> L(loc, id)
      case DirectDeclaratorDeclarator(_ld) =>
        _unpackDeclarator(scope, lqtp, _ld)
      case DirectDeclaratorArray(_, _) =>
        ??? // TODO: array
      case DirectDeclaratorFuncTypes(_ldd, paramTypes, ellipsis) =>
        val tps = _extractFuncArgs(new Scope(scope), paramTypes).map(selectFst2)
        val funcTp: Type[L] = T_func(lqtp, tps, ellipsis.map(L(_, ())))
        val funcTpQL: L[QType[L]] =
          L(ldd.loc, QType(TypeQualifiers.None, L(ldd.loc, funcTp)))
        unpackDirectDeclarator(scope, funcTpQL, _ldd)
      case DirectDeclaratorIdsList(_, _) =>
        ??? // TODO: name-only params list, "int f(x, y)"
    }

    unpackDirectDeclarator(scope, _unpackPtr(lqtp, ld.value.ptr), ld.value.dd)
  }

  private def _toIrStructDefn(
      newScope: Scope,
      lss: L[StructSpecifier]
  ): L[StructDefn[L]] = {
    def toSuField(lsd: L[StructDeclaration]): Seq[L[SuField[L]]] = {
      val baseType: L[QType[L]] = {
        val (ltss: Seq[L[TypeSpecifier]], ltqs: Seq[L[TypeQualifier]]) = {
          val init = Seq.empty[L[TypeSpecifier]] -> Seq.empty[L[TypeQualifier]]
          lsd.value.spQlList.foldLeft(init) { (p, x) =>
            x match {
              case L(loc, Left(_x)) => (p._1 :+ L(loc, _x), p._2)
              case L(loc, Right(_x)) => (p._1, p._2 :+ L(loc, _x))
            }
          }
        }

        val tqs = ltqs.foldLeft(TypeQualifiers[L](None, None)) { (acc, lq) =>
          mergeTypeQualifiers(acc, lq)
        }
        // pretend there is a declarator, so 'struct T' always
        // refer to previously defined 'struct T'.
        _toQType(newScope, ltss, lss.loc, tqs, true)
      }

      for ((ldOpt: Option[L[Declarator]],
            leOpt: Option[L[Expr]]) <- lsd.value.declratorList) yield {
        val t = ldOpt map { ld => _unpackDeclarator(newScope, baseType, ld) }
        val (tp, nameOpt) = t map { x =>
          x._1 -> Some(x._2)
        } getOrElse (baseType -> None)
        // TODO: bitFieldSize is ignored
        L(lss.loc, SuField(nameOpt, tp, None))
      }
    }
    val name = lss.value.name map { _ map { _.id } }
    val body = lss.value.body map { _ flatMap toSuField }
    L(lss.loc, StructDefn(name, body))
  }

  // 'struct T' could either declare a new type or refer to the previous type,
  // depending if declarator exists. for example:
  //
  // struct T *tp1;
  // void f() {
  //   struct T; // another 'struct T' is defined
  //   struct T *tp2 = tp1;
  // }
  //
  // will not pass type checking, because tp2 and tp1 are of different types.
  // but if the 'struct T' line is commented out, the code compiles again.
  private def _toIrSue(
      scope: Scope,
      lsue: L[TypeSpecifier],
      hasDeclarator: Boolean
  ): Type[L] = {
    val (sueType: SueType.Value,
         nameOpt: Option[L[String]],
         isBodyEmpty: Boolean) = {
      lsue.value match {
        case StructSpecifier(n, b) =>
          (SueType.Struct, n.map(x => L(x.loc, x.value.id)), b.isEmpty)
        case UnionSpecifier(n, b) =>
          (SueType.Union, n.map(x => L(x.loc, x.value.id)), b.isEmpty)
        case EnumSpecifier(n, b) =>
          (SueType.Enum, n.map(x => L(x.loc, x.value.id)), b.isEmpty)
        case _ => ProgrammingError()
      }
    }

    def findDeclScope(scope: Scope, name: String): Option[Scope] = {
      if (scope.nsSue.contains(name)) {
        Some(scope)
      } else {
        scope.outer.flatMap { findDeclScope(_, name) }
      }
    }

    def declInCurScope(
        name: L[String],
        sueType: SueType.Value,
        id: Int
    ): Type[Nothing] = {
      scope.nsSue.put(name.value, sueType -> id)
      sueType match {
        case SueType.Struct =>
          scope.structDefs.put(id, L(lsue.loc, StructDefn(Some(name), None)))
        case _ => ??? // TODO: Union, Enum
      }
      SueType.ir(sueType, id)
    }

    def updateSueDefn(id: Int): Type[L] = {
      val s = nameOpt match {
        case None => new Scope(scope)
        case Some(L(_, name)) => new Scope(scope, Some(name))
      }
      lsue match {
        case L(loc, ss: StructSpecifier) =>
          s.structDefs.put(id, _toIrStructDefn(s, L(loc, ss)))
          T_struct(id)
        case _ => ??? // TODO: Union, Enum
      }
    }

    // if lsue is a...
    //
    // (0) 'struct'; no name, no body =>
    //        parser error
    // (1) 'struct T' with empty body =>
    //        0. if no declarator: declare 'struct T'
    //        1. if T is already defined: refer to previous T
    //        2. otherwise: declare 'struct T'
    // (2) 'struct {...}'; no name, has body =>
    //        new anonymous struct in current scope
    // (3) 'struct T {...}'; has name, has body =>
    //        1. if T is defined in current scope: error
    //        2. if T is declared in current scope: update
    //        3. otherwise: new named struct in current scope
    (nameOpt, isBodyEmpty) match {
      case (None, true) => ??? // programming error
      case (Some(nameLoc@L(_, name)), true) =>
        val prevDecl = findDeclScope(scope, name) flatMap { _.nsSue.get(name) }
        (hasDeclarator, prevDecl) match {
          case (true, Some((sueTp, _))) if sueTp != sueType =>
            throw IllegalSourceException(lsue.loc,
              s"$name is used as $sueType, but was declared as $sueTp before")
          case (true, Some((_, id))) =>
            SueType.ir(sueType, id)
          case _ =>
            declInCurScope(nameLoc, sueType, scope.nextId.getAndAdd(1))
        }
      case (None, false) =>
        updateSueDefn(scope.nextId.getAndAdd(1))
      case (Some(nameLoc@L(_, name)), false) =>
        scope.nsSue.get(name) match {
          case Some((sueTp, id)) if sueTp != sueType =>
            throw IllegalSourceException(lsue.loc,
              s"$name is used as $sueType, but was declared as $sueTp before")
          case Some((_, id)) =>
            val isDefined: Boolean = sueType match {
              case SueType.Struct =>
                scope.structDefs(id).value.body.isDefined
              case _ => ??? // TODO: Union, Enum
            }
            if (isDefined) {
              throw IllegalSourceException(lsue.loc,
                s"$name is already redefined")
            }
            updateSueDefn(id)
          case _ =>
            val id = scope.nextId.getAndAdd(1)
            declInCurScope(nameLoc, sueType, id)
            updateSueDefn(id)
        }
    }
  }

  private def _toQType(
      scope: Scope,
      ltss: Seq[L[TypeSpecifier]],
      fallbackLoc: Loc,
      qs: TypeQualifiers[L],
      hasDeclarator: Boolean
  ): L[QType[L]] = {
    val loc = ltss.map(_.loc) match {
      case Seq() =>
        warnings += LocMessage(
          fallbackLoc, "type specifier missing, defaults to int")
        fallbackLoc
      case Seq(x) => x
      case xs => LocRange.of(xs.head, xs.last)
    }

    def Q(tp: Type[L]) = L(loc, QType(qs, L(loc, tp)))

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
      case Seq(ss: StructSpecifier) =>
        Q(_toIrSue(scope, L(loc, ss), hasDeclarator))
      case Seq(us: UnionSpecifier) =>
        ??? // TODO: union
      case Seq(es: EnumSpecifier) =>
        ??? // TODO: enum
      case Seq(TypedefName(TokId(name))) =>
        scope.nsOrdinary.get(name) match {
          case Some(OrdinaryType.TypeName) =>
            val qt = scope.typedefs(name)
            L(loc, QType(mergeTypeQualifiers(qs, qt.qs), L(loc, qt.tp.value)))
          case Some(_) =>
            throw IllegalSourceException(loc, s"$name is not a typename")
          case None =>
            throw IllegalSourceException(loc, s"unknown type name $name")
        }
    }
  }

  private def _extractDSS(
      scope: Scope,
      dss: Seq[L[DeclarationSpecifier]],
      fallbackLoc: Loc,
      hasDeclarator: Boolean
  ): L[(Option[L[StorageClassSpecifier]], L[QType[L]])] = {
    val storageClassAst: Seq[L[StorageClassSpecifier]] = dss.collect {
      case L(_, x: DeclarationSpecifierStorageClass) => x.scs
    }
    val typeSpecifiersAst: Seq[L[TypeSpecifier]] = dss.collect {
      case L(_, x: DeclarationSpecifierTypeSpecifier) => x.ts
    }
    val typeQualifiersAst: Seq[L[TypeQualifier]] = dss.collect {
      case L(_, x: DeclarationSpecifierTypeQualifier) => x.tq
    }

    val storageClass: Option[L[StorageClassSpecifier]] =
      storageClassAst match {
        case Seq(x) => Some(x)
        case Seq() => None
        case xs => throw IllegalSourceException(
          fallbackLoc, s"unexpected storage classes: ${xs.map(_.value)}")
      }
    val typeQualifiers: TypeQualifiers[L] =
      typeQualifiersAst.foldLeft(TypeQualifiers[L](None, None)) {
        (qs, q) => mergeTypeQualifiers(qs, q)
      }

    val loc = dss match {
      case Seq() => fallbackLoc
      case _ => LocRange.of(dss.head.loc, dss.last.loc)
    }
    val lqtp = _toQType(
      scope, typeSpecifiersAst, fallbackLoc, typeQualifiers, hasDeclarator)

    L(loc, storageClass -> lqtp)
  }

  private def _visitExternalDecl(scope: Scope,
                                 ed: L[AST.ExternalDecl]): Unit = {
    ed.value match {
      case Left(L(loc, fDef: FunctionDef)) =>
        // TODO: FunctionDef
      case Right(L(declLoc, decl: Declaration)) =>
        val hasDeclarator =
          decl.ids.getOrElse(Seq.empty).exists { _._2.isDefined }
        val dssExt = _extractDSS(scope, decl.dss, declLoc, hasDeclarator)
        val scsOpt: Option[L[StorageClassSpecifier]] = dssExt.value._1
        val baseTp: L[QType[L]] = dssExt.value._2

        decl.ids match {
          case None => ()
          case Some(xs) => for ((ld, init: Option[L[Initializer]]) <- xs) {
            val t = _unpackDeclarator(scope, baseTp, ld)
            val (tp: L[QType[L]], id: L[String]) = t
            // TODO: tp, id, init
            println(s"tp=<$tp>, id=<$id>, init=<$init>")
          }
        }
    }
  }
}

