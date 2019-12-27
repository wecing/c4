package c4.ast

import c4.ast.{proto => p}
import c4.io._
import c4.util.{Loc, LocPoint, LocRange, Located => L}

import scala.collection.mutable

object ProtoConverter {
  def toProto(tu: AST.TranslationUnit): p.TranslationUnit = {
    val converter = new ProtoConverter()
    converter.toProto(tu)
  }
}

private final class ProtoConverter {
  val _exprs: mutable.Map[p.Expr, Int] = mutable.Map.empty
  val _dds: mutable.Map[p.DirectDeclarator, Int] = mutable.Map.empty
  val _dads: mutable.Map[p.DirectAbstractDeclarator, Int] = mutable.Map.empty
  val _ptrs: mutable.Map[p.Pointer, Int] = mutable.Map.empty
  val _inits: mutable.Map[p.Initializer, Int] = mutable.Map.empty
  val _stmts: mutable.Map[p.Statement, Int] = mutable.Map.empty

  def _getIdx(expr: p.Expr): Int =
    _exprs.getOrElseUpdate(expr, _exprs.size + 1)

  def _getIdx(dd: p.DirectDeclarator): Int =
    _dds.getOrElseUpdate(dd, _dds.size + 1)

  def _getIdx(dad: p.DirectAbstractDeclarator): Int =
    _dads.getOrElseUpdate(dad, _dads.size + 1)

  def _getIdx(ptr: p.Pointer): Int =
    _ptrs.getOrElseUpdate(ptr, _ptrs.size + 1)

  def _getIdx(init: p.Initializer): Int =
    _inits.getOrElseUpdate(init, _inits.size + 1)

  def _getIdx(stmt: p.Statement): Int =
    _stmts.getOrElseUpdate(stmt, _stmts.size + 1)

  def _toList[T](m: mutable.Map[T, Int], fst: T): Seq[T] = {
    val reversed = for ((k, v) <- m) yield (v, k)
    val xs =
      (1 to m.size + 1)
        .foldRight(Seq.empty[T]) { (k, acc) => reversed(k) +: acc }
    fst +: xs
  }

  def toProto(tu: AST.TranslationUnit): p.TranslationUnit = {
    val ptu = tu.foldRight(p.TranslationUnit()) {
      case (L(loc1, Left(L(loc2, fd))), accTu) =>
        val ed = p.ExternalDecl(
          ed = p.ExternalDecl.Ed.Fd(_visit(fd)),
          loc = Some(_visit(loc2)))
        accTu.copy(
          eds = ed +: accTu.eds,
          edLocs = _visit(loc1) +: accTu.edLocs)
      case (L(loc1, Right(L(loc2, dl))), accTu) =>
        val ed = p.ExternalDecl(
          ed = p.ExternalDecl.Ed.Dl(_visit(dl)),
          loc = Some(_visit(loc2)))
        accTu.copy(
          eds = ed +: accTu.eds,
          edLocs = _visit(loc1) +: accTu.edLocs)
    }

    ptu.copy(
      exprs = _toList(_exprs, p.Expr()),
      directDeclarators = _toList(_dds, p.DirectDeclarator()),
      directAbstractDeclarators = _toList(_dads, p.DirectAbstractDeclarator()),
      pointers = _toList(_ptrs, p.Pointer()),
      initializers = _toList(_inits, p.Initializer()),
      statements = _toList(_stmts, p.Statement()))
  }

  def _visit(op: UnaryOp): p.Expr.Unary.Op = {
    op match {
      case PrefixInc => p.Expr.Unary.Op.PREFIX_INC
      case PrefixDec => p.Expr.Unary.Op.PREFIX_DEC
      case PostfixInc => p.Expr.Unary.Op.POSTFIX_INC
      case PostfixDec => p.Expr.Unary.Op.POSTFIX_DEC
      case Ref => p.Expr.Unary.Op.REF
      case Deref => p.Expr.Unary.Op.DEREF
      case Pos => p.Expr.Unary.Op.POS
      case Neg => p.Expr.Unary.Op.NEG
      case BitNot => p.Expr.Unary.Op.BIT_NOT
      case LogicNot => p.Expr.Unary.Op.LOGIC_NOT
    }
  }

  def _visit(op: BinaryOp): p.Expr.Binary.Op = {
    op match {
      case Assign => p.Expr.Binary.Op.ASSIGN
      case MulAssign => p.Expr.Binary.Op.MUL_ASSIGN
      case DivAssign => p.Expr.Binary.Op.DIV_ASSIGN
      case ModAssign => p.Expr.Binary.Op.MOD_ASSIGN
      case AddAssign => p.Expr.Binary.Op.ADD_ASSIGN
      case SubAssign => p.Expr.Binary.Op.SUB_ASSIGN
      case LShiftAssign => p.Expr.Binary.Op.L_SHIFT_ASSIGN
      case RShiftAssign => p.Expr.Binary.Op.R_SHIFT_ASSIGN
      case BinaryAndAssign => p.Expr.Binary.Op.BINARY_AND_ASSIGN
      case XorAssign => p.Expr.Binary.Op.XOR_ASSIGN
      case BinaryOrAssign => p.Expr.Binary.Op.BINARY_OR_ASSIGN
      case Comma => p.Expr.Binary.Op.COMMA
      case LogicOr => p.Expr.Binary.Op.LOGIC_OR
      case LogicAnd => p.Expr.Binary.Op.LOGIC_AND
      case BitOr => p.Expr.Binary.Op.BIT_OR
      case Xor => p.Expr.Binary.Op.XOR
      case BitAnd => p.Expr.Binary.Op.BIT_AND
      case Eq => p.Expr.Binary.Op.EQ
      case Neq => p.Expr.Binary.Op.NEQ
      case Less => p.Expr.Binary.Op.LESS
      case Gt => p.Expr.Binary.Op.GT
      case Leq => p.Expr.Binary.Op.LEQ
      case Geq => p.Expr.Binary.Op.GEQ
      case LShift => p.Expr.Binary.Op.L_SHIFT
      case RShift => p.Expr.Binary.Op.R_SHIFT
      case Add => p.Expr.Binary.Op.ADD
      case Sub => p.Expr.Binary.Op.SUB
      case Mul => p.Expr.Binary.Op.MUL
      case Div => p.Expr.Binary.Op.DIV
      case Mod => p.Expr.Binary.Op.MOD
    }
  }

  def _visit(tokInt: TokInteger): p.Expr.Integer = {
    val TokInteger(n, size, signed) = tokInt
    size match {
      case Int8 =>
        p.Expr.Integer(
          n = n.longValue() & 0xFF,
          size = p.Expr.Integer.Size.INT8,
          signed = signed)
      case Int16 =>
        p.Expr.Integer(
          n = n.longValue() & 0xFFFF,
          size = p.Expr.Integer.Size.INT16,
          signed = signed)
      case Int32 =>
        p.Expr.Integer(
          n = n.intValue(),
          size = p.Expr.Integer.Size.INT32,
          signed = signed)
      case Int64 =>
        p.Expr.Integer(
          n = n.longValue(),
          size = p.Expr.Integer.Size.INT64,
          signed = signed)
    }
  }

  def _visit(e: Expr): Int = {
    val expr: p.Expr.E =
      e match {
        case Id(id) => p.Expr.E.Id(id)
        case IntegerLit(tokInt) => p.Expr.E.Integer(_visit(tokInt))
        case FloatLit(TokFloat(f)) => p.Expr.E.Float(f)
        case DoubleLit(TokDouble(d)) => p.Expr.E.Double(d)
        case LongDoubleLit(TokLongDouble(ld)) =>
          p.Expr.E.Double(ld.doubleValue())
        case CharLit(TokChar(c)) => p.Expr.E.Char(c.asDigit & 0xFF)
        case WideCharLit(TokWideChar(c)) => p.Expr.E.WideChar(c.asDigit)
        case StrLit(TokStr(s)) => p.Expr.E.String(s)
        case WideStrLit(TokWideStr(s)) => p.Expr.E.WideString(s)
        case CastExpr(L(tpLoc, tp), L(eLoc, e)) =>
          p.Expr.E.Cast(
            p.Expr.Cast(
              tp = Some(_visit(tp)),
              tpLoc = Some(_visit(tpLoc)),
              eIdx = _visit(e),
              eLoc = Some(_visit(eLoc))))
        case ArrSubExpr(L(arrLoc, arr), L(subLoc, sub)) =>
          p.Expr.E.ArrSub(
            p.Expr.ArrSub(
              arrIdx = _visit(arr),
              arrLoc = Some(_visit(arrLoc)),
              subIdx = _visit(sub),
              subLoc = Some(_visit(subLoc))))
        case FunCallExpr(L(fnLoc, fn), args) =>
          p.Expr.E.FuncCall(
            p.Expr.FuncCall(
              fnIdx = _visit(fn),
              fnLoc = Some(_visit(fnLoc)),
              args = args.map(_.value).map(_visit),
              argLocs = args.map(_.loc).map(_visit)))
        case DotExpr(L(eLoc, e), L(fieldLoc, TokId(field))) =>
          p.Expr.E.Dot(
            p.Expr.Dot(
              eIdx = _visit(e),
              eLoc = Some(_visit(eLoc)),
              field = field,
              fieldLoc = Some(_visit(fieldLoc))))
        case PtrExpr(L(eLoc, e), L(fieldLoc, TokId(field))) =>
          p.Expr.E.Ptr(
            p.Expr.Ptr(
              eIdx = _visit(e),
              eLoc = Some(_visit(eLoc)),
              field = field,
              fieldLoc = Some(_visit(fieldLoc))))
        case SizeofValExpr(L(eLoc, e)) =>
          p.Expr.E.SizeofVal(
            p.Expr.SizeofVal(
              eIdx = _visit(e),
              eLoc = Some(_visit(eLoc))))
        case SizeofTypeExpr(L(tpLoc, tp)) =>
          p.Expr.E.SizeofTp(
            p.Expr.SizeofType(
              tp = Some(_visit(tp)),
              tpLoc = Some(_visit(tpLoc))))
        case UnaryExpr(L(eLoc, e), L(opLoc, op)) =>
          p.Expr.E.Unary(
            p.Expr.Unary(
              eIdx = _visit(e),
              eLoc = Some(_visit(eLoc)),
              op = _visit(op),
              opLoc = Some(_visit(opLoc))))
        case BinaryExpr(L(e1Loc, e1), L(e2Loc, e2), L(opLoc, op)) =>
          p.Expr.E.Binary(
            p.Expr.Binary(
              e1Idx = _visit(e1),
              e1Loc = Some(_visit(e1Loc)),
              e2Idx = _visit(e2),
              e2Loc = Some(_visit(e2Loc)),
              op = _visit(op),
              opLoc = Some(_visit(opLoc))))
        case TernaryExpr(condL, thenL, elseL) =>
          p.Expr.E.Ternary(
            p.Expr.Ternary(
              condIdx = _visit(condL.value),
              condLoc = Some(_visit(condL.loc)),
              thenIdx = _visit(thenL.value),
              thenLoc = Some(_visit(thenL.loc)),
              elseIdx = _visit(elseL.value),
              elseLoc = Some(_visit(elseL.loc))))
      }
    _getIdx(p.Expr(expr))
  }

  def _visit(sp: TypeSpecifier): p.TypeSpecifier = {
    val s: p.TypeSpecifier.S =
      sp match {
        case Void => p.TypeSpecifier.S.Void(p.TypeSpecifier.Void())
        case Char => p.TypeSpecifier.S.Char(p.TypeSpecifier.Char())
        case Short => p.TypeSpecifier.S.Short(p.TypeSpecifier.Short())
        case Int => p.TypeSpecifier.S.Int(p.TypeSpecifier.Int())
        case Long => p.TypeSpecifier.S.Long(p.TypeSpecifier.Long())
        case Float => p.TypeSpecifier.S.Float(p.TypeSpecifier.Float())
        case Double => p.TypeSpecifier.S.Double(p.TypeSpecifier.Double())
        case Signed => p.TypeSpecifier.S.Signed(p.TypeSpecifier.Signed())
        case Unsigned => p.TypeSpecifier.S.Unsigned(p.TypeSpecifier.Unsigned())
        case StructSpecifier(nameOpt, bodyOpt) =>
          val ss: p.TypeSpecifier.Struct =
            p.TypeSpecifier.Struct(
              name = nameOpt.map(_.value.id).getOrElse(""),
              nameLoc = nameOpt.map(_.loc).map(_visit),
              bodies = bodyOpt.getOrElse(Seq.empty).map(_.value).map(_visit),
              bodyLocs = bodyOpt.getOrElse(Seq.empty).map(_.loc).map(_visit))
          p.TypeSpecifier.S.Struct(ss)
        case UnionSpecifier(nameOpt, bodyOpt) =>
          val us: p.TypeSpecifier.Union =
            p.TypeSpecifier.Union(
              name = nameOpt.map(_.value.id).getOrElse(""),
              nameLoc = nameOpt.map(_.loc).map(_visit),
              bodies = bodyOpt.getOrElse(Seq.empty).map(_.value).map(_visit),
              bodyLocs = bodyOpt.getOrElse(Seq.empty).map(_.loc).map(_visit))
          p.TypeSpecifier.S.Union(us)
        case EnumSpecifier(nameOpt, bodyOpt) =>
          val es: p.TypeSpecifier.Enum =
            p.TypeSpecifier.Enum(
              name = nameOpt.map(_.value.id).getOrElse(""),
              nameLoc = nameOpt.map(_.loc).map(_visit),
              bodies = bodyOpt.getOrElse(Seq.empty).map {
                case (L(nLoc, TokId(n)), valueOpt) =>
                  p.TypeSpecifier.Enum.Body(
                    name = n,
                    nameLoc = Some(_visit(nLoc)),
                    value = valueOpt.map(_.value).map(_visit).getOrElse(0),
                    valueLoc = valueOpt.map(_.loc).map(_visit))
              })
          p.TypeSpecifier.S.Enum(es)
        case TypedefName(TokId(name)) => p.TypeSpecifier.S.TypedefName(name)
      }
    p.TypeSpecifier(s)
  }

  def _visit(ql: TypeQualifier): p.TypeQualifier = {
    ql match {
      case Const => p.TypeQualifier.CONST
      case Volatile => p.TypeQualifier.VOLATILE
    }
  }

  def _visit(sdl: StructDeclaration): p.StructDeclaration = {
    p.StructDeclaration(
      spQls = sdl.spQlList.map {
        case L(loc, Left(ts)) =>
          p.StructDeclaration.SpQl(
            elem = p.StructDeclaration.SpQl.Elem.Sp(_visit(ts)),
            loc = Some(_visit(loc)))
        case L(loc, Right(tq)) =>
          p.StructDeclaration.SpQl(
            elem = p.StructDeclaration.SpQl.Elem.Ql(_visit(tq)),
            loc = Some(_visit(loc)))
      },
      ds = sdl.declratorList.map {
        case (dOpt, eOpt) =>
          p.StructDeclaration.Decl(
            d = dOpt.map(_.value).map(_visit),
            dLoc = dOpt.map(_.loc).map(_visit),
            e = eOpt.map(_.value).map(_visit).getOrElse(0),
            eLoc = eOpt.map(_.loc).map(_visit))
      }
    )
  }

  def _visit(d: Declarator): p.Declarator = {
    p.Declarator(
      ptrIdx = d.ptr.map(_.value).map(_visit).getOrElse(0),
      ptrLoc = d.ptr.map(_.loc).map(_visit),
      ddIdx = _visit(d.dd.value),
      ddLoc = Some(_visit(d.dd.loc)))
  }

  def _visit(dd: DirectDeclarator): Int = {
    val ddOneof: p.DirectDeclarator.Dd =
      dd match {
        case DirectDeclaratorId(L(idLoc, TokId(id))) =>
          p.DirectDeclarator.Dd.Id(
            p.DirectDeclarator.Id(
              id = id,
              loc = Some(_visit(idLoc))))
        case DirectDeclaratorDeclarator(L(dLoc, d)) =>
          p.DirectDeclarator.Dd.D(
            p.DirectDeclarator.Declarator(
              d = Some(_visit(d)),
              loc = Some(_visit(dLoc))))
        case DirectDeclaratorArray(L(ddLoc, dd), sizeOpt) =>
          p.DirectDeclarator.Dd.Array(
            p.DirectDeclarator.Array(
              ddIdx = _visit(dd),
              ddLoc = Some(_visit(ddLoc)),
              sizeIdx = sizeOpt.map(_.value).map(_visit).getOrElse(0),
              sizeLoc = sizeOpt.map(_.loc).map(_visit)))
        case DirectDeclaratorFuncTypes(L(ddLoc, dd), paramTypes, ellipsis) =>
          p.DirectDeclarator.Dd.Ft(
            p.DirectDeclarator.FuncType(
              ddIdx = _visit(dd),
              ddLoc = Some(_visit(ddLoc)),
              pds = paramTypes.map(_.value).map(_visit),
              pdLocs = paramTypes.map(_.loc).map(_visit),
              hasEllipsis = ellipsis.isDefined,
              ellipsisLoc = ellipsis.map(_visit)))
        case DirectDeclaratorIdsList(L(ddLoc, dd), ids) =>
          p.DirectDeclarator.Dd.IdsList(
            p.DirectDeclarator.IdsList(
              ddIdx = _visit(dd),
              ddLoc = Some(_visit(ddLoc)),
              ids = ids.map(_.value.id),
              idLocs = ids.map(_.loc).map(_visit)))
      }
    _getIdx(p.DirectDeclarator(dd = ddOneof))
  }

  def _visit(ad: AbstractDeclarator): p.AbstractDeclarator = {
    p.AbstractDeclarator(
      ptrIdx = ad.ptr.map(_.value).map(_visit).getOrElse(0),
      ptrLoc = ad.ptr.map(_.loc).map(_visit),
      dadIdx = ad.dad.map(_.value).map(_visit).getOrElse(0),
      dadLoc = ad.dad.map(_.loc).map(_visit))
  }

  def _visit(dad: DirectAbstractDeclarator): Int = {
    val dadOneof: p.DirectAbstractDeclarator.Dad =
      dad match {
        case DirectAbstractDeclaratorSimple(L(adLoc, ad)) =>
          p.DirectAbstractDeclarator.Dad.Simple(
            p.DirectAbstractDeclarator.Simple(
              ad = Some(_visit(ad)),
              adLoc = Some(_visit(adLoc))))
        case DirectAbstractDeclaratorArray(dadOpt, sizeOpt) =>
          p.DirectAbstractDeclarator.Dad.Array(
            p.DirectAbstractDeclarator.Array(
              dadIdx = dadOpt.map(_.value).map(_visit).getOrElse(0),
              dadLoc = dadOpt.map(_.loc).map(_visit),
              sizeIdx = sizeOpt.map(_.value).map(_visit).getOrElse(0),
              sizeLoc = sizeOpt.map(_.loc).map(_visit)
            )
          )
        case DirectAbstractDeclaratorFunc(dadOpt, paramsOpt) =>
          p.DirectAbstractDeclarator.Dad.Func(
            p.DirectAbstractDeclarator.Func(
              dadIdx = dadOpt.map(_.value).map(_visit).getOrElse(0),
              dadLoc = dadOpt.map(_.loc).map(_visit),
              pds = paramsOpt.map(_._1).getOrElse(Seq.empty)
                .map(_.value).map(_visit),
              pdLocs = paramsOpt.map(_._1).getOrElse(Seq.empty)
                .map(_.loc).map(_visit),
              hasEllipsis = paramsOpt.flatMap(_._2).isDefined,
              ellipsisLoc = paramsOpt.flatMap(_._2).map(_visit)
            )
          )
      }
    _getIdx(p.DirectAbstractDeclarator(dad = dadOneof))
  }

  def _visit(pd: ParamDeclaration): p.ParamDeclaration = {
    p.ParamDeclaration(
      pd = pd match {
        case pdn: ParamDeclarationNamed =>
          p.ParamDeclaration.Pd.Name(
            p.ParamDeclaration.Named(
              dss = pdn.dss.map(_.value).map(_visit),
              dsLocs = pdn.dss.map(_.loc).map(_visit),
              d = Some(_visit(pdn.d.value)),
              dLoc = Some(_visit(pdn.d.loc))))
        case pdTo: ParamDeclarationTypeOnly =>
          p.ParamDeclaration.Pd.TypeOnly(
            p.ParamDeclaration.TypeOnly(
              dss = pdTo.dss.map(_.value).map(_visit),
              dsLocs = pdTo.dss.map(_.loc).map(_visit),
              ad = Some(_visit(pdTo.ad.value)),
              adLoc = Some(_visit(pdTo.ad.loc))))
        case pdTos: ParamDeclarationTypeOnlySimple =>
          p.ParamDeclaration.Pd.TypeOnlySimple(
            p.ParamDeclaration.TypeOnlySimple(
              dss = pdTos.dss.map(_.value).map(_visit),
              dsLocs = pdTos.dss.map(_.loc).map(_visit)))
      })
  }

  def _visit(ds: DeclarationSpecifier): p.DeclarationSpecifier = {
    ds match {
      case DeclarationSpecifierStorageClass(L(scsLoc, scs)) =>
        p.DeclarationSpecifier(
          elem = p.DeclarationSpecifier.Elem.Scs(_visit(scs)),
          loc = Some(_visit(scsLoc)))
      case DeclarationSpecifierTypeSpecifier(L(tsLoc, ts)) =>
        p.DeclarationSpecifier(
          elem = p.DeclarationSpecifier.Elem.Ts(_visit(ts)),
          loc = Some(_visit(tsLoc)))
      case DeclarationSpecifierTypeQualifier(L(tqLoc, tq)) =>
        p.DeclarationSpecifier(
          elem = p.DeclarationSpecifier.Elem.Tq(_visit(tq)),
          loc = Some(_visit(tqLoc)))
    }
  }

  def _visit(scs: StorageClassSpecifier): p.StorageClassSpecifier = {
    scs match {
      case Typedef => p.StorageClassSpecifier.TYPEDEF
      case Extern => p.StorageClassSpecifier.EXTERN
      case Static => p.StorageClassSpecifier.STATIC
      case Auto => p.StorageClassSpecifier.AUTO
      case Register => p.StorageClassSpecifier.REGISTER
    }
  }

  def _visit(ptr: Pointer): Int = {
    val proto: p.Pointer =
      p.Pointer(
        qs = ptr.qs.map(_.value).map(_visit),
        qLocs = ptr.qs.map(_.loc).map(_visit),
        ptrIdx = ptr.ptr.map(_.value).map(_visit).getOrElse(0),
        ptrLoc = ptr.ptr.map(_.loc).map(_visit))
    _getIdx(proto)
  }

  def _visit(init: Initializer): Int = {
    val proto: p.Initializer =
      init match {
        case InitializerExpr(L(eLoc, e)) =>
          val expr = p.Initializer.Expr(
            e = _visit(e),
            eLoc = Some(_visit(eLoc)))
          p.Initializer(init = p.Initializer.Init.Expr(expr))
        case InitializerStruct(il) =>
          val struct = p.Initializer.Struct(
            inits = il.map(_.value).map(_visit),
            initLocs = il.map(_.loc).map(_visit))
          p.Initializer(init = p.Initializer.Init.Struct(struct))
      }
    _getIdx(proto)
  }

  def _visit(dl: Declaration): p.Declaration = {
    p.Declaration(
      dss = dl.dss.map(_.value).map(_visit),
      dsLocs = dl.dss.map(_.loc).map(_visit),
      ids = dl.ids.getOrElse(Seq.empty).map {
        case (L(dLoc, d), Some(L(initLoc, init))) =>
          p.Declaration.Id(
            d = Some(_visit(d)),
            dLoc = Some(_visit(dLoc)),
            initIdx = _visit(init),
            initLoc = Some(_visit(initLoc)))
        case (L(dLoc, d), None) =>
          p.Declaration.Id(
            d = Some(_visit(d)),
            dLoc = Some(_visit(dLoc)))
      }
    )
  }

  def _visit(fd: FunctionDef): p.FunctionDef = {
    p.FunctionDef(
      dss = fd.dss.map(_.value).map(_visit),
      dsLocs = fd.dss.map(_.loc).map(_visit),
      d = Some(_visit(fd.d.value)),
      dLoc = Some(_visit(fd.d.loc)),
      dls = fd.dl.getOrElse(Seq.empty).map(_.value).map(_visit),
      dlLocs = fd.dl.getOrElse(Seq.empty).map(_.loc).map(_visit),
      body = Some(_visit(fd.body.value)),
      bodyLoc = Some(_visit(fd.body.loc)))
  }

  def _visit(s: Stmt): Int = {
    val stmt: p.Statement.Stmt =
      s match {
        case cs: CompoundStmt => p.Statement.Stmt.Compound(_visit(cs))
        case LabeledStmtId(L(idLoc, TokId(id)), L(stmtLoc, stmt)) =>
          p.Statement.Stmt.Labeled(
            p.Statement.Labeled(
              l = p.Statement.Labeled.L.Id(
                p.Statement.Labeled.Id(
                  id = id,
                  idLoc = Some(_visit(idLoc)),
                  stmtIdx = _visit(stmt),
                  stmtLoc = Some(_visit(stmtLoc))))))
        case LabeledStmtCase(L(eLoc, e), L(stmtLoc, stmt)) =>
          p.Statement.Stmt.Labeled(
            p.Statement.Labeled(
              l = p.Statement.Labeled.L.CaseS(
                p.Statement.Labeled.Case(
                  eIdx = _visit(e),
                  eLoc = Some(_visit(eLoc)),
                  stmtIdx = _visit(stmt),
                  stmtLoc = Some(_visit(stmtLoc))))))
        case LabeledStmtDefault(L(stmtLoc, stmt)) =>
          p.Statement.Stmt.Labeled(
            p.Statement.Labeled(
              l = p.Statement.Labeled.L.DefaultS(
                p.Statement.Labeled.Default(
                  stmtIdx = _visit(stmt),
                  stmtLoc = Some(_visit(stmtLoc))))))
        case ExprStmt(eOpt) =>
          p.Statement.Stmt.Expr(
            p.Statement.Expr(
              eIdx = eOpt.map(_.value).map(_visit).getOrElse(0),
              eLoc = eOpt.map(_.loc).map(_visit)))
        case IfStmt(L(condLoc, cond), L(thenLoc, thenS), elseOpt) =>
          p.Statement.Stmt.IfS(
            p.Statement.If(
              condIdx = _visit(cond),
              condLoc = Some(_visit(condLoc)),
              thenIdx = _visit(thenS),
              thenLoc = Some(_visit(thenLoc)),
              elseIdx = elseOpt.map(_.value).map(_visit).getOrElse(0),
              elseLoc = elseOpt.map(_.loc).map(_visit)))
        case SwitchStmt(L(eLoc, e), L(bodyLoc, body)) =>
          p.Statement.Stmt.SwitchS(
            p.Statement.Switch(
              eIdx = _visit(e),
              eLoc = Some(_visit(eLoc)),
              bodyIdx = _visit(body),
              bodyLoc = Some(_visit(bodyLoc))))
        case WhileStmt(L(eLoc, e), L(bodyLoc, body)) =>
          p.Statement.Stmt.WhileS(
            p.Statement.While(
              eIdx = _visit(e),
              eLoc = Some(_visit(eLoc)),
              bodyIdx = _visit(body),
              bodyLoc = Some(_visit(bodyLoc))))
        case DoWhileStmt(L(bodyLoc, body), L(eLoc, e)) =>
          p.Statement.Stmt.DoWhileS(
            p.Statement.DoWhile(
              bodyIdx = _visit(body),
              bodyLoc = Some(_visit(bodyLoc)),
              eIdx = _visit(e),
              eLoc = Some(_visit(eLoc))))
        case ForStmt(e1Opt, e2Opt, e3Opt, L(bodyLoc, body)) =>
          p.Statement.Stmt.ForS(
            p.Statement.For(
              e1Idx = e1Opt.map(_.value).map(_visit).getOrElse(0),
              e1Loc = e1Opt.map(_.loc).map(_visit),
              e2Idx = e2Opt.map(_.value).map(_visit).getOrElse(0),
              e2Loc = e2Opt.map(_.loc).map(_visit),
              e3Idx = e3Opt.map(_.value).map(_visit).getOrElse(0),
              e3Loc = e3Opt.map(_.loc).map(_visit),
              bodyIdx = _visit(body),
              bodyLoc = Some(_visit(bodyLoc))))
        case GotoStmt(L(idLoc, TokId(id))) =>
          p.Statement.Stmt.GotoS(
            p.Statement.Goto(
              id = id,
              idLoc = Some(_visit(idLoc))))
        case Continue => p.Statement.Stmt.ContinueS(p.Statement.Continue())
        case Break => p.Statement.Stmt.BreakS(p.Statement.Break())
        case Return(eOpt) =>
          p.Statement.Stmt.ReturnS(
            p.Statement.Return(
              eIdx = eOpt.map(_.value).map(_visit).getOrElse(0),
              eLoc = eOpt.map(_.loc).map(_visit)))
      }
    _getIdx(p.Statement(stmt = stmt))
  }

  def _visit(s: CompoundStmt): p.Statement.Compound = {
    p.Statement.Compound(
      dls = s.dl.map(_.value).map(_visit),
      dlLocs = s.dl.map(_.loc).map(_visit),
      stmtIdxes = s.sl.map(_.value).map(_visit),
      stmtLocs = s.sl.map(_.loc).map(_visit)
    )
  }

  def _visit(tp: TypeName): p.TypeName = {
    p.TypeName(
      spQls = tp.sqs.map {
        case L(spLoc, Left(sp)) =>
          p.TypeName.SpQl(
            elem = p.TypeName.SpQl.Elem.Sp(_visit(sp)),
            loc = Some(_visit(spLoc)))
        case L(qlLoc, Right(ql)) =>
          p.TypeName.SpQl(
            elem = p.TypeName.SpQl.Elem.Ql(_visit(ql)),
            loc = Some(_visit(qlLoc)))
      },
      ad = tp.ad.map(_.value).map(_visit),
      adLoc = tp.ad.map(_.loc).map(_visit))
  }

  // Just keeping it simple since the current design of Located is troublesome.
  def _visit(loc: Loc): p.Loc = {
    loc match {
      case LocPoint((line, col), fileName) =>
        val r = p.Loc.Range(
          lineBegin = line,
          colBegin = col,
          fileName = fileName.getOrElse(""))
        p.Loc(levels = Seq(r))
      case LocRange(LocPoint((line, col), fileName), _) =>
        val r = p.Loc.Range(
          lineBegin = line,
          colBegin = col,
          fileName = fileName.getOrElse(""))
        p.Loc(levels = Seq(r))
    }
  }
}
