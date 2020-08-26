package c4.io

import java.text.SimpleDateFormat
import java.util.{Date, Calendar}

import c4.messaging.{SimpleMessage, IllegalSourceException, Message}
import c4.util.legacy.{Located => L}
import c4.util.TextUtils

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object PPReader {

  // remove PPTokWhiteSpc's at beginning/end,
  // combine consecutive PPTokWhiteSpc's
  private def rmExtraSpaces(tokens: Seq[L[PPTok]]): Seq[L[PPTok]] = {
    tokens
      .dropWhile(_.value.isInstanceOf[PPTokWhiteSpc])
      .reverse
      .dropWhile(_.value.isInstanceOf[PPTokWhiteSpc])
      .reverse
      .foldLeft(Seq.empty[L[PPTok]])((accum: Seq[L[PPTok]], tok: L[PPTok]) => {
        if (
          accum.isEmpty
          || !accum.last.value.isInstanceOf[PPTokWhiteSpc]
          || !tok.value.isInstanceOf[PPTokWhiteSpc]
        ) {
          accum :+ tok
        } else {
          accum
        }
      })
  }

  // those case classes should be defined inside FuncMacro, but...
  sealed abstract class BodyPart
  final case class TokPart(tok: L[PPTok]) extends BodyPart
  final case class ToStrPart(arg: L[String]) extends BodyPart
  final case class ConcatPart(arg1: L[String], arg2: L[String]) extends BodyPart
  final case class ArgPart(arg: L[String]) extends BodyPart

  private final class FuncMacro(
      ctx: PPReaderCtx,
      rawArgNames: Seq[L[String]],
      rawBody: Seq[L[PPTok]]
  ) {

    val argNames: Seq[String] = rawArgNames.map(_.value)
    val bodyTokens: Seq[PPTok] = rmExtraSpaces(rawBody).map(_.value)

    override def equals(other: Any): Boolean =
      other match {
        case that: FuncMacro =>
          argNames == that.argNames && bodyTokens == that.bodyTokens
        case _ => false
      }

    val bodyParts: Seq[BodyPart] = {
      val argNamesSet: Set[String] = Set(argNames: _*)
      def recur(tokens: Seq[L[PPTok]], accum: Seq[BodyPart]): Seq[BodyPart] = {
        // TODO: very-large-scale-copy-and-paste detected
        tokens match {
          case Seq() => accum
          // "#x"
          case (tok1 @ L(loc1, PPTokSym("#"), fileName))
              :: (tok2 @ L(loc2, PPTokId(id), _))
              :: xs =>
            if (argNamesSet.contains(id)) {
              recur(xs, accum :+ ToStrPart(L(loc1, id, fileName)))
            } else {
              ctx.warnings += SimpleMessage(
                fileName.getOrElse(ctx.logicalFileName),
                loc2,
                s"Non-arg id $id appeared after '#' in function-like macro"
              )
              recur(xs, accum :+ TokPart(tok1) :+ TokPart(tok2))
            }
          // "# x"
          case (tok1 @ L(loc1, PPTokSym("#"), fileName))
              :: L(_, PPTokWhiteSpc(_), _)
              :: (tok2 @ L(loc2, PPTokId(id), _))
              :: xs =>
            if (argNamesSet.contains(id)) {
              recur(xs, accum :+ ToStrPart(L(loc1, id, fileName)))
            } else {
              ctx.warnings += SimpleMessage(
                fileName.getOrElse(ctx.logicalFileName),
                loc2,
                s"Non-arg id $id appeared after '#' in function-like macro"
              )
              recur(xs, accum :+ TokPart(tok1) :+ TokPart(tok2))
            }
          // "x##y"
          case (tok1 @ L(loc1, PPTokId(id1), fileName))
              :: (tok2 @ L(loc2, PPTokSym("##"), _))
              :: (tok3 @ L(loc3, PPTokId(id3), _))
              :: xs =>
            recur(
              xs,
              accum :+
                ConcatPart(L(loc1, id1, fileName), L(loc3, id3, fileName))
            )
          // "x ##y"
          case (tok1 @ L(loc1, PPTokId(id1), fileName))
              :: L(_, PPTokWhiteSpc(_), _)
              :: (tok2 @ L(loc2, PPTokSym("##"), _))
              :: (tok3 @ L(loc3, PPTokId(id3), _))
              :: xs =>
            recur(
              xs,
              accum :+
                ConcatPart(L(loc1, id1, fileName), L(loc3, id3, fileName))
            )
          // "x## y"
          case (tok1 @ L(loc1, PPTokId(id1), fileName))
              :: (tok2 @ L(loc2, PPTokSym("##"), _))
              :: L(_, PPTokWhiteSpc(_), _)
              :: (tok3 @ L(loc3, PPTokId(id3), _))
              :: xs =>
            recur(
              xs,
              accum :+
                ConcatPart(L(loc1, id1, fileName), L(loc3, id3, fileName))
            )
          // "x ## y"
          case (tok1 @ L(loc1, PPTokId(id1), fileName))
              :: L(_, PPTokWhiteSpc(_), _)
              :: (tok2 @ L(loc2, PPTokSym("##"), _))
              :: L(_, PPTokWhiteSpc(_), _)
              :: (tok3 @ L(loc3, PPTokId(id3), _))
              :: xs =>
            recur(
              xs,
              accum :+
                ConcatPart(L(loc1, id1, fileName), L(loc3, id3, fileName))
            )
          // arg of the func macro
          case (tok @ L(loc, PPTokId(id), fileName)) :: xs =>
            val p: BodyPart =
              if (argNamesSet.contains(id)) {
                ArgPart(L(loc, id, fileName))
              } else {
                TokPart(tok)
              }
            recur(xs, accum :+ p)
          // illegal "#x"
          case (tok @ L(loc, PPTokSym("#"), fileName)) :: xs =>
            ctx.warnings += SimpleMessage(
              fileName.getOrElse(ctx.logicalFileName),
              loc,
              "Token '#' not followed by argument of function-like macro"
            )
            recur(xs, accum :+ TokPart(tok))
          // illegal "x##y"
          case tok1 :: (tok2 @ L(loc, PPTokSym("##"), fileName)) :: xs =>
            ctx.warnings += SimpleMessage(
              fileName.getOrElse(ctx.logicalFileName),
              loc,
              "Token '##' not preceded or followed" +
                " by argument of function-like macro"
            )
            recur(xs, accum :+ TokPart(tok1) :+ TokPart(tok2))
          case tok :: xs =>
            recur(xs, accum :+ TokPart(tok))
        }
      }
      recur(rmExtraSpaces(rawBody), Seq.empty)
    }
  }

  sealed private class ObjMacro(rawBody: Seq[L[PPTok]]) {
    val bodyTokens: Seq[PPTok] = rmExtraSpaces(rawBody).map(_.value)

    def predefinedName: Option[String] = None

    override def equals(other: Any): Boolean =
      other match {
        case that: ObjMacro =>
          bodyTokens == that.bodyTokens && predefinedName == that.predefinedName
        case _ => false
      }
  }

  private object LineMacro extends ObjMacro(Seq.empty) {
    override def predefinedName: Option[String] = Some("__LINE__")
  }
  private object FileMacro extends ObjMacro(Seq.empty) {
    override def predefinedName: Option[String] = Some("__FILE__")
  }
  private object DateMacro extends ObjMacro(Seq.empty) {
    override def predefinedName: Option[String] = Some("__DATE__")
  }

  private final class PPReaderCtx(
      val warnings: ArrayBuffer[Message],
      fileName: String
  ) {
    var ppLineReader: PPLineReader = new PPLineReader(warnings, fileName)

    // __FILE__
    var logicalFileNameRepr: String = TextUtils.strReprQ(fileName)

    var logicalFileName: String = fileName

    var logicalLineNumOffset: Int = 0

    // (isCurTrue?, isTrueFound?, isElseFound?)
    var ifStatusStack: mutable.Stack[(Boolean, Boolean, Boolean)] =
      mutable.Stack()

    ///// preserved across files //////

    val macros: mutable.Map[String, Either[FuncMacro, ObjMacro]] = mutable.Map(
      LineMacro.predefinedName.get -> Right(LineMacro),
      FileMacro.predefinedName.get -> Right(FileMacro),
      DateMacro.predefinedName.get -> Right(DateMacro)
    )
  }

  // pp = macro expansion + #line processing
  private def pp(ctx: PPReaderCtx, rawTokens: Seq[L[PPTok]]): Seq[L[PPTok]] = {
    // #line transformation
    val tokens: Seq[L[PPTok]] =
      rawTokens.map(_.transform(ctx.logicalFileName, ctx.logicalLineNumOffset))
    // macro expansion
    // returns false if no macro expansion happened
    type T = Either[(Set[String], L[String]), L[PPTok]]
    def expand(tokens: Seq[T]): Seq[T] = {
      final case class FuncArgSearch(
          funcName: L[String],
          funcNameIgnoreSet: Set[String],
          lparen: Option[T],
          foundArgs: Seq[(Seq[T], T)],
          curArg: Seq[T],
          curParenDepth: Int
      )
      // when status is none, we are not searching for function's args
      var status: Option[FuncArgSearch] = None
      var foundMacro: Boolean = false

      val ret: Seq[T] =
        tokens.foldLeft(Seq.empty[T]) { (accum: Seq[T], tok: T) =>
          if (foundMacro) { accum :+ tok }
          else
            status match {
              case None =>
                tok match {
                  case Left((ignoreSet, name: L[String])) =>
                    if (ignoreSet.contains(name.value)) {
                      accum :+ tok
                    } else
                      ctx.macros.get(name.value) match {
                        case None =>
                          accum :+ tok
                        case Some(Left(funcMacro: FuncMacro)) =>
                          status = Some(
                            FuncArgSearch(
                              name,
                              ignoreSet,
                              None,
                              Seq.empty,
                              Seq.empty,
                              -1
                            )
                          )
                          // set foundMacro only when search is complete
                          accum
                        case Some(Right(objMacro: ObjMacro)) =>
                          // TODO: c89 also allows '##' to be used inside objMacros.
                          foundMacro = true
                          val located: L[Any] = tok match {
                            case Left((_, x)) => x
                            case Right(x)     => x
                          }
                          objMacro match {
                            case LineMacro =>
                              accum :+ Right(
                                L(
                                  located.loc,
                                  PPTokNum(located.loc._1.toString),
                                  located.fileName
                                )
                              )
                            case FileMacro =>
                              accum :+ Right(
                                L(
                                  located.loc,
                                  PPTokStr(ctx.logicalFileNameRepr),
                                  located.fileName
                                )
                              )
                            case DateMacro =>
                              val day: String = "%2d".format(
                                Calendar
                                  .getInstance()
                                  .get(Calendar.DAY_OF_MONTH)
                              )
                              val date: String =
                                new SimpleDateFormat("MMM %s yyyy".format(day))
                                  .format(new Date())
                              accum :+ Right(
                                L(
                                  located.loc,
                                  PPTokStr("\"" + date + "\""),
                                  located.fileName
                                )
                              )
                            case _ =>
                              accum ++ objMacro.bodyTokens.map {
                                case PPTokId(id) =>
                                  Left(
                                    (
                                      ignoreSet + name.value,
                                      L(name.loc, id, name.fileName)
                                    )
                                  )
                                case t: PPTok =>
                                  Right(L(name.loc, t, name.fileName))
                              }
                          }
                      }
                  case Right(_) =>
                    accum :+ tok
                }
              case Some(s) if s.curParenDepth >= 0 =>
                tok match {
                  case t @ Right(L(_, PPTokSym("("), _)) =>
                    status = Some(
                      s.copy(
                        curArg = s.curArg :+ t,
                        curParenDepth = s.curParenDepth + 1
                      )
                    )
                    accum
                  case t @ Right(L(_, PPTokSym(")"), _)) =>
                    if (s.curParenDepth == 0) {
                      def trim(x: Seq[T]): Seq[T] = {
                        def isWhiteSpc(t: T): Boolean =
                          t match {
                            case Right(L(_, PPTokWhiteSpc(_), _)) => true
                            case _                                => false
                          }
                        x.dropWhile(isWhiteSpc)
                          .reverse
                          .dropWhile(isWhiteSpc)
                          .reverse
                      }
                      // the argument of "F()" will be recognized as Seq(Seq()),
                      // instead of Seq.empty. this special check fixes that...
                      def checkNoArg(x: Seq[Seq[T]]): Seq[Seq[T]] = {
                        if (x == Seq(Seq.empty)) {
                          Seq.empty
                        } else {
                          x
                        }
                      }
                      // found all arguments
                      foundMacro = true
                      val args: Seq[Seq[T]] =
                        checkNoArg(s.foundArgs.map(_._1) :+ s.curArg)
                          .map(expand)
                          .map(trim)
                      val argsRaw: Seq[Seq[T]] = // not expanded yet
                        checkNoArg(s.foundArgs.map(_._1) :+ s.curArg).map(trim)
                      val funcMacro: FuncMacro =
                        ctx.macros.get(s.funcName.value).get.left.get
                      if (args.length != funcMacro.argNames.length) {
                        throw IllegalSourceException(
                          SimpleMessage(
                            ctx.logicalFileName,
                            s.funcName.loc,
                            s"Wrong number of arguments passed to" +
                              s" func-like macro ${s.funcName.value}:" +
                              s" expected ${funcMacro.argNames.length}," +
                              s" received ${args.length} (${args})"
                          )
                        )
                      }
                      val argsMap: collection.Map[String, Seq[T]] =
                        collection.Map(funcMacro.argNames.zip(args): _*)
                      val argsMapRaw: collection.Map[String, Seq[T]] =
                        collection.Map(funcMacro.argNames.zip(argsRaw): _*)
                      for ((k, v) <- argsMap.iterator) {
                        if (v.isEmpty) {
                          throw IllegalSourceException(
                            SimpleMessage(
                              ctx.logicalFileName,
                              s.funcName.loc,
                              s"Arg $k cannot be empty"
                            )
                          )
                        }
                      }
                      val expanded: Seq[T] = funcMacro.bodyParts.flatMap {
                        case p: TokPart =>
                          p.tok.value match {
                            case PPTokId(id) =>
                              Seq(
                                Left(
                                  (
                                    Set(s.funcName.value),
                                    L(s.funcName.loc, id, s.funcName.fileName)
                                  )
                                )
                              )
                            case t: PPTok =>
                              Seq(
                                Right(L(s.funcName.loc, t, s.funcName.fileName))
                              )
                          }
                        case p: ToStrPart =>
                          val str: String = {
                            var r = ""
                            var isPrevWhiteSpace = false
                            argsMapRaw.get(p.arg.value).get.foreach {
                              case Left((_, located)) =>
                                r += located.value
                                isPrevWhiteSpace = false
                              case Right(located) =>
                                located.value match {
                                  case x: PPTokWhiteSpc =>
                                    if (!isPrevWhiteSpace) {
                                      r += " "
                                      isPrevWhiteSpace = true
                                    }
                                  case x: PPTokStr =>
                                    r += x.repr
                                      .replace("\\", "\\\\") // \ => \\
                                      .replace("\"", "\\\"") // " => \"
                                    isPrevWhiteSpace = false
                                  case x: PPTokChar =>
                                    r += x.repr
                                      .replace("\\", "\\\\") // \ => \\
                                      .replace("\"", "\\\"") // " => \"
                                    isPrevWhiteSpace = false
                                  case _ =>
                                    r += located.value.raw
                                    isPrevWhiteSpace = false
                                }
                            }
                            r
                          }
                          Seq(
                            Right(
                              L(
                                s.funcName.loc,
                                PPTokStr("\"" + str + "\""),
                                s.funcName.fileName
                              )
                            )
                          )
                        case p: ConcatPart =>
                          // TODO: support other tokens as well!
                          val arg1: Seq[T] = argsMapRaw.getOrElse(
                            p.arg1.value,
                            Seq(
                              Left( // not arg, just an identifier
                                Set.empty[String],
                                L(
                                  s.funcName.loc,
                                  p.arg1.value,
                                  s.funcName.fileName
                                )
                              )
                            )
                          )
                          val arg2: Seq[T] = argsMapRaw.getOrElse(
                            p.arg2.value,
                            Seq(
                              Left( // not arg, just an identifier
                                Set.empty[String],
                                L(
                                  s.funcName.loc,
                                  p.arg2.value,
                                  s.funcName.fileName
                                )
                              )
                            )
                          )
                          val newT: T = (arg1.last, arg2.head) match {
                            case (Left((s1, x1)), Left((s2, x2))) =>
                              Left(
                                (
                                  s1 ++ s2,
                                  L(x1.loc, x1.value + x2.value, x1.fileName)
                                )
                              )
                            case (Left((s1, x1)), Right(L(_, PPTokNum(num), _)))
                                if !num.exists(".+-".contains(_)) =>
                              Left((s1, L(x1.loc, x1.value + num, x1.fileName)))
                            case _ =>
                              throw IllegalSourceException(
                                SimpleMessage(
                                  ctx.logicalFileName,
                                  s.funcName.loc,
                                  "NOT_YET_IMPLEMENTED: Args concatination in" +
                                    " func-like macros only supports identifiers"
                                )
                              )
                          }
                          (arg1.init :+ newT) ++ arg2.tail
                        case p: ArgPart =>
                          argsMap.get(p.arg.value).get.map {
                            case Left((ignoreSet, name)) =>
                              Left((ignoreSet + s.funcName.value, name))
                            case x => x
                          }
                      }
                      status = None
                      accum ++ expand(expanded)
                    } else {
                      status = Some(
                        s.copy(
                          curArg = s.curArg :+ t,
                          curParenDepth = s.curParenDepth - 1
                        )
                      )
                      accum
                    }
                  case t @ Right(L(_, PPTokSym(","), _))
                      if s.curParenDepth == 0 =>
                    status = Some(
                      s.copy(
                        foundArgs = s.foundArgs :+ (s.curArg, t),
                        curArg = Seq.empty
                      )
                    )
                    accum
                  case t =>
                    status = Some(s.copy(curArg = s.curArg :+ t))
                    accum
                }
              case Some(s) => // still searching for the first '('
                tok match {
                  case Right(L(_, PPTokWhiteSpc(_), _)) =>
                    accum // ignore white space
                  case t @ Right(L(_, PPTokSym("("), _)) =>
                    status = Some(
                      FuncArgSearch(
                        s.funcName,
                        s.funcNameIgnoreSet,
                        Some(t),
                        Seq.empty,
                        Seq.empty,
                        0
                      )
                    )
                    accum
                  case x =>
                    throw IllegalSourceException(
                      SimpleMessage(
                        ctx.logicalFileName,
                        x match {
                          case Left((_, t)) => t.loc
                          case Right(t)     => t.loc
                        },
                        s"Unexpected token ${x match {
                          case Left((_, t)) => t.value
                          case Right(t)     => t.value
                        }}; '(' expected"
                      )
                    )
                }
            }
        } ++ (status match {
          // Incomplete function-like macro invocation might be combined
          // with rest pp-tokens
          case None =>
            Seq.empty
          case Some(s: FuncArgSearch) =>
            Seq(Left((s.funcNameIgnoreSet, s.funcName))) ++
              s.lparen.map(Seq(_)).getOrElse(Seq.empty) ++
              s.foundArgs.flatMap(x => x._1 :+ x._2) ++
              s.curArg
        })
      if (foundMacro) {
        expand(ret)
      } else {
        ret
      }
    }
    def toT(tok: L[PPTok]): T =
      tok match {
        case L(loc, PPTokId(id), fileName) =>
          Left((Set.empty[String], L(loc, id, fileName)))
        case _ => Right(tok)
      }
    def fromT(t: T): L[PPTok] =
      t match {
        case Left((_, L(loc, id, fileName))) =>
          L(loc, PPTokId(id), fileName)
        case Right(x) => x
      }
    expand(tokens map toT).map(fromT)
  }

  private def ppEvalConstBoolExpr(
      ctx: PPReaderCtx,
      tokens: Seq[L[PPTok]]
  ): Boolean = {
    ??? // TODO
  }

  private def doPPCmd(ctx: PPReaderCtx, cmd: PPLine): Unit = {
    cmd match {
      case x: PPLineTokens =>
        throw new RuntimeException("programming error")
      case x: PPLineInclude =>
        throw new RuntimeException("programming error")
      case x: PPLineIf =>
        val b: Boolean = ppEvalConstBoolExpr(ctx, x.tokens)
        ctx.ifStatusStack.push((b, b, false))
      case x: PPLineIfdef =>
        val b: Boolean = ctx.macros.contains(x.name.value)
        ctx.ifStatusStack.push((b, b, false))
      case x: PPLineIfndef =>
        val b: Boolean = !ctx.macros.contains(x.name.value)
        ctx.ifStatusStack.push((b, b, false))
      case x: PPLineElif =>
        val b: Boolean = ppEvalConstBoolExpr(ctx, x.tokens)
        if (ctx.ifStatusStack.isEmpty || ctx.ifStatusStack.head._3) {
          throw IllegalSourceException(
            SimpleMessage(
              ctx.logicalFileName,
              (x.loc._1 + ctx.logicalLineNumOffset, x.loc._2),
              "unexpected #elif directive"
            )
          )
        } else if (ctx.ifStatusStack.head._2) {
          // has already found true; this branch should not be enabled anyway
          ctx.ifStatusStack.pop()
          ctx.ifStatusStack.push((false, true, false))
        } else {
          ctx.ifStatusStack.pop()
          ctx.ifStatusStack.push((b, b, false))
        }
      case x: PPLineElse =>
        if (ctx.ifStatusStack.isEmpty || ctx.ifStatusStack.head._3) {
          throw IllegalSourceException(
            SimpleMessage(
              ctx.logicalFileName,
              (x.loc._1 + ctx.logicalLineNumOffset, x.loc._2),
              "unexpected #else directive"
            )
          )
        } else {
          val foundTrue: Boolean = ctx.ifStatusStack.pop()._2
          ctx.ifStatusStack.push((!foundTrue, !foundTrue, true))
        }
      case x: PPLineEndif =>
        if (ctx.ifStatusStack.isEmpty) {
          throw IllegalSourceException(
            SimpleMessage(
              ctx.logicalFileName,
              (x.loc._1 + ctx.logicalLineNumOffset, x.loc._2),
              "unexpected #endif directive"
            )
          )
        } else {
          ctx.ifStatusStack.pop()
        }
      case x: PPLineDefineObj =>
        val objMacro: ObjMacro = new ObjMacro(x.tokens)
        ctx.macros.get(x.name.value) match {
          case None =>
            ctx.macros.put(x.name.value, Right(objMacro))
          case Some(m) =>
            if (m != Right(objMacro)) {
              throw IllegalSourceException(
                SimpleMessage(
                  ctx.logicalFileName,
                  (x.loc._1 + ctx.logicalLineNumOffset, x.loc._2),
                  s"Redefinition of macro ${x.name.value}"
                )
              )
            }
        }
      case x: PPLineDefineFunc =>
        val funcMacro: FuncMacro =
          new FuncMacro(ctx, x.argNames, x.tokens)
        ctx.macros.get(x.name.value) match {
          case None =>
            ctx.macros.put(x.name.value, Left(funcMacro))
          case Some(m) =>
            if (m != Left(funcMacro)) {
              throw IllegalSourceException(
                SimpleMessage(
                  ctx.logicalFileName,
                  (x.loc._1 + ctx.logicalLineNumOffset, x.loc._2),
                  s"Redefinition of macro ${x.name.value}"
                )
              )
            }
        }
      case x: PPLineUndef =>
        if (ctx.macros.contains(x.name.value)) {
          ctx.macros.remove(x.name.value)
        } else {
          throw IllegalSourceException(
            SimpleMessage(
              ctx.logicalFileName,
              (x.loc._1 + ctx.logicalLineNumOffset, x.loc._2),
              s"macro ${x.name.value} is not defined"
            )
          )
        }
      case x: PPLineLine =>
        // this uses the '#' as the line where #line appears; that is probably
        // incorrect, since the '#line' could be prefixed by comment blocks.
        if (x.fileNameRepr.isDefined) {
          ctx.logicalFileNameRepr = x.fileNameRepr.get.value
          // this directive is not affected by itself. :)
          // so if parsing of fileNameRepr fails, we need to use the old data
          // in ctx to report code location.
          ctx.logicalFileName = TextUtils.fromStrReprQ(
            x.fileNameRepr.get
              .transform(ctx.logicalFileName, ctx.logicalLineNumOffset)
          )
        }
        // x.loc._1 + lineNumOffset == x.line - 1
        ctx.logicalLineNumOffset = x.line - 1 - x.loc._1
      case x: PPLineError =>
        throw IllegalSourceException(
          SimpleMessage(
            ctx.logicalFileName,
            (x.loc._1 + ctx.logicalLineNumOffset, x.loc._2),
            x.msg
          )
        )
      case x: PPLinePragma => ()
      case x: PPLineNull   => ()
    }
  }

  private def read(
      ctx: PPReaderCtx,
      accum: Seq[L[PPTok]],
      toBeExpanded: Seq[L[PPTok]]
  ): Seq[L[PPTok]] = {
    ctx.ppLineReader.read() match {
      case None =>
        ctx.ppLineReader.close()
        accum ++ pp(ctx, toBeExpanded)
      case Some(PPLineTokens(tokens)) =>
        if (ctx.ifStatusStack.isEmpty || ctx.ifStatusStack.head._1) {
          read(ctx, accum, toBeExpanded ++ tokens)
        } else {
          read(ctx, accum, toBeExpanded)
        }
      case Some(ppCmd: PPLineInclude) =>
        val oldPPLineReader = ctx.ppLineReader
        val oldLogicalFileNameRepr = ctx.logicalFileNameRepr
        val oldLogicalFileName = ctx.logicalFileName
        val oldIfStatusStack = ctx.ifStatusStack
        // TODO: use PPLineReader.isCaret
        ctx.ppLineReader = new PPLineReader(ctx.warnings, ppCmd.name.value)
        ctx.logicalFileNameRepr = TextUtils.strReprQ(ppCmd.name.value)
        ctx.logicalFileName = ppCmd.name.value
        ctx.ifStatusStack = mutable.Stack()
        val newAccum: Seq[L[PPTok]] =
          read(ctx, accum ++ pp(ctx, toBeExpanded), Seq.empty)
        ctx.ppLineReader = oldPPLineReader
        ctx.logicalFileNameRepr = oldLogicalFileNameRepr
        ctx.logicalFileName = oldLogicalFileName
        ctx.ifStatusStack = oldIfStatusStack
        read(ctx, newAccum, Seq.empty)
      case Some(ppCmd) =>
        val newAccum: Seq[L[PPTok]] = accum ++ pp(ctx, toBeExpanded)
        doPPCmd(ctx, ppCmd)
        read(ctx, newAccum, Seq.empty)
    }
  }

  def read(warnings: ArrayBuffer[Message], fileName: String): Seq[L[PPTok]] = {
    read(new PPReaderCtx(warnings, fileName), Seq.empty, Seq.empty)
  }
}
