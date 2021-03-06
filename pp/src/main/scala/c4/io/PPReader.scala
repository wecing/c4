package c4.io

import java.text.SimpleDateFormat
import java.util.{Date, Calendar}

import c4.messaging.{SimpleMessage, IllegalSourceException, Message}
import c4.util.legacy.{Located => L}
import c4.util.TextUtils

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator.JavaTokenParsers

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
  // for each "x ## y", we only keep the "## y" part here
  final case class ConcatPart(arg: L[String]) extends BodyPart
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
          case (tok1 @ L(loc1, PPTokSym("##"), _))
              :: (tok2 @ L(loc2, PPTokId(id2), fileName))
              :: xs =>
            recur(xs, accum :+ ConcatPart(L(loc2, id2, fileName)))
          // "x## y"
          case (tok1 @ L(loc1, PPTokSym("##"), _))
              :: L(_, PPTokWhiteSpc(_), _)
              :: (tok2 @ L(loc2, PPTokId(id2), fileName))
              :: xs =>
            recur(xs, accum :+ ConcatPart(L(loc2, id2, fileName)))
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
          case tok :: xs =>
            recur(xs, accum :+ TokPart(tok))
        }
      }

      // sanity check
      rawBody.find {
        case L(_, PPTokWhiteSpc(_), _) => false
        case _                         => true
      } match {
        case Some(L(loc, PPTokSym("##"), fileName)) =>
          throw IllegalSourceException(
            SimpleMessage(
              fileName.getOrElse(ctx.logicalFileName),
              loc,
              "Token '##' not preceded by other tokens"
            )
          )
        case _ => ()
      }

      recur(rmExtraSpaces(rawBody), Seq.empty)
    }
  }

  sealed private class ObjMacro(rawBody: Seq[L[PPTok]]) {
    val bodyTokens: Seq[PPTok] = {
      def merge(t1: PPTok, t2: PPTok): PPTok = {
        def f(t: PPTok): String = {
          t match {
            case PPTokId(id)      => id
            case PPTokChar(repr)  => repr
            case PPTokNum(num)    => num
            case PPTokStr(repr)   => repr
            case PPTokSym(sym)    => sym
            case PPTokWhiteSpc(_) => ??? // unreachable
          }
        }
        val t = f(t1) + f(t2)

        val idPattern = "^[a-zA-Z_][a-zA-Z0-9_]*$".r
        val numPattern =
          "^(([0-9]*[.][0-9]+)|([0-9]+[.]?))[0-9a-zA-Z+-]*$".r
        if (idPattern.matches(t)) {
          PPTokId(t)
        } else if (numPattern.matches(t)) {
          PPTokNum(t)
        } else {
          PPTokSym(t)
        }
      }
      def recur(tokens: Seq[PPTok], accumRev: Seq[PPTok]): Seq[PPTok] = {
        (accumRev, tokens) match {
          // x ## y
          case (
                PPTokWhiteSpc(_) :: t1 :: acc,
                PPTokSym("##") :: PPTokWhiteSpc(_) :: t2 :: ts
              ) => {
            recur(ts, merge(t1, t2) :: acc)
          }
          // x## y
          case (t1 :: acc, PPTokSym("##") :: PPTokWhiteSpc(_) :: t2 :: ts) => {
            recur(ts, merge(t1, t2) :: acc)
          }
          // x ##y
          case (PPTokWhiteSpc(_) :: t1 :: acc, PPTokSym("##") :: t2 :: ts) => {
            recur(ts, merge(t1, t2) :: acc)
          }
          // x##y
          case (t1 :: acc, PPTokSym("##") :: t2 :: ts) => {
            recur(ts, merge(t1, t2) :: acc)
          }
          // other cases
          case (acc, t :: ts) => recur(ts, t +: acc)
          case (acc, Seq())   => acc
        }
      }
      val sanitized: Seq[L[PPTok]] = rmExtraSpaces(rawBody)
      sanitized.headOption
        .filter(_.value == PPTokSym("##"))
        .orElse(sanitized.lastOption.filter(_.value == PPTokSym("##")))
        .map {
          case L(loc, _, fileName) =>
            throw IllegalSourceException(
              SimpleMessage(
                fileName.getOrElse("<unknown>"),
                loc,
                "Token '##' not preceded or followed by other tokens"
              )
            )
        }
      recur(sanitized.map(_.value), Seq.empty).reverse
    }

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
      val fileName: String,
      predefMacros: Map[String, String]
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
      DateMacro.predefinedName.get -> Right(DateMacro),
      // predef macros used by x64 linux but not part of C89 spec
      "__amd64__" -> Right(new ObjMacro(Seq(L((0, 0), PPTokNum("1"), None)))),
      "__amd64" -> Right(new ObjMacro(Seq(L((0, 0), PPTokNum("1"), None)))),
      "__x86_64__" -> Right(new ObjMacro(Seq(L((0, 0), PPTokNum("1"), None)))),
      "__x86_64" -> Right(new ObjMacro(Seq(L((0, 0), PPTokNum("1"), None)))),
      "__STDC__" -> Right(new ObjMacro(Seq(L((0, 0), PPTokNum("1"), None)))),
      "__linux__" -> Right(new ObjMacro(Seq(L((0, 0), PPTokNum("1"), None))))
    ) ++ predefMacros.map {
      case (k, v) => {
        val idPattern = "^[a-zA-Z_][a-zA-Z0-9_]*$".r
        val numPattern =
          "^(([0-9]*[.][0-9]+)|([0-9]+[.]?))[0-9a-zA-Z+-]*$".r
        val tok = if (idPattern.matches(v)) {
          PPTokId(v)
        } else if (numPattern.matches(v)) {
          PPTokNum(v)
        } else {
          throw new NotImplementedError(
            s"Complex cmdline macro ${k} not supported"
          )
        }
        (k -> Right(new ObjMacro(Seq(L((0, 0), tok, None)))))
      }
    }

    // multi-include optimization: see
    // https://gcc.gnu.org/onlinedocs/cppinternals/Guard-Macros.html
    //
    // file name -> (guard name, is guard valid)
    val includeGuards: mutable.Map[String, (Option[String], Boolean)] =
      mutable.Map()
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

      // process a token when status is None
      def processTokSimple(accum: Queue[T], tok: T): Queue[T] = {
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
      }

      val ret: Queue[T] =
        tokens.foldLeft(Queue.empty[T]) { (accum: Queue[T], tok: T) =>
          if (foundMacro) { accum :+ tok }
          else
            status match {
              case None => processTokSimple(accum, tok)
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
                        ctx.macros
                          .get(s.funcName.value)
                          .flatMap(_.left.toOption)
                          .get
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
                      // values of `argsMap` and `argsMapRaw` could be empty Seq

                      def expandTokPart(p: TokPart): T = {
                        p.tok.value match {
                          case PPTokId(id) =>
                            Left(
                              (
                                Set(s.funcName.value),
                                L(s.funcName.loc, id, s.funcName.fileName)
                              )
                            )

                          case t: PPTok =>
                            Right(
                              L(s.funcName.loc, t, s.funcName.fileName)
                            )
                        }
                      }

                      def expandToStrPart(p: ToStrPart): T = {
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
                        Right(
                          L(
                            s.funcName.loc,
                            PPTokStr("\"" + str + "\""),
                            s.funcName.fileName
                          )
                        )
                      }

                      def expandConcatPart(
                          accumOrig: Seq[T],
                          p: ConcatPart
                      ): Seq[T] = {
                        val cntTaken: Int =
                          1 + accumOrig.reverse.indexWhere {
                            case Right(L(_, PPTokWhiteSpc(_), _)) => false
                            case _                                => true
                          }
                        val accum: Seq[T] = accumOrig.dropRight(cntTaken)
                        val arg1Orig: Seq[T] = accumOrig.takeRight(cntTaken)

                        val arg1 = trim(arg1Orig)
                        val arg2: Seq[T] = argsMapRaw.getOrElse(
                          p.arg.value,
                          Seq(
                            Left( // not arg, just an identifier
                              Set.empty[String],
                              L(
                                s.funcName.loc,
                                p.arg.value,
                                s.funcName.fileName
                              )
                            )
                          )
                        )

                        val newLocAndFileName
                            : Option[((Int, Int), Option[String])] =
                          arg1.lastOption.orElse(arg2.headOption).map {
                            case Left((_, x)) => (x.loc, x.fileName)
                            case Right(x)     => (x.loc, x.fileName)
                          }
                        val newIgnoreSet: Set[String] = {
                          def f(t: Option[T]): Set[String] = {
                            t match {
                              case None               => Set.empty
                              case Some(Left((x, _))) => x
                              case Some(Right(_))     => Set.empty
                            }
                          }
                          f(arg1.lastOption) ++ f(arg2.headOption)
                        }
                        val newTok: Option[String] = {
                          def f(t: T): String = {
                            t match {
                              case Left((_, x)) => x.value
                              case Right(x) =>
                                x.value match {
                                  case PPTokChar(repr)  => repr
                                  case PPTokNum(num)    => num
                                  case PPTokStr(repr)   => repr
                                  case PPTokSym(sym)    => sym
                                  case PPTokWhiteSpc(_) => ??? // unreachable
                                  case PPTokId(_)       => ??? // unreachable
                                }
                            }
                          }
                          val t1: Option[String] = arg1.lastOption.map(f)
                          val t2: Option[String] = arg2.headOption.map(f)
                          (t1, t2) match {
                            case (None, None) => None
                            case _ =>
                              Some(t1.getOrElse("") + t2.getOrElse(""))
                          }
                        }
                        val idPattern = "^[a-zA-Z_][a-zA-Z0-9_]*$".r
                        val numPattern =
                          "^(([0-9]*[.][0-9]+)|([0-9]+[.]?))[0-9a-zA-Z+-]*$".r

                        val newT: Seq[T] =
                          newLocAndFileName
                            .zip(newTok)
                            .map {
                              case ((loc, fileName), tok) => {
                                if (idPattern.matches(tok)) {
                                  Left(
                                    (newIgnoreSet, L(loc, tok, fileName))
                                  )
                                } else if (numPattern.matches(tok)) {
                                  Right(L(loc, PPTokNum(tok), fileName))
                                } else {
                                  // Invalid newly formed tokens will be
                                  // rejected when trying to convert PPTokSym
                                  // to C symbols
                                  Right(L(loc, PPTokSym(tok), fileName))
                                }
                              }
                            }
                            .map(x => Seq(x))
                            .getOrElse(Seq.empty)
                        accum ++ arg1.dropRight(1) ++ newT ++ arg2.drop(1)
                      }

                      val expanded: Seq[T] = funcMacro.bodyParts.foldLeft(
                        Seq.empty[T]
                      )((accum: Seq[T], p: BodyPart) =>
                        p match {
                          case p: TokPart    => accum :+ expandTokPart(p)
                          case p: ToStrPart  => accum :+ expandToStrPart(p)
                          case p: ConcatPart => expandConcatPart(accum, p)
                          case p: ArgPart =>
                            accum ++ argsMap.get(p.arg.value).get.map {
                              case Left((ignoreSet, name)) =>
                                Left((ignoreSet + s.funcName.value, name))
                              case x => x
                            }
                        }
                      )
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
                  case x => {
                    // func macro name not followed by LPAREN is acceptable; it
                    // just does not trigger func macro invocation, e.g.:
                    //    #define f(x) +1
                    //    int f = 41;
                    //    return f f("unused"); // 42
                    status = None
                    processTokSimple(
                      accum :+ Left(s.funcNameIgnoreSet -> s.funcName),
                      x
                    )
                  }
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
    // prePP: handle "defined(x)" in #if
    val prePP: Seq[L[PPTok]] = {
      def recur(
          accumRev: Seq[L[PPTok]],
          tokens: Seq[L[PPTok]]
      ): Seq[L[PPTok]] = {
        tokens match {
          case L(loc, PPTokId("defined"), fileName)
              :: L(_, PPTokId(id), _)
              :: xs => {
            val n =
              if (ctx.macros.contains(id)) {
                "1"
              } else {
                "0"
              }
            recur(L(loc, PPTokNum(n), fileName) +: accumRev, xs)
          }
          case L(loc, PPTokId("defined"), fileName)
              :: L(_, PPTokSym("("), _)
              :: L(_, PPTokId(id), _)
              :: L(_, PPTokSym(")"), _)
              :: xs => {
            val n =
              if (ctx.macros.contains(id)) {
                "1"
              } else {
                "0"
              }
            recur(L(loc, PPTokNum(n), fileName) +: accumRev, xs)
          }
          case x :: xs => recur(x +: accumRev, xs)
          case Nil     => accumRev
        }
      }
      def rmSpaces(ts: Seq[L[PPTok]]): Seq[L[PPTok]] = {
        ts.filter {
          case L(_, PPTokWhiteSpc(_), _) => false
          case _                         => true
        }
      }
      recur(Seq.empty, rmSpaces(tokens)).reverse
    }
    // postPP: pp + replace undefined ids with 0
    val postPP: Seq[L[PPTok]] = pp(ctx, prePP).map {
      case L(loc, PPTokId(_), fileName) => L(loc, PPTokNum("0"), fileName)
      case x                            => x
    }
    // preEval: convert to just numbers and operators, not yet parsed
    sealed abstract class EvalToken
    final case class Num(num: Long, signed: Boolean) extends EvalToken
    final case class Sym(sym: String) extends EvalToken
    val preEval: Seq[L[EvalToken]] = postPP.flatMap {
      case L(_, PPTokId(_), _)       => ??? // unreachable
      case L(_, PPTokWhiteSpc(_), _) => None // may be introduced by macros
      case L(loc, PPTokStr(_), fileName) =>
        throw IllegalSourceException(
          SimpleMessage(
            fileName.getOrElse(ctx.logicalFileName),
            loc,
            "Invalid preprocessing token"
          )
        )
      case t @ L(loc, PPTokNum(num), fileName) => {
        val octalPattern = "^(0[0-7]*)[ulUL]*$".r
        val decimalPattern = "^([1-9][0-9]*)[ulUL]*$".r
        val hexPattern = "^0[xX]([0-9a-fA-F]+)[ulUL]*$".r
        val n: BigInt =
          try {
            num match {
              case octalPattern(n) =>
                BigInt(n, 8)
              case decimalPattern(n) =>
                BigInt(n, 10)
              case hexPattern(n) =>
                BigInt(n, 16)
            }
          } catch {
            case e: java.lang.NumberFormatException =>
              throw IllegalSourceException(
                SimpleMessage(
                  fileName.getOrElse(ctx.logicalFileName),
                  loc,
                  "Invalid preprocessing token"
                )
              )
          }
        Some(t.copy(value = Num(n.longValue, n <= Long.MaxValue)))
      }
      case t @ L(_, PPTokChar(repr), _) => {
        val c: Char = TextUtils.fromCharReprQ(t.copy(value = repr))
        Some(t.copy(value = Num(c.toInt, true)))
      }
      case t @ L(_, PPTokSym(sym), _) => Some(t.copy(value = Sym(sym)))
    }
    // TODO: implement const expr eval: location, sign, efficiency
    val exprStr = preEval
      .map(_.value)
      .map {
        case Num(x, signed) => s"${signed.toString.toUpperCase} $x"
        case Sym(s)         => s
      }
      .mkString(" ")
    object ExprEvaluator extends JavaTokenParsers {
      def num: Parser[Num] =
        ident ~ wholeNumber ^^ evalNum | "(" ~> expr <~ ")"

      def prefix: Parser[Num] = rep("!" | "+" | "-" | "~") ~ num ^^ evalUnary
      def mul: Parser[Num] = prefix ~ rep(("*" | "/" | "%") ~ prefix) ^^ evalBin
      def add: Parser[Num] = mul ~ rep(("+" | "-") ~ mul) ^^ evalBin
      def sh: Parser[Num] = add ~ rep(("<<" | ">>") ~ add) ^^ evalBin
      def rel: Parser[Num] = sh ~ rep(("<=" | "<" | ">=" | ">") ~ sh) ^^ evalBin
      def eq: Parser[Num] = rel ~ rep(("==" | "!=") ~ rel) ^^ evalBin
      def bAnd: Parser[Num] = eq ~ rep("&" ~ eq) ^^ evalBin
      def bXor: Parser[Num] = bAnd ~ rep("^" ~ bAnd) ^^ evalBin
      def bOr: Parser[Num] = bXor ~ rep("|" ~ bXor) ^^ evalBin
      def lAnd: Parser[Num] = bOr ~ rep("&&" ~ bOr) ^^ evalBin
      def lOr: Parser[Num] = lAnd ~ rep("||" ~ lAnd) ^^ evalBin
      def ter: Parser[Num] = rep((lOr <~ "?") ~ (lOr <~ ":")) ~ lOr ^^ evalTer
      def expr = ter

      def evalNum: String ~ String => Num = {
        case id ~ n => Num(n.toLong, id == "TRUE")
      }
      def evalUnary: List[String] ~ Num => Num = {
        case ops ~ n =>
          ops.foldRight(n) {
            case ("!", n) => toN(n.num == 0)
            case ("+", n) => Num(n.num, true)
            case ("-", n) => Num(-n.num, true)
            case ("~", n) => Num(~n.num, false)
            case _        => ??? // unreachable
          }
      }
      def evalBin: Num ~ List[String ~ Num] => Num = {
        case n ~ xs =>
          xs.foldLeft(n) {
            case (x, "+" ~ y)  => Num(x.num + y.num, x.signed || y.signed)
            case (x, "-" ~ y)  => Num(x.num - y.num, x.signed || y.signed)
            case (x, "*" ~ y)  => Num(x.num * y.num, x.signed || y.signed)
            case (x, "/" ~ y)  => Num(x.num / y.num, x.signed || y.signed)
            case (x, "%" ~ y)  => Num(x.num % y.num, x.signed || y.signed)
            case (x, "<<" ~ y) => Num(x.num << y.num, x.signed || y.signed)
            case (x, ">>" ~ y) => Num(x.num >> y.num, x.signed || y.signed)
            case (x, "<" ~ y)  => toN(x.num < y.num)
            case (x, "<=" ~ y) => toN(x.num <= y.num)
            case (x, ">" ~ y)  => toN(x.num > y.num)
            case (x, ">=" ~ y) => toN(x.num >= y.num)
            case (x, "==" ~ y) => toN(x.num == y.num)
            case (x, "!=" ~ y) => toN(x.num != y.num)
            case (x, "&" ~ y)  => Num(x.num & y.num, false)
            case (x, "^" ~ y)  => Num(x.num ^ y.num, false)
            case (x, "|" ~ y)  => Num(x.num | y.num, false)
            case (x, "&&" ~ y) => toN(x.num != 0 && y.num != 0)
            case (x, "||" ~ y) => toN(x.num != 0 || y.num != 0)
            case (_, _ ~ _)    => ??? // unreachable
          }
      }
      // desired behavior: x1 ? y1 : (x2 ? y2 : (x3 ? y3 : z))
      def evalTer: List[Num ~ Num] ~ Num => Num = {
        case xs ~ end =>
          xs.find { case c ~ _ => c.num != 0 }
            .map { case _ ~ x => x }
            .getOrElse(end)
      }
      def toN(b: Boolean): Num = Num(if (b) 1 else 0, true)

      def eval(s: String): Boolean =
        parseAll(expr, s) match {
          case Error(msg, _) =>
            throw IllegalSourceException(
              SimpleMessage(
                preEval.head.fileName.getOrElse(ctx.fileName),
                preEval.head.loc,
                msg
              )
            )
          case Failure(msg, _) =>
            throw IllegalSourceException(
              SimpleMessage(
                preEval.head.fileName.getOrElse(ctx.fileName),
                preEval.head.loc,
                msg
              )
            )
          case Success(num, _) => num.num != 0
        }
    }
    ExprEvaluator.eval(exprStr)
  }

  private def doPPCmd(ctx: PPReaderCtx, cmd: PPLine): Unit = {
    cmd match {
      case x: PPLineTokens =>
        throw new RuntimeException("programming error")
      case x: PPLineInclude =>
        throw new RuntimeException("programming error")
      case x: PPLineIncludeTokens =>
        throw new RuntimeException("programming error")
      case x: PPLineIf =>
        // invalidate include guards
        if (ctx.ifStatusStack.isEmpty) {
          ctx.includeGuards.put(ctx.ppLineReader.fileName, (None, false))
        }

        // nested #if
        if (ctx.ifStatusStack.isEmpty || ctx.ifStatusStack.head._1) {
          val b: Boolean = ppEvalConstBoolExpr(ctx, x.tokens)
          ctx.ifStatusStack.push((b, b, false))
        } else {
          ctx.ifStatusStack.push((false, true, false))
        }
      case x: PPLineIfdef =>
        // invalidate include guards
        if (ctx.ifStatusStack.isEmpty) {
          ctx.includeGuards.put(ctx.ppLineReader.fileName, (None, false))
        }

        if (ctx.ifStatusStack.isEmpty || ctx.ifStatusStack.head._1) {
          val b: Boolean = ctx.macros.contains(x.name.value)
          ctx.ifStatusStack.push((b, b, false))
        } else {
          ctx.ifStatusStack.push((false, true, false))
        }
      case x: PPLineIfndef =>
        // update or invalidate include guards
        if (ctx.ifStatusStack.isEmpty) {
          ctx.includeGuards.get(ctx.ppLineReader.fileName) match {
            case Some((None, true)) =>
              ctx.includeGuards.put(
                ctx.ppLineReader.fileName,
                (Some(x.name.value), true)
              )
            case _ =>
              ctx.includeGuards.put(ctx.ppLineReader.fileName, (None, false))
          }
        }

        if (ctx.ifStatusStack.isEmpty || ctx.ifStatusStack.head._1) {
          val b: Boolean = !ctx.macros.contains(x.name.value)
          ctx.ifStatusStack.push((b, b, false))
        } else {
          ctx.ifStatusStack.push((false, true, false))
        }
      case x: PPLineElif =>
        // invalidate include guards
        if (ctx.ifStatusStack.length <= 1) {
          ctx.includeGuards.put(ctx.ppLineReader.fileName, (None, false))
        }

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
        // invalidate include guards
        if (ctx.ifStatusStack.length <= 1) {
          ctx.includeGuards.put(ctx.ppLineReader.fileName, (None, false))
        }

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
        // no need to invalidate include guards

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
        // invalidate include guards
        if (ctx.ifStatusStack.isEmpty) {
          ctx.includeGuards.put(ctx.ppLineReader.fileName, (None, false))
        }

        if (ctx.ifStatusStack.isEmpty || ctx.ifStatusStack.head._1) {
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
        }
      case x: PPLineDefineFunc =>
        // invalidate include guards
        if (ctx.ifStatusStack.isEmpty) {
          ctx.includeGuards.put(ctx.ppLineReader.fileName, (None, false))
        }

        if (ctx.ifStatusStack.isEmpty || ctx.ifStatusStack.head._1) {
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
        }
      case x: PPLineUndef =>
        // invalidate include guards
        if (ctx.ifStatusStack.isEmpty) {
          ctx.includeGuards.put(ctx.ppLineReader.fileName, (None, false))
        }

        if (ctx.ifStatusStack.isEmpty || ctx.ifStatusStack.head._1) {
          if (ctx.macros.contains(x.name.value)) {
            ctx.macros.remove(x.name.value)
            // seems like it's common to #undef macros unconditionally
            //
            // } else {
            //   throw IllegalSourceException(
            //     SimpleMessage(
            //       ctx.logicalFileName,
            //       (x.loc._1 + ctx.logicalLineNumOffset, x.loc._2),
            //       s"macro ${x.name.value} is not defined"
            //     )
            //   )
          }
        }
      case x: PPLineLine =>
        // invalidate include guards
        if (ctx.ifStatusStack.isEmpty) {
          ctx.includeGuards.put(ctx.ppLineReader.fileName, (None, false))
        }

        if (ctx.ifStatusStack.isEmpty || ctx.ifStatusStack.head._1) {
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
        }
      case x: PPLineError =>
        // no need to invalidate include guards

        if (ctx.ifStatusStack.isEmpty || ctx.ifStatusStack.head._1) {
          throw IllegalSourceException(
            SimpleMessage(
              ctx.logicalFileName,
              (x.loc._1 + ctx.logicalLineNumOffset, x.loc._2),
              x.msg
            )
          )
        }
      case x: PPLinePragma => {
        // invalidate include guards
        if (ctx.ifStatusStack.isEmpty) {
          ctx.includeGuards.put(ctx.ppLineReader.fileName, (None, false))
        }
        // do nothing
      }
      case x: PPLineNull => {
        // invalidate include guards
        if (ctx.ifStatusStack.isEmpty) {
          ctx.includeGuards.put(ctx.ppLineReader.fileName, (None, false))
        }
        // do nothing
      }
    }
  }

  private def read(
      ctx: PPReaderCtx,
      accum: Seq[L[PPTok]],
      toBeExpanded: Seq[L[PPTok]]
  ): Seq[L[PPTok]] = {
    ctx.ppLineReader.read() map {
      // convert #include pp-tokens to #include "..."
      case ppCmd: PPLineIncludeTokens
          if (ctx.ifStatusStack.isEmpty || ctx.ifStatusStack.head._1) => {
        pp(ctx, ppCmd.tokens) filter {
          case L(_, PPTokWhiteSpc(_), _) => false
          case _                         => true
        } match {
          case Seq(located @ L(_, PPTokStr(s), _)) => {
            val str = located.copy(value = s)
            PPLineInclude(
              ppCmd.loc,
              str.copy(value = TextUtils.fromStrReprQ(str)),
              false
            )
          }
          case _ => {
            // 3.8.2: The method by which a sequence of preprocessing tokens
            // between a < and a > preprocessing token pair or a pair of
            // characters is combined into a single header name preprocessing
            // token is implementation-defined.
            throw IllegalSourceException(
              SimpleMessage(
                ctx.logicalFileName,
                ppCmd.loc,
                s"Unrecognized #include invocation"
              )
            )
          }
        }
      }
      case ppCmd: PPLineIncludeTokens => PPLineTokens(Seq())
      case x                          => x
    } match {
      case None =>
        ctx.ppLineReader.close()
        accum ++ pp(ctx, toBeExpanded)
      case Some(PPLineTokens(tokens)) =>
        // invalidate include guards
        if (
          ctx.ifStatusStack.isEmpty && tokens
            .find(!_.value.isInstanceOf[PPTokWhiteSpc])
            .isDefined
        ) {
          ctx.includeGuards.put(ctx.ppLineReader.fileName, (None, false))
        }

        if (ctx.ifStatusStack.isEmpty || ctx.ifStatusStack.head._1) {
          read(ctx, accum, toBeExpanded ++ tokens)
        } else {
          read(ctx, accum, toBeExpanded)
        }
      case Some(ppCmd: PPLineInclude) =>
        // invalidate include guards
        if (ctx.ifStatusStack.isEmpty) {
          ctx.includeGuards.put(ctx.ppLineReader.fileName, (None, false))
        }

        if (ctx.ifStatusStack.isEmpty || ctx.ifStatusStack.head._1) {
          val newFileName =
            SearchPath.find(ctx.fileName, ppCmd.name.value, ppCmd.isCaret)
          if (newFileName.isEmpty) {
            throw IllegalSourceException(
              SimpleMessage(
                ctx.logicalFileName,
                (
                  ppCmd.name.loc._1 + ctx.logicalLineNumOffset,
                  ppCmd.name.loc._2
                ),
                s"File ${ppCmd.name.value} is not found"
              )
            )
          }

          val oldPPLineReader = ctx.ppLineReader
          val oldLogicalFileNameRepr = ctx.logicalFileNameRepr
          val oldLogicalFileName = ctx.logicalFileName
          val oldIfStatusStack = ctx.ifStatusStack

          val guardFound = ctx.includeGuards.get(newFileName.get) match {
            case Some((Some(g), true)) => ctx.macros.contains(g)
            case Some(_)               => false
            case None => {
              ctx.includeGuards.put(newFileName.get, (None, true))
              false
            }
          }

          if (guardFound) {
            read(ctx, accum ++ pp(ctx, toBeExpanded), Seq.empty)
          } else {
            ctx.ppLineReader = new PPLineReader(ctx.warnings, newFileName.get)
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
          }
        } else {
          read(ctx, accum, toBeExpanded)
        }
      case Some(ppCmd) =>
        val newAccum: Seq[L[PPTok]] = accum ++ pp(ctx, toBeExpanded)
        doPPCmd(ctx, ppCmd)
        read(ctx, newAccum, Seq.empty)
    }
  }

  def read(
      warnings: ArrayBuffer[Message],
      fileName: String,
      predefMacros: Map[String, String]
  ): Seq[L[PPTok]] = {
    read(
      new PPReaderCtx(warnings, fileName, predefMacros),
      Seq.empty,
      Seq.empty
    )
  }
}
