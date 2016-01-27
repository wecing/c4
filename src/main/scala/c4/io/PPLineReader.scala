package c4.io

import c4.messaging.{SimpleMessage, IllegalSourceException, Message}
import c4.util.{Located => L, TextUtils}

import scala.collection.mutable.ArrayBuffer


sealed abstract class PPTok(val raw: String)
final case class PPTokId(id: String) extends PPTok(id)
final case class PPTokNum(num: String) extends PPTok(num)
final case class PPTokChar(repr: String) extends PPTok(repr)
final case class PPTokStr(repr: String) extends PPTok(repr)
final case class PPTokSym(sym: String) extends PPTok(sym)
final case class PPTokWhiteSpc(c: Char) extends PPTok(c.toString)

sealed abstract class PPLine
final case class PPLineTokens(tokens: Seq[L[PPTok]]) extends PPLine
final case class PPLineIf(loc: (Int, Int), tokens: Seq[L[PPTok]]) extends PPLine
final case class PPLineIfdef(loc: (Int, Int), name: L[String]) extends PPLine
final case class PPLineIfndef(loc: (Int, Int), name: L[String]) extends PPLine
final case class PPLineElif(loc: (Int, Int),
                            tokens: Seq[L[PPTok]]) extends PPLine
final case class PPLineElse(loc: (Int, Int)) extends PPLine
final case class PPLineEndif(loc: (Int, Int)) extends PPLine
final case class PPLineInclude(loc: (Int, Int),
                               name: L[String],
                               isCaret: Boolean) extends PPLine
final case class PPLineDefineObj(loc: (Int, Int),
                                 name: L[String],
                                 tokens: Seq[L[PPTok]]) extends PPLine
final case class PPLineDefineFunc(loc: (Int, Int),
                                  name: L[String],
                                  argNames: Seq[L[String]],
                                  tokens: Seq[L[PPTok]]) extends PPLine
final case class PPLineUndef(loc: (Int, Int), name: L[String]) extends PPLine
final case class PPLineLine(loc: (Int, Int),
                            line: Int, // use repr here for __FILE__
                            fileNameRepr: Option[L[String]]) extends PPLine
final case class PPLineError(loc: (Int, Int), msg: String) extends PPLine
final case class PPLinePragma(loc: (Int, Int)) extends PPLine
final case class PPLineNull(loc: (Int, Int)) extends PPLine // null directive

object PPLineTokens {
  val empty: PPLineTokens = PPLineTokens(Seq.empty)
}

// at this phase, either the file is empty or each line ends with '\n'.
class PPLineReader(val warnings: ArrayBuffer[Message],
                   val fileName: String) {
  private val file: SourcePhase2Reader =
    new SourcePhase2Reader(warnings, fileName)

  def close() = file.close()

  // returns None only on EOF
  def read(): Option[PPLine] = {
    file.read() match {
      case None => None
      case Some(('\n', _)) => read() // empty line, retry on next line
      case Some((c: Char, _)) if TextUtils.isWhiteSpace(c) => read()
      case Some(p@('/', loc1)) =>
        // it is possible that there's a pp directive after block comment,
        // so we cannot simply ungetc() then readPPTokensLine() here.
        file.read().get match {
          case ('\n', _) =>
            Some(PPLineTokens(Seq(L(loc1, PPTokSym("/")))))
          case ('/', _) =>
            ignoreLineComment()
            read() // empty line, retry on next line
          case ('*', loc2) =>
            ignoreBlockComment(loc2)
            read() // treat block comment as space and continue
          case q =>
            file.ungetc(q)
            file.ungetc(p)
            Some(PPLineTokens(readPPTokensLine(false)))
        }
      case Some(('#', loc1)) =>
        Some(readPPDirective(loc1))
      case Some(p) =>
        file.ungetc(p)
        Some(PPLineTokens(readPPTokensLine(false)))
    }
  }

  // stops on seeing '\n'
  private def ignoreLineComment(): Unit = {
    file.read().get._1 match {
      case '\n' => ()
      case _ => ignoreLineComment()
    }
  }

  // stops on seeing "*/"
  private def ignoreBlockComment(fromLoc: (Int, Int)): Unit = {
    def recur(prev: (Char, (Int, Int))): Unit = {
      (prev, file.read()) match {
        case ((_, loc), None) =>
          throw new IllegalSourceException(new SimpleMessage(
            fileName, loc, "file ends in incomplete block comment"
          ))
        case (('*', _), Some(('/', _))) => ()
        case (_, Some(c)) => recur(c)
      }
    }
    recur((' ', fromLoc))
  }

  // does NOT stop on EOL/EOF
  private def takeWhile(pred: Char => Boolean): Seq[(Char, (Int, Int))] = {
    var x: (Char, (Int, Int)) = file.read().get
    var r: Seq[(Char, (Int, Int))] = Seq.empty
    while (pred(x._1)) {
      r = r :+ x
      x = file.read().get
    }
    file.ungetc(x)
    r
  }

  private def isAlpha(c: Char): Boolean = {
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
  }
  private def isDigit(c: Char): Boolean = '0' <= c && c <= '9'

  // reads one (logical) line of PPTokens, stops on seeing '\n'
  private def readPPTokensLine(inPPDirective: Boolean): Seq[L[PPTok]] = {
    def readPPNum(acc: String): String = {
      file.read().get match {
        case ('.', _) =>
          readPPNum(acc + '.')
        case (x, _) if isDigit(x) =>
          readPPNum(acc + x)
        case (x, _) if x == 'e' || x == 'E' =>
          file.read().get match {
            case (y, _) if y == '+' || y == '-' =>
              readPPNum(acc + x + y)
            case p =>
              file.ungetc(p)
              readPPNum(acc + x) // fall back to "pp-number nondigit"
          }
        case (x, _) if isAlpha(x) || x == '_' =>
          readPPNum(acc + x)
        case p =>
          file.ungetc(p)
          acc
      }
    }
    // skipping the prefix <L'>, <L">, <'>, or <">, return the rest
    // character/string literal string.
    def readCharStrLiteral(isStr: Boolean): Option[String] = {
      val endsWith: Char = if (isStr) '"' else '\''
      val simpleEscapeChars: Set[Char] = Set(
        '\'', '"', '?', '\\', 'a', 'b', 'f', 'n', 'r', 't', 'v')
      def isHexDigit(c: Char): Boolean = {
        isDigit(c) || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F')
      }
      def isOctDigit(c: Char): Boolean = '0' <= c && c <= '7'
      // Left is used to ungetc() all read characters back
      // when seeing '\n' in the middle of a str/char literal.
      def recur(consumedStack: Seq[(Char, (Int, Int))]
               ): Either[Seq[(Char, (Int, Int))], String] = {
        file.read().get match {
          case p@('\n', _) =>
            Left(p +: consumedStack)
          case (x, _) if x == endsWith =>
            Right((x +: consumedStack.map(_._1)).reverse.mkString)
          case p@('\\', _) =>
            file.read().get match {
              case q@(x, _) if simpleEscapeChars.contains(x) =>
                recur(q +: p +: consumedStack)
              case q@(x, _) if isOctDigit(x) =>
                val xs: Seq[(Char, (Int, Int))] = takeWhile(isOctDigit)
                recur(xs.reverse ++ (q +: p +: consumedStack))
              case q@('x', _) =>
                val xs: Seq[(Char, (Int, Int))] = takeWhile(isHexDigit)
                if (xs.isEmpty) {
                  Left(q +: p +: consumedStack)
                } else {
                  recur(xs.reverse ++ (q +: p +: consumedStack))
                }
              case q@(x, _) if isHexDigit(x) =>
                val xs: Seq[(Char, (Int, Int))] = takeWhile(isHexDigit)
                recur(xs.reverse ++ (q +: consumedStack))
            }
          case p =>
            recur(p +: consumedStack)
        }
      }
      recur(Seq.empty) match {
        case Left(xs) =>
          for (x <- xs) { file.ungetc(x) }
          None
        case Right(str) =>
          Some(str)
      }
    }
    def recur(accum: Seq[L[PPTok]]): Seq[L[PPTok]] = {
      file.read().get match {
        case ('\n', _) =>
          accum
        case ('/', loc1) =>
          file.read().get match {
            case ('/', _) =>
              ignoreLineComment()
              accum
            case ('*', loc2) =>
              ignoreBlockComment(loc2)
              // still on the same logical line!
              recur(accum :+ L(loc1, PPTokWhiteSpc(' ')))
            case ('=', _) =>
              recur(accum :+ L(loc1, PPTokSym("/=")))
            case p =>
              file.ungetc(p)
              recur(accum :+ L(loc1, PPTokSym("/")))
          }
        case (c1, loc1) if TextUtils.isWhiteSpace(c1) =>
          if (inPPDirective && c1 != ' ' && c1 != '\t') {
            throw IllegalSourceException(SimpleMessage(
              fileName, loc1,
              s"unexpected whitespace character <${TextUtils.repr(c1)}>" +
                " inside preprocessing directive"))
          } else {
            recur(accum :+ L(loc1, PPTokWhiteSpc(c1)))
          }
        case ('L', loc1) =>
          file.read().get match {
            case p@(x, loc2) if x == '\'' || x == '"' =>
              readCharStrLiteral(x == '"') match {
                case None =>
                  val tokL: L[PPTok] = L(loc1, PPTokId("L"))
                  val tokQ: L[PPTok] = L(loc2, PPTokSym(s"$x"))
                  recur(accum :+ tokL :+ tokQ)
                case Some(s) =>
                  if (x == '"') {
                    recur(accum :+ L(loc1, PPTokStr("L\"" + s)))
                  } else {
                    recur(accum :+ L(loc1, PPTokChar("L\'" + s)))
                  }
              }
            case p =>
              file.ungetc(p)
              val xs: Seq[(Char, (Int, Int))] =
                takeWhile(c => isDigit(c) || isAlpha(c) || c == '_')
              recur(accum :+ L(loc1, PPTokId("L" + xs.map(_._1).mkString)))
          }
        case (c1, loc1) if c1 == '\'' || c1 == '"' =>
          readCharStrLiteral(c1 == '"') match {
            case None =>
              recur(accum :+ L(loc1, PPTokSym(s"$c1")))
            case Some(s) =>
              if (c1 == '\'') {
                recur(accum :+ L(loc1, PPTokChar(s"$c1$s")))
              } else {
                recur(accum :+ L(loc1, PPTokStr(s"$c1$s")))
              }
          }
        case (c1, loc1) if isAlpha(c1) || c1 == '_' => // identifier
          def readId(acc: String): String = {
            file.read().get match {
              case (x, _) if isAlpha(x) || isDigit(x) || x == '_' =>
                readId(acc + x.toString)
              case p =>
                file.ungetc(p)
                acc
            }
          }
          val id: String = readId(c1.toString)
          if (id == "sizeof") {
            recur(accum :+ L(loc1, PPTokSym("sizeof")))
          } else {
            recur(accum :+ L(loc1, PPTokId(id)))
          }
        case ('.', loc1) => // pp-number, part 1
          file.read().get match {
            case (x, _) if isDigit(x) =>
              recur(accum :+ L(loc1, PPTokNum(readPPNum("." + x))))
            case p@('.', _) =>
              file.read().get match {
                case ('.', _) =>
                  recur(accum :+ L(loc1, PPTokSym("...")))
                case q =>
                  file.ungetc(q)
                  file.ungetc(p)
                  recur(accum :+ L(loc1, PPTokSym(".")))
              }
            case p =>
              file.ungetc(p)
              recur(accum :+ L(loc1, PPTokSym(".")))
          }
        case (c1, loc1) if isDigit(c1) => // pp-number, part 2
          recur(accum :+ L(loc1, PPTokNum(readPPNum(c1.toString))))
        case (c1, loc1) => // operator/punctuator/etc
          // "sizeof" and "..." are not handled here
          val s2set = Set(
            "++", "--", "<<", ">>", "<=", ">=", "==", "!=", "&&", "||",
            "*=", "/=", "%=", "+=", "-=", "&=", "^=", "|=", "##")
          val s3set = Set("<<=", ">>=")
          val sym: String = file.read().get match {
            case p@('\n', _) =>
              file.ungetc(p)
              c1.toString
            case p@(c2, _) => file.read().get match {
              case q@(c3, _) =>
                val s1 = c1.toString
                val s2 = s1 + c2
                val s3 = s2 + c3
                if (s3set.contains(s3)) {
                  s3
                } else {
                  file.ungetc(q)
                  if (s2set.contains(s2)) {
                    s2
                  } else {
                    file.ungetc(p)
                    s1
                  }
                }
            }
          }
          recur(accum :+ L(loc1, PPTokSym(sym)))
      }
    }
    recur(Seq.empty)
  }

  // called after seeing '#'
  private def readPPDirective(loc: (Int, Int)): PPLine = {
    // does NOT include '\n'.
    def ignoreWhiteSpaceChars(): Unit = {
      file.read().get match {
        case (' ', _) => ignoreWhiteSpaceChars()
        case ('\t', _) => ignoreWhiteSpaceChars()
        case p1@('/', _) =>
          file.read().get match {
            case ('/', _) =>
              // read up to the first '\n'
              def recur(): Unit = {
                file.read().get match {
                  case q@('\n', _) =>
                    file.ungetc(q)
                    ()
                  case _ =>
                    recur()
                }
              }
              recur()
            case ('*', loc1) =>
              def unfinishedBlockComment(loc: (Int, Int)) = {
                IllegalSourceException(SimpleMessage(
                  fileName, loc, "file ends with unfinished block comment"))
              }
              // read up to the first '*/'
              def recur(prev: (Char, (Int, Int))): Unit = {
                file.read() match {
                  case None =>
                    throw unfinishedBlockComment(prev._2)
                  case Some(q@('/', _)) =>
                    if (prev._1 == '*') {
                      ()
                    } else {
                      recur(q)
                    }
                  case Some(q) =>
                    recur(q)
                }
              }
              recur((' ', loc1))
            case p2 =>
              file.ungetc(p2)
              file.ungetc(p1)
              ()
          }
        case p1 => // '\n' and other chars are handled here
          file.ungetc(p1)
          ()
      }
      takeWhile(c => TextUtils.isWhiteSpace(c) && c != '\n')
    }
    def unexpectedChar(p: (Char, (Int, Int))): IllegalSourceException = {
      throw IllegalSourceException(new SimpleMessage(
        fileName, p._2,
        s"Unexpected character ${TextUtils.repr(p._1)} at" +
          s" line ${p._2._1}, col ${p._2._2}; identifier expected"))
    }
    def readEmptyLine(): Unit = {
      ignoreWhiteSpaceChars()
      file.read().get match {
        case ('\n', _) => ()
        case p => throw unexpectedChar(p)
      }
    }
    def takeWhileIsId(): Seq[(Char, (Int, Int))] = {
      val p1: Seq[(Char, (Int, Int))] = takeWhile(c => isAlpha(c) || c == '_')
      if (p1.isEmpty) {
        p1
      } else {
        p1 ++ takeWhile(c => isAlpha(c) || c == '_' || isDigit(c))
      }
    }
    def readIdLine(): L[String] = {
      ignoreWhiteSpaceChars()
      val idChars: Seq[(Char, (Int, Int))] = takeWhileIsId()
      if (idChars.isEmpty) {
        throw unexpectedChar(file.read().get)
      }
      readEmptyLine()
      L(idChars.head._2, idChars.map(_._1).mkString)
    }
    ignoreWhiteSpaceChars()
    val cmdCharSeq: Seq[(Char, (Int, Int))] = takeWhileIsId()
    if (cmdCharSeq.isEmpty) {
      return PPLineNull(loc)
    }
    val cmdLoc: (Int, Int) = cmdCharSeq.head._2
    val cmd: String = cmdCharSeq.map(_._1).mkString
    cmd match {
      case "if" =>
        PPLineIf(loc, readPPTokensLine(true))
      case "ifdef" =>
        PPLineIfdef(loc, readIdLine())
      case "ifndef" =>
        PPLineIfndef(loc, readIdLine())
      case "elif" =>
        PPLineElif(loc, readPPTokensLine(true))
      case "else" =>
        readEmptyLine()
        PPLineElse(loc)
      case "endif" =>
        readEmptyLine()
        PPLineEndif(loc)
      case "include" =>
        // TODO: c89 allows the arguments of #include to be macro-expanded
        def readIncludeHeader(endsOn: Char, accum: String): String = {
          file.read().get match {
            case ('\n', loc1) => throw IllegalSourceException(SimpleMessage(
              fileName, loc1,
              "Unexpected newline character"))
            case (c, _) if c == endsOn => accum
            case (c, _) => readIncludeHeader(endsOn, accum + c)
          }
        }
        ignoreWhiteSpaceChars()
        file.read().get match {
          case ('<', loc1) =>
            val name: L[String] = L(loc1, readIncludeHeader('>', ""))
            readEmptyLine()
            PPLineInclude(loc, name, isCaret = true)
          case ('"', loc1) =>
            val name: L[String] = L(loc1, readIncludeHeader('"', ""))
            readEmptyLine()
            PPLineInclude(loc, name, isCaret = false)
          case (c, loc1) =>
            throw IllegalSourceException(SimpleMessage(
              fileName, loc1,
              s"Unexpected character ${TextUtils.repr(c)} in #include"))
        }
      case "define" =>
        def expectId(): L[String] = {
          ignoreWhiteSpaceChars()
          val idChars: Seq[(Char, (Int, Int))] = takeWhileIsId()
          if (idChars.isEmpty) {
            throw IllegalSourceException(new SimpleMessage(
              fileName, file.read().get._2,
              "Illegal #define directive: identifier expected"))
          }
          L(idChars.head._2, idChars.map(_._1).mkString)
        }
        val name: L[String] = expectId()
        file.read().get match {
          case ('(', _) =>
            ignoreWhiteSpaceChars()
            file.read().get match {
              case (')', _) =>
                // no args
                PPLineDefineFunc(loc, name, Seq.empty, readPPTokensLine(true))
              case p =>
                file.ungetc(p)
                val firstArg: L[String] = expectId()
                def readArgs(accum: Seq[L[String]]): Seq[L[String]] = {
                  ignoreWhiteSpaceChars()
                  file.read().get match {
                    case (')', _) => accum
                    case (',', _) => readArgs(accum :+ expectId())
                    case (c, loc1) =>
                      throw IllegalSourceException(new SimpleMessage(
                        fileName, loc1,
                        s"Unexpected character <${TextUtils.repr(c)}> inside" +
                          " #define, comma or ')' expected"))
                  }
                }
                PPLineDefineFunc(
                  loc, name, readArgs(Seq(firstArg)), readPPTokensLine(true))
            }
          case p =>
            file.ungetc(p)
            PPLineDefineObj(loc, name, readPPTokensLine(true))
        }
      case "undef" =>
        PPLineUndef(loc, readIdLine())
      case "line" =>
        val toks: Seq[L[PPTok]] = readPPTokensLine(true)
        def isPPTokWhiteSpc(tok: L[PPTok]): Boolean = tok.value match {
          case PPTokWhiteSpc(_) => true
          case _ => false
        }
        def getLineNum(tok: L[PPTokNum]): Int = {
          val str: String = tok.value.num.filter(isDigit).mkString
          if (str.length == tok.value.num.length) {
            str.toInt
          } else {
            throw IllegalSourceException(new SimpleMessage(
              fileName, tok.loc,
              s"Illegal line number inside #line directive: ${tok.value.num}"))
          }
        }
        def getFileNameRepr(tok: L[PPTokStr]): L[String] = {
          val repr: String = tok.value.repr
          if (repr.charAt(0) != '"') {
            throw new IllegalSourceException(new SimpleMessage(
              fileName, tok.loc,
              s"Illegal file name inside #line directive: $repr"))
          }
          L(tok.loc, repr)
        }
        toks.filterNot(isPPTokWhiteSpc) match {
          case num :: Seq() if num.value.isInstanceOf[PPTokNum] =>
            PPLineLine(loc, getLineNum(num.asInstanceOf[L[PPTokNum]]), None)
          case num :: str :: Seq() if num.value.isInstanceOf[PPTokNum]
                                      && str.value.isInstanceOf[PPTokStr] =>
            PPLineLine(
              loc,
              getLineNum(num.asInstanceOf[L[PPTokNum]]),
              Some(getFileNameRepr(str.asInstanceOf[L[PPTokStr]]))
            )
          case _ =>
            throw IllegalSourceException(new SimpleMessage(
              fileName, loc, "Illegal #line preprocessing directive"))
        }
      case "error" =>
        PPLineError(loc, readPPTokensLine(true).map(_.value.raw).mkString.trim)
      case "pragma" =>
        readEmptyLine()
        PPLinePragma(loc)
      case _ =>
        throw IllegalSourceException(SimpleMessage(
          fileName, cmdLoc, s"unrecognized preprocessing directive: $cmd"))
    }
  }
}
