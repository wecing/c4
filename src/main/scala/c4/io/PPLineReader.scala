package c4.io

import c4.messaging.{SimpleMessage, IllegalSourceException, Message}
import c4.util.{Located => L, CharUtil}

import scala.collection.mutable.ArrayBuffer


sealed abstract class PPTok
final case class PPTokId(id: String) extends PPTok
final case class PPTokNum(num: String) extends PPTok
final case class PPTokChar(repr: String) extends PPTok
final case class PPTokStr(repr: String) extends PPTok
final case class PPTokSym(sym: String) extends PPTok
final case class PPTokWhiteSpc(c: Char) extends PPTok

sealed abstract class PPLine
final case class PPLineTokens(tokens: Seq[L[PPTok]]) extends PPLine

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
      case Some((c: Char, _)) if CharUtil.isWhiteSpace(c) => read()
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
            Some(PPLineTokens(readPPTokensLine()))
        }
      case Some(('#', loc1)) =>
        // TODO: PP directive
        ???
      case Some(p) =>
        file.ungetc(p)
        Some(PPLineTokens(readPPTokensLine()))
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

  // reads one (logical) line of PPTokens, stops on seeing '\n'
  private def readPPTokensLine(): Seq[L[PPTok]] = {
    def isAlpha(c: Char): Boolean = {
      ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
    }
    def isDigit(c: Char): Boolean = {
      '0' <= c && c <= '9'
    }
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
      def takeWhile(pred: Char => Boolean): Seq[(Char, (Int, Int))] = {
        var x: (Char, (Int, Int)) = file.read().get
        var r: Seq[(Char, (Int, Int))] = Seq.empty
        while (pred(x._1)) {
          r = r :+ x
          x = file.read().get
        }
        file.ungetc(x)
        r
      }
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
              recur(accum) // still on the same logical line!
            case ('=', _) =>
              recur(accum :+ L(loc1, PPTokSym("/=")))
            case p =>
              file.ungetc(p)
              recur(accum :+ L(loc1, PPTokSym("/")))
          }
        case (c1, loc1) if CharUtil.isWhiteSpace(c1) =>
          recur(accum :+ L(loc1, PPTokWhiteSpc(c1)))
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
              recur(accum :+ L(loc1, PPTokId("L")))
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
}
