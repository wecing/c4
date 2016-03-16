package c4.io

import c4.messaging.{SimpleMessage, IllegalSourceException, Message}
import c4.util.{Located => L, TextUtils}

import scala.collection.mutable.ArrayBuffer

sealed abstract class IntSize {
  def nBytes: Int
  def range(signed: Boolean): (BigInt, BigInt)
}
case object Int8 extends IntSize {
  override def nBytes: Int = 1
  override def range(signed: Boolean): (BigInt, BigInt) = {
    if (signed) {
      (-0x80, 0x7F)
    } else {
      (0, 0xFF)
    }
  }
}
case object Int16 extends IntSize {
  override def nBytes: Int = 2
  override def range(signed: Boolean): (BigInt, BigInt) = {
    if (signed) {
      (-0x8000, 0x7FFF)
    } else {
      (0, 0xFFFF)
    }
  }
}
case object Int32 extends IntSize {
  override def nBytes: Int = 4
  override def range(signed: Boolean): (BigInt, BigInt) = {
    if (signed) {
      (Int.MinValue, Int.MaxValue)
    } else {
      (0, 0xFFffFFffL)
    }
  }
}
case object Int64 extends IntSize {
  override def nBytes: Int = 8
  override def range(signed: Boolean): (BigInt, BigInt) = {
    if (signed) {
      (Long.MinValue, Long.MaxValue)
    } else {
      (0, BigInt("FFffFFffFFffFFff", 16))
    }
  }
}

sealed abstract class Tok
final case class TokId(id: String) extends Tok
final case class TokInteger(n: BigInt,
                            size: IntSize,
                            signed: Boolean) extends Tok
final case class TokFloat(f: Float) extends Tok
final case class TokDouble(d: Double) extends Tok
final case class TokLongDouble(ld: BigDecimal) extends Tok
final case class TokChar(c: Char) extends Tok
final case class TokWideChar(c: Char) extends Tok
final case class TokStr(s: String) extends Tok {
  override def toString: String = s"TokStr(${TextUtils.strReprQ(s)})"
}
final case class TokWideStr(s: String) extends Tok
// keywords
case object Tok_auto extends Tok
case object Tok_break extends Tok
case object Tok_case extends Tok
case object Tok_char extends Tok
case object Tok_const extends Tok
case object Tok_continue extends Tok
case object Tok_default extends Tok
case object Tok_do extends Tok
case object Tok_double extends Tok
case object Tok_else extends Tok
case object Tok_enum extends Tok
case object Tok_extern extends Tok
case object Tok_float extends Tok
case object Tok_for extends Tok
case object Tok_goto extends Tok
case object Tok_if extends Tok
case object Tok_int extends Tok
case object Tok_long extends Tok
case object Tok_register extends Tok
case object Tok_return extends Tok
case object Tok_short extends Tok
case object Tok_signed extends Tok
case object Tok_sizeof extends Tok
case object Tok_static extends Tok
case object Tok_struct extends Tok
case object Tok_switch extends Tok
case object Tok_typedef extends Tok
case object Tok_union extends Tok
case object Tok_unsigned extends Tok
case object Tok_void extends Tok
case object Tok_volatile extends Tok
case object Tok_while extends Tok
// symbols
case object Tok_lsbrkt extends Tok // [
case object Tok_rsbrkt extends Tok // ]
case object Tok_lparen extends Tok // (
case object Tok_rparen extends Tok // )
case object Tok_lbrace extends Tok // {
case object Tok_rbrace extends Tok // }
case object Tok_dot extends Tok // .
case object Tok_comma extends Tok // ,
case object Tok_semicolon extends Tok // ;
case object Tok_ellipsis extends Tok // ...
case object Tok_-> extends Tok
case object Tok_++ extends Tok
case object Tok_-- extends Tok
case object Tok_& extends Tok
case object Tok_* extends Tok
case object Tok_+ extends Tok
case object Tok_- extends Tok
case object Tok_~ extends Tok
case object Tok_! extends Tok
case object Tok_/ extends Tok
case object Tok_% extends Tok
case object Tok_<< extends Tok
case object Tok_>> extends Tok
case object Tok_< extends Tok
case object Tok_> extends Tok
case object Tok_<= extends Tok
case object Tok_>= extends Tok
case object Tok_== extends Tok
case object Tok_!= extends Tok
case object Tok_^ extends Tok
case object Tok_| extends Tok
case object Tok_&& extends Tok
case object Tok_|| extends Tok
case object Tok_? extends Tok
case object Tok_: extends Tok
case object Tok_= extends Tok
case object Tok_*= extends Tok
case object Tok_/= extends Tok
case object Tok_%= extends Tok
case object Tok_+= extends Tok
case object Tok_-= extends Tok
case object Tok_<<= extends Tok
case object Tok_>>= extends Tok
case object Tok_&= extends Tok
case object Tok_^= extends Tok
case object Tok_|= extends Tok


/**
 * Translation phase 5, 6, 7.
 */
class SourcePhase7Reader(val warnings: ArrayBuffer[Message],
                         val fileName: String) {
  private[this] var tokens: Seq[L[Tok]] = {
    PPReader
      .read(warnings, fileName)
      .foldLeft(Seq.empty[L[Tok]]) { (acc: Seq[L[Tok]], ppTok: L[PPTok]) =>
        ppTok.value match {
          case PPTokId(id) =>
            val dict = Map(
              "auto" -> Tok_auto,
              "break" -> Tok_break,
              "case" -> Tok_case,
              "char" -> Tok_char,
              "const" -> Tok_const,
              "continue" -> Tok_continue,
              "default" -> Tok_default,
              "do" -> Tok_do,
              "double" -> Tok_double,
              "else" -> Tok_else,
              "enum" -> Tok_enum,
              "extern" -> Tok_extern,
              "float" -> Tok_float,
              "for" -> Tok_for,
              "goto" -> Tok_goto,
              "if" -> Tok_if,
              "int" -> Tok_int,
              "long" -> Tok_long,
              "register" -> Tok_register,
              "return" -> Tok_return,
              "short" -> Tok_short,
              "signed" -> Tok_signed,
              "sizeof" -> Tok_sizeof, // never used; sizeof is treated as sym
              "static" -> Tok_static,
              "struct" -> Tok_struct,
              "switch" -> Tok_switch,
              "typedef" -> Tok_typedef,
              "union" -> Tok_union,
              "unsigned" -> Tok_unsigned,
              "void" -> Tok_void,
              "volatile" -> Tok_volatile,
              "while" -> Tok_while)
            val newTok = dict.getOrElse(id, TokId(id))
            acc :+ L(ppTok.loc, newTok, ppTok.fileName)
          case PPTokNum(num) =>
            val floatingPattern =
              "^(([0-9]*[.][0-9]+)|([0-9]+[.]))(([eE][-+]?[0-9]+)?)([flFL]?)$".r
            val octalPattern = "^(0[0-7]*)([ulUL]*)$".r
            val decimalPattern = "^([1-9][0-9]*)([ulUL]*)$".r
            val hexPattern = "^0[xX]([0-9a-zA-Z]+)([ulUL]*)$".r
            def toTokInteger(s: String, radix: Int,
                             suffix: String): TokInteger = {
              // The type of an integer constant is the first of the
              // corresponding list in which its value can be represented.
              //
              // Unsuffixed decimal:
              //    int, long int, unsigned long int;
              // unsuffixed octal or hexadecimal:
              //    int, unsigned int, long int, unsigned long int;
              // suffixed by the letter u or U:
              //    unsigned int, unsigned long int;
              // suffixed by the letter l or L:
              //    long int, unsigned long int;
              // suffixed by both the letters u or U and l or L:
              //    unsigned long int.
              val int = (Int32, true)
              val unsignedInt = (Int32, false)
              val longInt = (Int64, true)
              val unsignedLongInt = (Int64, false)

              lazy val illegalEx = IllegalSourceException(SimpleMessage(
                ppTok.fileName.getOrElse("<unknown>"), ppTok.loc,
                s"illegal floating number syntax: $num"))

              val sizes: Seq[(IntSize, Boolean)] = suffix.toLowerCase match {
                case "" if radix == 10 => Seq(int, longInt, unsignedLongInt)
                case "" => Seq(int, unsignedInt, longInt, unsignedLongInt)
                case "u" => Seq(unsignedInt, unsignedLongInt)
                case "l" => Seq(longInt, unsignedLongInt)
                case "ul" | "lu" => Seq(unsignedLongInt)
                case _ => throw illegalEx
              }

              val v = BigInt(s, radix)
              val suitableSizes: Seq[(IntSize, Boolean)] =
                sizes.dropWhile { case (sz, signed) =>
                  val (min, max) = sz.range(signed)
                  !(min <= v && v <= max)
                }
              suitableSizes match {
                case (sz, signed) +: _ => TokInteger(v, sz, signed)
                case _ => throw illegalEx
              }
            }
            num match {
              case floatingPattern(f, _, _, e, _, s) =>
                // f: "123.3" or ".45" or "34."
                // e: "e01" or "e+01" or "e-01"
                // s: one of [flFL]
                val v = BigDecimal(f) * BigDecimal(2).pow(e.tail.toInt)
                val newTok: Tok = s match {
                  case "" => TokDouble(v.toDouble)
                  case "f" | "F" => TokFloat(v.toFloat)
                  case _ => TokLongDouble(v)
                }
                acc :+ L(ppTok.loc, newTok, ppTok.fileName)
              case octalPattern(n, suffix) =>
                acc :+ L(ppTok.loc, toTokInteger(n, 8, suffix), ppTok.fileName)
              case decimalPattern(n, suffix) =>
                acc :+ L(ppTok.loc, toTokInteger(n, 10, suffix), ppTok.fileName)
              case hexPattern(n, suffix) =>
                acc :+ L(ppTok.loc, toTokInteger(n, 16, suffix), ppTok.fileName)
              case _ =>
                throw IllegalSourceException(SimpleMessage(
                  ppTok.fileName.getOrElse("<unknown>"), ppTok.loc,
                  s"illegal floating number syntax: $num"))
            }
          case PPTokChar(repr) =>
            val isWide = repr.head == 'L'
            val r = if (isWide) repr.tail else repr
            val c = TextUtils.fromCharReprQ(L(ppTok.loc, r, ppTok.fileName))
            val newTok = if (isWide) TokWideChar(c) else TokChar(c)
            acc :+ L(ppTok.loc, newTok, ppTok.fileName)
          case PPTokStr(repr) =>
            val isWide = repr.head == 'L'
            val str = TextUtils.fromStrReprQ(
              L(ppTok.loc, if (isWide) repr.tail else repr, ppTok.fileName))
            acc.last.value match {
              case TokStr(prevStr) if !isWide =>
                val newTok = TokStr(prevStr + str)
                acc.init :+ L(acc.last.loc, newTok, acc.last.fileName)
              case TokWideStr(prevStr) if isWide =>
                val newTok = TokWideStr(prevStr + str)
                acc.init :+ L(acc.last.loc, newTok, acc.last.fileName)
              case _ =>
                val newTok = if (isWide) TokWideStr(str) else TokStr(str)
                acc :+ L(ppTok.loc, newTok, ppTok.fileName)
            }
          case PPTokSym(sym) =>
            val dict = Map(
              "[" -> Tok_lsbrkt,
              "]" -> Tok_rsbrkt,
              "(" -> Tok_lparen,
              ")" -> Tok_rparen,
              "{" -> Tok_lbrace,
              "}" -> Tok_rbrace,
              "." -> Tok_dot,
              "," -> Tok_comma,
              ";" -> Tok_semicolon,
              "..." -> Tok_ellipsis,
              "->" -> Tok_->,
              "++" -> Tok_++,
              "--" -> Tok_--,
              "&" -> Tok_&,
              "*" -> Tok_*,
              "+" -> Tok_+,
              "-" -> Tok_-,
              "~" -> Tok_~,
              "!" -> Tok_!,
              "sizeof" -> Tok_sizeof,
              "/" -> Tok_/,
              "%" -> Tok_%,
              "<<" -> Tok_<<,
              ">>" -> Tok_>>,
              "<" -> Tok_<,
              ">" -> Tok_>,
              "<=" -> Tok_<=,
              ">=" -> Tok_>=,
              "==" -> Tok_==,
              "!=" -> Tok_!=,
              "^" -> Tok_^,
              "|" -> Tok_|,
              "&&" -> Tok_&&,
              "||" -> Tok_||,
              "?" -> Tok_?,
              ":" -> Tok_:,
              "=" -> Tok_=,
              "*=" -> Tok_*=,
              "/=" -> Tok_/=,
              "%=" -> Tok_%=,
              "+=" -> Tok_+=,
              "-=" -> Tok_-=,
              "<<=" -> Tok_<<=,
              ">>=" -> Tok_>>=,
              "&=" -> Tok_&=,
              "^=" -> Tok_^=,
              "|=" -> Tok_|=)
            dict.get(sym) match {
              case Some(tok) => acc :+ L(ppTok.loc, tok, ppTok.fileName)
              case None => throw IllegalSourceException(SimpleMessage(
                ppTok.fileName.getOrElse("<unknown>"), ppTok.loc,
                s"unknown token: < $sym >"))
            }
          case PPTokWhiteSpc(_) =>
            acc
        }
      }
  }

  def get(): Option[L[Tok]] = {
    if (tokens.isEmpty) {
      None
    } else {
      val r = Some(tokens.head)
      tokens = tokens.tail
      r
    }
  }

  def unget(tok: L[Tok]): Unit = {
    tokens = tok +: tokens
  }
}
