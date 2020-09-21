package c4.io

import c4.ast.{C4Symbols => Sym}
import c4.messaging.{IllegalSourceException, Message, SimpleMessage}
import c4.util.{Located, TextUtils}
import c4.util.legacy.{Located => L}

import scala.collection.mutable.ArrayBuffer

sealed abstract class IntSize {
  def nBytes: Int
  def range(signed: Boolean): (BigInt, BigInt)
}
case object Int8 extends IntSize {
  override def nBytes: Int = 1
  override def range(signed: Boolean): (BigInt, BigInt) = {
    if (signed) {
      (-0x80, 0x7f)
    } else {
      (0, 0xff)
    }
  }
}
case object Int16 extends IntSize {
  override def nBytes: Int = 2
  override def range(signed: Boolean): (BigInt, BigInt) = {
    if (signed) {
      (-0x8000, 0x7fff)
    } else {
      (0, 0xffff)
    }
  }
}
case object Int32 extends IntSize {
  override def nBytes: Int = 4
  override def range(signed: Boolean): (BigInt, BigInt) = {
    if (signed) {
      (Int.MinValue, Int.MaxValue)
    } else {
      (0, 0xffffffffL)
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

sealed abstract class Tok(val cupSymbol: Int)
final case class TokId(id: String) extends Tok(Sym.ID)
final case class TokInteger(n: BigInt, size: IntSize, signed: Boolean)
    extends Tok(Sym.INTEGER_LIT)
final case class TokFloat(f: Float) extends Tok(Sym.FLOAT_LIT)
final case class TokDouble(d: Double) extends Tok(Sym.DOUBLE_LIT)
final case class TokLongDouble(ld: BigDecimal) extends Tok(Sym.LONG_DOUBLE_LIT)
final case class TokChar(c: Char) extends Tok(Sym.CHAR_LIT)
final case class TokWideChar(c: Char) extends Tok(Sym.WIDE_CHAR_LIT)
final case class TokStr(s: String) extends Tok(Sym.STR_LIT) {
  override def toString: String = s"TokStr(${TextUtils.strReprQ(s)})"
}
final case class TokWideStr(s: String) extends Tok(Sym.WIDE_STR_LIT)
// keywords
case object Tok_auto extends Tok(Sym.AUTO)
case object Tok_break extends Tok(Sym.BREAK)
case object Tok_case extends Tok(Sym.CASE)
case object Tok_char extends Tok(Sym.CHAR)
case object Tok_const extends Tok(Sym.CONST)
case object Tok_continue extends Tok(Sym.CONTINUE)
case object Tok_default extends Tok(Sym.DEFAULT)
case object Tok_do extends Tok(Sym.DO)
case object Tok_double extends Tok(Sym.DOUBLE)
case object Tok_else extends Tok(Sym.ELSE)
case object Tok_enum extends Tok(Sym.ENUM)
case object Tok_extern extends Tok(Sym.EXTERN)
case object Tok_float extends Tok(Sym.FLOAT)
case object Tok_for extends Tok(Sym.FOR)
case object Tok_goto extends Tok(Sym.GOTO)
case object Tok_if extends Tok(Sym.IF)
case object Tok_int extends Tok(Sym.INT)
case object Tok_long extends Tok(Sym.LONG)
case object Tok_register extends Tok(Sym.REGISTER)
case object Tok_return extends Tok(Sym.RETURN)
case object Tok_short extends Tok(Sym.SHORT)
case object Tok_signed extends Tok(Sym.SIGNED)
case object Tok_sizeof extends Tok(Sym.SIZEOF)
case object Tok_static extends Tok(Sym.STATIC)
case object Tok_struct extends Tok(Sym.STRUCT)
case object Tok_switch extends Tok(Sym.SWITCH)
case object Tok_typedef extends Tok(Sym.TYPEDEF)
case object Tok_union extends Tok(Sym.UNION)
case object Tok_unsigned extends Tok(Sym.UNSIGNED)
case object Tok_void extends Tok(Sym.VOID)
case object Tok_volatile extends Tok(Sym.VOLATILE)
case object Tok_while extends Tok(Sym.WHILE)
// symbols
case object Tok_lsbrkt extends Tok(Sym.LSBRKT) // [
case object Tok_rsbrkt extends Tok(Sym.RSBRKT) // ]
case object Tok_lparen extends Tok(Sym.LPAREN) // (
case object Tok_rparen extends Tok(Sym.RPAREN) // )
case object Tok_lbrace extends Tok(Sym.LBRACE) // {
case object Tok_rbrace extends Tok(Sym.RBRACE) // }
case object Tok_dot extends Tok(Sym.DOT) // .
case object Tok_comma extends Tok(Sym.COMMA) // ,
case object Tok_semicolon extends Tok(Sym.SEMICOLON) // ;
case object Tok_ellipsis extends Tok(Sym.ELLIPSIS) // ...
case object Tok_-> extends Tok(Sym.PTR)
case object Tok_++ extends Tok(Sym.INC)
case object Tok_-- extends Tok(Sym.DEC)
case object Tok_& extends Tok(Sym.B_AND)
case object Tok_* extends Tok(Sym.STAR)
case object Tok_+ extends Tok(Sym.ADD)
case object Tok_- extends Tok(Sym.SUB)
case object Tok_~ extends Tok(Sym.TILDE)
case object Tok_! extends Tok(Sym.L_NOT)
case object Tok_/ extends Tok(Sym.DIV)
case object Tok_% extends Tok(Sym.MOD)
case object Tok_<< extends Tok(Sym.LSHIFT)
case object Tok_>> extends Tok(Sym.RSHIFT)
case object Tok_< extends Tok(Sym.LE)
case object Tok_> extends Tok(Sym.GT)
case object Tok_<= extends Tok(Sym.LEQ)
case object Tok_>= extends Tok(Sym.GEQ)
case object Tok_== extends Tok(Sym.EQ)
case object Tok_!= extends Tok(Sym.NEQ)
case object Tok_^ extends Tok(Sym.XOR)
case object Tok_| extends Tok(Sym.B_OR)
case object Tok_&& extends Tok(Sym.L_AND)
case object Tok_|| extends Tok(Sym.L_OR)
case object Tok_? extends Tok(Sym.QMARK)
case object Tok_: extends Tok(Sym.COLON)
case object Tok_= extends Tok(Sym.ASSIGN)
case object Tok_*= extends Tok(Sym.MUL_ASSIGN)
case object Tok_/= extends Tok(Sym.DIV_ASSIGN)
case object Tok_%= extends Tok(Sym.MOD_ASSIGN)
case object Tok_+= extends Tok(Sym.ADD_ASSIGN)
case object Tok_-= extends Tok(Sym.SUB_ASSIGN)
case object Tok_<<= extends Tok(Sym.LSHIFT_ASSIGN)
case object Tok_>>= extends Tok(Sym.RSHIFT_ASSIGN)
case object Tok_&= extends Tok(Sym.B_AND_ASSIGN)
case object Tok_^= extends Tok(Sym.XOR_ASSIGN)
case object Tok_|= extends Tok(Sym.B_OR_ASSIGN)

/**
  * Translation phase 5, 6, 7.
  *
  * C89 translation phase 7 includes parsing / code generation, but this class
  * does not.
  */
class SourcePhase7Reader(
    val warnings: ArrayBuffer[Message],
    val fileName: String,
    predefMacros: Map[String, String]
) {
  private var tokens: Seq[L[Tok]] = {
    PPReader
      .read(warnings, fileName, predefMacros)
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
              "while" -> Tok_while
            )
            val newTok = dict.getOrElse(id, TokId(id))
            acc :+ L(ppTok.loc, newTok, ppTok.fileName)
          case PPTokNum(num) =>
            val floatingPattern =
              "^(([0-9]*[.][0-9]+)|([0-9]+[.]))(([eE][-+]?[0-9]+)?)([flFL]?)$".r
            val octalPattern = "^(0[0-7]*)([ulUL]*)$".r
            val decimalPattern = "^([1-9][0-9]*)([ulUL]*)$".r
            val hexPattern = "^0[xX]([0-9a-fA-F]+)([ulUL]*)$".r
            def toTokInteger(s: String, radix: Int, suffix: String)
                : TokInteger = {
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

              lazy val illegalEx = IllegalSourceException(
                SimpleMessage(
                  ppTok.fileName.getOrElse("<unknown>"),
                  ppTok.loc,
                  s"illegal floating number syntax: $num"
                )
              )

              val sizes: Seq[(IntSize, Boolean)] = suffix.toLowerCase match {
                case "" if radix == 10 => Seq(int, longInt, unsignedLongInt)
                case ""                => Seq(int, unsignedInt, longInt, unsignedLongInt)
                case "u"               => Seq(unsignedInt, unsignedLongInt)
                case "l"               => Seq(longInt, unsignedLongInt)
                case "ul" | "lu"       => Seq(unsignedLongInt)
                case _                 => throw illegalEx
              }

              val v = BigInt(s, radix)
              val suitableSizes: Seq[(IntSize, Boolean)] =
                sizes.dropWhile {
                  case (sz, signed) =>
                    val (min, max) = sz.range(signed)
                    !(min <= v && v <= max)
                }
              suitableSizes match {
                case (sz, signed) +: _ => TokInteger(v, sz, signed)
                case _                 => throw illegalEx
              }
            }
            num match {
              case floatingPattern(f, _, _, e, _, s) =>
                // f: "123.3" or ".45" or "34."
                // e: "e01" or "e+01" or "e-01"
                // s: one of [flFL]
                val eVal = if (e.isEmpty) 0 else e.tail.toInt
                val v = BigDecimal(f) * BigDecimal(10).pow(eVal)
                val newTok: Tok = s match {
                  case ""        => TokDouble(v.toDouble)
                  case "f" | "F" => TokFloat(v.toFloat)
                  case _         => TokLongDouble(v)
                }
                acc :+ L(ppTok.loc, newTok, ppTok.fileName)
              case octalPattern(n, suffix) =>
                acc :+ L(ppTok.loc, toTokInteger(n, 8, suffix), ppTok.fileName)
              case decimalPattern(n, suffix) =>
                acc :+ L(ppTok.loc, toTokInteger(n, 10, suffix), ppTok.fileName)
              case hexPattern(n, suffix) =>
                acc :+ L(ppTok.loc, toTokInteger(n, 16, suffix), ppTok.fileName)
              case _ =>
                throw IllegalSourceException(
                  SimpleMessage(
                    ppTok.fileName.getOrElse("<unknown>"),
                    ppTok.loc,
                    s"illegal floating number syntax: $num"
                  )
                )
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
              L(ppTok.loc, if (isWide) repr.tail else repr, ppTok.fileName)
            )
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
              "|=" -> Tok_|=
            )
            dict.get(sym) match {
              case Some(tok) => acc :+ L(ppTok.loc, tok, ppTok.fileName)
              case None =>
                throw IllegalSourceException(
                  SimpleMessage(
                    ppTok.fileName.getOrElse("<unknown>"),
                    ppTok.loc,
                    s"unknown token: < $sym >"
                  )
                )
            }
          case PPTokWhiteSpc(_) =>
            acc
        }
      }
  }

  def get(): Option[Located[Tok]] = {
    if (tokens.isEmpty) {
      None
    } else {
      val r = Some(tokens.head.toNew)
      tokens = tokens.tail
      r
    }
  }

  // def unget(tok: L[Tok]): Unit = {
  //   tokens = tok +: tokens
  // }
}
