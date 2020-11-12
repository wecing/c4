package c4.util

import c4.messaging.{IllegalSourceException, SimpleMessage}
import c4.util.legacy.{Located => L}
import scala.collection.mutable.ArrayBuilder

object TextUtils {
  private val reprMap: Map[Char, String] = Map(
    ' ' -> "space",
    '\t' -> "\\t",
    '\n' -> "\\n",
    '\r' -> "\\r",
    '\f' -> "\\f",
    '\u000b' -> "\\v"
  )

  def repr(c: Char): String = {
    if ('!' <= c && c <= '~') {
      return c.toString
    }
    reprMap.getOrElse(c, s"\\x${c.toInt.toHexString}")
  }

  def fromCharReprQ(reprQ: L[String]): Char = {
    var legal = reprQ.value.head == '\'' && reprQ.value.last == '\''
    var c: Char = '\u0000'
    if (legal) {
      try {
        val str = fromStrReprQ(
          L(reprQ.loc, "\"" + reprQ.value.init.tail + "\"", reprQ.fileName)
        )
        if (str.length != 1) {
          legal = false
        } else {
          c = str.head
        }
      } catch {
        case _: IllegalSourceException =>
          legal = false
      }
    }
    if (!legal) {
      throw IllegalSourceException(
        SimpleMessage(
          reprQ.fileName.getOrElse("<unknown>"),
          reprQ.loc,
          "Illegal character literal"
        )
      )
    }
    c
  }

  // given a string, return a legal C string constant representation
  def strReprQ(s: String): String = {
    (new StringBuilder)
      .addOne('"')
      .append(s.replace("\n", "\\n").replace("\"", "\\\""))
      .addOne('"')
      .result()
  }

  def strReprQ(buf: Array[Byte], isWide: Boolean): String = {
    val builder = new StringBuilder
    builder.addOne('\"')

    var idx = 0
    while (idx < buf.length) {
      val c = if (isWide) {
        (buf(idx) & (buf(idx + 1) << 8)).toChar
      } else {
        (buf(idx) & 0xff).toChar
      }
      if (c == '\n') {
        builder.addOne('\\').addOne('n')
      } else if (c == '"') {
        builder.addOne('\\').addOne('"')
      } else {
        builder.addOne(c)
      }

      if (isWide) {
        idx += 2
      } else {
        idx += 1
      }
    }
    builder.addOne('\"')

    builder.result()
  }

  // given a C string quoted with "", extract the string content
  // (use UTF-8 for non-ascii characters, but does not utf-8 encode hex/oct
  // escapes)
  def fromStrReprQToBuf(reprQ: L[String], isWide: Boolean): Array[Byte] = {
    val octPattern = raw"\\[0-7]+".r
    val hexPattern = raw"\\x[0-9a-fA-F]+".r
    val builder = new ArrayBuilder.ofByte

    val escDict: Map[Char, Char] = Map(
      '\'' -> '\'',
      '"' -> '"',
      '?' -> '\u003f',
      '\\' -> '\\',
      'a' -> '\u0007',
      'b' -> '\u0008',
      'f' -> '\f',
      'n' -> '\n',
      'r' -> '\r',
      't' -> '\t',
      'v' -> '\u000b'
    )

    var str = reprQ.value.tail.init
    while (str.nonEmpty) {
      str = octPattern
        .findPrefixOf(str)
        .map(s => {
          val n =
            s.tail.map(c => c - '0').foldLeft(0) { (sum, n) => sum * 8 + n }
          builder.addOne(n.toByte)
          if (isWide) {
            // little-endian
            builder.addOne((n >> 8).toByte)
          }
          str.drop(s.length())
        })
        .orElse {
          hexPattern
            .findPrefixOf(str)
            .map(s => {
              val n = s
                .drop(2)
                .map { c =>
                  if ('0' <= c && c <= '9') { c - '0' }
                  else { c.toLower - 'a' + 10 }
                }
                .foldLeft(0) { (sum, n) => sum * 16 + n }
              builder.addOne(n.toByte)
              if (isWide) {
                // little-endian
                builder.addOne((n >> 8).toByte)
              }
              str.drop(s.length())
            })
        }
        .getOrElse {
          if (str.length() >= 2 && str(0) == '\\') {
            escDict.get(str(1)).map(c => builder.addOne(c.toByte)).getOrElse {
              throw IllegalSourceException(
                SimpleMessage(
                  reprQ.fileName.getOrElse("<unknown>"),
                  reprQ.loc,
                  "Illegal string literal"
                )
              )
            }
            str.drop(2)
          } else {
            val c = str.head
            builder.addAll(c.toString().getBytes())
            str.tail
          }
        }
    }
    builder.result()
  }

  // given a C string quoted with "", extract the string content to UTF-16 str
  // (does NOT disambiguate non-ascii chars and hex/oct escapes)
  def fromStrReprQ(reprQ: L[String]): String = {
    sealed abstract class EscCase
    case object Unknown extends EscCase
    final case class Oct(v: Seq[Int]) extends EscCase {
      def toChar: Char = v.foldLeft(0)((a, n) => a * 8 + n).toChar
    }
    final case class Hex(v: Seq[Int]) extends EscCase {
      def toCharOpt: Option[Char] = {
        val xs: Seq[Int] = v.dropWhile(_ == 0)
        if (xs.length > 4) {
          None
        } else {
          Some(xs.foldLeft(0)((a, n) => a * 16 + n).toChar)
        }
      }
    }

    def f(
        a: Option[(String, Option[EscCase])],
        c: Char
    ): Option[(String, Option[EscCase])] = {
      val escDict: Map[Char, Char] = Map(
        '\'' -> '\'',
        '"' -> '"',
        '?' -> '\u003f',
        '\\' -> '\\',
        'a' -> '\u0007',
        'b' -> '\u0008',
        'f' -> '\f',
        'n' -> '\n',
        'r' -> '\r',
        't' -> '\t',
        'v' -> '\u000b'
      )
      val r: Option[(String, Option[EscCase])] = a flatMap {
        case (acc, None) if c == '\\' => Some((acc, Some(Unknown)))
        case (acc, None)              => Some((acc + c, None))
        case (acc, Some(Unknown)) if '0' <= c && c <= '7' =>
          Some((acc, Some(Oct(Seq(c - '0')))))
        case (acc, Some(Unknown)) if c == 'x' =>
          Some((acc, Some(Hex(Seq.empty))))
        case (acc, Some(Unknown)) =>
          escDict.get(c) map { s => (acc + s, None) }
        case (acc, Some(oct @ Oct(xs))) =>
          if ('0' <= c && c <= '7') {
            val esc = Oct(xs :+ (c - '0'))
            if (esc.v.length < 3) {
              Some((acc, Some(esc)))
            } else {
              Some((acc + esc.toChar, None))
            }
          } else { // oct.v will never be empty
            if (c == '\\') {
              Some((acc + oct.toChar, Some(Unknown)))
            } else {
              Some((acc + oct.toChar, None))
            }
          }
        case (acc, Some(hex @ Hex(xs))) =>
          val ord: Int =
            if ('0' <= c && c <= '9') {
              c - '0'
            } else if ('a' <= c && c <= 'f') {
              c - 'a' + 10
            } else if ('A' <= c && c <= 'F') {
              c - 'A' + 10
            } else {
              -1
            }
          if (ord != -1) {
            Some((acc, Some(Hex(xs :+ ord))))
          } else if (xs.isEmpty) {
            None
          } else if (c == '\\') {
            hex.toCharOpt.flatMap(n => Some((acc + n, Some(Unknown))))
          } else {
            hex.toCharOpt.flatMap(n => Some((acc + n + c, None)))
          }
      }
      r
    }

    val r: Option[String] = for {
      s <- Option(reprQ.value)
      if s.length >= 2
      if s.head == '"' && s.last == '"'
      foldBegin: Option[(String, Option[EscCase])] = Some(("", None))
      (acc, rem: Option[EscCase]) <- s.tail.init.foldLeft(foldBegin) { (a, b) =>
        f(a, b)
      }
      ret <- rem match {
        case None              => Some(acc)
        case Some(Unknown)     => None
        case Some(Oct(Seq()))  => None
        case Some(c @ Oct(xs)) => Some(acc + c.toChar)
        case Some(Hex(Seq()))  => None
        case Some(c @ Hex(xs)) => c.toCharOpt.flatMap(n => Some(acc + n))
      }
    } yield ret

    r match {
      case Some(x) => x
      case None =>
        throw IllegalSourceException(
          SimpleMessage(
            reprQ.fileName.getOrElse("<unknown>"),
            reprQ.loc,
            "Illegal string literal"
          )
        )
    }
  }

  // '\r' is not considered as white space character in C89.
  private val whiteSpaceChars: Set[Char] = Set(' ', '\t', '\n', '\f', '\u000b')

  def isWhiteSpace(c: Char): Boolean = {
    whiteSpaceChars.contains(c)
  }
}
