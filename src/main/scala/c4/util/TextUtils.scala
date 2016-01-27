package c4.util

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

  // like repr, but quoted
  def strReprQ(s: String): String = {
    "" +
      "\"" +
      s.replace("\n", "\\n")
        .replace("\"", "\\\"") +
      "\""
  }

  def fromStrReprQ(reprQ: Located[String]): String = {
    reprQ.value // TODO - this needs to be implemented...
  }

  // '\r' is not considered as white space character in C89.
  private val whiteSpaceChars: Set[Char] = Set(' ', '\t', '\n', '\f', '\u000b')

  def isWhiteSpace(c: Char): Boolean = {
    whiteSpaceChars.contains(c)
  }
}
