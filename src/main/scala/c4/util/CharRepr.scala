package c4.util

object CharRepr {
  val reprMap: Map[Char, String] = Map(
    ' ' -> "space",
    '\t' -> "\\t",
    '\n' -> "\\n",
    '\r' -> "\\r"
  )

  def apply(c: Char): String = {
    if ('!' <= c && c <= '~') {
      return c.toString
    }
    reprMap.getOrElse(c, s"\\x${c.toInt.toHexString}")
  }
}
