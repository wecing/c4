package c4.util

final case class Located[+T](loc: Loc, value: T)

final case class Loc(pos: (Int, Int), fileName: Option[String])
