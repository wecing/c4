package c4.util

final case class Located[+T](loc: Loc, value: T)

sealed abstract class Loc
final case class LocPoint(pos: (Int, Int), fileName: Option[String]) extends Loc
final case class LocRange(begin: LocPoint, end: LocPoint) extends Loc
