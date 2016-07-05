package c4.util

import cats._

final case class Located[+T](loc: Loc, value: T) {
  def map[P](f: (T => P)): Located[P] = Located(loc, f(value))

  def deLoc: Id[T] = value
}

object Located {
  def of[T](begin: Loc, end: Loc, value: T): Located[T] = {
    if (begin == end) {
      Located(begin, value)
    } else {
      Located(LocRange.of(begin, end), value)
    }
  }

  def of[T](loc: Loc, value: T): Located[T] = Located(loc, value) // for Java

  def unit(loc: Loc): Located[Unit] = Located.of(loc, ())
}

sealed abstract class Loc {
  def head: LocPoint = this match {
    case p: LocPoint => p
    case LocRange(p, _) => p
  }
}
final case class LocPoint(pos: (Int, Int), fileName: Option[String]) extends Loc
final case class LocRange(begin: LocPoint, end: LocPoint) extends Loc

object LocRange {
  def of(begin: Loc, end: Loc): Loc = {
    val b = begin match {
      case r: LocRange => r.begin
      case p: LocPoint => p
    }
    val e = end match {
      case r: LocRange => r.end
      case p: LocPoint => p
    }
    LocRange(b, e)
  }
}
