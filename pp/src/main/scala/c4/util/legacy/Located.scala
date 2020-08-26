package c4.util.legacy

import c4.util.{Located => L, LocPoint}

final case class Located[+T](
    loc: (Int, Int),
    value: T,
    fileName: Option[String] = None
) {
  def transform(newFileName: String, lineNumOffset: Int): Located[T] = {
    Located((loc._1 + lineNumOffset, loc._2), value, Some(newFileName))
  }

  def toNew = L(LocPoint(loc, fileName), value)
}
