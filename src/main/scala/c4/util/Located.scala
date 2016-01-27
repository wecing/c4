package c4.util

final case class Located[+T](loc: (Int, Int),
                             value: T,
                             fileName: Option[String] = None) {
  def transform(newFileName: String, lineNumOffset: Int): Located[T] = {
    Located((loc._1 + lineNumOffset, loc._2), value, Some(newFileName))
  }
}
