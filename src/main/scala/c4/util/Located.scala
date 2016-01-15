package c4.util

final case class Located[+T](loc: (Int, Int), value: T)
