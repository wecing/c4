package c4.messaging

import c4.util.Loc

object IllegalSourceException {
  def apply(loc: Loc, msg: String) = new IllegalSourceException(loc, msg)
}

final case class IllegalSourceException(msg: Message)
    extends RuntimeException(msg.toString()) {
  def this(loc: Loc, msg: String) {
    this(new LocMessage(loc, msg))
  }
}
