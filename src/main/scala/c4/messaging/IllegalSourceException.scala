package c4.messaging

import c4.util.Loc

final case class IllegalSourceException(msg: Message) extends RuntimeException {
  def this(loc: Loc, msg: String) {
    this(new LocMessage(loc, msg))
  }
}
