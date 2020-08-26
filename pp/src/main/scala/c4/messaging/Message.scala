package c4.messaging

import c4.util.{Loc, LocPoint, LocRange}

sealed abstract class Message {
  // legacy
  def location: (Int, Int)
}

// legacy
final case class SimpleMessage(
    fileName: String,
    loc: (Int, Int),
    message: String
) extends Message {
  override def location: (Int, Int) = loc
  override def toString: String = s"$fileName ${loc._1}:${loc._2}: $message"
}

final case class LocMessage(loc: Loc, msg: String) extends Message {
  override def location: (Int, Int) = {
    loc match {
      case LocPoint(pos, _)              => pos
      case LocRange(LocPoint(pos, _), _) => pos
    }
  }

  override def toString: String = {
    def desc(p: LocPoint): String =
      s"${p.fileName.getOrElse("<Unknown>")} ${p.pos._1}:${p.pos._2}"
    loc match {
      case p: LocPoint    => desc(p)
      case LocRange(x, y) => s"${desc(x)} - ${desc(y)}"
    }
  }
}
