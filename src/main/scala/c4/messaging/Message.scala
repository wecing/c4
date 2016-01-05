package c4.messaging

sealed abstract class Message {
  def location: (Int, Int)
}

final case class SimpleMessage(fileName: String,
                               loc: (Int, Int),
                               message: String) extends Message {
  override def location: (Int, Int) = loc
  override def toString: String = s"$fileName ${loc._1}:${loc._2}: $message"
}
