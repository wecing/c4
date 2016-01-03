package c4

object Main {
  def main(args: Array[String]): Unit = {
    println(s"args = <${args.deep.mkString(" ")}>")
  }
}
