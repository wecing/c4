package c4

import java.io.FileNotFoundException

import c4.io.PPLineReader
import c4.messaging.{Message, IllegalSourceException}

import scala.collection.mutable.ArrayBuffer


object Main {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      System.err.println("usage: c4 FILE")
      System.exit(1)
    }

    var reader: PPLineReader = null

    try {
      val warnings: ArrayBuffer[Message] = ArrayBuffer.empty
      reader = new PPLineReader(warnings, args(0))
      var eof = false
      while (!eof) {
        reader.read() match {
          case None => eof = true
          case Some(line) => println(line)
        }
      }
      for (w <- warnings) {
        println(s"warning $w")
      }
    } catch {
      case e: IllegalSourceException =>
        System.err.println(s"error ${e.msg}")
        System.exit(1)
      case e: FileNotFoundException =>
        System.err.println(s"error: Cannot find file ${args(0)}")
    } finally {
      if (reader != null) {
        reader.close()
      }
    }
  }
}
