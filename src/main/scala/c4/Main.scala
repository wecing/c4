package c4

import java.io.FileNotFoundException

import c4.io.SourcePhase2Reader
import c4.messaging.{Message, IllegalSourceException}
import c4.util.CharRepr

import scala.collection.mutable.ArrayBuffer

// phase 3:
//    preprocessing-token:
//      header-name
//      identifier
//      pp-number
//      character-constant
//      string-literal
//      operator
//      punctuator
//      (each other non-white-space characters)
//      (each white-space characters)


object Main {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      System.err.println("usage: c4 FILE")
      System.exit(1)
    }

    var reader: SourcePhase2Reader = null

    try {
      val warnings: ArrayBuffer[Message] = ArrayBuffer.empty
      reader = new SourcePhase2Reader(warnings, args(0))
      var eof = false
      while (!eof) {
        val r: Option[(Char, (Int, Int))] = reader.read()
        r match {
          case None => eof = true
          case Some((c: Char, (line, col))) =>
            println(s"\t${CharRepr(c)}\t($line, $col)")
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
