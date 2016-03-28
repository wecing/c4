package c4

import java.io.FileNotFoundException

import c4.ast.CupParser
import c4.ast.C4Scanner
import c4.io.SourcePhase7Reader
import c4.messaging.{IllegalSourceException, Message}

import scala.collection.mutable.ArrayBuffer


object Main {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      System.err.println("usage: c4 FILE")
      System.exit(1)
    }

    try {
      val warnings: ArrayBuffer[Message] = ArrayBuffer.empty
      val reader = new SourcePhase7Reader(warnings, args(0))
      val parser = new CupParser(new C4Scanner(reader))
      parser.parse()
      // var stop = false
      // while (!stop) {
      //   reader.get() match {
      //     case None => stop = true
      //     case Some(t) =>
      //       println(t.value.toString)
      //   }
      // }
      // println()
      for (w <- warnings) {
        println(s"warning $w")
      }
    } catch {
      case e: IllegalSourceException =>
        System.err.println(s"error ${e.msg}")
        System.exit(1)
      case e: FileNotFoundException =>
        System.err.println(s"error: Cannot find file ${args(0)}")
    }
  }
}
