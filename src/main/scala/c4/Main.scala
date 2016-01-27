package c4

import java.io.FileNotFoundException

import c4.io.{PPTok, PPReader}
import c4.messaging.{Message, IllegalSourceException}
import c4.util.Located

import scala.collection.mutable.ArrayBuffer


object Main {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      System.err.println("usage: c4 FILE")
      System.exit(1)
    }

    try {
      val warnings: ArrayBuffer[Message] = ArrayBuffer.empty
      val tokens: Seq[Located[PPTok]] = PPReader.read(warnings, args(0))
      var prevLineNum: Int = 0
      for (t <- tokens) {
        if (t.loc._1 != prevLineNum) {
          println()
          prevLineNum = t.loc._1
        }
        print(t.value.raw)
      }
      println()
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
