package c4

import c4.ast.{AST, C4Scanner, C4Parser, ProtoConverter}
import c4.io.SourcePhase7Reader
import c4.messaging.{IllegalSourceException, Message}

import java.io.FileNotFoundException

import scala.collection.mutable.ArrayBuffer

object Parser {
  def main(args: Array[String]): Unit = {
    var fileName: Option[String] = None
    var printText = false

    args.foreach {
      case "--help" | "-h" => printUsage(0)
      case "--text" | "-t" => printText = true
      case s if s.startsWith("-") => printUsage(1)
      case s => fileName = Some(s)
    }

    if (fileName.isEmpty) {
      printUsage(1)
    }

    try {
      val warnings: ArrayBuffer[Message] = ArrayBuffer.empty
      val reader = new SourcePhase7Reader(warnings, fileName.get)
      val parser = new C4Parser(new C4Scanner(reader))
      val ast = parser.parse().value.asInstanceOf[AST.TranslationUnit]
      val astProto = ProtoConverter.toProto(ast)

      if (warnings.nonEmpty) {
        for (w <- warnings) {
          println(s"warning: $w")
        }
        System.exit(1)
      }

      if (printText) {
        System.out.print(astProto.toProtoString)
      } else {
        System.out.print(astProto.writeTo(System.out))
      }
    } catch {
      case e: IllegalSourceException =>
        System.err.println(s"error: ${e.msg}")
        System.exit(1)
      case e: FileNotFoundException =>
        System.err.println(s"error: Cannot find file ${fileName.get}")
        System.exit(1)
    }
  }

  def printUsage(status: Int): Unit = {
    val stream = if (status == 0) { System.out } else { System.err };
    stream.println(
      """USAGE: c4 [options] file
        |
        |OPTIONS:
        |  --text | -t    Output AST in textproto format instead of binary
        |  --help | -h    Print help
        |""".stripMargin)
    System.exit(status)
  }
}
