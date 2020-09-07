package c4

import c4.ast.{AST, C4Scanner, C4Parser, ProtoConverter}
import c4.io.{PPReader, SourcePhase7Reader}
import c4.messaging.{IllegalSourceException, Message}

import java.io.FileNotFoundException

import scala.collection.mutable.ArrayBuffer

object Parser {
  def main(args: Array[String]): Unit = {
    var fileName: Option[String] = None
    var printText = false
    var ppOnly = false

    args.foreach {
      case "--help" | "-h"        => printUsage(0)
      case "--text" | "-t"        => printText = true
      case "-E"                   => ppOnly = true
      case s if s.startsWith("-") => printUsage(1)
      case s                      => fileName = Some(s)
    }

    if (fileName.isEmpty) {
      printUsage(1)
    }

    try {
      val warnings: ArrayBuffer[Message] = ArrayBuffer.empty
      if (ppOnly) {
        var prevFileName: Option[String] = None
        var prevLn = 1
        for (tok <- PPReader.read(warnings, fileName.get)) {
          if (tok.loc._1 != prevLn || tok.fileName != prevFileName) {
            System.out.println()
            if (prevLn + 1 < tok.loc._1) {
              System.out.println()
            }
            for (i <- 1 until tok.loc._2) {
              System.out.print(' ')
            }
          }
          prevLn = tok.loc._1
          prevFileName = tok.fileName
          System.out.print(tok.value.raw)
        }
        System.out.println()
      } else {
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
          astProto.writeTo(System.out)
        }
      }
    } catch {
      case e: IllegalSourceException =>
        System.err.println(s"error: ${e.msg}")
        System.exit(1)
      case e: FileNotFoundException =>
        System.err.println(s"error: Cannot find file ${e.getMessage()}")
        System.exit(1)
    }
  }

  def printUsage(status: Int): Unit = {
    val stream = if (status == 0) { System.out }
    else { System.err };
    stream.println("""USAGE: c4 [options] file
        |
        |OPTIONS:
        |  --text | -t    Output AST in textproto format instead of binary
        |  --help | -h    Print help
        |  -E             Only run the preprocessor
        |""".stripMargin)
    System.exit(status)
  }
}
